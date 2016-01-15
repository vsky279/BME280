-- ***************************************************************************
-- 64-bit and 96-bit floating point arithmetics
--
-- Written by Lukas Voborsky, @voborsky
--
-- MIT license, http://opensource.org/licenses/MIT
-- ***************************************************************************

return({
fmt = function(self, H, L, E)
	local r = string.format("%8X", H).." "..string.format("%8X", L)
	if E then r=r.." *2^"..E end
	return r
end,

shr = function(self, aH, aL, n, aE)
	if n > 31 then
		aL = bit.arshift(aH, n-32)
		aH = bit.arshift(aH, 31)
	else if n>0 then
		aL = bit.bor(bit.rshift(aL, n), bit.lshift(aH, 32-n))
		aH = bit.arshift(aH, n)
	end end
	if aE then aE = aE + n end
	return aH, aL, aE
end,

shl = function(self, aH, aL, n, aE)
	if n > 31 then
		aH = bit.lshift(aL, n-32)
		aL = 0
	else if n>0 then
		aH = bit.bor(bit.lshift(aH, n), bit.rshift(aL, 32-n))
		aL = bit.lshift(aL, n)
	end end
	if aE then aE = aE - n end
	return aH, aL, aE
end,

nlz = function (self, x)
	local y
	local n=32
	local c=16
	repeat
		y = bit.rshift(x, c)
		if y~=0 then n=n-c; x=y end
		c=bit.rshift(c,1)
	until c==0
	return n-x
end,

add64 = function (self, aH, aL, bH, bL, aE, bE)
	if aE and bE and aE<bE then -- for floating point numbers swap a and b so that aE is bE
		local tH, tL, tE
		tH, tL, tE = aH, aL, aE
		aH, aL, aE = bH, bL, bE
		bH, bL, bE = tH, tL, tE
	end
	--print("a="..self:fmt(aH, aL, aE), "b="..self:fmt(bH, bL, bE))
	if aE and bE and aE>bE then
		local c=aE-bE
		local aZ=self:nlz(aH); if aZ==32 then aZ=aZ+self:nlz(aL) end
		if aZ>0 then aZ=aZ-1 end
		if c>aZ then c=aZ end
		aH, aL, aE = self:shl(aH, aL, c, aE)
		if aE>bE then bH, bL, bE = self:shr(bH, bL, aE-bE) end
	end
	--print("a="..self:fmt(aH, aL, aE), "b="..self:fmt(bH, bL, bE))

	local a1 = bit.clear(aL, 31) --bit.band(aL, 0x7FFFFFFF)
	local b1 = bit.clear(bL, 31) --bit.band(bL, 0x7FFFFFFF)
	local rH = aH + bH + ((bit.rshift(a1+b1, 31)+bit.rshift(aL, 31)+bit.rshift(bL, 31))>1 and 1 or 0)
	local rL = aL + bL

	if aE then
		return self:red(rH, rL, aE)
	else
		return rH, rL
	end
end,

-- Hacker's Delight 2nd Edition - 8–1 Multiword Multiplication
-- but can't use method 3 for signed multiplication, method 1 is more appropriate
mult64 = function (self, aH, aL, bH, bL, aE, bE)
	--print("a="..self:fmt(aH, aL), "b="..self:fmt(bH, bL))
	local aN = bit.isset(aH, 31) and 1 or 0
	local bN = bit.isset(bH, 31) and 1 or 0
	if aN == 1 then aH, aL = bit.bnot(aH), bit.bnot(aL) + 1 end
	if bN == 1 then bH, bL = bit.bnot(bH), bit.bnot(bL) + 1 end
	--print("a="..self:fmt(aH, aL), "b="..self:fmt(bH, bL))
	
	local u = {bit.band(aL, 0xFFFF), bit.rshift(aL,16), bit.band(aH, 0xFFFF), bit.rshift(aH,16)}
	local v = {bit.band(bL, 0xFFFF), bit.rshift(bL,16), bit.band(bH, 0xFFFF), bit.rshift(bH,16)}
  
	local w={}; local i; local j
	for i=1,8 do w[i]=0 end
 
	for j=1,4 do
		local k=0
		for i=1,4 do
			local t = u[i] * v[j] + w[i+j-1] + k
			w[i+j-1] = bit.band(t, 0xFFFF)
			k = bit.rshift(t, 16)
		end
		w[j+4] = k
	end

	--for i=1,8 do print("w["..i.."]="..string.format("%4X", w[i])) end

	-- deal with overflow
	local ovfl = self:nlz(bit.bor(bit.lshift(w[8], 16), w[7]))
	if ovfl==32 then ovfl = ovfl + self:nlz(bit.bor(bit.lshift(w[6], 16), w[5])) end
	ovfl = 4 - ovfl / 16
	local rH, rL
	rH, rL = bit.bor(bit.lshift(w[4 + ovfl], 16), w[3 + ovfl]), bit.bor(bit.lshift(w[2 + ovfl], 16), w[1 + ovfl])
	ovfl = ovfl * 16
	if bit.isset(rH, 31) then --save 1 bit for the sign
		ovfl = ovfl + 1
		rH, rL = self:shr(rH, rL, 1)
		rH=bit.clear(rH, 31)
	end

	if aN+bN == 1 then rH, rL = bit.bnot(rH), bit.bnot(rL) + 1 end

	if aE and bE then -- we're dealing with floating point
		return self:red(rH, rL, aE + bE + ovfl)
	else
		return rH, rL, 0
	end
end,

-- Hacker's Delight 2nd Edition - 9–2 Multiword Division
div64 = function (self, aH, aL, bH, bL, aE, bE)
    local aN = bit.isset(aH, 31) and 1 or 0
    local bN = bit.isset(bH, 31) and 1 or 0
    if aN == 1 then aH, aL = bit.bnot(aH), bit.bnot(aL) + 1 end
    if bN == 1 then bH, bL = bit.bnot(bH), bit.bnot(bL) + 1 end

    if aE then
        local n=self:nlz(aH, aL)
        if n>1 then aH, aL, aE=self:shl(aH, aL, n-1, aE) end
    end

	local u = {bit.band(aL, 0xFFFF), bit.rshift(aL,16), bit.band(aH, 0xFFFF), bit.rshift(aH,16)}
	local v = {bit.band(bL, 0xFFFF), bit.rshift(bL,16), bit.band(bH, 0xFFFF), bit.rshift(bH,16)}

	local qhat, rhat, p, s, i, j, t, k
	local q={}
	local r={}

	--print("u="..string.format("%5X",u[1])..string.format("%5X",u[2])..string.format("%5X",u[3])..string.format("%5X",u[4]))
	--print("v="..string.format("%5X",v[1])..string.format("%5X",v[2])..string.format("%5X",v[3])..string.format("%5X",v[4]))
	local m = 4; while u[m]==0 do m=m-1 end
	local n = 4; while v[n]==0 do n=n-1 end
	--print("m=", m)
	--print("n=", n)
	
	if m<n or n<=0 or v[n]==0 then
		return nil, nil, nil
	end

	for i=1,4 do if not q[i] then q[i]=0 end end
	if n==1 then -- Take care of the case of a single-digit divisor here.
		k = 0
		for j = m,1,-1 do
			--q[j] = bit.band((k*0x10000 + u[j])/v[1], 0xFFFF)
			q[j] = bit.band(bit.lshift(bit.rshift(k*0x10000 + u[j],1)/v[1],1), 0xFFFF)
			if q[j]>0 and q[j]*v[1]>k*0x10000 + u[j] then q[j]=q[j]-1 end -- correction for not having 32bit unsigned type
			k = (k*0x10000 + u[j]) - q[j]*v[1]
		end
		r[1] = bit.band(k, 0xFFFF);
		for i=1,4 do if not q[i] then q[i]=0 end end
        local rH, rL
        rH, rL = bit.bor(bit.lshift(q[4], 16), q[3]), bit.bor(bit.lshift(q[2], 16), q[1])
        if aN+bN == 1 then rH, rL = bit.bnot(rH), bit.bnot(rL) + 1 end
        if aE and bE then
            return self:red(rH, rL, aE - bE)
        else
            return rH, rL, 0
        end
	end
	-- Normalize by shifting v left just enough so that
	-- its high-order bit is on, and shift u left the
	-- same amount. We may have to append a high-order
	-- digit on the dividend; we do that unconditionally.
	
	for i=1,m do q[i]=0 end
	s = self:nlz(v[n]) - 16 -- 0 <= s <= 16.
	local vn={}
	for i=n,2,-1 do
		vn[i] = bit.band(bit.bor(bit.lshift(v[i], s), bit.arshift(v[i-1], 16-s)), 0xFFFF)
	end
	vn[1] = bit.band(bit.lshift(v[1], s), 0xFFFF)
	--print("vn="..string.format("%5X",vn[1])..string.format("%5X",vn[2]))--..string.format("%5X",vn[3])..string.format("%5X",vn[4]))
	
	local un={}
	un[m+1] = bit.band(bit.arshift(u[m], 16-s), 0xFFFF)
	for i=m,2,-1 do
		un[i] = bit.band(bit.bor(bit.lshift(u[i], s), bit.arshift(u[i-1], 16-s)), 0xFFFF)
	end
	un[1] = bit.band(bit.lshift(u[1], s), 0xFFFF)
	--print("un="..string.format("%5X",un[1])..string.format("%5X",un[2])..string.format("%5X",un[3])..string.format("%5X",un[4])..string.format("%5X",un[5]))

	for j = m-n+1,1,-1 do
		--print("j=",j)
		-- Compute estimate qhat of q[j].
		qhat = bit.rshift(un[j+n]*0x10000 + un[j+n-1],1)/bit.rshift(vn[n],1)
		if qhat>0 and qhat*v[n]>un[j+n]*0x10000 + un[j+n-1] then qhat=qhat-1 end -- correction for not having 32bit unsigned type

		rhat = (un[j+n]*0x10000 + un[j+n-1]) - qhat*vn[n]
		--print(string.format("%5X",qhat), string.format("%5X",rhat), string.format("%5X",un[j+n]), string.format("%5X",un[j+n-1]), string.format("%5X",vn[n]))
		repeat
			local again=false
			--print("qhat=", string.format("%5X",qhat), "rhat=", string.format("%5X",rhat), "qhat*vn[n-1]=", qhat*vn[n-1], "0x10000*rhat + un[j+n-1])=",0x10000*rhat + un[j+n-1])
			if (qhat >= 0x10000 or (bit.rshift(qhat*vn[n-1],1) > bit.rshift(0x10000*rhat + un[j+n-1],1)) -- correction for not having 32bit unsigned type
			or qhat*vn[n-1]-0x10000*rhat + un[j+n-1] == 1) then 
				qhat = qhat - 1;
				rhat = rhat + vn[n];
				if (rhat < 0x10000) then again=true end
			end
		until not again
		--print("qhat=", string.format("%5X",qhat), "rhat=", string.format("%5X",rhat))
		
		-- Multiply and subtract.
		k = 0
		for i=1,n do
			p = qhat*vn[i];
			t = un[i+j-1] - k - bit.band(p, 0xFFFF)
			un[i+j-1] = bit.band(t, 0xFFFF)
			k = bit.rshift(p, 16) - bit.rshift(t, 16)
			--print(p, t, k)
		end
		t = un[j+n] - k
		un[j+n] = bit.band(t, 0xFFFF)
		--print("un="..string.format("%5X",un[1])..string.format("%5X",un[2])..string.format("%5X",un[3])..string.format("%5X",un[4])..string.format("%5X",un[5]))
	
		q[j] = qhat -- Store quotient digit.
		--print("q[j]=",qhat)
		if (t < 0) then -- If we subtracted too
			q[j] = q[j] - 1 -- much, add back.
			k = 0
			for i=1,n do
				t = un[i+j-1] + vn[i] + k
				un[i+j-1] = bit.band(t, 0xFFFF)
				k = bit.rshift(t, 16)
			end
			un[j+n+1] = bit.band(un[j+n+1] + k, 0xFFFF)
		 end
	end
	
	-- If the caller wants the remainder, unnormalize
	-- it and pass it back.
	-- for i=1,n do
		-- r[i] = bit.bor(bit.rshift(un[i], s), bit.lshift(un[i + 1], 16-s))
	-- end
	for i=1,4 do if not q[i] then q[i]=0 end end

	local rH, rL
	rH, rL = bit.bor(bit.lshift(q[4], 16), q[3]), bit.bor(bit.lshift(q[2], 16), q[1])
    if aN+bN == 1 then rH, rL = bit.bnot(rH), bit.bnot(rL) + 1 end
    
	if aE and bE then
		return self:red(rH, rL, aE - bE)
	else
		return rH, rL, 0
	end
end,
	
red = function(self, aH, aL, aE) -- reduce floating number = remove 0 from right and adjust exponent accordingly
	-- calculate number of trailing zeros
	local yH, yL
	local xH = aH
	local xL = aL
	local n=64
	local c=32
	--print("n="..n, "c="..c, "x="..self:fmt(xH, xL))
	repeat
		--y = bit.lshift(x, c)
		if c > 31 then
			yH = bit.rshift(xL, c-32)
			yL = 0
		else
			yH = bit.bor(bit.lshift(xH, c), bit.rshift(xL, 32-c))
			yL = bit.lshift(xL, c)
		end
		if yH~=0 or yL~=0 then n=n-c; xH=yH; xL=yL end
		c=bit.rshift(c,1)
		--print("n="..n, "c="..c, "x="..self:fmt(xH, xL), "y="..self:fmt(yH, yL))
	until c==0

	return self:shr(aH, aL, n-1, aE)
end,

ln = function (self, kH, kL, kE) -- -log of 1-x
	local ksH, ksL, ksE, rH, rL, rE, kdH, kdL, kdE
	ksH = kH; ksL= kL; ksE = kE
	rH = kH; rL=kL; rE=kE
	for i=2,11 do
		--print("-----------------------------")
		--print("k="..self:fmt(kH, kL, kE))
		--print("ks="..self:fmt(ksH, ksL, ksE))
		tmr.wdclr() -- call this to pat the (watch)dog!
		ksH, ksL, ksE = self:mult64(kH, kL, ksH, ksL, kE, ksE)
		--print("ks="..self:fmt(ksH, ksL, ksE))
		kdH, kdL, kdE = self:div64(ksH, ksL, 0, i, ksE, 0)
		--print("kd="..self:fmt(kdH, kdL, kdE))
		rH, rL, rE = self:add64(rH, rL, kdH, kdL, rE, kdE)
		--print("r="..self:fmt(rH, rL, rE))
	end
	--rH, rL = bit.bnot(rH), bit.bnot(rL) + 1
	return rH, rL, rE
end,

exp = function (self, kH, kL, kE) -- 
	local ksH, ksL, ksE, rH, rL, rE, kdH, kdL, kdE, f
	ksH = kH; ksL= kL; ksE = kE
	--print("k="..self:fmt(kH, kL, kE))
	rH, rL, rE = self:add64(kH, kL, 0, 1, kE, 0)
	f=1
	--print("r="..self:fmt(rH, rL, rE))
	
	for i=2,11 do
		tmr.wdclr() -- call this to pat the (watch)dog!
		f=f*i -- factorial
		--print("ks="..self:fmt(ksH, ksL, ksE), "f="..f)
		ksH, ksL, ksE = self:mult64(kH, kL, ksH, ksL, kE, ksE)
		--print("ks="..self:fmt(ksH, ksL, ksE), "f="..f)
		kdH, kdL, kdE = self:div64(ksH, ksL, 0, f, ksE, 0)
		--print("kd="..self:fmt(kdH, kdL, kdE))
		rH, rL, rE = self:add64(rH, rL, kdH, kdL, rE, kdE)
		--print("r="..self:fmt(rH, rL, rE))
	end
	--rH, rL = bit.bnot(rH), bit.bnot(rL) + 1
	return rH, rL, rE
end
})
