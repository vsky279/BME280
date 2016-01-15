-- ***************************************************************************
-- BMP280 module for ESP8266 with nodeMCU
--
-- Written by Lukas Voborsky, @voborsky
--
-- MIT license, http://opensource.org/licenses/MIT
-- ***************************************************************************

return({
scl=4,
sda=3,
addr=nil,
alt = 0,
hH = 0,
hL = 1,
hE = 0,
dig_T = {},
dig_P = {},
dig_H = {},

readReg = function(self, reg, size, signed, LE, valw)
	size = size or 2
	LE = LE and true or false
	
	i2c.start(0) --beginTransmission((uint8_t)_i2caddr);
	i2c.address(0, self.addr, i2c.TRANSMITTER)
	i2c.write(0, reg);
	if valw then i2c.write(0, valw) end
	i2c.stop(0);

	if not size or size == 0 then return nil end
	
	i2c.start(0) --beginTransmission((uint8_t)_i2caddr);
	i2c.address(0, self.addr, i2c.RECEIVER)
	local data=i2c.read(0, size) --requestFrom((uint8_t)_i2caddr, (byte)2);
	i2c.stop(0) --endTransmission();

	local i
	local value = 0
	for i=LE and size or 1, LE and 1 or size, LE and -1 or 1 do
		value = bit.bor(bit.lshift(value, 8), data:byte(i))
	end

	if signed and (value > 0x7fff) then
		value = value - 0x10000
	end

	return value
end,

init = function(self, pscl, psda)
	local BMP280_REGISTER_CONTROL = 0xF4
	local BMP280_REGISTER_CONTROL_HUM = 0xF2
	--local BMP280_REGISTER_CONFIG = 0xF5
	local BMP280_REGISTER_DIG_T = 0x88
	local BMP280_REGISTER_DIG_P = 0x8E
	local BMP280_REGISTER_DIG_H1 = 0xA1
	local BMP280_REGISTER_DIG_H2 = 0xE1
	local BMP280_REGISTER_CHIPID = 0xD0
	--local BMP280_REGISTER_VERSION = 0xD1
	--local BMP280_REGISTER_SOFTRESET = 0xE0
	--local BMP280_REGISTER_CAL26 = 0xE1

	tmr.wdclr()
	if pscl then self.scl=pscl end
	if psda then self.sda=psda end

	print("Checking sda/scl to GPIO - IO index "..self.sda.."/"..self.scl)
	i2c.setup(0, self.sda, self.scl, i2c.SLOW)

	local tp
	for i=0,127 do
		i2c.start(0)
		local c=i2c.address(0, i, i2c.TRANSMITTER)
		i2c.stop(0)
		if c then
			self.addr=i
			local chipid = self:readReg(BMP280_REGISTER_CHIPID, 1)
			if chipid == 0x60 then tp="BME280" end
			if chipid == 0x58 then tp="BMP280" end
			if tp then
				print("Device "..tp.." found at address 0x"..string.format("%02X", i))
				break
			end
		end
	end
	if not tp then return end

	self.dig_T[1] = self:readReg(BMP280_REGISTER_DIG_T, 2, false, true) -- not signed, LE
	for i=2, 3 do self.dig_T[i] = self:readReg(BMP280_REGISTER_DIG_T + (i-1)*2, 2, true, true) end --signed, LE

	self.dig_P[1] = self:readReg(BMP280_REGISTER_DIG_P, 2, false, true) -- not signed, LE
	for i=2, 9 do self.dig_P[i] = self:readReg(BMP280_REGISTER_DIG_P + (i-1)*2, 2, true, true) end -- signed, LE
	
	if tp=="BME280" then
		self.dig_H[1] = self:readReg(BMP280_REGISTER_DIG_H1, 1) -- char
		self.dig_H[2] = self:readReg(BMP280_REGISTER_DIG_H2, 2, true, true) -- signed, LE
		self.dig_H[3] = self:readReg(BMP280_REGISTER_DIG_H2 + 2, 1) -- char
		local dH = self:readReg(BMP280_REGISTER_DIG_H2 + 3, 2, true, false) -- signed, BE
		self.dig_H[4] = bit.arshift(bit.band(dH, 0xFF00), 4) + bit.band(dH, 0xF)
		self.dig_H[5] =  bit.arshift(self:readReg(BMP280_REGISTER_DIG_H2 + 5, 2, true, true), 4) -- signed, LE
		self.dig_H[6] = self:readReg(BMP280_REGISTER_DIG_H2 + 7, 1) -- signed, char
		if bit.isset(self.dig_H[6], 7) then self.dig_H[6]=self.dig_H[6] - 0x100 end -- signed
	end
	
	--print(self.dig_T[1], self.dig_T[2], self.dig_T[3], self.dig_P[1], self.dig_P[2], self.dig_P[3], self.dig_P[4], self.dig_P[5], self.dig_P[6], self.dig_P[7], self.dig_P[8], self.dig_P[9], self.dig_H[1], self.dig_H[2], self.dig_H[3], self.dig_H[4], self.dig_H[5], self.dig_H[6])

	self:readReg(BMP280_REGISTER_CONTROL_HUM, 0, nil, nil, 0x05);
	self:readReg(BMP280_REGISTER_CONTROL, 0, nil, nil, 0x3F);

	return self.addr
end,

readAdc = function(self, meter)
	local BMP280_REGISTER_DATA = {0xFA, 0xF7, 0xFD} -- temperature, pressure, humidity
	local raw = self:readReg(BMP280_REGISTER_DATA[meter], meter == 3 and 2 or 3)
	if meter~= 3 then raw = bit.rshift(raw, 4) end
	if raw==0x80000 then return nil else return raw end
end,

compensateT = function(self, adc_T)
	--var1  = ((((adc_T>>3) – ((BME280_S32_t)self.dig_T1<<1))) * ((BME280_S32_t)self.dig_T2)) >> 11;
	local var1  = bit.arshift((bit.arshift(adc_T,3) - bit.lshift(self.dig_T[1],1)) * self.dig_T[2], 11)
	--var2  = (((((adc_T>>4) – ((BME280_S32_t)self.dig_T1)) * ((adc_T>>4) – ((BME280_S32_t)self.dig_T1))) >> 12) *  
	--	  ((BME280_S32_t)self.dig_T3)) >> 14; 
	local var2  = bit.arshift(
		bit.arshift(
			(bit.arshift(adc_T,4) - self.dig_T[1]) * (bit.arshift(adc_T,4) - self.dig_T[1])
		, 12) * self.dig_T[3]
	, 14) 
	local t_fine = var1 + var2
	local T = bit.arshift(t_fine * 5 + 128, 8)
	return T, t_fine
end,

compensateP = function(self, adc_P, t_fine)
	-- // Returns pressure in Pa as unsigned 32 bit integer in Q24.8 format (24 integer bits and 8 fractional bits). 
	-- // Output value of “24674867” represents 24674867/256 = 96386.2 Pa = 963.862 hPa 
	local a=require("math64")

	-- var1 = ((BME280_S64_t)t_fine) – 128000;
	local var1L = t_fine - 128000
	local var1H = bit.isset(var1L, 31) and -1 or 0

	-- var2 = var1 * var1 * (BME280_S64_t)self.dig_P[6];
	local var2H, var2L
	var2H, var2L = a:mult64(var1H, var1L, var1H, var1L)
	var2H, var2L = a:mult64(var2H, var2L, bit.isset(self.dig_P[6],31) and -1 or 0, self.dig_P[6])

	-- var2 = var2 + ((var1*(BME280_S64_t)self.dig_P[5])<<17);
	do
		local var1P5H, var1P5L
		var1P5H, var1P5L = a:mult64(var1H, var1L, bit.isset(self.dig_P[5],31) and -1 or 0, self.dig_P[5])
		var1P5H, var1P5L = a:shl(var1P5H, var1P5L, 17)
		var2H, var2L = a:add64(var2H, var2L, var1P5H, var1P5L)
	end

	-- var2 = var2 + (((BME280_S64_t)self.dig_P[4])<<35);
	var2H = var2H + bit.lshift(self.dig_P[4], 3) --bit.lshift(self.dig_P[4], 35)

	-- var1 = ((var1 * var1 * (BME280_S64_t)self.dig_P[3])>>8) + ((var1 * (BME280_S64_t)self.dig_P[2])<<12);
	do
		local var1_2H, var1_2L 
		var1_2H, var1_2L = a:mult64(var1H, var1L, bit.isset(self.dig_P[2],31) and -1 or 0, self.dig_P[2])
		var1_2H, var1_2L = a:shl(var1_2H, var1_2L, 12)
		
		local var1_1H, var1_1L
		var1_1H, var1_1L = a:mult64(var1H, var1L, var1H, var1L)
		var1_1H, var1_1L = a:mult64(var1_1H, var1_1L, bit.isset(self.dig_P[3],31) and -1 or 0, self.dig_P[3])
		var1_1H, var1_1L = a:shr(var1_1H, var1_1L, 8)
		var1H, var1L = a:add64(var1_1H, var1_1L, var1_2H, var1_2L)
	end

	-- var1 = (((((BME280_S64_t)1)<<47)+var1))*((BME280_S64_t)self.dig_P[1])>>33;
	var1H, var1L = a:add64(var1H, var1L, 0x8000, 0)
	var1H, var1L = a:mult64(var1H, var1L, bit.isset(self.dig_P[1],31) and -1 or 0, self.dig_P[1])
	var1H, var1L = a:shr(var1H, var1L, 33)
	
	local pL
	if var1H ~= 0 or var1L~=0 then
		pL = 1048576 - adc_P
		local pH = 0
		--p = (((p<<31)-var2)*3125)/var1;
		pH = bit.arshift(pL, 1) -- lshift 31 - 32
		pL = bit.lshift(pL, 31)
		pH, pL = a:add64(pH, pL, bit.bnot(var2H), bit.bnot(var2L) + 1)
		pH, pL = a:mult64(pH, pL, 0, 3125)
		pH, pL = a:div64(pH, pL, var1H, var1L)

		--var1 = (((BME280_S64_t)self.dig_P[9]) * (p>>13) * (p>>13)) >> 25;
		do
			local p13H, p13L
			p13H, p13L = a:shr(pH, pL, 13)
			p13H, p13L = a:mult64(p13H, p13L, p13H, p13L)
			p13H, p13L = a:mult64(p13H, p13L, bit.isset(self.dig_P[9],31) and -1 or 0, self.dig_P[9])
			var1H, var1L = a:shr(p13H, p13L, 25)
		end

		--var2 = (((BME280_S64_t)self.dig_P[8]) * p) >> 19; 
		var2H, var2L = a:mult64(pH, pL, bit.isset(self.dig_P[8],31) and -1 or 0, self.dig_P[8])
		var2H, var2L = a:shr(var2H, var2L, 19)

		--p = ((p + var1 + var2) >> 8) + (((BME280_S64_t)self.dig_P[7])<<4); 
		pH, pL = a:add64(pH, pL, var1H, var1L)
		pH, pL = a:add64(pH, pL, var2H, var2L)
		pH, pL = a:shr(pH, pL, 8)
		local p7L = bit.lshift(self.dig_P[7], 4)
		local p7H = bit.arshift(self.dig_P[7], 32-4)
		pH, pL = a:add64(pH, pL, p7H, p7L)

		pL=bit.rshift(pL*10, 8) --
	end
	return pL
end,

compensateH = function(self, adc_H, t_fine)
	local a=require("math64")
	
	-- v_x1_u32r = (t_fine – ((BME280_S32_t)76800)); 
	local v_x1L = t_fine - 76800;
	local v_x1H = bit.isset(v_x1L, 31) and -1 or 0
	
	-- v_x1_u32r = (((((adc_H << 14) – (((BME280_S32_t)dig_H4) << 20) – (((BME280_S32_t)dig_H5) * v_x1_u32r)) + 
		-- ((BME280_S32_t)16384)) >> 15) * (((((((v_x1_u32r * ((BME280_S32_t)dig_H6)) >> 10) * (((v_x1_u32r * 
		-- ((BME280_S32_t)dig_H3)) >> 11) + ((BME280_S32_t)32768))) >> 10) + ((BME280_S32_t)2097152)) * 
		-- ((BME280_S32_t)dig_H2) + 8192) >> 14)); 
	-- (((adc_H << 14) – (((BME280_S32_t)dig_H4) << 20) – (((BME280_S32_t)dig_H5) * v_x1_u32r)) + ((BME280_S32_t)16384)) >> 15)
	local aH, aL
	do
		local a2H, a2L
		aH, aL = a:shl(0, adc_H, 14)
		a2H, a2L = a:shl(bit.isset(self.dig_H[4], 31) and 0 or -1, -self.dig_H[4], 20)
		aH, aL = a:add64(aH, aL, a2H, a2L)
		a2H, a2L = a:mult64(bit.isset(self.dig_H[5], 31) and -1 or 0, self.dig_H[5], bit.bnot(v_x1H), -v_x1L) -- -v_x1 so we can use directly in subtraction
		aH, aL = a:add64(aH, aL, a2H, a2L)
	end
	aH, aL = a:add64(aH, aL, 0, 16384)
	aH, aL = a:shr(aH, aL, 15)
	
	--(((((((v_x1_u32r * ((BME280_S32_t)dig_H6)) >> 10) * 
		-- (((v_x1_u32r * 
		-- ((BME280_S32_t)dig_H3)) >> 11) + ((BME280_S32_t)32768))) >> 10) + ((BME280_S32_t)2097152)) * 
		-- ((BME280_S32_t)dig_H2) + 8192) >> 14)
	local bH, bL
	do
		local b2H, b2L
		bH, bL = a:mult64(v_x1H, v_x1L, bit.isset(self.dig_H[6], 31) and -1 or 0, self.dig_H[6])
		bH, bL = a:shr(bH, bL, 10)
		
		b2H, b2L = a:mult64(v_x1H, v_x1L, bit.isset(self.dig_H[3], 31) and -1 or 0, self.dig_H[3])
		b2H, b2L = a:shr(b2H, b2L, 11)
		b2H, b2L = a:add64(b2H, b2L, 0, 32768)
		bH, bL = a:mult64(bH, bL, b2H, b2L)
	end
	bH, bL = a:shr(bH, bL, 10)
	bH, bL = a:add64(bH, bL, 0, 2097152)
	bH, bL = a:mult64(bH, bL, bit.isset(self.dig_H[2], 31) and -1 or 0, self.dig_H[2])
	bH, bL = a:add64(bH, bL, 0, 8192)
	bH, bL = a:shr(bH, bL, 14)
	
	v_x1H, v_x1L = a:mult64(aH, aL, bH, bL)

	-- v_x1_u32r = (v_x1_u32r – (((((v_x1_u32r >> 15) * (v_x1_u32r >> 15)) >> 7) * ((BME280_S32_t)dig_H1)) >> 4));
	do
		local v_x12H, v_x12L
		v_x12H, v_x12L = a:shr(v_x1H, v_x1L, 15)
		v_x12H, v_x12L = a:mult64(v_x12H, v_x12L, v_x12H, v_x12L)
		v_x12H, v_x12L = a:shr(v_x12H, v_x12L, 7)
		v_x12H, v_x12L = a:mult64(v_x12H, v_x12L, bit.isset(self.dig_H[1], 31) and 0 or -1, -self.dig_H[1])
		v_x12H, v_x12L = a:shr(v_x12H, v_x12L, 4)
		v_x1H, v_x1L = a:add64(v_x1H, v_x1L, v_x12H, v_x12L)
	end
	
	-- v_x1_u32r = (v_x1_u32r < 0 ? 0 : v_x1_u32r);
	if bit.isset(v_x1H, 31) then v_x1L = 0 end
	-- v_x1_u32r = (v_x1_u32r > 419430400 ? 419430400 : v_x1_u32r);   
	if bit.rshift(v_x1L, 1) > 0x19000000 then v_x1L = 0x19000000 end
	-- v_x1H has further undefined value and can't be used
	
	-- return (BME280_U32_t)(v_x1_u32r>>12); 
	v_x1L = bit.rshift(v_x1L, 12)
	return bit.arshift(v_x1L * 1000, 10)
 end,

qfe2qnh = function(self, p, pAlt)
	if pAlt~=self.alt then
		self:altCal(pAlt)
	end

	local a=require("math64")
	local pH, pE, pL
	pH, pL, pE = a:mult64(0, p, self.hH, self.hL, 0, self.hE)
	pH, pL, pE = a:shr(pH, pL, -pE, pE)

	return pL
end,

altCal = function(self, pAlt)
	local a=require("math64")
	local kH=0xB -- 0.0000225577
	local kL=0xD3A4AB91
	local kE=-51
	local cH = 0x15060 -- 5.25588
	local cL=0x5681ECD5
	local cE=-46

	local rH, rL, rE
	print("Calculating altitude correction coefficient for alt "..pAlt.." m")
	rH, rL, rE = a:mult64(0, pAlt, kH, kL, 0, kE)

	--ln(1-(1-r))=ln(r)
	rH, rL, rE = a:ln(rH, rL, rE) 
	rH, rL, rE = a:mult64(rH, rL, cH, cL, rE, cE)
	rH, rL, rE = a:exp(rH, rL, rE) 

	self.hH, self.hL, self.hE = rH, rL, rE
	self.alt = pAlt

	return rH, rL, rE
end,

altitude = function(self, P, P0)
	local a=require("math64")
	local cH=0x30B51 -- 1/5.25588
	local cL=0x50E22871
	local cE=-52
	local kH=0x10E92D -- 1/0.0000225577 * 100
	local kL=0x044C38E6
	local kE=-30

	local rH, rL, rE
	rH, rL, rE = a:div64(P0-P, 0, 0, P0, -32, 0)

	--ln(1-(1-r))=ln(r)
	rH, rL, rE = a:ln(rH, rL, rE) 
	rH, rL = bit.bnot(rH), bit.bnot(rL) + 1
	rH, rL, rE = a:mult64(rH, rL, cH, cL, rE, cE)
	rH, rL, rE = a:exp(rH, rL, rE) 
	rH, rL, rE = a:add64(0, 1, bit.bnot(rH), bit.bnot(rL) + 1, 0, rE) 
	rH, rL, rE = a:mult64(rH, rL, kH, kL, rE, kE)

	rH, rL = a:shr(rH, rL, -rE)
	return rL
end
})
