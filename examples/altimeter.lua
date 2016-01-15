alt = 320

local QNH=0

function initialize()
	d:init()
	local adc_T = d:readAdc(1)
	local adc_P = d:readAdc(2)

	if not adc_T or not adc_P then return nil end
	
	print(string.format("adc_T=%X\tadc_P=%X", adc_T, adc_P))
	
	local T, t_fine
	T, t_fine = d:compensateT(adc_T)
	print((string.gsub(string.format("T=%d.%2d", T/100, T%100), " ", "0")), "t_fine="..t_fine)
	
	local P = d:compensateP(adc_P, t_fine)
	print((string.gsub(string.format("QFE=%d.%3d", P/1000, P%1000), " ", "0")))
	d:altCal(alt)
	QNH = d:qfe2qnh(P, alt)
	print((string.gsub(string.format("QNH=%d.%3d", QNH/1000, QNH%1000), " ", "0")))

	return QNH
end

function altitude()
	local adc_T = d:readAdc(1)
	local adc_P = d:readAdc(2)

	local T, t_fine
	T, t_fine = d:compensateT(adc_T, dig_T)
	
	local P = d:compensateP(adc_P, t_fine)
	curAlt=d:altitude(P, QNH)
	print((string.gsub(string.format("altitude=%d.%2d", curAlt/100, curAlt%100), " ", "0")), string.format("adc_T=%X\tadc_P=%X", adc_T, adc_P))
end

d=require("bme280")
a=require("math64")
if not initialize() then
	tmr.alarm(0,5*1000,1, function()
		if initialize() then tmr.alarm(0,2*1000,1,altitude) end
	end)
else
	tmr.alarm(0,2*1000,1,altitude)
end
