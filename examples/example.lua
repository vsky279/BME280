d=require("bme280")

alt=320 -- altitude of the measurement place

d:init()

-- calculate coefficient to get air pressure converted to sea level (QNH)
d:altCal(alt)
print("h="..string.format("%9X", d.hH)..string.format("%9X", d.hL).." *2^"..d.hE)

-- read sensor
adc_T = d:readAdc(1)
adc_P = d:readAdc(2)
adc_H = d:readAdc(3)
if not adc_T or not adc_P then return end
print(string.format("adc_T=%X\tadc_P=%X\tadc_H=%X", adc_T, adc_P, adc_H))

-- calculate temperature
T, t_fine = d:compensateT(adc_T)
print((string.gsub(string.format("T=%d.%2d", T/100, T%100), " ", "0")), "t_fine="..t_fine)

-- calculate aire pressure
P = d:compensateP(adc_P, t_fine)
print((string.gsub(string.format("QFE=%d.%3d", P/1000, P%1000), " ", "0")))

-- convert measure air pressure to sea level pressure
QNH = d:qfe2qnh(P, alt)
print((string.gsub(string.format("QNH=%d.%3d", QNH/1000, QNH%1000), " ", "0")))

H = d:compensateH(adc_H, t_fine)
print((string.gsub(string.format("humidity=%d.%3d%%", H/1000, H%1000), " ", "0")))

-- altimeter function - calculate altitude based on current sea level pressure (QNH) and measure pressure
adc_T = d:readAdc(1)
adc_P = d:readAdc(2)
T, t_fine = d:compensateT(adc_T)
P = d:compensateP(adc_P, t_fine)
curAlt=d:altitude(P, QNH)
print(string.gsub(string.format("altitude=%d.%2d", curAlt/100, curAlt%100), " ", "0"), "adc_T="..string.format("%X", adc_T), "adc_P="..string.format("%X", adc_P))

d=nil;package.loaded["bme280"]=nil;
