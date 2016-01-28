# BMP280/BME280 module

**Very inefficient way to implement all those calculations**

**Please use Arduino IDE or wait for BME280 NodeMCU native library.**

Library for BMP280/BME280 Digital Pressure Sensor for ESP8266 lua based firmware NodeMCU **integer** version.

Due to memory limitations libraries it is better to compile (`node.compile()`) libraries so it can be use in some more complex scripts.

Compensation formulas follow the double precision floating point algorithm defined in BME280 datasheet ([http://ae-bst.resource.bosch.com/media/products/dokumente/bme280/BST-BME280_DS001-11.pdf](http://ae-bst.resource.bosch.com/media/products/dokumente/bme280/BST-BME280_DS001-11.pdf)).

To get air pressure converted to sea level pressure (QNH) you need to specify altitude of the measurement point. 
Air pressure at given altitude and sea level pressure follow the formula
<blockquote> <p><i>P = QNH (1 - 2.25577 10<sup>-5</sup> h)<sup>5.25588</sup></i></p>
<p><i>where </i></p>
<p><i>P = air pressure (Pa)</i></p>
<p><i>h = altitude above sea level (m)</i></p>
<p><i>QNH = sea level air pressure (m); 101325 for [Standard Atmosphere](https://en.wikipedia.org/wiki/International_Standard_Atmosphere)</i></p>
</blockquote>

## Index
* [init()](#init)
* [readAdc()](#readadc)
* [compensateT()](#compensatet)
* [compensateP()](#compensatep)
* [compensateH()](#compensateh)
* [altCal()](#altcal)
* [qfe2qnh()](#qfe2qnh)
* [altitude()](#altitude)

## Example
```lua
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
```

## Methods

###init()

####Description
Initialize module. Initialization is mandatory before read values.

####Syntax
`init(sda, scl)`

####Parameters
* `sda` - SDA pin  
* `scl` - SCL pin

####Returns
nil

**-** [Back to index](#index)

###readAdc()
####Description
Read raw temperature/pressure/relative humidity value.

####Syntax
`readAdc(meter)`

####Parameters  
* `meter` - 1 for temperature, 2 for pressure, 3 for relative humidity

####Returns
raw reading value

**-** [Back to index](#index)

###compensateT()
####Description
Returns the temperature of the last temperature reading.
Calculation follows the algorithm defined in BME280 datasheet.

####Syntax
`compensateT(adc_T)`

####Parameters
* `adc_T` - raw temperature reading

####Returns  
* `t` - last temperature reading in 0.01ºC (divide by 100 to get ºC).
* `t_fine` - value to be used in pressure compensation calculation

**-** [Back to index](#index)

###compensateP()
####Description
Returns the pressure of the last pressure reading.
Calculation follows the algorithm defined in BME280 datasheet.

####Syntax
`compensateP(adc_P, t_fine)`

####Parameters
* `adc_P` - raw pressure reading
* `t_fine` - latest t_fine temperature value, returned by compensateT method

####Returns  
* `P` - last pressure reading in Pa (divide by 100 to get commonly used hPa).

**-** [Back to index](#index)

###compensateH()
####Description
Returns the pressure of the last pressure reading.
Calculation follows the algorithm defined in BME280 datasheet.

####Syntax
`compensateH(adc_H, t_fine)`

####Parameters
* `adc_H` - raw relative humidity reading
* `t_fine` - latest t_fine temperature value, returned by compensateT method

####Returns  
* `H` - last relative humidity reading in % times 100(divide by 100 to get %).

**-** [Back to index](#index)

###altCal()
Description
Calculates and returns coefficient converting current pressure to sea level pressure for given altitude. The result is saved in the library so that for given altitude it does not have to be calculated again.

####Syntax
`altCal(altitude)`

####Parameters
* `altitude` - altitude in meters of measurement point

####Returns  
* `cH, cL, cE` - (cH << 32 | cL) * 2^cE - double precision floating point coefficient, QNH = P * c

**-** [Back to index](#index)

###qfe2qnh()
Description
Converts measure pressure to sea level pressure for given altitude. If no altCal(altitude) has been called before or if altitude changes the conversion coefficient is recalculated.

####Syntax
`qfe2qnh(P, altitude)`

####Parameters
* `P` - measured pressure
* `altitude` - altitude in meters of measurement point

####Returns  
* `QNH` - sea level pressure

**-** [Back to index](#index)

###altitude()
####Description
Calculates altitude for measured pressure and QNH, i.e. altimeter function.

####Syntax
`altitude(P, QNH)`

####Parameters
* `P` - measured pressure
* `QNH` - current sea level pressure

####Returns  
* `altitude` - altitude in meters of measurement point

**-** [Back to index](#index)

====
#math64 module
Double precision floating point signed numbers arithmetic operations. Used by BMP280 module.

##Index
* [fmt()](#fmt)
* [shr()](#shr)
* [shl()](#shl)
* [add64()](#add64)
* [mult64()](#mult64)
* [div64()](#div64)
* [red()](#red)
* [ln()](#ln)
* [exp()](#exp)

###fmt()

####Description
Formats number as 64-bit hexadecimal for printing.

####Syntax
`fmt(H, L)`

`fmt(H, L, E)`

####Parameters
* `H` - high signed long of 64-bit number
* `L` - low unsigned long of 64-bit number
* `E` - (optional) exponent

####Returns
* string with formatted number

####Example
```lua
> a=require("math64")
> print("a="..a:fmt(0x12345678, 0x9ABCDEF0))
a=12345678 9ABCDEF0
> print("a="..a:fmt(0x12345678, 0x9ABCDEF0, 20))
a=12345678 9ABCDEF0 *2^20
```
**-** [Back to index](#index-1)

###shr()

####Description
Bit shift right operation by `n` bits. It's arithmetic shift (corresponding to bit.arshift).
If E is specified the resulting exponent is adjusted accordingly

####Syntax
`shr(H, L, n)`

`shr(H, L, n, E)`

####Parameters
* `H` - high signed long of 64-bit number
* `L` - low unsigned long of 64-bit number
* `n` - number of bits to shift (1 to 63)
* `E` - (optional) exponent

####Returns
* `H, L, E` - right-shifted number

**-** [Back to index](#index-1)

###shl()

####Description
Bit shift left operation by `n` bits.
If E is specified the resulting exponent is adjusted accordingly

####Syntax
`shl(H, L, n)`

`shl(H, L, n, E)`

####Parameters
* `H` - high signed long of 64-bit number
* `L` - low unsigned long of 64-bit number
* `n` - number of bits to shift (1 to 63)
* `E` - (optional) exponent

####Returns
* `H, L, E` - left-shifted number

**-** [Back to index](#index-1)

###add64()

####Description
Signed 64-bit or 96-bit floating point addition.

####Syntax
`add64(aH, aL, bH, bL)`

`add64(aH, aL, bH, bL, aE, bE)`

####Parameters
* `aH` - high signed long of 64-bit number a
* `aL` - low unsigned long of 64-bit number a
* `bH` - high signed long of 64-bit number b
* `bL` - low unsigned long of 64-bit number b
* `aE` - (optional) exponent of a
* `bE` - (optional) exponent of b

####Returns
* `H, L, E` - `a+b`

**-** [Back to index](#index-1)

###mult64()

####Description
Signed 64-bit or 96-bit floating point multiplication.
For subtraction do `add64(aH, aL, bit.bnot(rH), bit.bnot(rL) + 1)`.

####Syntax
`mult64(aH, aL, bH, bL, aE, bE)`

####Parameters
* `aH` - high signed long of 64-bit number a
* `aL` - low unsigned long of 64-bit number a
* `bH` - high signed long of 64-bit number b
* `bL` - low unsigned long of 64-bit number b
* `aE` - (optional) exponent of a
* `bE` - (optional) exponent of b

####Returns
* `H, L, E` - `a*b`

**-** [Back to index](#index-1)

###div64()
Signed 64-bit or 96-bit floating point division.

####Description

####Syntax
`div64(aH, aL, bH, bL, aE, bE)`

####Parameters
* `aH` - high signed long of 64-bit number a
* `aL` - low unsigned long of 64-bit number a
* `bH` - high signed long of 64-bit number b
* `bL` - low unsigned long of 64-bit number b
* `aE` - (optional) exponent of a
* `bE` - (optional) exponent of b

####Returns
* `H, L, E` - `a/b`

**-** [Back to index](#index-1)

###red()

####Description
"Reduce" floating number, i.e. removes 0 from right and adjust exponent accordingly (if floating)

####Syntax
`red(H, L, E)`

####Parameters
* `H` - high signed long of 64-bit number
* `L` - low unsigned long of 64-bit number
* `E` - (optional) exponent

####Returns
* `H, L, E` - "reduced" number

####Example
```lua
> a=require("math64")
> print("a="..a:fmt(a:red(0x12345678, 0x9ABCDEF0, -10)))
a= 1234567 89ABCDEF *2^-6
```

**-** [Back to index](#index-1)

###ln()

####Description
Approximation of e-based logarithm for (1-x)<2.

The result is **`-ln(1-x)`**

####Syntax
`ln(H, L, E)`

####Parameters
* `H` - high signed long of 64-bit number
* `L` - low unsigned long of 64-bit number
* `E` - exponent

####Returns
* `H, L, E` - `-ln(1-x)`

**-** [Back to index](#index-1)

###exp()

####Description
Approximation of e-based exponential.

####Example
```lua
> a=require("math64")
> H, L, E = 0, 1, -2 -- 0.25
> H, L, E = a:exp(H, L, E)
> print(a:fmt(H, L, E))
2916BB 247B4601 *2^-53 -- 1.28402478337574
> H, L, E = a:add64(0, 1, bit.bnot(H), bit.bnot(L) + 1, 0, E) -- 1-x
> print(a:fmt(H, L, E))
FFF6E944 DB84B9FF *2^-53 -- -0.284024783375799
> H, L, E = a:ln(H, L, E)
> print(a:fmt(H, L, E))
FFFFFBFF FC478323 *2^-44 -- -0.250003548339009
```

####Syntax
`exp(kH, kL, kE)`

####Parameters
* `H` - high signed long of 64-bit number
* `L` - low unsigned long of 64-bit number
* `E` - exponent

####Returns
* `H, L, E` - `exp(x)`

####Example
```lua
> a=require("math64")
> H, L, E = 0, 1, 0
> H, L, E = a:exp(H, L, E)
> print(a:fmt(H, L, E))
		0 56FB2A29 *2^-29 -- 2.71815975196659
```

**-** [Back to index](#index-1)
