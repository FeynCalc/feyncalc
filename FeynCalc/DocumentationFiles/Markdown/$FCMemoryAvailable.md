## $FCMemoryAvailable

`$FCMemoryAvailable` is  a global variable which is set to an integer `n`, where `n` is the available amount of main memory in MB. The default is `1/4` of `$SystemMemory`. It should be increased if possible. The higher $FCMemoryAvailable can be, the more intermediate steps do not have to be repeated by FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [MemSet](MemSet.md), [FCMemoryAvailable](FCMemoryAvailable.md).

### Examples

```mathematica
$SystemMemory
```

$$32832512000$$

```mathematica
Floor[$SystemMemory/10^6/4]
```

$$8208$$

```mathematica
$FCMemoryAvailable
```

$$8208$$
