##  FCRemoveTypesettingRules 

FCRemoveTypesettingRules[expr] removes all typesetting rules attached to expr. Effectively it sets the FormatValues of expr to an empty list..

###  Examples 

```mathematica
FCAttachTypesettingRules
FCAttachTypesettingRule[ST1, {SubscriptBox, "S", "T,1"}]
ST1 
 
FCRemoveTypesettingRules[ST1]
ST1
```

$$\text{FCAttachTypesettingRules}$$

$$S_{T,1}$$

$$\text{ST1}$$