```txt

alias first_name "first-name-id"
alias last_name "last-name-id"
alias age "age-id"
alias years "years-id"
alias desc "desc-id"

push "person-id"
set first_name "Bob"
set last_name "Yakson"
set age.years 47
set desc "This is Mr. Bob Yakson."

```

```re

Set(["person-id", "first-name-id"], "Bob")

// Set("entity-id", "attr-id", "actor-id", Number(42))

Set(["name-id", "name-id"], String("name"))

Set(["other-name-id", "other-name-id"], String("name"))
Alias("other-name-id", "name-id")



Set(["jeff-id", "kilos-id"], "oisjdfiojsf") // this does not make sense
Set(["jeff-id", "pounds-id"], 4232) // this does not make sense
Set(["weight-id", "pounds-unit-id"], 42)
Set(["jeff-id", "person-weight-id"], jeffs-weight-in-pounds)

Set(["weight-in-pounds-id", "metric"], "pounds-id")
Set(["unit-pounds", "unit-kilos"], conversion)
Set(["weight-in-pounds-id", "weight-in-kilos-id"], convertToKilos)
Set(["id1", "id2", "id3", "id4"], 43)

```
