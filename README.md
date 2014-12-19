whois.erl
=========
This project performs [WHOIS]() request 


Requirements
============
This project is self-contained and it doesn't have any requirement.

It is being developed and tested on Erlang/OTP 17.


Installation
============


API
===

```erlang
TlRecord = whois:lookup('domain.com').
```

```erlang
TldRecord = whois:lookup('sub.domain.com').
```

TldRecord
---------
```erlang
TldRecord
```

Data
====
You can update it, indepently of the rhythm of this project, using TODO.


Contributing
============

Development workflow
--------------------
1. Create an issue
TODO

Tools
-----

Tests
-----


Authors
=======
Juan Antonio Martín Lucas (https://github.com/noijd)


License
=======
This package is licensed under the MIT license.