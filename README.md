whois.erl
=========
This project performs [WHOIS](https://en.wikipedia.org/wiki/Whois) request 
It [RFC 3912](https://tools.ietf.org/html/rfc3912)


Requirements
============
This project is self-contained and it doesn't have any requirement.

It is being developed and tested on Erlang/OTP 17.


Installation
============


API
===

TODO async...
```erlang
TlRecord = whois:lookup("domain.com").
```

```erlang
TldRecord = whois:lookup(<<"sub.domain.com">>).
```

TldRecord
---------
```erlang
TldRecord
```

Server
------
Instead of using `whois:lookup/1`, which involves preparing the module on every call, it is possible to run a server, and query it directly: TODO
```erlang
Pid = whois_server:start(Options),

receive
  {ok, TldRecords} ->
    other:action(TldRecords),
    loop();
  {error, Reason} ->
    Reason
end,

Pid ! {whois, "domain.com"},
%% Pid ! {whois, ["domain1.com", "domain2.com"]},
whois_server:start:stop().
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
