# Now, as the behaviours are in their own folder, this option is not required
COMPILE_FIRST = _behaviours/whois_adapter _behaviours/whois_parser

PROJECT = whois

include erlang.mk
