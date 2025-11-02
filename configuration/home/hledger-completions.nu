def yesno [] { [yes no y n] }
def yes-no-auto [] { [auto yes no y n] }
def one-to-nine [] { [1 2 3 4 5 6 7 8 9] }

export extern "hledger" [
  # Flags:
  --conf: string  # Use extra options defined in this config file. If not specified, searches upward and in XDG config dir for hledger.conf (or .hledger.conf in $HOME).
  --no-conf(-n)  # ignore any config file

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args: any
]

# show the commands list (default)
export extern "hledger commands" [
  --builtin  # show only builtin commands, not addons

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args: any
]
# show brief demos in the terminal
export extern "hledger demo" [
  demo: string
  --speed(-s): float  # playback speed (1 is original speed, .5 is half, 2 is double, etc (default: 2))

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args: any
]              
# Show the hledger user manual with info, man, or a pager. With a (case insensitive) TOPIC argument, try to open it at that section heading.
export extern "hledger help" [
  -i  # info
  -m  # man
  -p  # pager

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  topic?: string
]

# run commands from an interactive prompt
export extern "hledger repl" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args
]
# run command scripts from files or arguments
export extern "hledger run" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args
]

def view-add-edit [] { [view add edit] }

# run a web UI (hledger-web)
export extern "hledger web" [
  --serve-browse  # server mode: serve the web UI and JSON API, and open a browser, and exit if inactive for 2m (default)
  --serve  # server mode: just serve the web UI and JSON API
  --serve-api  # server mode: just serve the JSON API
  --allow: string@view-add-edit  # set the user's access level for changing data (default: `add`). It also accepts `sandstorm` for use on that platform (reads permissions from the `X-Sandstorm-Permissions` request header).
  --cors: string  # allow cross-origin requests from the specified origin; setting ORIGIN to "*" allows requests from any origin
  --host: string  # listen on this IP address (default: 127.0.0.1)
  --port: int  # listen on this TCP port (default: 5000)
  --socket: string  # listen on the given unix socket instead of an IP address and port (only on unix)
  --base-url: string  # set the base url (default: http://IPADDR:PORT)
  --test  # run hledger-web's tests and exit. hspec test runner args may follow a --, eg: hledger-web --test -- --help

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...patterns
]


# add transactions using interactive prompts
export extern "hledger add" [
  --no-new-accounts  # don't allow creating new accounts

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args
]
# add new transactions from other files, eg CSV files
export extern "hledger import" [
  --catchup  # just mark all transactions as already imported
  --dry-run  # just show the transactions to be imported

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  ...args
]

# show account names
export extern "hledger accounts" [
  --used(-u)  # show only accounts used by transactions
  --declared(-d)  # show only accounts declared by account directive
  --unused  # show only accounts declared but not used
  --undeclared  # show only accounts used but not declared
  --types  # also show account types when known
  --positions  # also show where accounts were declared
  --directives  # show as account directives, for use in journals
  --find  # find the first account matched by the first argument (a case-insensitive infix regexp or account name)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default)
  --tree(-t)  # list/tree mode: show accounts as a tree
  --drop: int  # flat mode: omit N leading account name parts

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show transaction codes
export extern "hledger codes" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show commodity/currency symbols
export extern "hledger commodities" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)
]
# show transaction descriptions
export extern "hledger descriptions" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show data files in use
export extern "hledger files" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  regex?: string
]
# show note part of transaction descriptions
export extern "hledger notes" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show payee part of transaction descriptions
export extern "hledger payees" [
  --declared  # show payees declared with payee directives
  --used  # show payees referenced by transactions

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show historical market prices
export extern "hledger prices" [
  --show-reverse  # also show the prices inferred by reversing known prices

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show journal statistics
export extern "hledger stats" [
  --verbose(-v)  # show more detailed output
  --output-file(-o): string  # write output to FILE.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show tag names
export extern "hledger tags" [
  --values  # list tag values instead of tag names
  --parsed  # show tags/values in the order they were parsed, including duplicates

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]

def "hledger print formats" [] { [txt beancount csv tsv html fods json sql] }

# show full transaction entries, or export journal data
export extern "hledger print" [
  --explicit(-x)  # show all amounts explicitly
  --show-costs  # show transaction prices even with conversion postings
  --round: string  # how much rounding or padding should be done when displaying amounts ?: none - show original decimal digits, as in journal (default), soft - just add or remove decimal zeros to match precision, hard - round posting amounts to precision (can unbalance transactions), all  - also round cost amounts to precision (can unbalance transactions)
  --invert  # display all amounts with reversed sign
  --new  # show only newer-dated transactions added in each file since last run
  --match(-m): string  # fuzzy search for one recent transaction with description closest to DESC
  --base-url: string  # in html output, generate links to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --location  # add file/line number tags to print output
  --output-format(-O): string@"hledger print formats"  # select the output format. Supported formats: txt, beancount, csv, tsv, html, fods, json, sql.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]

def "hledger aregister formats" [] { [txt html csv tsv json] }

# show transactions & running balance in one account
export extern "hledger aregister" [
  --txn-dates  # filter strictly by transaction date, not posting date. Warning: this can show a wrong running balance.
  --no-elide  # don't show only 2 commodities per amount
  --cumulative  # accumulation mode: show running total from report start date
  --historical(-H)  # accumulation mode: show historical running total/balance (includes postings before report start date) (default)
  --invert  # display all amounts with reversed sign
  --heading: string@yesno  # show heading row above table: yes (default) or no
  --width(-w): int  # set output width (default: terminal width). -wN,M sets description width as well.
  --output-format(-O): string@"hledger aregister formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  acctpat: string
  query?: string
]
# show transactions & running balance in one account
export extern "hledger areg" [
  --txn-dates  # filter strictly by transaction date, not posting date. Warning: this can show a wrong running balance.
  --no-elide  # don't show only 2 commodities per amount
  --cumulative  # accumulation mode: show running total from report start date
  --historical(-H)  # accumulation mode: show historical running total/balance (includes postings before report start date) (default)
  --invert  # display all amounts with reversed sign
  --heading: string@yesno  # show heading row above table: yes (default) or no
  --width(-w): int  # set output width (default: terminal width). -wN,M sets description width as well.
  --output-format(-O): string@"hledger aregister formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  acctpat: string
  query?: string
]

def "hledger register formats" [] { [txt csv tsv html fods json] }

# show postings & running total in one or more accounts
export extern "hledger register" [
  --cumulative  # accumulation mode: show running total from report start date (default)
  --historical(-H)  # accumulation mode: show historical running total/balance (includes postings before report start date)
  --average(-A)  # show running average of posting amounts instead of total (implies --empty)
  --match(-m): string  # fuzzy search for one recent posting with description closest to DESC
  --related(-r)  # show postings' siblings instead
  --invert  # display all amounts with reversed sign
  --sort: string  # sort by: date, desc, account, amount, absamount, or a comma-separated combination of these. For a descending sort, prefix with -. (Default: date)
  --width(-w): int  # set output width (default: terminal width). -wN,M sets description width as well.
  --align-all  # guarantee alignment across all lines (slower)
  --base-url: string  # in html output, generate links to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger register formats"  # select the output format. Supported formats: txt, csv, tsv, html, fods, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show postings & running total in one or more accounts
export extern "hledger reg" [
  --cumulative  # accumulation mode: show running total from report start date (default)
  --historical(-H)  # accumulation mode: show historical running total/balance (includes postings before report start date)
  --average(-A)  # show running average of posting amounts instead of total (implies --empty)
  --match(-m): string  # fuzzy search for one recent posting with description closest to DESC
  --related(-r)  # show postings' siblings instead
  --invert  # display all amounts with reversed sign
  --sort: string  # sort by: date, desc, account, amount, absamount, or a comma-separated combination of these. For a descending sort, prefix with -. (Default: date)
  --width(-w): int  # set output width (default: terminal width). -wN,M sets description width as well.
  --align-all  # guarantee alignment across all lines (slower)
  --base-url: string  # in html output, generate links to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger register formats"  # select the output format. Supported formats: txt, csv, tsv, html, fods, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]

def "hledger balancesheet formats" [] { [txt html csv tsv json] }

# show assets and liabilities
export extern "hledger balancesheet" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show assets and liabilities
export extern "hledger bs" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show assets, liabilities and equity
export extern "hledger balancesheetequity" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show assets, liabilities and equity
export extern "hledger bse" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show changes in liquid assets
export extern "hledger cashflow" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show changes in liquid assets
export extern "hledger cf" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show revenues and expenses
export extern "hledger incomestatement" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show revenues and expenses
export extern "hledger is" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balancesheet formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]

def "hledger balance formats" [] { [txt html csv tsv json fods] }

# show balance changes, end balances, gains, budgets..
export extern "hledger balance" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --budget: string  # calculation mode: show sum of posting amounts together with budget goals defined by periodic transactions. With a DESCPAT argument (must be separated by = not space), use only periodic transactions with matching description
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --related(-r)  # show the other accounts transacted with, instead
  --invert  # display all amounts with reversed sign
  --transpose  # switch rows and columns (use vertical time axis)
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balance formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show balance changes, end balances, gains, budgets..
export extern "hledger bal" [
  --sum  # calculation mode: show sum of posting amounts (default)
  --valuechange  # calculation mode: show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)
  --gain  # calculation mode: show unrealised capital gain/loss (historical balance value minus cost basis)
  --budget: string  # calculation mode: show sum of posting amounts together with budget goals defined by periodic transactions. With a DESCPAT argument (must be separated by = not space), use only periodic transactions with matching description
  --count  # calculation mode: show the count of postings
  --change  # accumulation mode: accumulate amounts from column start to column end (in multicolumn reports)
  --cumulative # accumulation mode: accumulate amounts from report start (specified by e.g. -b/--begin) to column end
  --historical(-H)  # accumulation mode: accumulate amounts from journal start to column end (includes postings before report start date) (default)
  --flat(-l)  # list/tree mode: show accounts as a flat list (default). Amounts exclude subaccount amounts, except where the account is depth-clipped.
  --tree(-t) # list/tree mode: show accounts as a tree. Amounts include subaccount amounts.
  --drop: int  # in list mode, omit N leading account name parts
  --declared  # include non-parent declared accounts (best used with -E)
  --average(-A)  # show a row average column (in multicolumn reports)
  --row-total(-T)  # show a row total column (in multicolumn reports)
  --summary-only  # display only row summaries (e.g. row total, average) (in multicolumn reports)
  --no-total(-N)  # omit the final total row
  --no-elide  # in tree mode, don't squash boring parent accounts
  --format: string  # use this custom line format (in simple reports)
  --sort-amount(-S)  # sort by amount instead of account code/name
  --percent(-%)  # express values in percentage of each column's total
  --related(-r)  # show the other accounts transacted with, instead
  --invert  # display all amounts with reversed sign
  --transpose  # switch rows and columns (use vertical time axis)
  --layout: string  # how to show multi-commodity amounts: 'wide[,WIDTH]': all commodities on one line, 'tall': each commodity on a new line, 'bare': bare numbers, symbols in a column
  --base-url: string  # in html output, generate hyperlinks to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)
  --output-format(-O): string@"hledger balance formats"  # select the output format. Supported formats: txt, html, csv, tsv, json.
  --output-file(-o): string  # write output to FILE. A file extension matching one of the above formats selects that format.

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]
# show return on investments
export extern "hledger roi" [
  --cashflow  # show all amounts that were used to compute returns
  --investment: string  # query to select your investment transactions
  --profit-loss: string  # query to select profit-and-loss or appreciation/valuation transactions
  --pnl: string  # query to select profit-and-loss or appreciation/valuation transactions

  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]

# show posting counts as a bar chart
export extern "hledger activity" [
  # General input/data transformation flags:
  --file(-f): string  # Read data from FILE, or from stdin if FILE is -, inferring format from extension or a FMT: prefix. Can be specified more than once. If not specified, reads from $LEDGER_FILE or $HOME/.hledger.journal.
  --rules: string  # Use rules defined in this rules file for converting subsequent CSV/SSV/TSV files. If not specified, uses FILE.csv.rules for each FILE.csv.
   --alias: string  # transform account names from A to B, or by replacing regular expression matches --auto                 generate extra postings by applying auto posting rules ("=") to all transactions
   --forecast  # Generate extra transactions from periodic rules ("~"), from after the latest ordinary transaction until 6 months from now. Or, during the specified PERIOD (the equals is required). Auto posting rules will also be applied to these transactions. In hledger-ui, also make future-dated transactions visible at startup.
   --ignore-assertions(-I)  # don't check balance assertions by default
   --infer-costs  # infer conversion equity postings from costs
   --infer-equity  # infer costs from conversion equity postings
   --infer-market-prices  # infer market prices from costs
   --pivot: string  # use a different field or tag as account names
   --strict(-s)  # do extra error checks (and override -I)
   --verbose-tags  # add tags indicating generated/modified data

  # General output/reporting flags (supported by some commands):
  --begin(-b): string   # include postings/transactions on/after this date
  --end(-e): string  # include postings/transactions before this date (with a report interval, will be adjusted to following subperiod end)
  --daily(-D)  # set report interval: 1 day
  --weekly(-W)  # set report interval: 1 week
  --monthly(-M)  # set report interval: 1 month
  --quarterly(-Q)  # set report interval: 1 quarter
  --yearly(-Y)  # set report interval: 1 year
  --period(-p): string  # set begin date, end date, and/or report interval, with more flexibility
  --today: string  # override today's date (affects relative dates)
  --date2: string  # match/use secondary dates instead (deprecated)
  --unmarked(-U)  # include only unmarked postings/transactions
  --pending(-P)  # include only pending postings/transactions
  --cleared(-C)  # include only cleared postings/transactions (-U/-P/-C can be combined)
  --real(-R)  # include only non-virtual postings
  --empty(-E)  #  Show zero items, which are normally hidden. In hledger-ui & hledger-web, do the opposite.
  --depth: string  # if a number (or -NUM): show only top NUM levels of accounts. If REGEXP=NUM, only apply limiting to accounts matching the regular expression.
  --cost(-B)  # convert amounts to their cost/sale amount (@/@@)
  --market(-V)  # valuation mode: show amounts converted to market value at period end(s) in their default valuation commodity. Short for --value=end.
  --exchange(-X): string  # valuation mode: show amounts converted to market value at period end(s) in the specified commodity. Short for --value=end,COMM.
  --value: string  # valuation mode: show amounts converted to market value on the specified date(s) in their default valuation commodity or a specified commodity. WHEN can be: 'then': value on transaction dates, 'end': value at period end(s), 'now': value today, YYYY-MM-DD: value on given date
  --commodity-style(-c)  # Override a commodity's display style. Eg: -c '$1000.' or -c '1.000,00 EUR'
  --pretty: string@yesno  # Use box-drawing characters in text output? The optional 'y'/'yes' or 'n'/'no' arg requires =.

  # General help flags:
  --help(-h)  # show command line help
  --tldr  # show command examples with tldr
  --info  # show the manual with info
  --man  # show the manual with man
  --version  # show version information
  --debug: int@one-to-nine  # show this much debug output (default: 1)
  --pager: string@yesno  # use a pager when needed ? y/yes (default) or n/no
  --color: string@yes-no-auto  # use ANSI color ? y/yes, n/no, or auto (default)

  query?: string
]

# generate transactions to zero/restore/assert balances
export extern "hledger close" [
  ...args
]
# add postings to transactions, like print --auto
export extern "hledger rewrite" [
  ...args
]

# run any of hledger's built-in correctness checks
export extern "hledger check" [
  ...args
]
# compare an account's transactions in two journals
export extern "hledger diff" [
  ...args
]
# check and show the status of the hledger installation
export extern "hledger setup" [
  ...args
]
# run some self tests
export extern "hledger test" [
  ...args
]
