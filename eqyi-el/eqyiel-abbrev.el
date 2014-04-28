;;; eqyiel-abbrev.el

(setq save-abbrevs t)
(setq-default abbrev-mode t)

(define-abbrev-table 'sql-mode-abbrev-table
    (mapcar #'(lambda (v) (list v (upcase v) nil 1))
        '("absolute" "action" "add" "after" "all" "allocate" "alter" "and" "any"
        "are" "array" "as" "asc" "asensitive" "assertion" "asymmetric" "at"
        "atomic" "authorization" "avg" "before" "begin" "between" "bigint"
        "binary" "bit" "bitlength" "blob" "boolean" "both" "breadth" "by" "call"
        "called" "cascade" "cascaded" "case" "cast" "catalog" "char"
        "char_length" "character" "character_length" "check" "clob" "close"
        "coalesce" "collate" "collation" "column" "commit" "condition" "connect"
        "connection" "constraint" "constraints" "constructor" "contains"
        "continue" "convert" "corresponding" "count" "create" "cross" "cube"
        "current" "current_date" "current_default_transform_group"
        "current_path" "current_role" "current_time" "current_timestamp"
        "current_transform_group_for_type" "current_user" "cursor" "cycle"
        "data" "date" "day" "deallocate" "dec" "decimal" "declare" "default"
        "deferrable" "deferred" "delete" "depth" "deref" "desc" "describe"
        "descriptor" "deterministic" "diagnostics" "disconnect" "distinct" "do"
        "domain" "double" "drop" "dynamic" "each" "element" "else" "elseif"
        "end" "equals" "escape" "except" "exception" "exec" "execute" "exists"
        "exit" "external" "extract" "false" "fetch" "filter" "first" "float"
        "for" "foreign" "found" "free" "from" "full" "function" "general" "get"
        "global" "go" "goto" "grant" "group" "grouping" "handler" "having"
        "hold" "hour" "identity" "if" "immediate" "in" "indicator" "initially"
        "inner" "inout" "input" "insensitive" "insert" "int" "integer"
        "intersect" "interval" "into" "is" "isolation" "iterate" "join" "key"
        "language" "large" "last" "lateral" "leading" "leave" "left" "level"
        "like" "local" "localtime" "localtimestamp" "locator" "loop" "lower"
        "map" "match" "map" "member" "merge" "method" "min" "minute" "modifies"
        "module" "month" "multiset" "names" "national" "natural" "nchar" "nclob"
        "new" "next" "no" "none" "not" "null" "nullif" "numeric" "object"
        "octet_length" "of" "old" "on" "only" "open" "option" "or" "order"
        "ordinality" "out" "outer" "output" "over" "overlaps" "pad" "parameter"
        "partial" "partition" "path" "position" "precision" "prepare" "preserve"
        "primary" "prior" "privileges" "procedure" "public" "range" "read"
        "reads" "real" "recursive" "ref" "references" "referencing" "relative"
        "release" "repeat" "resignal" "restrict" "result" "return" "returns"
        "revoke" "right" "role" "rollback" "rollup" "routine" "row" "rows"
        "savepoint" "schema" "scope" "scroll" "search" "second" "section"
        "select" "sensitive" "session" "session_user" "set" "sets" "signal"
        "similar" "size" "smallint" "some" "space" "specific" "specifictype"
        "sql" "sqlcode" "sqlerror" "sqlexception" "sqlstate" "sqlwarning"
        "start" "state" "static" "submultiset" "substring" "sum" "symmetric"
        "system" "system_user" "table" "tablesample" "temporary" "then" "time"
        "timestamp" "timezone_hour" "timezone_minute" "to" "trailing"
        "transaction" "translate" "translation" "treat" "trigger" "trim" "true"
        "under" "undo" "union" "unique" "unknown" "unnest" "until" "update"
        "upper" "usage" "user" "using" "value" "values" "varchar" "varying"
        "view" "when" "whenever" "where" "while" "window" "with" "within"
        "without" "work" "write" "year" "zone")))

(define-abbrev-table 'org-mode-abbrev-table
  '((">>" "→" nil 1)
    ("wtf" "WTF" nil 1)))

;; (define-abbrev-table 'global-abbrev-table
;;   '(("no" "thanks" nil 1)
;;     ("I" "don't" nil 1)
;;     ("need" "any" nil 1)
;;     ("global" "abbrevs" nil 1)
;;     ("right" "now" nil 1)))

;; doesn't work?
(define-abbrev-table 'circe-channel-mode-abbrev-table
  '(("tias" "Try It And See™")
    (":smoking:" "(-。-)y-゜゜゜")
    (":confused:" "(゜-゜)")
    (":happy:" "(✿◠‿◠)")
    (":surprised:" "°o°")
    (":infatuation:" "(*°∀°)=3")
    (":shrug:" "¯\_(ツ)_/¯")
    (":kitaa:" "キタ━━━(゜∀゜)━━━!!!!!")
    (":tableflip:" "(╯°□°）╯︵ ┻━┻")
    (":rageflip:" "(ノಠ益ಠ)ノ彡┻━┻")
    (":doubleflip:" "┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻")
    (":lookofdisapproval:" "ಠ_ಠ")))

(provide 'eqyiel-abbrev)
