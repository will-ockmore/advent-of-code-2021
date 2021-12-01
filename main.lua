local fennel = require("lib.fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)
local mylib = require("wrap") 
