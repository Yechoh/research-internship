define(function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var CleanHighlightRules = function() {

    var keywordMapper = this.createKeywordMapper({
        "variable.language": "this",
        "keyword": 
            "foreign from definition implementation import module system"
            + "case code if in let of where with" +
            "infix infixl infixr" +
            "export class derive" +
            "special instance",
       // "constant.language": 
          //  "TRUE FALSE NULL SPACE",
        //"support.type": 
          //  "c n i p f d t x string xstring decfloat16 decfloat34",
        /*"keyword.operator":
            "abs sign ceil floor trunc frac acos asin atan cos sin tan" +
            " abapOperator cosh sinh tanh exp log log10 sqrt" +
            " strlen xstrlen charlen numofchar dbmaxlen lines" */
    }, "text", true, " ");

    this.$rules = {
        "start" : [
            {token : "string", regex : "\"", next  : "string"},
            {token : "string", regex : "'", next  : "qstring"},
            {token : "doc.comment", regex : "/\\*.*\\*/"},
            {token : "comment",  regex : "//.*$"},
           // {token : "invalid", regex: "\\.{2,}"},
            {token: "support.type", regex: /^\s*::.*$/, next : "type"},
            {token: "support.function", regex: /^.*::.*$/},
            {token : "keyword.operator", regex: /:==|=:|=>|=|;|->|\\\\|<-|<-:|\.|!|&|#!|#|\*|->|:/},
            {token : "paren.lparen", regex : "[\\[({]"},
            {token : "paren.rparen", regex : "[\\])}]"},
            {token : "constant.numeric", regex: "[+-]?\\d+\\b"},
           // {token : "variable.parameter", regex : /sy|pa?\d\d\d\d\|t\d\d\d\.|innnn/}, 
            //{token : "keyword", regex : compoundKeywords}, 
            {token : "constant.language", regex: /Bool|Char|File|Int|Real|World/},
           // {token : "variable.parameter", regex : /\w+-\w+(?:-\w+)*/}, 
            {token : keywordMapper, regex : "\\b\\w+\\b"},
            {caseInsensitive: false}
        ],
        "qstring" : [
            {token : "constant.language.escape",   regex : "''"},
            {token : "string", regex : "'",     next  : "start"},
            {defaultToken : "string"}
        ],
        "string" : [
            {token : "constant.language.escape",   regex : "\"\""},
            {token : "string", regex : "\"",     next  : "start"},
            {defaultToken : "string"}
        ],
        "type" : [
			{token : "doc.comment", regex : "/\\*.*\\*/"},
            {token : "comment",  regex : "//.*$"},
            {token : "support.type", regex: /^\s*$/, next: "start"},
            {defaultToken: "support.type"}
        ]
    };
};
oop.inherits(CleanHighlightRules, TextHighlightRules);

exports.CleanHighlightRules = CleanHighlightRules;
});
