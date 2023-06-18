use core::panic;
use std::collections::HashMap;

use nom::{branch::alt, bytes::complete::take_while1, combinator::map, multi::fold_many0, IResult};

use crate::tokenizer::{
    comments::{handle_comment, handle_shebang},
    handle_regex,
    misc::{is_ws, own_error},
    next_token::next_token,
    template_string::handle_template_string_continue,
    tokens::{Comment, JsToken},
};

pub(crate) struct Tokenizer {
    pub text: String,
    pub filename: String,
    pub finished: bool,
    pub html5_comments: bool,
    pub shebang: bool,
    pub pos: usize,
    pub line: usize,
    pub col: usize,
    pub regex_allowed: bool,
    pub brace_counter: usize,
    pub template_braces: Vec<usize>,
    pub newline_before: bool,
    pub comments_before: Vec<Comment>,
    pub directives: HashMap<String, u32>,
    pub directive_stack: Vec<Vec<String>>,
    // Last token and metadata
    pub last_token: Option<JsToken>,
    pub tokpos: usize,
    pub tokline: usize,
    pub tokcol: usize,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Tokenizer {
            text: String::from(""),
            filename: String::from(""),
            finished: false,
            html5_comments: true,
            shebang: true,
            regex_allowed: true,
            brace_counter: 0,
            template_braces: vec![],
            directives: HashMap::new(),
            directive_stack: vec![vec![]],
            pos: 0,
            line: 1,
            col: 0,
            // Last token and metadata
            last_token: None,
            tokpos: 0,
            tokline: 0,
            tokcol: 0,
            newline_before: false,
            comments_before: vec![],
        }
    }
}

impl Tokenizer {
    pub(crate) fn new(
        text: String,
        filename: String,
        html5_comments: bool,
        shebang: bool,
    ) -> Tokenizer {
        Tokenizer {
            text,
            filename,
            html5_comments,
            shebang,
            ..Default::default()
        }
    }

    pub(crate) fn looking_at(&self, s: &str) -> bool {
        self.text[self.pos..].starts_with(s)
    }

    pub(crate) fn skip_whitespace_and_comments(
        &mut self,
        shebang_allowed: bool,
    ) -> Result<Option<Vec<Comment>>, nom::Err<nom::error::Error<String>>> {
        let shebang = if shebang_allowed {
            self.consume_optional_token(handle_shebang)?
        } else {
            None
        };

        let html5_comments = self.html5_comments;
        let comment_parser = map(|i| handle_comment(i, html5_comments), |item| vec![item]);
        let ws_parser = map(take_while1(is_ws), |_| vec![]);

        let comments_and_ws = fold_many0(
            alt((comment_parser, ws_parser)),
            || {
                if let Some(shebang) = &shebang {
                    vec![shebang.clone()]
                } else {
                    vec![]
                }
            },
            |mut acc: Vec<Comment>, items: Vec<Comment>| {
                acc.extend(items);
                acc
            },
        );

        self.consume_optional_token(comments_and_ws)
    }

    pub(crate) fn next_token(&mut self) -> Result<JsToken, nom::Err<nom::error::Error<String>>> {
        if self.finished {
            panic!("Tokenizer is finished, because it has returned an Err before");
        }

        if let Some(comments) = self.skip_whitespace_and_comments(self.shebang && self.pos == 0)? {
            self.comments_before = comments;
        } else {
            self.comments_before = vec![];
        };

        /** Sometimes our parser will give us a non-answer. Simplify the response and set `finished` if necessary */
        fn handle_non_tokens(
            result: Result<Option<JsToken>, nom::Err<nom::error::Error<String>>>,
        ) -> (bool, Result<JsToken, nom::Err<nom::error::Error<String>>>) {
            match result {
                Ok(Some(JsToken::EOF) | None) => (true, Ok(JsToken::EOF)),
                Err(err) => (true, Err(err)),
                Ok(Some(tok)) => (false, Ok(tok)),
            }
        }

        // Advance newlines we saw between `self.pos` and `self.tokpos`
        let newline_idx = self.text[self.tokpos..self.pos].rfind('\n');
        if let Some(mut idx) = newline_idx {
            self.newline_before = true;
            self.col = self.pos - idx - 1;

            loop {
                self.line += 1;

                if let Some(new_idx) = self.text[self.tokpos..self.tokpos + idx].rfind('\n') {
                    idx = new_idx;
                } else {
                    break;
                }
            }
        } else {
            self.newline_before = false;
            self.col += self.pos - self.tokpos;
        }

        self.tokpos = self.pos;
        self.tokline = self.line;
        self.tokcol = self.col;

        let result = match self.consume_optional_token(next_token) {
            Ok(Some(ref tpl @ JsToken::TemplateString(_, is_begin, is_end)))
                if is_begin != is_end =>
            {
                // "}...${"
                if is_end {
                    self.template_braces.pop();
                } else {
                    self.template_braces.push(self.brace_counter);
                    self.brace_counter += 1;
                }

                Ok(tpl.clone())
            }
            Ok(Some(JsToken::Punc(x))) if &x == "{" => {
                self.brace_counter += 1;
                Ok(JsToken::Punc(x))
            }
            Ok(Some(JsToken::Punc(x))) if &x == "}" => {
                self.brace_counter -= 1;

                if self.template_braces.last() == Some(&self.brace_counter) {
                    self.template_braces.pop();
                    self.pos -= 1; // Unconsume the "}"

                    // We're in a template string, time to continue
                    let (finished, result) = handle_non_tokens(
                        self.consume_optional_token(handle_template_string_continue),
                    );
                    self.finished = finished;
                    result
                } else {
                    Ok(JsToken::Punc(x))
                }
            }
            other => {
                let (finished, result) = handle_non_tokens(other);
                self.finished = finished;
                result
            }
        };

        if let Ok(ref tok) = result {
            self.last_token = Some(tok.clone());
        }

        result
    }

    pub(crate) fn latest_raw(&self) -> &str {
        &self.text[self.tokpos..self.pos]
    }

    pub(crate) fn token_sourceinfo(
        &self,
    ) -> (
        String,
        String,
        usize,
        usize,
        usize,
        bool,
        Vec<Comment>,
        String,
    ) {
        // new AST_Token(type, value, line, col, pos, nlb, comments_before, comments_after, file);
        let line = self.tokline;
        let col = self.tokcol;
        let pos = self.tokpos;
        let value = self.latest_raw().to_string();
        let type_ = "unknown".to_string();
        let nlb = self.newline_before;
        let comments_before = self.comments_before.clone();
        let file = self.filename.clone();

        (type_, value, line, col, pos, nlb, comments_before, file)
    }

    /** Parser read too much and the /= or / is marking a regexp instead */
    pub(crate) fn next_token_regexp(&mut self, prefix_size: usize) -> IResult<&str, JsToken> {
        let input = &self.text[self.pos - prefix_size..];
        let (input, regexp) = handle_regex(input)?;
        self.pos = self.text.len() - input.len();
        Ok((input, regexp))
    }

    pub(crate) fn consume_optional_token<'a, ParseF, Ret>(
        &'a mut self,
        parser: ParseF,
    ) -> Result<Option<Ret>, nom::Err<nom::error::Error<String>>>
    where
        ParseF: FnOnce(&'a str) -> IResult<&'a str, Ret, nom::error::Error<&str>>,
    {
        match parser(&self.text[self.pos..]) {
            Ok((input, token)) => {
                self.pos = self.text.len() - input.len();
                Ok(Some(token))
            }
            Err(err @ nom::Err::Failure(_)) => Err(own_error(err)),
            Err(_) => Ok(None),
        }
    }
}

mod test {
    /*
    #[cfg(test)]
    fn toks<'a>(source: &'a str) -> Result<Vec<JsToken>, String> {
        let mut tokenizer = Tokenizer::new(
            String::from(source),
            String::from("test.js"),
            true,
            true,
        );

        tokenizer.all()
    }

    #[test]
    fn test_several_tokens() {
        let tokens = toks("...").unwrap();
        assert_eq!(tokens, vec![
            JsToken::Expand,
        ]);
    }
    */

    #[cfg(test)]
    use crate::tokenizer::{
        tokenizer::Tokenizer,
        tokens::{Comment, JsToken},
    };

    #[test]
    fn test_tokenizer() {
        let mut tokenizer = Tokenizer::new(
            String::from("#!/bin/sh\n// Hello\n.01"),
            String::from("test.js"),
            true,
            true,
        );
        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::Num(0.01));
        assert_eq!(
            tokenizer.comments_before,
            vec![
                Comment::shebang_comment("/bin/sh"),
                Comment::line_comment(" Hello"),
            ]
        );

        let mut tokenizer = Tokenizer::new(
            String::from("1..toString"),
            String::from("test.js"),
            true,
            true,
        );

        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::Num(1.0));

        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::Punc(".".into()));

        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::Name("toString".to_string()));

        let token = tokenizer.next_token();
        assert_eq!(token.unwrap(), JsToken::EOF);

        let mut tokenizer = Tokenizer::new(
            String::from("/* 1..toString"),
            String::from("test.js"),
            true,
            true,
        );

        let token = tokenizer.next_token().unwrap_err();
        assert!(matches!(token, nom::Err::Failure(_)));
    }

    #[test]
    fn test_tokenizer_sourcepos() {
        let mut tokenizer = Tokenizer::new(
            String::from("/* hi\n */\n1 + //comment"),
            String::from("test.js"),
            true,
            true,
        );
        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::Num(1.0));
        insta::assert_debug_snapshot!(tokenizer.token_sourceinfo(), @r###"
        (
            "unknown",
            "1",
            3,
            0,
            10,
            true,
            [
                Comment {
                    kind: Comment2,
                    value: " hi\n ",
                },
            ],
            "test.js",
        )
        "###);

        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::Operator("+".into()));
        insta::assert_debug_snapshot!(tokenizer.token_sourceinfo(), @r###"
        (
            "unknown",
            "+",
            3,
            2,
            12,
            false,
            [],
            "test.js",
        )
        "###);

        let token = tokenizer.next_token().unwrap();
        assert_eq!(token, JsToken::EOF);
        insta::assert_debug_snapshot!(tokenizer.token_sourceinfo(), @r###"
        (
            "unknown",
            "",
            3,
            13,
            23,
            false,
            [
                Comment {
                    kind: Comment1,
                    value: "comment",
                },
            ],
            "test.js",
        )
        "###)
    }
}

/*

function tokenizer($TEXT, filename, html5_comments, shebang) {
    var S = {
        text            : $TEXT,
        filename        : filename,
        pos             : 0,
        tokpos          : 0,
        line            : 1,
        tokline         : 0,
        col             : 0,
        tokcol          : 0,
        newline_before  : false,
        regex_allowed   : false,
        brace_counter   : 0,
        template_braces : [],
        comments_before : [],
        directives      : {},
        directive_stack : []
    };

    function peek() { return get_full_char(S.text, S.pos); }

    // Used because parsing ?. involves a lookahead for a digit
    function is_option_chain_op() {
        const must_be_dot = S.text.charCodeAt(S.pos + 1) === 46;
        if (!must_be_dot) return false;

        const cannot_be_digit = S.text.charCodeAt(S.pos + 2);
        return cannot_be_digit < 48 || cannot_be_digit > 57;
    }

    function next(signal_eof, in_string) {
        var ch = get_full_char(S.text, S.pos++);
        if (signal_eof && !ch)
            throw EX_EOF;
        if (NEWLINE_CHARS.has(ch)) {
            S.newline_before = S.newline_before || !in_string;
            ++S.line;
            S.col = 0;
            if (ch == "\r" && peek() == "\n") {
                // treat a \r\n sequence as a single \n
                ++S.pos;
                ch = "\n";
            }
        } else {
            if (ch.length > 1) {
                ++S.pos;
                ++S.col;
            }
            ++S.col;
        }
        return ch;
    }

    function forward(i) {
        while (i--) next();
    }

    function looking_at(str) {
        return S.text.substr(S.pos, str.length) == str;
    }

    function find_eol() {
        var text = S.text;
        for (var i = S.pos, n = S.text.length; i < n; ++i) {
            var ch = text[i];
            if (NEWLINE_CHARS.has(ch))
                return i;
        }
        return -1;
    }

    function find(what, signal_eof) {
        var pos = S.text.indexOf(what, S.pos);
        if (signal_eof && pos == -1) throw EX_EOF;
        return pos;
    }

    function start_token() {
        S.tokline = S.line;
        S.tokcol = S.col;
        S.tokpos = S.pos;
    }

    var prev_was_dot = false;
    var previous_token = null;
    function token(type, value, is_comment) {
        S.regex_allowed = ((type == "operator" && !UNARY_POSTFIX.has(value)) ||
                           (type == "keyword" && KEYWORDS_BEFORE_EXPRESSION.has(value)) ||
                           (type == "punc" && PUNC_BEFORE_EXPRESSION.has(value))) ||
                           (type == "arrow");
        if (type == "punc" && (value == "." || value == "?.")) {
            prev_was_dot = true;
        } else if (!is_comment) {
            prev_was_dot = false;
        }
        const line     = S.tokline;
        const col      = S.tokcol;
        const pos      = S.tokpos;
        const nlb      = S.newline_before;
        const file     = filename;
        let comments_before = [];
        let comments_after  = [];

        if (!is_comment) {
            comments_before = S.comments_before;
            comments_after = S.comments_before = [];
        }
        S.newline_before = false;
        const tok = new AST_Token(type, value, line, col, pos, nlb, comments_before, comments_after, file);

        if (!is_comment) previous_token = tok;
        return tok;
    }

    function skip_whitespace() {
        while (WHITESPACE_CHARS.has(peek()))
            next();
    }

    function read_while(pred) {
        var ret = "", ch, i = 0;
        while ((ch = peek()) && pred(ch, i++))
            ret += next();
        return ret;
    }

    function handle_error(err) {
        js_error(err, filename, S.tokline, S.tokcol, S.tokpos);
    }

    function read_num(prefix) {
        var has_e = false, after_e = false, has_x = false, has_dot = prefix == ".", is_big_int = false, numeric_separator = false;
        var num = read_while(function(ch, i) {
            if (is_big_int) return false;

            var code = ch.charCodeAt(0);
            switch (code) {
              case 95: // _
                return (numeric_separator = true);
              case 98: case 66: // bB
                return (has_x = true); // Can occur in hex sequence, don't return false yet
              case 111: case 79: // oO
              case 120: case 88: // xX
                return has_x ? false : (has_x = true);
              case 101: case 69: // eE
                return has_x ? true : has_e ? false : (has_e = after_e = true);
              case 45: // -
                return after_e || (i == 0 && !prefix);
              case 43: // +
                return after_e;
              case (after_e = false, 46): // .
                return (!has_dot && !has_x && !has_e) ? (has_dot = true) : false;
            }

            if (ch === "n") {
                is_big_int = true;

                return true;
            }

            return RE_NUM_LITERAL.test(ch);
        });
        if (prefix) num = prefix + num;

        LATEST_RAW = num;

        if (RE_OCT_NUMBER.test(num) && next_token.has_directive("use strict")) {
            handle_error("Legacy octal literals are not allowed in strict mode");
        }
        if (numeric_separator) {
            if (num.endsWith("_")) {
                handle_error("Numeric separators are not allowed at the end of numeric literals");
            } else if (num.includes("__")) {
                handle_error("Only one underscore is allowed as numeric separator");
            }
            num = num.replace(/_/g, "");
        }
        if (num.endsWith("n")) {
            const without_n = num.slice(0, -1);
            const allow_e = RE_HEX_NUMBER.test(without_n);
            const valid = handle_js_number(without_n, allow_e);
            if (!has_dot && RE_BIG_INT.test(num) && !isNaN(valid))
                return token("big_int", without_n);
            handle_error("Invalid or unexpected token");
        }
        var valid = handle_js_number(num);
        if (!isNaN(valid)) {
            return token("num", valid);
        } else {
            handle_error("Invalid syntax: " + num);
        }
    }

    function is_octal(ch) {
        return ch >= "0" && ch <= "7";
    }

    function hex_bytes(n, strict_hex) {
        var num = 0;
        for (; n > 0; --n) {
            if (!strict_hex && isNaN(parseInt(peek(), 16))) {
                return parseInt(num, 16) || "";
            }
            var digit = next(true);
            if (isNaN(parseInt(digit, 16)))
                handle_error("Invalid hex-character pattern in string");
            num += digit;
        }
        return parseInt(num, 16);
    }

    var read_string = with_eof_error("Unterminated string constant", function() {
        const start_pos = S.pos;
        var quote = next(), ret = [];
        for (;;) {
            var ch = next(true, true);
            if (ch == "\\") ch = read_escaped_char(true, true);
            else if (ch == "\r" || ch == "\n") handle_error("Unterminated string constant");
            else if (ch == quote) break;
            ret.push(ch);
        }
        var tok = token("string", ret.join(""));
        LATEST_RAW = S.text.slice(start_pos, S.pos);
        tok.quote = quote;
        return tok;
    });

    function skip_line_comment(type) {
        var regex_allowed = S.regex_allowed;
        var i = find_eol(), ret;
        if (i == -1) {
            ret = S.text.substr(S.pos);
            S.pos = S.text.length;
        } else {
            ret = S.text.substring(S.pos, i);
            S.pos = i;
        }
        S.col = S.tokcol + (S.pos - S.tokpos);
        S.comments_before.push(token(type, ret, true));
        S.regex_allowed = regex_allowed;
        return next_token;
    }

    var skip_multiline_comment = with_eof_error("Unterminated multiline comment", function() {
        var regex_allowed = S.regex_allowed;
        var i = find("*\/", true);
        var text = S.text.substring(S.pos, i).replace(/\r\n|\r|\u2028|\u2029/g, "\n");
        // update stream position
        forward(get_full_char_length(text) /* text length doesn't count \r\n as 2 char while S.pos - i does */ + 2);
        S.comments_before.push(token("comment2", text, true));
        S.newline_before = S.newline_before || text.includes("\n");
        S.regex_allowed = regex_allowed;
        return next_token;
    });

    var read_name = with_eof_error("Unterminated identifier name", function() {
        var name = [], ch, escaped = false;
        var read_escaped_identifier_char = function() {
            escaped = true;
            next();
            if (peek() !== "u") {
                handle_error("Expecting UnicodeEscapeSequence -- uXXXX or u{XXXX}");
            }
            return read_escaped_char(false, true);
        };

        // Read first character (ID_Start)
        if ((ch = peek()) === "\\") {
            ch = read_escaped_identifier_char();
            if (!is_identifier_start(ch)) {
                handle_error("First identifier char is an invalid identifier char");
            }
        } else if (is_identifier_start(ch)) {
            next();
        } else {
            return "";
        }

        name.push(ch);

        // Read ID_Continue
        while ((ch = peek()) != null) {
            if ((ch = peek()) === "\\") {
                ch = read_escaped_identifier_char();
                if (!is_identifier_char(ch)) {
                    handle_error("Invalid escaped identifier char");
                }
            } else {
                if (!is_identifier_char(ch)) {
                    break;
                }
                next();
            }
            name.push(ch);
        }
        const name_str = name.join("");
        if (RESERVED_WORDS.has(name_str) && escaped) {
            handle_error("Escaped characters are not allowed in keywords");
        }
        return name_str;
    });

    var read_regexp = with_eof_error("Unterminated regular expression", function(source) {
        var prev_backslash = false, ch, in_class = false;
        while ((ch = next(true))) if (NEWLINE_CHARS.has(ch)) {
            handle_error("Unexpected line terminator");
        } else if (prev_backslash) {
            source += "\\" + ch;
            prev_backslash = false;
        } else if (ch == "[") {
            in_class = true;
            source += ch;
        } else if (ch == "]" && in_class) {
            in_class = false;
            source += ch;
        } else if (ch == "/" && !in_class) {
            break;
        } else if (ch == "\\") {
            prev_backslash = true;
        } else {
            source += ch;
        }
        const flags = read_name();
        return token("regexp", "/" + source + "/" + flags);
    });

    function read_operator(prefix) {
        function grow(op) {
            if (!peek()) return op;
            var bigger = op + peek();
            if (OPERATORS.has(bigger)) {
                next();
                return grow(bigger);
            } else {
                return op;
            }
        }
        return token("operator", grow(prefix || next()));
    }

    function handle_slash() {
        next();
        switch (peek()) {
          case "/":
            next();
            return skip_line_comment("comment1");
          case "*":
            next();
            return skip_multiline_comment();
        }
        return S.regex_allowed ? read_regexp("") : read_operator("/");
    }

    function handle_eq_sign() {
        next();
        if (peek() === ">") {
            next();
            return token("arrow", "=>");
        } else {
            return read_operator("=");
        }
    }

    function handle_dot() {
        next();
        if (is_digit(peek().charCodeAt(0))) {
            return read_num(".");
        }
        if (peek() === ".") {
            next();  // Consume second dot
            next();  // Consume third dot
            return token("expand", "...");
        }

        return token("punc", ".");
    }

    function read_word() {
        var word = read_name();
        if (prev_was_dot) return token("name", word);
        return KEYWORDS_ATOM.has(word) ? token("atom", word)
            : !KEYWORDS.has(word) ? token("name", word)
            : OPERATORS.has(word) ? token("operator", word)
            : token("keyword", word);
    }

    function read_private_word() {
        next();
        return token("privatename", read_name());
    }

    function with_eof_error(eof_error, cont) {
        return function(x) {
            try {
                return cont(x);
            } catch(ex) {
                if (ex === EX_EOF) handle_error(eof_error);
                else throw ex;
            }
        };
    }

    function next_token(force_regexp) {
        if (force_regexp != null)
            return read_regexp(force_regexp);
        if (shebang && S.pos == 0 && looking_at("#!")) {
            start_token();
            forward(2);
            skip_line_comment("comment5");
        }
        for (;;) {
            skip_whitespace();
            start_token();
            if (html5_comments) {
                if (looking_at("<!--")) {
                    forward(4);
                    skip_line_comment("comment3");
                    continue;
                }
                if (looking_at("-->") && S.newline_before) {
                    forward(3);
                    skip_line_comment("comment4");
                    continue;
                }
            }
            var ch = peek();
            if (!ch) return token("eof");
            var code = ch.charCodeAt(0);
            switch (code) {
              case 34: case 39: return read_string();
              case 46: return handle_dot();
              case 47: {
                  var tok = handle_slash();
                  if (tok === next_token) continue;
                  return tok;
              }
              case 61: return handle_eq_sign();
              case 63: {
                  if (!is_option_chain_op()) break;  // Handled below

                  next(); // ?
                  next(); // .

                  return token("punc", "?.");
              }
              case 96: return read_template_characters(true);
              case 123:
                S.brace_counter++;
                break;
              case 125:
                S.brace_counter--;
                if (S.template_braces.length > 0
                    && S.template_braces[S.template_braces.length - 1] === S.brace_counter)
                    return read_template_characters(false);
                break;
            }
            if (is_digit(code)) return read_num();
            if (PUNC_CHARS.has(ch)) return token("punc", next());
            if (OPERATOR_CHARS.has(ch)) return read_operator();
            if (code == 92 || is_identifier_start(ch)) return read_word();
            if (code == 35) return read_private_word();
            break;
        }
        handle_error("Unexpected character '" + ch + "'");
    }

    next_token.context = () => S;

    return next_token;

}

*/

/** Public interface the parser can use to manage what directives are around. May not be something we implement in rust? */
pub(crate) trait DirectiveManagement {
    fn add_directive(&mut self, directive: &str);
    fn push_directives_stack(&mut self);
    fn pop_directives_stack(&mut self);
    fn has_directive(&self, directive: &str) -> bool;
}

impl DirectiveManagement for Tokenizer {
    fn add_directive(&mut self, directive: &str) {
        self.directive_stack
            .last_mut()
            .unwrap()
            .push(directive.to_string());

        let count = *self.directives.get(directive).unwrap_or(&0);
        self.directives.insert(directive.to_string(), count + 1);
    }

    fn has_directive(&self, directive: &str) -> bool {
        *self.directives.get(directive).unwrap_or(&0) > 0
    }

    fn push_directives_stack(&mut self) {
        self.directive_stack.push(vec![]);
    }

    fn pop_directives_stack(&mut self) {
        let directives = &mut self.directive_stack.last_mut().unwrap();

        for directive in directives.iter() {
            let count = *self
                .directives
                .get(directive)
                .expect("all directives in the directive stack also exist in the directives map");
            self.directives.insert(directive.to_string(), count - 1);
        }

        self.directive_stack.pop();
    }
}

/*
    next_token.add_directive = function(directive) {
        S.directive_stack[S.directive_stack.length - 1].push(directive);

        if (S.directives[directive] === undefined) {
            S.directives[directive] = 1;
        } else {
            S.directives[directive]++;
        }
    };

    next_token.push_directives_stack = function() {
        S.directive_stack.push([]);
    };

    next_token.pop_directives_stack = function() {
        var directives = S.directive_stack[S.directive_stack.length - 1];

        for (var i = 0; i < directives.length; i++) {
            S.directives[directives[i]]--;
        }

        S.directive_stack.pop();
    };

    next_token.has_directive = function(directive) {
        return S.directives[directive] > 0;
    };
*/
