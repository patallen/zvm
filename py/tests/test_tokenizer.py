from unittest import TestCase
from ..tokenizer import Tokenizer, TokenType


class TokenizerTests(TestCase):
    def test_scan_token(self):
        tokenizer = Tokenizer("1+2")
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.PLUS)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.EOF)

    def test_scan_token_with_parens(self):
        tokenizer = Tokenizer("(3 + 2) * 4")
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.L_PAREN)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.PLUS)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.R_PAREN)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.STAR)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.EOF)

    def test_scan_token_with_floats(self):
        tokenizer = Tokenizer("3.2 + 08.20")
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)
        self.assertEqual(token.value, "3.2")

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.PLUS)

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.NUMBER)
        self.assertEqual(token.value, "08.20")

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.EOF)

    def test_multi_char_tokens(self):
        tokenizer = Tokenizer("! != < <= > >= = == * **")

        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.BANG)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.BANG_EQ)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.LT)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.LT_EQ)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.GT)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.GT_EQ)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.EQ)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.EQ_EQ)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.STAR)
        token = tokenizer.scan_token()
        self.assertEqual(token.ty, TokenType.STAR_STAR)
