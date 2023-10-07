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
        self.assertEqual(token.ty, TokenType.ASTERISK)

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
