module Program.Compile where

import Program.Common.Data

--import Data.List
import Data.Maybe
import Data.Functor
import Control.Arrow
--import Control.Exception

-- FIXME: I'm so sorry... I have sinned...
step :: Maybe [Token] -> Maybe String
step ts = ts
    <&> (   fmap compile
        >>> concat
        >>> (init <>)
        >>> (<> exit)
        )
  where
    init = formatAssembly
        [ "    ;; -- PROGRAM INIT --"
        , "BITS 64"
        , "segment .text"
        , "global _start"
        , "_start:"
        ]
    -- NOTE: `exit` syscall required for correct termination
    exit = formatAssembly
        [ "    ;; -- PROGRAM EXIT --"
        , "    mov rax, 60"
        , "    mov rdi, 0"
        , "    syscall"
        ]

-- TODO: Implement compilation of literal values
compile :: Token -> String
compile (TokenNatural nat) = formatAssembly
    [ "    ;; -- Push N64 (uint64_t) --"
    , ("    push " ++ show nat)
    ]
compile (TokenSymbol sym)  = compileSymbol sym
compile _ = error "[ERROR] Unreachable, token not supported by compiler"

-- TODO: Better error handling
compileSymbol :: String -> String
compileSymbol sym = case lookup sym stdlib of
    Just asm -> asm
    Nothing  -> error "[ERROR] Symbol not found"

formatAssembly :: [String] -> String
formatAssembly asm = concat $ (<> "\n") <$> asm

-- TODO: Replace with actual hashmap
-- TODO: Implement (the core of) a standard library
-- TODO: Typing
stdlib :: [(String, String)]
stdlib =
    [ ("add", formatAssembly
        [ "    ;; -- ADD --"
        , "    pop    rax"
        , "    pop    rbx"
        , "    add    rax, rbx"
        , "    push   rax"
        ]
      )
    , ("sub", formatAssembly
        [ "    ;; -- SUB --"
        , "    pop    rax"
        , "    pop    rbx"
        , "    sub    rax, rbx"
        , "    push   rax"
        ]
      )
    , ("print", formatAssembly -- Credit goes to the Porth project (Tsoding)
        [ "    ;; -- PRINT --"
        , "    pop    rdi"
        , "    mov    r9, -3689348814741910323"
        , "    sub    rsp, 40"
        , "    mov    BYTE [rsp+31], 10"
        , "    lea    rcx, [rsp+30]"
        , ".L2:"
        , "    mov    rax, rdi"
        , "    lea    r8, [rsp+32]"
        , "    mul    r9"
        , "    mov    rax, rdi"
        , "    sub    r8, rcx"
        , "    shr    rdx, 3"
        , "    lea    rsi, [rdx+rdx*4]"
        , "    add    rsi, rsi"
        , "    sub    rax, rsi"
        , "    add    eax, 48"
        , "    mov    BYTE [rcx], al"
        , "    mov    rax, rdi"
        , "    mov    rdi, rdx"
        , "    mov    rdx, rcx"
        , "    sub    rcx, 1"
        , "    cmp    rax, 9"
        , "    ja     .L2"
        , "    lea    rax, [rsp+32]"
        , "    mov    edi, 1"
        , "    sub    rdx, rax"
        , "    xor    eax, eax"
        , "    lea    rsi, [rsp+32+rdx]"
        , "    mov    rdx, r8"
        , "    mov    rax, 1"
        , "    syscall"
        , "    add    rsp, 40"
        ]
      )
    ]
