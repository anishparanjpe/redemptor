module Show where

import Text.PrettyPrint

import Gmachine
import Language
import Util

showResults :: [GmState] -> String
showResults states =
  render (vcat $ [text "Supercombinator definitions\n"]
              ++ (map (showSC state) globals)
              ++ [text "\n\nState transitions\n\n"]
              ++ (map showState states))
  where (state@(_, _, _, globals, _) : _) = states

showSC :: GmState -> (Name, Addr) -> Doc
showSC (_, _, heap, _, _) (name, addr) =
  vcat $ [ (text "Code for" <+> text name),
           showInstructions code
         ]
  where (NGlobal _ code) = hLookup heap addr

showInstructions :: GmCode -> Doc
showInstructions is = vcat $ [text "Code:{"]
                          ++ (map (nest 4 . showInstruction) is)
                          ++ [text "}"]

showInstruction :: Instruction -> Doc
showInstruction (Pushglobal f) = (text "Pushglobal") <+> (text f)
showInstruction (Pushint n) = (text "Pushint") <+> (integer n)
showInstruction (Push n) = (text "Push") <+> (int n)
showInstruction Mkap = text "Mkap"
showInstruction (Update n) = (text "Update") <+> (int n)
showInstruction (Pop n) = (text "Pop") <+> (int n)
showInstruction Unwind = text "Unwind"

showState :: GmState -> Doc
showState state@(code, _, _, _, _)
  = vcat $ [showStack state, showInstructions code]

showStack :: GmState -> Doc
showStack state@(_, stack, _, _, _)
  = vcat $ [text "Stack:["]
        ++ map (nest 4 . showStackItem state) (reverse stack)
        ++ [text "]"]

showStackItem :: GmState -> Addr -> Doc
showStackItem state@(_, _, heap, _, _) addr
  = text ("#" ++ show addr) <+> text ":" <+> showNode state addr (hLookup heap addr)

showNode :: GmState -> Addr -> Node -> Doc
showNode _ _ (NNum n) = integer n
showNode _ _ (NAp addr1 addr2)
  = text "Ap" <+> text ("#" ++ show addr1) <+> text ("#" ++ show addr2)
showNode (_, _, _, globals, _) addr (NGlobal _ _) = text "Global" <+> text v
  where v = head [ n | (n, b) <- globals, addr == b ]
showNode _ _ (NInd addr) = text "Ind" <+> text ("#" ++ show addr)
