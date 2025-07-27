{-# OPTIONS_GHC -w #-}
module L.L2.Frontend.Parser (l2Parser, parserTest, parser) where

import L.L2.Frontend.Lexer
  ( Alex
  , runAlex
  , alexMonadScan
  , alexError
  , Token(..)
  , Lexeme(..)
  , happyLexer
  )
import L.L2.Frontend.Syntax (L2(..), S2(..), E2(..))
import Utils.Var  (Var(..))
import Utils.Value(Value(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,73) ([256,22,0,0,0,5633,512,0,8,2048,0,1,512,49152,9,128,49152,9,0,0,484,0,0,0,0,49152,9,0,8,496,2496,0,8672,1024,49152,9,2496,49152,9,2496,0,1,61440,1,0,0,0,16,0,0,0,32768,1,384,0,0,5633,0,64,4,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","L2","S2","E2","num","str","id","\":=\"","\";\"","\"(\"","\")\"","\"+\"","\"-\"","\"*\"","\"/\"","read","print","\",\"","def","in","end","%eof"]
        bit_start = st Prelude.* 24
        bit_end = (st Prelude.+ 1) Prelude.* 24
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..23]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_4
action_0 (18) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (21) = happyShift action_7
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (24) = happyAccept
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (9) = happyShift action_4
action_3 (18) = happyShift action_5
action_3 (19) = happyShift action_6
action_3 (21) = happyShift action_7
action_3 (4) = happyGoto action_12
action_3 (5) = happyGoto action_3
action_3 _ = happyReduce_1

action_4 (10) = happyShift action_11
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (12) = happyShift action_10
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (12) = happyShift action_9
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (9) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (10) = happyShift action_20
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (7) = happyShift action_14
action_9 (8) = happyShift action_15
action_9 (9) = happyShift action_16
action_9 (12) = happyShift action_17
action_9 (6) = happyGoto action_19
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (8) = happyShift action_18
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (7) = happyShift action_14
action_11 (8) = happyShift action_15
action_11 (9) = happyShift action_16
action_11 (12) = happyShift action_17
action_11 (6) = happyGoto action_13
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_2

action_13 (11) = happyShift action_29
action_13 (14) = happyShift action_23
action_13 (15) = happyShift action_24
action_13 (16) = happyShift action_25
action_13 (17) = happyShift action_26
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_12

action_15 _ = happyReduce_13

action_16 _ = happyReduce_14

action_17 (7) = happyShift action_14
action_17 (8) = happyShift action_15
action_17 (9) = happyShift action_16
action_17 (12) = happyShift action_17
action_17 (6) = happyGoto action_28
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (20) = happyShift action_27
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (13) = happyShift action_22
action_19 (14) = happyShift action_23
action_19 (15) = happyShift action_24
action_19 (16) = happyShift action_25
action_19 (17) = happyShift action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (7) = happyShift action_14
action_20 (8) = happyShift action_15
action_20 (9) = happyShift action_16
action_20 (12) = happyShift action_17
action_20 (6) = happyGoto action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (14) = happyShift action_23
action_21 (15) = happyShift action_24
action_21 (16) = happyShift action_25
action_21 (17) = happyShift action_26
action_21 (22) = happyShift action_37
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (11) = happyShift action_36
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (7) = happyShift action_14
action_23 (8) = happyShift action_15
action_23 (9) = happyShift action_16
action_23 (12) = happyShift action_17
action_23 (6) = happyGoto action_35
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (7) = happyShift action_14
action_24 (8) = happyShift action_15
action_24 (9) = happyShift action_16
action_24 (12) = happyShift action_17
action_24 (6) = happyGoto action_34
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (7) = happyShift action_14
action_25 (8) = happyShift action_15
action_25 (9) = happyShift action_16
action_25 (12) = happyShift action_17
action_25 (6) = happyGoto action_33
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (7) = happyShift action_14
action_26 (8) = happyShift action_15
action_26 (9) = happyShift action_16
action_26 (12) = happyShift action_17
action_26 (6) = happyGoto action_32
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_31
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (13) = happyShift action_30
action_28 (14) = happyShift action_23
action_28 (15) = happyShift action_24
action_28 (16) = happyShift action_25
action_28 (17) = happyShift action_26
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_4

action_30 _ = happyReduce_11

action_31 (13) = happyShift action_39
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_10

action_33 _ = happyReduce_9

action_34 (16) = happyShift action_25
action_34 (17) = happyShift action_26
action_34 _ = happyReduce_8

action_35 (16) = happyShift action_25
action_35 (17) = happyShift action_26
action_35 _ = happyReduce_7

action_36 _ = happyReduce_6

action_37 (9) = happyShift action_4
action_37 (18) = happyShift action_5
action_37 (19) = happyShift action_6
action_37 (21) = happyShift action_7
action_37 (4) = happyGoto action_38
action_37 (5) = happyGoto action_3
action_37 _ = happyReduce_1

action_38 (23) = happyShift action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (11) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_5

action_41 _ = happyReduce_3

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 (L2 []
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (let L2 xs = happy_var_2 in L2 ( happy_var_1 : xs )
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 7 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (let (L2 inner) = happy_var_6 in Def (Var happy_var_2) happy_var_4 inner
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LAssign (Var happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 7 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TString happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LRead happy_var_3 (Var happy_var_5)
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LPrint happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LAdd happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LMinus happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LMul happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LDiv happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn6
		 (LVal (VInt happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn6
		 (LVal (VStr happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn6
		 (LVar (Var happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyNewToken action sts stk
	= happyLexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TEOF -> action 24 24 tk (HappyState action) sts stk;
	Token _ (TNumber happy_dollar_dollar) -> cont 7;
	Token _ (TString happy_dollar_dollar) -> cont 8;
	Token _ (TIdent happy_dollar_dollar) -> cont 9;
	Token _ TAssign -> cont 10;
	Token _ TSemi -> cont 11;
	Token _ TLParen -> cont 12;
	Token _ TRParen -> cont 13;
	Token _ TPlus -> cont 14;
	Token _ TMinus -> cont 15;
	Token _ TTimes -> cont 16;
	Token _ TDiv -> cont 17;
	Token _ TRead -> cont 18;
	Token _ TPrint -> cont 19;
	Token _ TComma -> cont 20;
	Token _ TDef -> cont 21;
	Token _ TIn -> cont 22;
	Token _ TEnd -> cont 23;
	_ -> happyError' (tk, [])
	})

happyError_ explist 24 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = ((>>=))
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parser = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parserTest :: String -> IO ()
parserTest s = do
  r <- l2Parser s 
  print r 

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)


l2Parser :: String -> IO (Either String L2)
l2Parser content = do 
  pure $ runAlex content parser
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
