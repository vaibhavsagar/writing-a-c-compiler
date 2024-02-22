module CodeGen where

import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as Map

import qualified Tacky

data Program = Program Function deriving (Eq, Show)
data Function = Function { functionName :: Identifier, instructions :: [Instruction] } deriving (Eq, Show)
data Instruction
    = Mov { movSrc :: Operand, movDst :: Operand }
    | Unary UnaryOperator Operand
    | AllocateStack Int
    | Ret
    deriving (Eq, Show)
data UnaryOperator = Neg | Not deriving (Eq, Show)
data Operand
    = Imm Int
    | Reg Register
    | Pseudo Identifier
    | Stack Int
    deriving (Eq, Show)
data Register = AX | R10 deriving (Eq, Show)
newtype Identifier = Identifier String deriving (Eq, Show)

data PseudoregisterState = PseudoregisterState
    { pseudoregisterMap :: Map.Map String Int
    , pseudoregisterOffset :: Int
    } deriving (Eq, Show)

codegen :: Tacky.Program -> Program
codegen program = let
    stage1 = codeGenProgram program
    (offset, stage2) = replacePseudoregistersProgram stage1
    stage3 = fixupInstructionsProgram offset stage2
    in stage3

codeGenProgram :: Tacky.Program -> Program
codeGenProgram (Tacky.Program function) = Program (codeGenFunction function)

codeGenFunction :: Tacky.Function -> Function
codeGenFunction (Tacky.Function (Tacky.Identifier name) is) = Function (Identifier name) (concatMap codeGenInstruction is)

codeGenInstruction :: Tacky.Instruction -> [Instruction]
codeGenInstruction instr = case instr of
    Tacky.ReturnInstruction value ->
        [ Mov (codeGenValue value) (Reg AX)
        , Ret
        ]
    Tacky.UnaryInstruction op src dst ->
        [ Mov (codeGenValue src) (codeGenValue dst)
        , Unary (codeGenUnaryOperator op) (codeGenValue dst)
        ]

codeGenValue :: Tacky.Value -> Operand
codeGenValue value = case value of
    Tacky.ConstantValue int -> Imm int
    Tacky.VarValue (Tacky.Identifier name) -> Pseudo $ Identifier name

codeGenUnaryOperator :: Tacky.UnaryOperator -> UnaryOperator
codeGenUnaryOperator op = case op of
    Tacky.Complement -> Not
    Tacky.Negate -> Neg

replacePseudoregistersProgram :: Program -> (Int, Program)
replacePseudoregistersProgram (Program function) = let
    (offset, processedFunction) = replacePseudoregistersFunction function
    in (offset, Program processedFunction)

replacePseudoregistersFunction :: Function -> (Int, Function)
replacePseudoregistersFunction (Function name is) = let
    (is', finalState) = runState (traverse replacePseudoregistersInstruction is) $ PseudoregisterState mempty 0
    in (pseudoregisterOffset finalState, Function name is')

replacePseudoregistersInstruction :: Instruction -> State PseudoregisterState Instruction
replacePseudoregistersInstruction instr = case instr of
    Mov src dst -> do
        srcProcessed <- replacePseudoregister src
        dstProcessed <- replacePseudoregister dst
        pure $ Mov srcProcessed dstProcessed
    Unary op operand -> do
        operandProcessed <- replacePseudoregister operand
        pure $ Unary op operandProcessed
    _ -> pure instr

replacePseudoregister :: Operand -> State PseudoregisterState Operand
replacePseudoregister o = case o of
    Pseudo (Identifier key) -> do
        PseudoregisterState registerMap registerOffset <- get
        case Map.lookup key registerMap of
            Just offset -> pure $ Stack offset
            Nothing -> do
                let newOffset = registerOffset - 4
                let newRegisterMap = Map.insert key newOffset registerMap
                put $ PseudoregisterState newRegisterMap newOffset
                pure $ Stack newOffset
    _ -> pure o

fixupInstructionsProgram :: Int -> Program -> Program
fixupInstructionsProgram offset (Program function) = let
    processedFunction = fixupInstructionsFunction offset function
    in Program processedFunction

fixupInstructionsFunction :: Int -> Function -> Function
fixupInstructionsFunction offset (Function name instrs) = let
    processedInstrs = concatMap fixupInstruction instrs
    in Function name ([AllocateStack (abs offset)] <> processedInstrs)

fixupInstruction :: Instruction -> [Instruction]
fixupInstruction instruction = case instruction of
    Mov src@Stack{} dst@Stack{} -> [Mov src (Reg R10), Mov (Reg R10) dst]
    _ -> [instruction]
