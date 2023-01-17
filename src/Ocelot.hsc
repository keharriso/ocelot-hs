{-
The MIT License (MIT)

Copyright (c) 2023 Kevin Harrison

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

{-# LANGUAGE CApiFFI #-}

module Ocelot
    ( CSymbol (..)
    , CSymbolClass (..)
    , CSymbolLinkage (..)
    , CType (..)
    , RecordField
    , EnumField
    , EnumValue
    , parse ) where

#include "ocelot.h"

import Data.Int
import Data.Foldable ( traverse_ )
import Data.Word
import Foreign ( Ptr, nullPtr, free, newArray0, peekArray0, castPtr, plusPtr )
import Foreign.C ( CString, newCString, peekCString )
import Foreign.Storable ( peekByteOff )

data CSymbol = CSymbol
    { symbolName :: String
    , symbolClass :: CSymbolClass
    , symbolLinkage :: CSymbolLinkage
    , symbolElaborated :: Bool
    , symbolType :: CType }
    deriving (Eq, Show)

data CSymbolClass
    = CS_Function
    | CS_Type
    | CS_Variable
    deriving (Enum, Eq, Show)

data CSymbolLinkage
    = CS_Private
    | CS_Public
    | CS_Extern
    deriving (Enum, Eq, Show)

data CType
    = CT_Void
    | CT_Pointer CType Int
    | CT_Array CType Int
    | CT_Char
    | CT_UChar
    | CT_Short
    | CT_UShort
    | CT_Int
    | CT_UInt
    | CT_Long
    | CT_ULong
    | CT_LLong
    | CT_ULLong
    | CT_Float
    | CT_Double
    | CT_LDouble
    | CT_Bool
    | CT_Function (Maybe String) CType [CType] Bool
    | CT_Struct (Maybe String) (Maybe [RecordField])
    | CT_Union (Maybe String) (Maybe [RecordField])
    | CT_Enum (Maybe String) (Maybe [EnumField])
    deriving (Eq, Show)

type RecordField = (String, CType)
type EnumField = (String, EnumValue)
type EnumValue = #{type long long}

foreign import capi "ocelot.h ocelot_parse" ocelotParse :: CString -> Ptr CString -> IO (Ptr ())
foreign import capi "ocelot.h ocelot_symbols_get_all" ocelotSymbolsGetAll :: Ptr () -> IO (Ptr ())
foreign import capi "ocelot.h ocelot_symbols_delete" ocelotSymbolsDelete :: Ptr () -> IO ()

parse :: String -> [String] -> IO (Maybe [CSymbol])
parse filePath includeDirs = do
    filePathCs <- newCString filePath
    includeDirsArray <- newArray0 nullPtr =<< traverse newCString includeDirs
    symbolTable <- ocelotParse filePathCs includeDirsArray
    free filePathCs
    includeDirsCsList <- peekArray0 nullPtr includeDirsArray
    traverse_ free includeDirsCsList 
    free includeDirsArray
    if symbolTable /= nullPtr
        then do
            symbolsPointer <- ocelotSymbolsGetAll symbolTable
            let symbolsArray = castPtr symbolsPointer :: Ptr (Ptr ())
            symbolPointers <- peekArray0 nullPtr symbolsArray
            symbols <- traverse createSymbol symbolPointers
            ocelotSymbolsDelete symbolTable
            free symbolsPointer
            return $ Just symbols
        else return Nothing

createSymbol :: Ptr () -> IO CSymbol
createSymbol p = do
    rawName <- #{peek ocelot_symbol, name} p
    rawClass <- #{peek ocelot_symbol, symbol_class} p
    rawLinkage <- #{peek ocelot_symbol, linkage} p
    rawElaborated <- #{peek ocelot_symbol, elaborated} p
    rawTypePtr <- #{peek ocelot_symbol, type} p

    sName <- peekCString rawName
    let sClass = toEnum $ fromIntegral (rawClass :: #{type ocelot_symbol_class})
    let sLinkage = toEnum $ fromIntegral (rawLinkage :: #{type ocelot_symbol_linkage})
    let sElaborated = rawElaborated :: Bool
    sType <- createType rawTypePtr

    return $ CSymbol
        { symbolName = sName
        , symbolClass = sClass
        , symbolLinkage = sLinkage
        , symbolElaborated = sElaborated
        , symbolType = sType }

createType :: Ptr () -> IO CType
createType p = do
    rawName <- #{peek ocelot_type, name} p
    rawClass <- #{peek ocelot_type, type_class} p
    let rawCompoundPtr = #{ptr ocelot_type, compound} p

    case (rawClass :: #{type ocelot_type_class}) of
        0  -> return CT_Void
        1  -> createPointerType rawCompoundPtr
        2  -> createArrayType rawCompoundPtr
        3  -> return CT_Char
        4  -> return CT_UChar
        5  -> return CT_Short
        6  -> return CT_UShort
        7  -> return CT_Int
        8  -> return CT_UInt
        9  -> return CT_Long
        10 -> return CT_ULong
        11 -> return CT_LLong
        12 -> return CT_ULLong
        13 -> return CT_Float
        14 -> return CT_Double
        15 -> return CT_LDouble
        16 -> return CT_Bool
        17 -> createFunctionType rawName rawCompoundPtr
        18 -> createStructType rawName rawCompoundPtr
        19 -> createUnionType rawName rawCompoundPtr
        20 -> createEnumType rawName rawCompoundPtr
        _  -> return CT_Void

createPointerType :: Ptr () -> IO CType
createPointerType p = do
    let rawPointer = #{ptr ocelot_compound_type, pointer} p
    rawIndirection <- #{peek struct ocelot_pointer_type, indirection} rawPointer
    rawBaseType <- #{peek struct ocelot_pointer_type, base_type} rawPointer

    let pIndirection = rawIndirection :: #{type int}
    pBaseType <- createType rawBaseType

    return $ CT_Pointer pBaseType $ fromIntegral pIndirection

createArrayType :: Ptr () -> IO CType
createArrayType p = do
    let rawPointer = #{ptr ocelot_compound_type, array} p
    rawSize <- #{peek struct ocelot_array_type, size} rawPointer
    rawBaseType <- #{peek struct ocelot_array_type, base_type} rawPointer

    let aSize = rawSize :: #{type int}
    aBaseType <- createType rawBaseType

    return $ CT_Array aBaseType $ fromIntegral aSize

unwrapName :: Ptr () -> IO (Maybe String)
unwrapName rawName
    | rawName == nullPtr = return Nothing
    | otherwise = do
        name <- peekCString $ castPtr rawName
        return $ Just name

createFunctionType :: Ptr () -> Ptr () -> IO CType
createFunctionType rawName p = do
    let rawPointer = #{ptr ocelot_compound_type, function} p
    rawParameters <- #{peek struct ocelot_function_type, parameters} rawPointer
    rawReturnType <- #{peek struct ocelot_function_type, return_type} rawPointer
    rawVariadic <- #{peek struct ocelot_function_type, variadic} rawPointer

    fName <- unwrapName rawName
    fParameters <- createFunctionParameterTypes rawParameters
    fReturnType <- createType rawReturnType
    let fVariadic = rawVariadic :: Bool

    return $ CT_Function fName fReturnType fParameters fVariadic

createFunctionParameterTypes :: Ptr () -> IO [CType]
createFunctionParameterTypes p = do
    rawParameters <- peekArray0 nullPtr (castPtr p)
    traverse createType rawParameters

createStructType :: Ptr () -> Ptr () -> IO CType
createStructType = createRecordType CT_Struct

createUnionType :: Ptr () -> Ptr () -> IO CType
createUnionType = createRecordType CT_Union

createRecordType :: ((Maybe String) -> (Maybe [RecordField]) -> CType) -> Ptr () -> Ptr () -> IO CType
createRecordType recordType rawName p = do
    name <- unwrapName rawName
    rawFieldsPointer <- #{peek ocelot_compound_type, record_fields} p
    if rawFieldsPointer /= nullPtr then do
        rawFields <- peekArray0 nullPtr rawFieldsPointer
        fields <- traverse createRecordField rawFields
        return $ recordType name (Just fields)
    else
        return $ recordType name Nothing
    

createRecordField :: Ptr () -> IO RecordField
createRecordField p = do
    rawName <- #{peek ocelot_record_field, name} p
    rawType <- #{peek ocelot_record_field, type} p

    fieldName <- peekCString rawName
    fieldType <- createType rawType

    return (fieldName, fieldType)

createEnumType :: Ptr () -> Ptr () -> IO CType
createEnumType rawName p = do
    name <- unwrapName rawName
    rawFieldsPointer <- #{peek ocelot_compound_type, enum_fields} p
    if rawFieldsPointer /= nullPtr then do
        rawFields <- peekArray0 nullPtr rawFieldsPointer
        fields <- traverse createEnumField rawFields
        return $ CT_Enum name (Just fields)
    else
        return $ CT_Enum name Nothing

createEnumField :: Ptr () -> IO EnumField
createEnumField p = do
    rawName <- #{peek ocelot_enum_field, name} p
    rawValue <- #{peek ocelot_enum_field, value} p

    fieldName <- peekCString rawName
    let fieldValue = rawValue :: #{type long long}

    return (fieldName, fieldValue)