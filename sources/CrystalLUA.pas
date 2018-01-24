unit CrystalLUA;

{******************************************************************************}
{ Copyright (c) 2010-2018 Dmitry Mozulyov                                      }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ email: softforyou@inbox.ru                                                   }
{ skype: dimandevil                                                            }
{ repository: https://github.com/d-mozulyov/CrystalLUA                         }
{******************************************************************************}

// you can define LUA_INITIALIZE to create and destroy Lua: TLua instance automatically
//{$define LUA_INITIALIZE}

// you can choose encoding by define LUA_UNICODE or LUA_ANSI directly
// but if you ignore - it will be defined automatically by UNICODE directive case
//{$define LUA_UNICODE}
//{$define LUA_ANSI}

// you can disable Classes unit using if you want.
// it may minimize exe size for simple applications, such as a console
//{$define LUA_NOCLASSES}


// compiler directives
{$ifdef FPC}
  {$mode delphi}
  {$asmmode intel}
  {$define INLINESUPPORT}
  {$define INLINESUPPORTSIMPLE}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
{$else}
  {$if CompilerVersion >= 24}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion >= 15}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ifend}
  {$if CompilerVersion >= 20}
    {$define INLINESUPPORT}
  {$ifend}
  {$if CompilerVersion >= 17}
    {$define INLINESUPPORTSIMPLE}
  {$ifend}
  {$if CompilerVersion < 23}
    {$define CPUX86}
  {$ifend}
  {$if CompilerVersion >= 23}
    {$define UNITSCOPENAMES}
    {$define RETURNADDRESS}
  {$ifend}
  {$if CompilerVersion >= 21}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
    {$define EXTENDEDRTTI}
  {$ifend}
  {$if (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
    {$define INTERNALCODEPAGE}
  {$ifend}
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$if Defined(CPUX86) or Defined(CPUX64)}
  {$define CPUINTEL}
{$ifend}
{$if Defined(CPUX64) or Defined(CPUARM64)}
  {$define LARGEINT}
{$ifend}
{$if (not Defined(CPUX64)) and (not Defined(CPUARM64))}
  {$define SMALLINT}
{$ifend}
{$if Defined(FPC) or (CompilerVersion >= 18)}
  {$define OPERATORSUPPORT}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}
{$ifdef KOL}
  {$defined LUA_NOCLASSES}
{$endif}

{$if Defined(LUA_UNICODE) and Defined(LUA_ANSI)}
  {$MESSAGE ERROR 'defined both encodings: LUA_UNICODE and LUA_ANSI'}
{$ifend}
{$if (not Defined(LUA_UNICODE)) and (not Defined(LUA_ANSI))}
   {$ifdef UNICODE}
      {$define LUA_UNICODE}
   {$else}
      {$define LUA_ANSI}
   {$endif}
{$ifend}

{$ifdef MSWINDOWS}
  {$define LUA_NATIVEFUNC}
{$endif}

interface
  uses {$ifdef UNITSCOPENAMES}System.Types, System.TypInfo, System.RTLConsts{$else}Types, TypInfo, RTLConsts{$endif},
       {$ifdef UNICODE}{$ifdef UNITSCOPENAMES}System.Character{$else}Character{$endif},{$endif}
       {$ifdef EXTENDEDRTTI}{$ifdef UNITSCOPENAMES}System.Rtti{$else}Rtti{$endif},{$endif}
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif},{$endif}
       {$ifdef POSIX}Posix.Base, Posix.String_, Posix.Unistd, Posix.SysTypes, Posix.PThread,{$endif}
       {$ifdef KOL}
         KOL, err
       {$else}
         {$ifdef UNITSCOPENAMES}System.SysUtils{$else}SysUtils{$endif}
       {$endif}
       {$ifNdef LUA_NOCLASSES}
         {$ifdef UNITSCOPENAMES}, System.Classes{$else}, Classes{$endif}
       {$endif};

type
  // standard types
  {$ifdef FPC}
    PUInt64 = ^UInt64;
  {$else}
    {$if CompilerVersion < 15}
      UInt64 = Int64;
      PUInt64 = ^UInt64;
    {$ifend}
    {$if CompilerVersion < 19}
      NativeInt = Integer;
      NativeUInt = Cardinal;
    {$ifend}
    {$if CompilerVersion < 22}
      PNativeInt = ^NativeInt;
      PNativeUInt = ^NativeUInt;
    {$ifend}
  {$endif}
  {$if Defined(FPC) or (CompilerVersion < 23)}
  TExtended80Rec = Extended;
  PExtended80Rec = ^TExtended80Rec;
  {$ifend}
  TBytes = array of Byte;
  PBytes = ^TBytes;

  // exception class
  ELua = class(Exception)
  {$ifdef KOL}
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: NativeUInt); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(Ident: NativeUInt; const Args: array of const); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const); overload;
  {$endif}
  end;

  // CrystalLUA string types
  {$ifNdef UNICODE}
    UnicodeString = WideString;
    PUnicodeString = PWideString;
  {$endif}
  {$ifdef NEXTGEN}
    AnsiChar = Byte;
    PAnsiChar = PByte;
    AnsiString = TBytes;
    WideString = UnicodeString;
    PWideString = PUnicodeString;
    ShortString = array[Byte] of Byte;
    PShortString = ^ShortString;
  {$endif}  
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    LuaString = UnicodeString;
    PLuaString = PUnicodeString;
    {$ifNdef UNICODE}
      {$define LUA_LENGTH_SHIFT}
    {$endif}
    LuaChar = WideChar;
    PLuaChar = PWideChar;
  {$else}
    LuaString = AnsiString;
    PLuaString = PAnsiString;
    LuaChar = AnsiChar;
    PLuaChar = PAnsiChar;
  {$ifend}

  // internal string identifier: utf8 or ansi
  __luaname = type PAnsiChar;
  // internal character pointer: utf8 or ansi
  __luadata = type PAnsiChar;
  // internal character storage: utf8 or ansi
  __luabuffer = type AnsiString;
  // internal memory offset (optional pointer in 32-bit platforms)
  __luapointer = type Integer;
  // internal parametrized function
  __luafunction = type {$ifdef LUA_NATIVEFUNC}Pointer{$else}Integer{$endif};

  // internal containers
  __TLuaMemoryHeap = array[1..12{$ifdef LARGEINT}* 2{$endif}] of Byte;
  __TLuaStack = array[1..12{$ifdef LARGEINT}* 2{$endif}] of Byte;
  __TLuaBuffer = array[1..12{$ifdef LARGEINT}* 2{$endif}] of Byte;
  __TLuaDictionary = array[1..20{$ifdef LARGEINT}* 2{$endif}] of Byte;

type
  TLua = class;
  PLuaArg = ^TLuaArg;
  PLuaTable = ^TLuaTable;

  // incorrect script use exception
  ELuaScript = class(ELua);

  // script stack overflow
  ELuaStackOverflow = class(ELuaScript);

  // Lua types, that is used between script and native side
  TLuaArgType = (
    // simple types
    ltEmpty, ltBoolean, ltInteger, ltDouble, ltString, ltPointer,
    // difficult types
    ltClass, ltObject, ltRecord, ltArray, ltSet, ltTable
    {ToDo: ltInterface, ltMethod, ltReference }
    );

  // internal base stucture for records, arrays, sets, etc (inside TLuaArg)
  TLuaDifficultType = object
  protected
    FAlign: array[0..1] of Byte; {FLuaType + reserved}
    FIsRef: Boolean;
    FIsConst: Boolean;
  public
    Data: Pointer;
    { info: information type }
    property IsRef: Boolean read FIsRef write FIsRef;
    property IsConst: Boolean read FIsConst write FIsConst;
  end;

  // Record instance information: Pointer/IsRef/IsConst/RecordInfo
  PLuaRecordInfo = ^TLuaRecordInfo;
  TLuaRecord = object(TLuaDifficultType)
  public
    Info: PLuaRecordInfo;
  end;
  PLuaRecord = ^TLuaRecord;

  // Array instance information: Pointer/IsRef/IsConst/ArrayInfo
  PLuaArrayInfo = ^TLuaArrayInfo;
  TLuaArray = object(TLuaDifficultType)
  public
    Info: PLuaArrayInfo;
  end;
  PLuaArray = ^TLuaArray;

  // Set instance information: Pointer/IsRef/IsConst/SetInfo
  PLuaSetInfo = ^TLuaSetInfo;
  TLuaSet = object(TLuaDifficultType)
  public
    Info: PLuaSetInfo;
  end;
  PLuaSet = ^TLuaSet;

  // universal CrystalLUA argument
  TLuaArg = object
  private
    FStringValue: LuaString;
    F: packed record
      LuaType: TLuaArgType;
      AdvancedData: array[0..2] of Byte; { used in TLuaRecord/TLuaArray/TLuaTable etc }
    case Integer of
      0: (VDouble: Double);
      1: (VBoolean: Boolean);
      2: (VPointer: Pointer);
      3: (VInteger: Integer);
      4: (VNativeInt: NativeInt);
      5: (VData: Pointer; VInfo: Pointer);
    end;

    function TypeException(const AType: TLuaArgType): ELua;
    function VariantException: ELua;
    procedure CheckDifficultSetter(const Value: TLuaDifficultType; const Name: PChar; const ReturnAddress: Pointer);
    function  GetLuaTypeName: string;
    function  GetEmpty: Boolean;
    procedure SetEmpty(const Value: Boolean);
    function  GetBoolean: Boolean;
    procedure SetBoolean(const Value: Boolean);
    function  GetInteger: Integer;
    procedure SetInteger(const Value: Integer);
    function  GetDouble: Double;
    procedure SetDouble(Value: Double);
    function  GetString: LuaString;
    procedure SetString(const Value: LuaString);
    function  GetPointer: Pointer;
    procedure SetPointer(const Value: Pointer);
    function  GetVariant: Variant;
    procedure SetVariant(const Value: Variant);
    function  GetClass: TClass;
    procedure SetClass(const Value: TClass);
    function  GetObject: TObject;
    procedure SetObject(const Value: TObject);
    function  GetRecord: TLuaRecord;
    procedure SetRecord(const Value: TLuaRecord);
    function  GetArray: TLuaArray;
    procedure SetArray(const Value: TLuaArray);
    function  GetSet: TLuaSet;
    procedure SetSet(const Value: TLuaSet);
    function  GetTable: PLuaTable;
  public
    property LuaType: TLuaArgType read F.LuaType;
    property LuaTypeName: string read GetLuaTypeName;
    property Empty: Boolean read GetEmpty write SetEmpty;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;
    property AsInteger: Integer read GetInteger write SetInteger;
    property AsDouble: Double read GetDouble write SetDouble;
    property AsString: LuaString read GetString write SetString;
    property AsPointer: Pointer read GetPointer write SetPointer;
    property AsVariant: Variant read GetVariant write SetVariant;
    property AsClass: TClass read GetClass write SetClass;
    property AsObject: TObject read GetObject write SetObject;
    property AsRecord: TLuaRecord read GetRecord write SetRecord;
    property AsArray: TLuaArray read GetArray write SetArray;
    property AsSet: TLuaSet read GetSet write SetSet;
    property AsTable: PLuaTable read GetTable;

    function ForceBoolean: Boolean;
    function ForceInteger: NativeInt;
    function ForceDouble: Double;
    function ForceString: LuaString;
    function ForcePointer: Pointer;
    function ForceVariant: Variant;
    function ForceClass: TClass;
    function ForceObject: TObject;
    function ForceRecord: TLuaRecord;
    function ForceArray: TLuaArray;
    function ForceSet: TLuaSet;
    function ForceTable: PLuaTable;
  end;
  TLuaArgs = array of TLuaArg;
  TLuaArgList = array[0..High(Integer) div SizeOf(TLuaArg) - 1] of TLuaArg;
  PLuaArgList = ^TLuaArgList;

  TLuaProcCallback = function(const Args: TLuaArgs): TLuaArg;
  TLuaMethodCallback = function(const Instance: Pointer; const Args: TLuaArgs): TLuaArg;
  TLuaMethodKind = (mkStatic, mkInstance, mkClass, mkConstructor, mkDestructor, {ToDo}mkOperator);

  TLuaProcParam = packed record
    Name: LuaString;
    TypeInfo: PTypeInfo;
    Flags: TParamFlags;
    PointerDepth: Integer;
  end;
  PLuaProcParam = ^TLuaProcParam;

                           (*
  // highlevel interface to traverse table items with pair <Key, Value>
  TLuaPair = object
  private
  {$hints off}
    Mode: integer; // начало. итерация. конец.
    Lua: TLua;
    Handle: pointer; // TLua.Handle
    Index: integer; // натуральный индекс таблицы
    KeyIndex, ValueIndex: integer; // натуральные индексы для ключа и значения
    FIteration: integer; // текущая итерация

    procedure ThrowNotInitialized(const CodeAddr: pointer);
    procedure ThrowValueType(const CodeAddr: pointer; const pop: boolean=false);
    procedure ThrowBroken(const CodeAddr: pointer; const Action: string);
    function  Initialize(const ALua: TLua; const AIndex: integer; const UseKey: boolean): boolean;
    function  GetKey: string;
    function  GetKeyEx: Variant;
    function  GetValue: Variant;
    function  GetValueEx: TLuaArg;
    procedure SetValue(const AValue: Variant);
    procedure SetValueEx(const AValue: TLuaArg);
    function  GetBroken: boolean;
  public
    function  Next(): boolean;
    procedure Break();

    property Iteration: integer read FIteration;
    property Broken: boolean read GetBroken;
    property Key: string read GetKey;
    property KeyEx: Variant read GetKeyEx;
    property Value: Variant read GetValue write SetValue;
    property ValueEx: TLuaArg read GetValueEx write SetValueEx;
  end;
  {$hints on}      *)

  // highlevel interface to read and modify Lua-tables
  TLuaTable = object
  protected
    FNone: Byte; { TLuaArgType = ltTable }
    FAdvancedData: array[0..2] of Byte;
    FLua: TLua;
    FIndex: Integer;
    function  GetLength: Integer;
(*    function  GetCount: integer;

    procedure ThrowValueType(const CodeAddr: pointer; const pop: boolean=false);
    function  GetValue(const AIndex: integer): Variant;
    procedure SetValue(const AIndex: integer; const NewValue: Variant);
    function  GetValueEx(const AIndex: integer): TLuaArg;
    procedure SetValueEx(const AIndex: integer; const NewValue: TLuaArg);
    function  GetKeyValue(const Key: string): Variant;
    procedure SetKeyValue(const Key: string; const NewValue: Variant);
    function  GetKeyValueEx(const Key: Variant): TLuaArg;
    procedure SetKeyValueEx(const Key: Variant; const NewValue: TLuaArg);  *)
  public
    // перебор элементов
    (*function Pairs(var Pair: TLuaPair): boolean; overload;
    function Pairs(var Pair: TLuaPair; const FromKey: Variant): boolean; overload;  *)

    // length
    property Length: Integer read GetLength; // длинна (для массивов)
  (*  property Count: integer read GetCount; // общее количество элементов

    // values
    property Value[const Index: integer]: Variant read GetValue write SetValue;
    property KeyValue[const Key: string]: Variant read GetKeyValue write SetKeyValue;
    property ValueEx[const Index: integer]: TLuaArg read GetValueEx write SetValueEx;
    property KeyValueEx[const Key: Variant]: TLuaArg read GetKeyValueEx write SetKeyValueEx; *)
  end;


  // ссылка
  // создана для быстрого оперирования глобальными объектами, заточенными под Lua
  TLuaReference = class
  private
    Index: Integer;
  (*  Data: TLuaTable;
    FLocked: boolean;

    procedure Initialize(const ALua: TLua);
    procedure ThrowLocked(const Operation: string; const CodeAddr: pointer);
    procedure ThrowValueType(const CodeAddr: pointer);
    function  GetValue: Variant;
    function  GetValueEx: TLuaArg;
    procedure SetValue(const NewValue: Variant);
    procedure SetValueEx(const NewValue: TLuaArg);
  private
    property Lua: TLua read Data.Lua write Data.Lua;
    property Locked: boolean read FLocked write FLocked;
  public
    destructor Destroy; override;
    function AsTableBegin(var Table: PLuaTable): boolean;
    function AsTableEnd(var Table: PLuaTable): boolean;

    property Value: Variant read GetValue write SetValue;
    property ValueEx: TLuaArg read GetValueEx write SetValueEx; *)
  end;
//  TLuaReferenceDynArray = array of TLuaReference;
//  {$hints on}

  TLuaMetaKind = (mtClass, mtRecord, mtArray, mtSet);
  TLuaMetaType = object
  protected
    F: record
      Marker: Integer;
      Kind: TLuaMetaKind;
      {ToDo Managed, HFA: Boolean}
      Align: array[0..2] of Byte;
      Size: Integer;

      {$ifdef LARGEINT}
      Ptr: __luapointer;
      {$endif}
      Ref: Integer;
    case Integer of
      0: (TypeInfo: PTypeInfo);
      1: (AClass: TClass);
    end;
    FLua: TLua;
    FName: LuaString;

    {$ifdef SMALLINT}
    function GetPtr: __luapointer; {$ifdef INLINESUPPORT}inline;{$endif}
    {$endif}
    function GetManaged: Boolean;
    function GetHFA: Boolean;
    function GetHFAElementType: TFloatType;
    function GetHFAElementCount: Integer;
    procedure CheckInstance(const AKind: TLuaMetaKind; const ReturnAddress: Pointer);
  protected
    property Kind: TLuaMetaKind read F.Kind;
    property Ptr: __luapointer read {$ifdef SMALLINT}GetPtr{$else .LARGEINT}F.Ptr{$endif};
    property Ref: Integer read F.Ref;
    property Managed: Boolean read GetManaged{F.Managed};
    property HFA: Boolean read GetHFA{F.HFA};
    property HFAElementType: TFloatType read GetHFAElementType;
    property HFAElementCount: Integer read GetHFAElementCount;

    procedure Clear(const Instance: Pointer{/TObject}; const FillZero: Boolean);
    procedure Assign(const Instance, Value: Pointer{/TObject});
  public
    property Lua: TLua read FLua;
    property Name: LuaString read FName;
    property Size: Integer read F.Size;
  end;
  PLuaMetaType = ^TLuaMetaType;

  // operators
  TLuaOperator = (loNeg, loAdd, loSub, loMul, loDiv, loMod, loPow, loCompare);
  TLuaOperators = set of TLuaOperator;
  TLuaOperatorCallback = procedure(var AResult, ALeft, ARight; const Kind: TLuaOperator);

  // all information (such as name, field, methods)
  // you should use it to operate records between native and script
  TLuaRecordInfo = object(TLuaMetaType)
  protected
    FNameSpace: __TLuaDictionary;
    FOperators: TLuaOperators;
    FOperatorCallback: TLuaOperatorCallback;

    procedure InternalRegField(const FieldName: LuaString; const FieldOffset: NativeInt; const TypeInfo: Pointer);
    procedure SetOperators(const Value: TLuaOperators);
    procedure SetOperatorCallback(const Value: TLuaOperatorCallback);
  public
    procedure RegField(const FieldName: LuaString; const FieldOffset: Integer; const TypeInfo: Pointer); overload;
    procedure RegField(const FieldName: LuaString; const FieldPointer: Pointer; const TypeInfo: Pointer; const RecordInstance: Pointer = nil); overload;
    procedure RegMethod(const MethodName: LuaString; const Callback: TLuaMethodCallback; const ArgsCount: Integer = -1; const IsStatic: Boolean = False); overload;
    procedure RegMethod(const MethodName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo; const IsStatic: Boolean = False); overload;
    procedure RegMethod(const MethodName: LuaString; const Address: Pointer;
     const Params: array of TLuaProcParam; const ResultType: PTypeInfo = nil;
     const IsStatic: Boolean = False; const CallConv: TCallConv = ccReg); overload;

    property Operators: TLuaOperators read FOperators write SetOperators;
    property OperatorCallback: TLuaOperatorCallback read FOperatorCallback write SetOperatorCallback;
  end;

  // information needed to use arrays between native and script side
  TLuaArrayInfo = object(TLuaMetaType)
  protected
   (*ItemInfo: array[0..31] of byte; // TLuaPropertyInfo; *)
    // basics
    FIsDynamic: Boolean;
    FDimention: Integer;
    FBounds: PInteger; // static array bounds
    FItemSize: Integer;

    // finalizations
    FFinalTypeInfo: PTypeInfo; // final item type info or self for dynamic
    FFinalItemsCount: Integer;

    // managing: multiplies (static), typeinfo (dynamic)
    FMultiplies: array[0..0] of NativeInt;
  public
    property IsDynamic: Boolean read FIsDynamic;
    property Dimention: Integer read FDimention;
    property Bounds: PInteger read FBounds;
    property ItemSize: Integer read FItemSize;
  end;

  // information needed to use sets between native and script side
  TLuaSetInfo = object(TLuaMetaType)
  protected
    FItemTypeInfo: PTypeInfo;
    FLow: Integer;
    FHigh: Integer;
    FCorrection: Integer;
    FRealSize: Integer;
    FAndMasks: Integer;

    function Description(const Value: Pointer): LuaString;
  public
    property Low: Integer read FLow;
    property High: Integer read FHigh;
  end;

  // (internal) information needed to use classes between native and script side
  PLuaClassInfo = ^TLuaClassInfo;
  TLuaClassInfo = object(TLuaMetaType)
  protected
    FNameSpace: __TLuaDictionary;
    function VmtOffset(const Proc: Pointer): NativeInt;
  public
    // lua_CFunction
    CFunctionCreate, CFunctionFree: Pointer;

    // special registered method
    AssignCallback: Pointer;
    CreateCallback: Pointer;
    CreateCallbackArgsCount: Integer;

    // advanced
    Parent: __luapointer;
    DefaultProperty: __luapointer{inherited bit reset};
    property AClass: TClass read F.AClass;
  end;

  TLuaScriptBOM = (sbNone, sbUTF8, sbUTF16, sbUTF16BE, cbUTF32, cbUTF32BE);
  TLuaBufferUnique = function(var Data: __luabuffer): NativeInt;
  TLuaOnPreprocessScript = procedure(var Data, Source: __luabuffer; var Offset: Integer; const UnitName: LuaString; const Unique: TLuaBufferUnique) of object;

  TLuaUnitLine = record
    Chars: __luadata;
    Count: Integer;
  end;

  TLuaUnit = class(TObject)
  private
    FLua: TLua;
    FIndex: Integer;
    FName: LuaString;
    FNameLength: Integer;
    FData: __luabuffer;
    FDataOffset: Integer;
    FFileName: string;
    FLinesCount: Integer;
    FLines: array of TLuaUnitLine;
 
    function GetItem(Index: Integer): LuaString;
    function GetLine(Index: Integer): TLuaUnitLine;
    procedure InitializeLines;
  public
    {$ifNdef LUA_NOCLASSES}
    procedure SaveToStream(const Stream: TStream);
    {$endif}
    {$ifdef KOL}
    procedure SaveToStream(const Stream: KOL.PStream);
    {$endif}
    procedure SaveToFile(const FileName: string); overload;
    procedure SaveToFile; overload;

    property Lua: TLua read FLua;
    property Index: Integer read FIndex;
    property Name: LuaString read FName;
    property FileName: string read FFileName;
    property Data: __luabuffer read FData;
    property LinesCount: Integer read FLinesCount;
    property Items[Index: Integer]: LuaString read GetItem; default;
    property Lines[Index: Integer]: TLuaUnitLine read GetLine;
  end;


{ TLuaResult/Buffer object
  Temporary memory and argument manager for registered callbacks }

  TLuaResult = object
  protected
    FItems: PLuaArgList;
    FCount: Integer;
    FCapacity: Integer;
    FParamItems: PLuaArgList;
    FParamCount: Integer;
    FParamCapacity: Integer;
    FHeapCurrent: PByte;
    FHeapOverflow: PByte;
    FHeapEmpty: NativeInt;
    FHeap: __TLuaMemoryHeap;
    FMetaStructs: Pointer;

    procedure GrowCapacity(var List: PLuaArgList; var Capacity: Integer; const Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetParamCount(const Value: Integer);
    function GrowAlloc(Size: NativeUInt): Pointer;
    function Alloc(Size: NativeUInt): Pointer;
    function AllocMetaType(const MetaType: PLuaMetaType): Pointer;
    procedure Clear;
    procedure FinalizeArgs(var List: PLuaArgList; var Capacity, ACount: Integer);
    procedure Finalize;
  public
    property Items: PLuaArgList read FItems;
    property Count: Integer read FCount write SetCount;
  end;
  PLuaResult = ^TLuaResult;

  TLuaResultBuffer = object(TLuaResult)
  public
    function AllocRecord(const RecordInfo: PLuaRecordInfo): Pointer;
    function AllocArray(const ArrayInfo: PLuaArrayInfo): Pointer;
    function AllocSet(const SetInfo: PLuaSetInfo): Pointer;
  end;
  PLuaResultBuffer = ^TLuaResultBuffer;

  TLuaPropPtr = (ppAuto, ppField, ppProc, ppClassField, ppClassProc);
  PLuaPropPtr = ^TLuaPropPtr;

{ TLua class
  Script and registered types/functions manager }

  TLua = class(TObject)
  protected
    // unicode routine
    FCodePage: Word;
    FUnicodeTable: array[Byte] of Word;
    FUTF8Table: array[Byte] of Cardinal;

    procedure SetCodePage(Value: Word);
    function AnsiFromUnicode(ADest: PAnsiChar; ACodePage: Word; ASource: PWideChar; ALength: Integer): Integer;
    procedure UnicodeFromAnsi(ADest: PWideChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer);
    { class function Utf8FromUnicode(ADest: PAnsiChar; ASource: PWideChar; ALength: Integer): Integer; static; }
    { class function UnicodeFromUtf8(ADest: PWideChar; ASource: PAnsiChar; ALength: Integer): Integer; static; }
    function Utf8FromAnsi(ADest: PAnsiChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer): Integer;
    function AnsiFromUtf8(ADest: PAnsiChar; ACodePage: Word; ASource: PAnsiChar; ALength: Integer): Integer;
    function AnsiFromAnsi(ADest: PAnsiChar; ADestCodePage: Word; ASource: PAnsiChar; ASourceCodePage: Word; ALength: Integer): Integer;
  protected
    // stack helpers
    FHandle: Pointer;
    FInternalBuffer: __TLuaBuffer;
    FInvokableBuilder: array[1..28{$ifdef LARGEINT}* 2{$endif}] of Byte{TLuaInvokableBuilder};
    FStringBuffer: record
      {$ifNdef NEXTGEN}
      Short: ShortString;
      Ansi: AnsiString;
      Wide: WideString;
      {$endif}
      Unicode: UnicodeString;
      Lua: LuaString;
      LuaReserved: LuaString;
      Default: string;
    end;
    FReturnAddress: Pointer;
    FTempBuffer: packed record
      V: Variant;
      Intf: IInterface;
      M: TMethod;
    case Integer of
      0: (B: Boolean);
      1: (D: Double);
      2: (E: Extended);
      3: (I: Integer);
      4: (C: Cardinal);
      5: (I64: Int64);
      6: (N: NativeInt);
    end;

    procedure unpack_lua_string(var Result: LuaString; const RttiName: ShortString); overload;
    procedure unpack_lua_string(var Result: LuaString; const Chars: __luadata; const Count: Integer); overload;
    procedure unpack_lua_string(var Result: LuaString; const Name: __luaname); overload;
    procedure unpack_lua_string(var Result: LuaString; const Buffer: __luabuffer); overload;

    procedure push_ansi_chars(const S: PAnsiChar; const CodePage: Word; const Count: Integer);
    procedure push_utf8_chars(const S: PAnsiChar; const Count: Integer);
    procedure push_wide_chars(const S: PWideChar; const Count: Integer);
    {$ifNdef NEXTGEN}
    procedure push_short_string(const S: ShortString);
    procedure push_ansi_string(const S: AnsiString);
    procedure push_wide_string(const S: WideString);
    {$endif}
    procedure push_unicode_string(const S: UnicodeString);
    procedure push_lua_string(const S: LuaString); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}

    {$ifNdef NEXTGEN}
    procedure stack_short_string(var S: ShortString; const StackIndex: Integer; const MaxLength: Integer);
    procedure stack_ansi_string(var S: AnsiString; const StackIndex: Integer; const CodePage: Word);
    procedure stack_wide_string(var S: WideString; const StackIndex: Integer);
    {$endif}
    procedure stack_unicode_string(var S: UnicodeString; const StackIndex: Integer);
    procedure stack_lua_string(var S: LuaString; const StackIndex: Integer); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure stack_force_unicode_string(var S: UnicodeString; const StackIndex: Integer; const ExtendedMode: Boolean = True);

    function  push_userdata(const MetaType: PLuaMetaType; const Instance: Pointer; const DcDestroy: Boolean): Pointer{PLuaUserData};
    function  push_difficult_property(const PropertyInfo; const Instance: Pointer): Pointer{PLuaUserData};
    function  push_luaarg(const LuaArg: TLuaArg): Boolean;
    function  push_variant(const Value: Variant): Boolean;
    function  push_argument(const Value: TVarRec): Boolean;
    procedure stack_pop(const Count: Integer = 1);
    function  stack_variant(var Ret: Variant; const StackIndex: Integer; const OleMode: Boolean): Boolean;
    function  stack_luaarg(var Ret: TLuaArg; const StackIndex: integer; const AllowLuaTable: Boolean): Boolean;
  protected
    // global name space
    FRef: Integer;
    FEmptyRefs: __TLuaStack;
    FMemoryHeap: __TLuaMemoryHeap;
    FStandardNames: __TLuaDictionary {__luaname, Value};
    FNames: array[1..24{$ifdef LARGEINT}* 2{$endif}] of Byte{TLuaNames};
    FMetaTypes: __TLuaDictionary {Pointer, PMetaType};
    FGlobalEntities: __TLuaDictionary {__luaname, PLuaGlobalEntity};
    FGlobalMetaTable: Integer;
    FFormatWrapper: Integer;
//    FMethodInfo: PLuaRecordInfo;

    // глобальные процедуры, переменные, калбеки глобальных переменных из Lua
   (* GlobalNative: TLuaClassInfo; // нативные: методы и перменные
    GlobalVariables: TLuaGlobalVariableDynArray; // полный список включая Lua-переменные
  //  property  NameSpaceHash: TLuaHashIndexDynArray read GlobalNative.NameSpace; // Hash по всем глобальным переменным и функциям
  *)

    // internal reference table
    function global_alloc_ref: Integer;
    procedure global_free_ref(var Ref: Integer);
    procedure global_fill_value(const Ref: Integer);
    procedure global_push_value(const Ref: Integer);

    // public globals
    function GetGlobalEntity(const Name: LuaString; const ModeCreate: Boolean): Pointer{PLuaGlobalEntity};
    function GetRecordInfo(const Name: LuaString): PLuaRecordInfo;
    function GetArrayInfo(const Name: LuaString): PLuaArrayInfo;
    function GetSetInfo(const Name: LuaString): PLuaSetInfo;
    function GetVariable(const Name: LuaString): Variant;
    procedure SetVariable(const Name: LuaString; const Value: Variant);
    function GetVariableEx(const Name: LuaString): TLuaArg;
    procedure SetVariableEx(const Name: LuaString; const Value: TLuaArg);
    function GetTableVariable(const Table, Name: LuaString): Variant;
    procedure SetTableVariable(const Table, Name: LuaString; const Value: Variant);
    function GetTableVariableEx(const Table, Name: LuaString): TLuaArg;
    procedure SetTableVariableEx(const Table, Name: LuaString; const Value: TLuaArg);
  private
    // registrations
    {$ifdef LUA_NATIVEFUNC}
    FCFunctionHeap: array[1..8{$ifdef LARGEINT}* 2{$endif}] of Byte{TLuaCFunctionHeap};
    {$endif}
   (* FEmptyMethods: __TLuaStack;
    FEmptyProperties: __TLuaStack; *)

  //  FProcList: __TLuaList {TLuaClosureInfo};
  //  FPropertiesList: __TLuaList {TLuaPropertyInfo};


   (* FInitialized: boolean;
    ClassesInfo: TLuaClassInfoDynArray;
    mt_properties: integer;
    cfunction_assign: pointer;
    cfunction_inherits_from: pointer;
    cfunction_tostring: pointer; // преобразование метатаблиц и userdata в строку
    cfunction_dynarray_resize: pointer;
    cfunction_array_include: pointer;
    cfunction_set_include: pointer;
    cfunction_set_exclude: pointer;
    cfunction_set_contains: pointer;
    ClassesIndexes: TLuaClassIndexDynArray; // быстрый поисковик по классам, структурам и массивам
    ClassesIndexesByName: TLuaClassIndexDynArray; // то же самое по именам
    EnumerationList: TIntegerDynArray; // список enumeration typeinfo, чтобы по несколько раз не регистрировать Enum-ы
  *)     (*
    procedure INITIALIZE_NAME_SPACE();    *)

  (*  function AllocMethod: __luapointer;
    procedure FreeMethod(const Value: __luapointer);
    function AllocProperty: __luapointer;
    procedure FreeProperty(const Value: __luapointer);  *)

    function push_newfunction(const Callback: Pointer; const P1, P2: __luapointer; const StoreMode: Boolean): __luafunction;
    procedure push_function(const Func: __luafunction);
    function function_topointer(const StackIndex: Integer): Pointer;

    procedure push_closure(const Name: __luaname; const Kind: Byte{TLuaClosureKind}; const InstanceKind: Byte{TLuaInstanceKind};
      const Instance: Pointer; const Address: Pointer; const InvokableMode: Boolean; const InvokableArgsCount: __luapointer);

    procedure InternalRegisterCallback(const Name: __luaname; const Callback: Pointer; const P1: __luapointer = -1; const P2: __luapointer = -1);
    procedure InternalUnregisterCallback(const Name: __luaname);
    function  InternalRegisterMetaTable(const MetaType: PLuaMetaType = nil): Integer;
    function  InternalTableToMetaType(const StackIndex: Integer): PLuaMetaType;
    function  InternalAddGlobal(const AKind: Byte{TLuaGlobalKind}; const Name: __luaname): Pointer{PLuaGlobalEntity};
    function  InternalAddMetaType(const Kind: TLuaMetaKind; const Name: LuaString; const TypeInfo: Pointer;
      const InstanceSize: Integer; const AdditionalSize: NativeInt = 0): PLuaMetaType;
    function  InternalNearestClass(const AClass: TClass): PLuaMetaType;
    function  InternalAddClass(const AClass: TClass; const UsePublished: Boolean): PLuaClassInfo;
    function  InternalAddRecord(const Name: LuaString; const TypeInfo: Pointer; const UseRttiContext: Boolean): PLuaRecordInfo;
    function  InternalAddArray(const Identifier, ItemTypeInfo: Pointer; const ABounds: array of Integer): PLuaArrayInfo;
    function  InternalAddSet(const TypeInfo: Pointer): PLuaSetInfo;
    function  InternalBuildInvokable(const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind): __luapointer; overload;
    function  InternalBuildInvokable(const Params: array of TLuaProcParam; const ResultType: PTypeInfo; const MethodKind: TLuaMethodKind; const CallConv: TCallConv): __luapointer; overload;
    function  InternalAddMethod(const MetaType: PLuaMetaType; const Name: LuaString;
      Address: Pointer; const MethodKind: TLuaMethodKind;
      const Invokable: __luapointer; const ArgsCount{Universal}: Integer = -1): __luapointer;
    procedure InternalReplaceChildNameSpace(const ParentMetaItem: Pointer{PLuaDictionaryItem};
      const Name: __luaname; const LastValue, Value: __luapointer; const IsDefaultProp: Boolean);
    function  InternalAddProperty(const MetaType: PLuaMetaType; const Name: LuaString;
      const TypeInfo: Pointer; const IsDefault, IsAutoRegister: Boolean; const Getter, Setter, Flags: NativeUInt;
      const IndexParams: Integer): __luapointer;
  private
    // script callbacks
    function push_stdandard(const MetaType: PLuaClassInfo; const StdIndex: Integer; const UserData: Pointer): Boolean;
    function set_stdandard(const MetaType: PLuaClassInfo; const StdIndex: Integer; const UserData: Pointer): Boolean;
    function call_prop_getter(const Instance: Pointer; const AProp: Pointer): Pointer;
    procedure call_prop_setter(const Instance: Pointer; const AProp: Pointer; const Value: NativeInt);

                (*
    function __tostring(): integer;
    function __inherits_from(): integer;
    function __assign(): integer;
    function __initialize_by_table(const userdata: PLuaUserData; const stack_index: integer): integer;
    function __tmethod_call(const Method: TMethod): integer;
    function __len(const ClassInfo: TLuaClassInfo): integer;
    function __operator(const ClassInfo: TLuaClassInfo; const Kind: integer): integer;
    function __constructor(const ClassInfo: TLuaClassInfo; const __create: boolean): integer;
    function __destructor(const ClassInfo: TLuaClassInfo; const __free: boolean): integer;
    function __call(const ClassInfo: TLuaClassInfo): integer;   *)
    function __index(const AMetaType: __luapointer): Integer;
    function __newindex(const AMetaType: __luapointer): Integer;
    function __global_index(const ModifyLow, ModifyHigh: Cardinal): Integer;
    function __global_newindex(const ModifyLow, ModifyHigh: Cardinal): Integer;
(*    function __array_index(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
    function __array_newindex(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
    function __array_dynamic_resize(): integer;
    function __array_include(const mode: integer{constructor, include, concat}): integer;
    function __set_method(const is_construct: boolean; const method: integer{0..2}): integer;   *)
    function __print: Integer;
    function __printf: Integer;
    function ProcCallback(const AClassInfo: __luapointer; const AProcInfo: __luapointer): Integer;

 (*   PLuaInvokable

    function ClosureCallback(const Instance: Pointer; const AClosure: Pointer): Integer;  *)

    function ClosureCallback(const AClosure: Pointer{PLuaClosure}; const ArgsCount: Integer): Integer;
    function InvokableCallback(const AClosure: Pointer{PLuaClosure}; const AInvokableData: Pointer): Integer;
  private
    // scripts and units routine
    FOnPreprocessScript: TLuaOnPreprocessScript;
    FScriptStack: array[1 - 1{Call buffer}..16] of TLuaResultBuffer;
    FScriptStackIndex: NativeUInt;
    FArgs: TLuaArgs;
    FArgsCount: Integer;

         (*
 //   FReferences: TLuaReferenceDynArray; // список ссылок (LUA_REGISTRYINDEX)  *)
    FUnitCount: Integer;
    FUnits: array of TLuaUnit;
    procedure InternalError(const Text: LuaString);
    procedure InternalErrorFmt(const FmtStr: LuaString; const Args: array of const);
    procedure CheckScriptError(const ErrCode: Integer; AUnit: TLuaUnit = nil);
    function BeginScriptStack(const ReturnAddress: Pointer): Pointer;
    procedure EndScriptStack(const ReturnAddress: Pointer);
    function  GetResultBuffer: PLuaResultBuffer;
    function  InternalConvertScript(var Data: __luabuffer): Integer;
    procedure InternalPreprocessScript(var Buffer: __luabuffer; const Offset: Integer);
    function  InternalRunScript(var Data: __luabuffer; const Offset: Integer; const AUnitName: __luaname; const AUnit: TLuaUnit; const MakeResult: Boolean; var ExceptionAddress: Pointer; const ReturnAddress: Pointer): Exception;
    procedure InternalLoadScript(var Data: __luabuffer; const UnitName: LuaString; const FileName: string);
 (*   function  InternalCheckArgsCount(PArgs: pinteger; ArgsCount: integer; const ProcName: string; const AClass: TClass): integer;
    function  StackArgument(const Index: integer): string; *)
    function  GetUnit(const Index: Integer): TLuaUnit;
    function  GetUnitByName(const Name: LuaString): TLuaUnit;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GarbageCollection;
   (* procedure SaveNameSpace(const FileName: string); dynamic;
    function CreateReference(const global_name: string=''): TLuaReference;
    class function GetProcAddress(const ProcName: pchar; const throw_exception: boolean = false): pointer; // низкий уровень. адрес функции lua.dll
                         *)

    // errors
    procedure Error(const Text: LuaString); overload;
    procedure Error(const FmtStr: LuaString; const Args: array of const); overload;

    // scripts
    procedure RunScript(const Script: LuaString);
    procedure LoadScript(const FileName: string); overload;
    procedure LoadScript(const Buffer: Pointer; const BufferSize: Integer; const BOM: TLuaScriptBOM; const UnitName: LuaString = ''); overload;
    {$ifNdef LUA_NOCLASSES}
    procedure LoadScript(const Stream: TStream; const UnitName: LuaString; const ASize: Integer = -1); overload;
    {$endif}
    {$ifdef KOL}
    procedure LoadScript(const Stream: KOL.PStream; const UnitName: LuaString; const ASize: Integer = -1); overload;
    {$endif}
    {$ifdef MSWINDOWS}
    procedure LoadScript(const Instance: THandle; const Name, ResType: PChar; const UnitName: LuaString); overload;
    {$endif}

   (*
    // проверка при калбеке
    function  CheckArgsCount(const ArgsCount: array of integer; const ProcName: string=''; const AClass: TClass=nil): integer; overload;
    function  CheckArgsCount(const ArgsCount: TIntegerDynArray; const ProcName: string=''; const AClass: TClass=nil): integer; overload;
    procedure CheckArgsCount(const ArgsCount: integer; const ProcName: string=''; const AClass: TClass=nil); overload;
      *)
    // methods
    function VariableExists(const Name: LuaString): Boolean; overload;
    function ProcExists(const Name: LuaString): Boolean; overload;
    function VariableExists(const Table, Name: LuaString): Boolean; overload;
    function ProcExists(const Table, Name: LuaString): Boolean; overload;
    function Call(const ProcName: LuaString; const Args: array of TLuaArg): PLuaResult; overload;
    function Call(const ProcName: LuaString; const Args: array of const): PLuaResult; overload;
    function Call(const Table, ProcName: LuaString; const Args: array of TLuaArg): PLuaResult; overload;
    function Call(const Table, ProcName: LuaString; const Args: array of const): PLuaResult; overload;

    // registrations
    procedure RegClass(const AClass: TClass; const UsePublished: Boolean = True);
    procedure RegClasses(const AClasses: array of TClass; const UsePublished: Boolean = True);
    function  RegRecord(const Name: LuaString; const TypeInfo: PTypeInfo; const UseRttiContext: Boolean = True): PLuaRecordInfo;
    function  RegArray(const Identifier: Pointer; const ItemTypeInfo: PTypeInfo; const ABounds: array of Integer): PLuaArrayInfo;
    function  RegSet(const TypeInfo: PTypeInfo): PLuaSetInfo;
    procedure RegProc(const ProcName: LuaString; const Callback: TLuaProcCallback; const ArgsCount: Integer = -1); overload;
    procedure RegProc(const ProcName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo); overload;
    procedure RegProc(const ProcName: LuaString; const Address: Pointer; const Params: array of TLuaProcParam;
      const ResultType: PTypeInfo = nil; const CallConv: TCallConv = ccReg); overload;
    procedure RegMethod(const AClass: TClass; const MethodName: LuaString; const Callback: TLuaMethodCallback; const ArgsCount: Integer = -1; const MethodKind: TLuaMethodKind = mkInstance); overload;
    procedure RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind = mkInstance); overload;
    procedure RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer;
      const Params: array of TLuaProcParam; const ResultType: PTypeInfo = nil;
      const MethodKind: TLuaMethodKind = mkInstance; const CallConv: TCallConv = ccReg); overload;
    procedure RegProperty(const AClass: TClass; const Name: LuaString; const TypeInfo: PTypeInfo;
      const Getter, Setter: Pointer; const GetterPtr: TLuaPropPtr = ppAuto; const SetterPtr: TLuaPropPtr = ppAuto;
      const Params: PLuaRecordInfo = nil; const Default: Boolean = False; const ConstRef: Boolean = True; const Index: Integer = Low(Integer));
    procedure RegVariable(const Name: LuaString; const Instance; const TypeInfo: PTypeInfo; const IsConst: Boolean = False);
    procedure RegConst(const Name: LuaString; const Value: Variant); overload;
    procedure RegConst(const Name: LuaString; const Value: TLuaArg); overload;
    procedure RegEnum(const TypeInfo: PTypeInfo);

    // advanced properties
    property ResultBuffer: PLuaResultBuffer read GetResultBuffer;
    property Variable[const Name: LuaString]: Variant read GetVariable write SetVariable;
    property VariableEx[const Name: LuaString]: TLuaArg read GetVariableEx write SetVariableEx;
    property TableVariable[const Table, Name: LuaString]: Variant read GetTableVariable write SetTableVariable;
    property TableVariableEx[const Table, Name: LuaString]: TLuaArg read GetTableVariableEx write SetTableVariableEx;
    property RecordInfo[const Name: LuaString]: PLuaRecordInfo read GetRecordInfo;
    property ArrayInfo[const Name: LuaString]: PLuaArrayInfo read GetArrayInfo;
    property SetInfo[const Name: LuaString]: PLuaSetInfo read GetSetInfo;

    // basic properties
    property Handle: Pointer read FHandle;
    property Args: TLuaArgs read FArgs;
    property ArgsCount: Integer read FArgsCount;

    // script units
    property OnPreprocessScript: TLuaOnPreprocessScript read FOnPreprocessScript write FOnPreprocessScript;
    property UnitCount: Integer read FUnitCount;
    property Units[const Index: Integer]: TLuaUnit read GetUnit;
    property UnitByName[const Name: LuaString]: TLuaUnit read GetUnitByName;
  end;


const
  // special registering methods: constructor and Assign
  LUA_CONSTRUCTOR = 'constructor';
  LUA_ASSIGN = 'assign';

  // special TypeInfo
  TypeInfoTClass  = PTypeInfo(NativeUInt($7FFF0000));
  TypeInfoPointer = PTypeInfo(NativeUInt($7EEE0000));
  TypeInfoUniversal = PTypeInfo(NativeUInt($7DDD0000));
  TypeInfoPWideChar = PTypeInfo(NativeUInt($7CCC0000));
  TypeInfoPAnsiChar = PTypeInfo(NativeUInt($7BBB0000));
  TypeInfoPUTF8Char = PTypeInfo(NativeUInt($7BBB0000) + 65001);

  // special difficult properties parameters
  INDEXED_PROPERTY = PLuaRecordInfo(NativeUInt($7EEEEEEE));
  NAMED_PROPERTY   = PLuaRecordInfo(NativeUInt($7AAAAAAA));

  // all possible operators
  ALL_OPERATORS: TLuaOperators = [Low(TLuaOperator)..High(TLuaOperator)];


// helper functions
function CreateLua: TLua;
function LuaArgs(const Count: Integer): TLuaArgs;
function LuaArg(const Value: Boolean): TLuaArg; overload;
function LuaArg(const Value: Integer): TLuaArg; overload;
function LuaArg(const Value: Double): TLuaArg; overload;
function LuaArg(const Value: LuaString): TLuaArg; overload;
function LuaArg(const Value: Pointer): TLuaArg; overload;
function LuaArg(const Value: TClass): TLuaArg; overload;
function LuaArg(const Value: TObject): TLuaArg; overload;
function LuaArg(const Value: TLuaRecord): TLuaArg; overload;
function LuaArg(const Value: TLuaArray): TLuaArg; overload;
function LuaArg(const Value: TLuaSet): TLuaArg; overload;
function LuaArg(const Value: Variant): TLuaArg; overload;
(*
function LuaRecord(const Data: pointer; const Info: PLuaRecordInfo; const IsRef: boolean=true; const IsConst: boolean=false): TLuaRecord;
function LuaArray(const Data: pointer; const Info: PLuaArrayInfo; const IsRef: boolean=true; const IsConst: boolean=false): TLuaArray;
function LuaSet(const Data: pointer; const Info: PLuaSetInfo; const IsRef: boolean=true; const IsConst: boolean=false): TLuaSet;
*)

function LuaProcCallback(const Address: Pointer): TLuaProcCallback;
function LuaMethodCallback(const Address: Pointer): TLuaMethodCallback;
function LuaProcParam(const Name: LuaString; const TypeInfo: PTypeInfo;
  const Flags: TParamFlags = [pfConst]; const PointerDepth: Integer = 0): TLuaProcParam;


var
  { user defined library path, used at the first TLua class constructor }
  LuaLibraryPath: string =
    {$if Defined(CPUX86)}
      'lua.dll'
    {$elseif Defined(CPUX64)}
      'lua64.dll'
    {$else}
      {$MESSAGE ERROR 'Planform not yet supported'}
    {$ifend}
  ;

{$ifdef LUA_INITIALIZE}
var
  Lua: TLua;
{$endif}

implementation

{ ELua }

{$ifdef KOL}
constructor ELua.Create(const Msg: string);
begin
  inherited Create(e_Custom, Msg);
end;

constructor ELua.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, Msg, Args);
end;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: string;
  end;

function EnumStringModules(Instance: Longint; Data: Pointer): Boolean;
var
  Buffer: array [0..1023] of Char;
begin
  with PStrData(Data)^ do
  begin
    SetString(Str, Buffer, Windows.LoadString(Instance, Ident, Buffer, sizeof(Buffer)));
    Result := Str = '';
  end;
end;

function FindStringResource(Ident: Integer): string;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  EnumResourceModules(EnumStringModules, @StrData);
  Result := StrData.Str;
end;

function LoadStr(Ident: Integer): string;
begin
  Result := FindStringResource(Ident);
end;

constructor ELua.CreateRes(Ident: NativeUInt);
begin
  inherited Create(e_Custom, LoadStr(Ident));
end;

constructor ELua.CreateRes(ResStringRec: PResStringRec);
begin
  inherited Create(e_Custom, System.LoadResString(ResStringRec));
end;

constructor ELua.CreateResFmt(Ident: NativeUInt;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, LoadStr(Ident), Args);
end;

constructor ELua.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, System.LoadResString(ResStringRec), Args);
end;
{$endif}

function ETypeRegistered(const Name: LuaString): ELua;
begin
  Result := ELua.CreateFmt('Type "%s" is already registered', [Name]);
end;

function EInvalidFieldOffset(const Value: NativeInt): ELua;
begin
  Result := ELua.CreateFmt('Invalid field offset %d', [Value]);
end;

{$if CompilerVersion < 27}
resourcestring
  sArgumentInvalid = 'Invalid argument';
{$ifend}

function EInvalidArgument: ELua;
begin
  Result := ELua.CreateRes(Pointer(@sArgumentInvalid));
end;

function EInvalidScriptStackIndex(const Index: NativeUInt): ELuaStackOverflow;
begin
  Result := ELuaStackOverflow.CreateFmt('Invalid script stack index: %d', [Index]);
end;


{ Lua API routine }

var
  LuaHandle: THandle;
  LuaPath: string;
  LuaInitialized: Boolean;

type
  Plua_State = Pointer;
  lua_CFunction = function(L: Plua_State): Integer; cdecl;
  lua_Number = Double;
  lua_Integer = NativeInt;
  size_t = NativeUInt;
  Psize_t = ^size_t;

  lua_Debug = record
    event: Integer;
    name: __luaname;                 { (n) }
    namewhat: __luaname;             { (n) `global', `local', `field', `method' }
    what: __luaname;                 { (S) `Lua', `C', `main', `tail'}
    source: __luaname;               { (S) }
    currentline: Integer;            { (l) }
    nups: Integer;                   { (u) number of upvalues }
    linedefined: Integer;            { (S) }
    short_src: array[0..59] of Byte; { (S) }
    i_ci: Integer;
    gap: array[0..7] of Byte;        { lua_getstack bug fix }
  end;
  Plua_Debug = ^lua_Debug;

var
  LUA_VERSION_52: Boolean = False;
  LUA_REGISTRYINDEX: Integer = -10000;
  LUA_UPVALUEINDEX: Integer = -10003;

const
  LUA_MULTRET = -1;

  LUA_TNONE          = -1;
  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA      = 7;

  LUA_METATYPE_MARKER = Ord('L') or (Ord('M') shl 8) or (Ord('T') shl 16) or (Ord('M') shl 24);

var
  lua_open: function: Plua_State; cdecl;
  luaL_openlibs: procedure(L: Plua_State); cdecl;
  lua_close: procedure(L: Plua_State); cdecl;
  lua_gc: function(L: Plua_State; what: Integer; data: Integer): Integer; cdecl;
  luaL_loadbuffer: function(L: Plua_State; const buff: __luadata; size: size_t; const name: __luaname): Integer; cdecl;
  luaL_loadbufferx: function(L: Plua_State; const buff: __luadata; size: size_t; const name, mode: __luaname): Integer; cdecl;
  lua_pcall: function(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
  lua_pcallk: function(L: Plua_State; nargs, nresults, errf, ctx: Integer; k: lua_CFunction): Integer; cdecl;
  lua_error: function(L: Plua_State): Integer; cdecl;
  lua_next: function(L: Plua_State; idx: Integer): Integer; cdecl;
  lua_getstack: function(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
  lua_getinfo: function(L: Plua_State; const what: __luaname; ar: Plua_Debug): Integer; cdecl;
  lua_getupvalue: function(L: Plua_State; funcindex, n: Integer): __luaname; cdecl;

  lua_type: function(L: Plua_State; idx: Integer): Integer; cdecl;
  lua_gettop: function(L: Plua_State): Integer; cdecl;
  lua_settop: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_remove: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_insert: procedure(L: Plua_State; idx: Integer); cdecl;

  lua_pushnil: procedure(L: Plua_State); cdecl;
  lua_pushboolean: procedure(L: Plua_State; b: LongBool); cdecl;
  lua_pushinteger: procedure(L: Plua_State; n: lua_Integer); cdecl;
  lua_pushnumber: procedure(L: Plua_State; n: lua_Number); cdecl;
  lua_pushlstring: procedure(L: Plua_State; const s: __luadata; l_: size_t); cdecl;
  lua_pushcclosure: procedure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
  lua_pushlightuserdata: procedure(L: Plua_State; p: Pointer); cdecl;
  lua_newuserdata: function(L: Plua_State; sz: size_t): Pointer; cdecl;
  lua_pushvalue: procedure(L: Plua_State; Idx: Integer); cdecl;
  lua_toboolean: function(L: Plua_State; idx: Integer): LongBool; cdecl;
  lua_tointeger: function(L: Plua_State; idx: Integer): lua_Integer; cdecl;
  lua_tointegerx: function(L: Plua_State; idx: Integer; isnum: PInteger): lua_Integer; cdecl;
  lua_tonumber: function(L: Plua_State; idx: Integer): lua_Number; cdecl;
  lua_tonumberx: function(L: Plua_State; idx: Integer; isnum: PInteger): lua_Number; cdecl;
  lua_tolstring: function(L: Plua_State; idx: Integer; len: Psize_t): __luadata; cdecl;
  lua_tocfunction: function(L: Plua_State; idx: Integer): lua_CFunction; cdecl;
  lua_touserdata: function(L: Plua_State; idx: Integer): Pointer; cdecl;
  lua_topointer: function(L: Plua_State; idx: Integer): Pointer; cdecl;
  lua_objlen: function(L: Plua_State; idx: Integer): size_t; cdecl;

  lua_rawgeti: procedure(L: Plua_State; idx, n: Integer); cdecl;
  lua_rawseti: procedure(L: Plua_State; idx, n: Integer); cdecl;
  lua_rawget: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_rawset: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_createtable: procedure(L: Plua_State; narr: Integer; nrec: Integer); cdecl; { old newtable }
  lua_setmetatable: function(L: Plua_State; objindex: Integer): Integer; cdecl;

// for 5.2+ versions
function luaL_loadbuffer_52(L: Plua_State; const buff: __luadata; size: size_t; const name: __luaname): Integer; cdecl;
begin
  Result := luaL_loadbufferx(L, buff, size, name, nil);
end;

function lua_pcall_52(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
begin
  Result := lua_pcallk(L, nargs, nresults, errf, 0, nil);
end;

function lua_tointeger_52(L: Plua_State; idx: Integer): lua_Integer; cdecl;
begin
  Result := lua_tointegerx(L, idx, nil);
end;

function lua_tonumber_52(L: Plua_State; idx: Integer): lua_Number; cdecl;
begin
  Result := lua_tonumberx(L, idx, nil);
end;

function lua_toint64(L: Plua_State; idx: Integer): Int64; register;
{$if Defined(CPUX86)}
asm
  push edx
  push eax
  call lua_tonumber

  fistp qword ptr [esp]
  pop eax
  pop edx
end;
{$elseif Defined(CPUX64)}
asm
  push rcx
  call lua_tonumber
  cvtsd2si rax, xmm0
  pop rcx
end;
{$else}
begin
  Result := Round(lua_tonumber(L, idx));
end;
{$ifend}

procedure GetLuaTypeName(var Result: string; const LuaType: Integer);
begin
  case (LuaType) of
    LUA_TNONE         : Result := 'LUA_TNONE';
    LUA_TNIL          : Result := 'LUA_TNIL';
    LUA_TBOOLEAN      : Result := 'LUA_TBOOLEAN';
    LUA_TLIGHTUSERDATA: Result := 'LUA_TLIGHTUSERDATA';
    LUA_TNUMBER       : Result := 'LUA_TNUMBER';
    LUA_TSTRING       : Result := 'LUA_TSTRING';
    LUA_TTABLE        : Result := 'LUA_TTABLE';
    LUA_TFUNCTION     : Result := 'LUA_TFUNCTION';
    LUA_TUSERDATA     : Result := 'LUA_TUSERDATA';
  else
    Result := Format('UNKNOWN (%d)', [LuaType]);
  end;
end;

function LoadLuaLibrary: THandle;
var
  S: string;
  Buffer: array[0..1024] of Char;
  BufferPtr: PChar;
begin
  if (LuaPath = '') then
  begin
    if (FileExists(LuaLibraryPath)) then
    begin
      LuaPath := ExpandFileName(LuaLibraryPath);
    end else
    begin
      BufferPtr := @Buffer[0];
      SetString(S, BufferPtr, GetModuleFileName(hInstance, BufferPtr, High(Buffer)));
      LuaPath := ExtractFilePath(S) + ExtractFileName(LuaLibraryPath);
    end;

    LuaHandle := LoadLibrary(PChar(LuaPath));
  end;

  Result := LuaHandle;
end;

procedure FreeLuaLibrary;
begin
  if (LuaHandle <> 0) then
  begin
    FreeLibrary(LuaHandle);
    LuaHandle := 0;
  end;
end;

// initialize Lua library, load and emulate API
function InitializeLua: Boolean;
var
  Buffer: Pointer;

  function FailLoad(var Proc; const ProcName: PChar): Boolean;
  begin
    Pointer(Proc) := GetProcAddress(LuaHandle, ProcName);
    Result := (Pointer(Proc) = nil);
  end;

begin
  Result := False;
  if (not LuaInitialized) then
  begin
    if (LoadLuaLibrary = 0) then Exit;

    LUA_VERSION_52 := not FailLoad(Buffer, 'lua_tounsignedx');
    if (LUA_VERSION_52) then
    begin
      LUA_REGISTRYINDEX := (-1000000 - 1000);
      LUA_UPVALUEINDEX := LUA_REGISTRYINDEX - 1;
    end;

    if FailLoad(@lua_open, 'luaL_newstate') then Exit;
    if FailLoad(@luaL_openlibs, 'luaL_openlibs') then Exit;
    if FailLoad(@lua_close, 'lua_close') then Exit;
    if FailLoad(@lua_gc, 'lua_gc') then Exit;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@luaL_loadbufferx, 'luaL_loadbufferx') then Exit;
      luaL_loadbuffer := luaL_loadbuffer_52;
    end else
    begin
      if FailLoad(@luaL_loadbuffer, 'luaL_loadbuffer') then Exit;
    end;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@lua_pcallk, 'lua_pcallk') then Exit;
      lua_pcall := lua_pcall_52;
    end else
    begin
      if FailLoad(@lua_pcall, 'lua_pcall') then Exit;
    end;
    if FailLoad(@lua_error, 'lua_error') then Exit;
    if FailLoad(@lua_next, 'lua_next') then Exit;
    if FailLoad(@lua_getstack, 'lua_getstack') then Exit;
    if FailLoad(@lua_getinfo, 'lua_getinfo') then Exit;
    if FailLoad(@lua_getupvalue, 'lua_getupvalue') then Exit;

    if FailLoad(@lua_type, 'lua_type') then Exit;
    if FailLoad(@lua_gettop, 'lua_gettop') then Exit;
    if FailLoad(@lua_settop, 'lua_settop') then Exit;
    if FailLoad(@lua_remove, 'lua_remove') then Exit;
    if FailLoad(@lua_insert, 'lua_insert') then Exit;

    if FailLoad(@lua_pushnil, 'lua_pushnil') then Exit;
    if FailLoad(@lua_pushboolean, 'lua_pushboolean') then Exit;
    if FailLoad(@lua_pushinteger, 'lua_pushinteger') then Exit;
    if FailLoad(@lua_pushnumber, 'lua_pushnumber') then Exit;
    if FailLoad(@lua_pushlstring, 'lua_pushlstring') then Exit;
    if FailLoad(@lua_pushcclosure, 'lua_pushcclosure') then Exit;
    if FailLoad(@lua_pushlightuserdata, 'lua_pushlightuserdata') then Exit;
    if FailLoad(@lua_newuserdata, 'lua_newuserdata') then Exit;
    if FailLoad(@lua_pushvalue, 'lua_pushvalue') then Exit;
    if FailLoad(@lua_toboolean, 'lua_toboolean') then Exit;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@lua_tointegerx, 'lua_tointegerx') then Exit;
      if FailLoad(@lua_tonumberx, 'lua_tonumberx') then Exit;
      lua_tointeger := lua_tointeger_52;
      lua_tonumber := lua_tonumber_52;
    end else
    begin
      if FailLoad(@lua_tointeger, 'lua_tointeger') then Exit;
      if FailLoad(@lua_tonumber, 'lua_tonumber') then Exit;
    end;
    if FailLoad(@lua_tolstring, 'lua_tolstring') then Exit;
    if FailLoad(@lua_tocfunction, 'lua_tocfunction') then Exit;
    if FailLoad(@lua_touserdata, 'lua_touserdata') then Exit;
    if FailLoad(@lua_topointer, 'lua_topointer') then Exit;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@lua_objlen, 'lua_rawlen') then Exit;
    end else
    begin
      if FailLoad(@lua_objlen, 'lua_objlen') then Exit;
    end;

    if FailLoad(@lua_rawgeti, 'lua_rawgeti') then Exit;
    if FailLoad(@lua_rawseti, 'lua_rawseti') then Exit;
    if FailLoad(@lua_rawget, 'lua_rawget') then Exit;
    if FailLoad(@lua_rawset, 'lua_rawset') then Exit;
    if FailLoad(@lua_createtable, 'lua_createtable') then Exit;
    if FailLoad(@lua_setmetatable, 'lua_setmetatable') then Exit;

    LuaInitialized := True;
  end;

  Result := True;
end;

// safe TLua constuctor
function CreateLua: TLua;
begin
  if (not InitializeLua) then
  begin
    Result := nil;
  end else
  begin
    Result := TLua.Create;
  end;
end;


{ Lookup routine }

const
  BIT_SCANS: array[Byte] of Byte = ({failure}8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0,
    2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2,
    0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0,
    1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1,
    0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
    4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 7, 0, 1, 0, 2, 0, 1, 0, 3,
    0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0,
    1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1,
    0, 2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0,
    2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2,
    0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0
  );

const
  CODEPAGE_UTF8 = 65001;

var
  CODEPAGE_DEFAULT: Word;
  UTF8CHAR_SIZE: array[Byte] of Byte;

procedure InitUnicodeLookups;
type
  TNativeUIntArray = array[0..256 div SizeOf(NativeUInt) - 1] of NativeUInt;
var
  i, X: NativeUInt;
  NativeArr: ^TNativeUIntArray;
begin
  CODEPAGE_DEFAULT := GetACP;
  NativeArr := Pointer(@UTF8CHAR_SIZE);

  // 0..127
  X := {$ifdef LARGEINT}$0101010101010101{$else}$01010101{$endif};
  for i := 0 to 128 div SizeOf(NativeUInt) - 1 do
  NativeArr[i] := X;

  // 128..191 (64) fail (0)
  Inc(NativeUInt(NativeArr), 128);
  X := 0;
  for i := 0 to 64 div SizeOf(NativeUInt) - 1 do
  NativeArr[i] := X;

  // 192..223 (32)
  Inc(NativeUInt(NativeArr), 64);
  X := {$ifdef LARGEINT}$0202020202020202{$else}$02020202{$endif};
  for i := 0 to 32 div SizeOf(NativeUInt) - 1 do
  NativeArr[i] := X;

  // 224..239 (16)
  Inc(NativeUInt(NativeArr), 32);
  X := {$ifdef LARGEINT}$0303030303030303{$else}$03030303{$endif};
  {$ifdef LARGEINT}
    NativeArr[0] := X;
    NativeArr[1] := X;
  {$else}
    NativeArr[0] := X;
    NativeArr[1] := X;
    NativeArr[2] := X;
    NativeArr[3] := X;
  {$endif}

  // 240..247 (8)
  Inc(NativeUInt(NativeArr), 16);
  {$ifdef LARGEINT}
    NativeArr[0] := $0404040404040404;
  {$else}
    NativeArr[0] := $04040404;
    NativeArr[1] := $04040404;
  {$endif}

  // 248..251 (4) --> 5
  // 252..253 (2) --> 6
  // 254..255 (2) --> fail (0)
  {$ifdef LARGEINT}
    NativeArr[1] := $0000060605050505;
  {$else}
    NativeArr[2] := $05050505;
    NativeArr[3] := $00000606;
  {$endif}
end;


{ Sets routine }

type
  TSet1 = set of 0..7;
  TSet2 = set of 0..15;
  TSet4 = set of 0..31;
  TSetBuffer = array[0..32 div SizeOf(NativeUInt) - 1] of NativeUInt;

function SetBitInitialize(Bit: Integer): TSetBuffer;
{$ifdef CPUX86}
asm
  xor ecx, ecx
  mov [edx +  0], ecx
  mov [edx +  4], ecx
  mov [edx +  8], ecx
  mov [edx + 12], ecx
  mov [edx + 16], ecx
  mov [edx + 20], ecx
  mov [edx + 24], ecx
  mov [edx + 28], ecx

  bts [edx], eax
end;
{$else}
var
  Null: NativeUInt;
  Dest: PNativeUInt;
begin
  Null := 0;
  Result[0] := Null;
  Result[1] := Null;
  Result[2] := Null;
  Result[3] := Null;
  {$ifdef SMALLINT}
  Result[4] := Null;
  Result[5] := Null;
  Result[6] := Null;
  Result[7] := Null;
  {$endif}

  Dest := @Result[Bit shr {$ifdef SMALLINT}5{$else .LARGEINT}6{$endif}];
  Bit := Bit and {$ifdef SMALLINT}31{$else .LARGEINT}63{$endif};
  Dest^ := Dest^ or (NativeUInt(1) shl Bit);
end;
{$endif}

procedure SetBitInclude(Value: PByte; Bit: Integer);
{$ifdef CPUX86}
asm
  bts [eax], edx
end;
{$else}
begin
  Inc(Value, Bit shr 3);
  Bit := Bit and 7;
  Value^ := Value^ or (1 shl Bit);
end;
{$endif}

procedure SetBitExclude(Value: PByte; Bit: Integer);
{$ifdef CPUX86}
asm
  btr [eax], edx
end;
{$else}
begin
  Inc(Value, Bit shr 3);
  Bit := Bit and 7;
  Value^ := Value^ and (not (1 shl Bit));
end;
{$endif}

function SetBitContains(Value: PByte; Bit: Integer): Boolean;
{$ifdef CPUX86}
asm
  bt [eax], edx
  setc al
end;
{$else}
begin
  Inc(Value, Bit shr 3);
  Bit := Bit and 7;
  Result := (Value^ and (1 shl Bit) <> 0);
end;
{$endif}

function  _SetLe(Left, Right: PByte; Size: Integer): Boolean;
{$ifdef CPUX86}
asm
  test ecx, 3
  jnz @@loop

  push ebx
  shr ecx, 2
@@loop_dwords:
        mov ebx, [edx]
        not ebx
        and ebx, [eax]
        jne @@pop_ebx_exit
        add edx, 4
        add eax, 4
        dec ecx
        jnz @@loop_dwords
        jmp @@pop_ebx_exit

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        AND     CH,[EAX]
        JNE     @@exit
        INC     EDX
        INC     EAX
        DEC     CL
        JNZ     @@loop
        JMP     @@exit

@@pop_ebx_exit:
  pop ebx
@@exit:
  setz al
end;
{$else}
label
  ret_false;
var
  L, R: NativeUInt;
begin
  if (Size >= SizeOf(NativeUInt)) then
  repeat
    L := PNativeUInt(Left)^;
    R := PNativeUInt(Right)^;
    Dec(Size, SizeOf(NativeUInt));
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));
    if (L and (not R) <> 0) then goto ret_false;
  until (Size < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (Size >= SizeOf(Cardinal)) then
  begin
    L := PCardinal(Left)^;
    R := PCardinal(Right)^;
    Dec(Size, SizeOf(Cardinal));
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
    if (L and (not R) <> 0) then goto ret_false;
  end;
  {$endif}

  if (Size >= SizeOf(Byte)) then
  repeat
    L := PByte(Left)^;
    R := PByte(Right)^;
    Dec(Size, SizeOf(Byte));
    Inc(Left, SizeOf(Byte));
    Inc(Right, SizeOf(Byte));
    if (L and (not R) <> 0) then goto ret_false;
  until (Size < SizeOf(Byte));

  Result := True;
  Exit;
ret_false:
  Result := False;
end;
{$endif}

function _SetEq(Left, Right: PByte; Size: Integer): Boolean;
{$ifdef CPUX86}
asm
  push ebx
  jmp @1

@@dwords:
    mov ebx, [eax]
    cmp ebx, [edx]
    jne @@exit
    add eax, 4
    add edx, 4
@1: sub ecx, 4
    jge @@dwords
    je  @@exit

    add ecx, 3
@@loop:
    movzx ebx, byte ptr [eax + ecx]
    cmp bl, [edx + ecx]
    jne @@exit
    dec ecx
    jnz @@loop

@@exit:
  pop ebx
  sete al
end;
{$else}
label
  ret_false;
begin
  if (Size >= SizeOf(NativeUInt)) then
  repeat
    if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto ret_false;
    Dec(Size, SizeOf(NativeUInt));
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));
  until (Size < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (Size >= SizeOf(Cardinal)) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto ret_false;
    Dec(Size, SizeOf(Cardinal));
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Size >= SizeOf(Byte)) then
  repeat
    if (PByte(Left)^ <> PByte(Right)^) then goto ret_false;
    Dec(Size, SizeOf(Byte));
    Inc(Left, SizeOf(Byte));
    Inc(Right, SizeOf(Byte));
  until (Size < SizeOf(Byte));

  Result := True;
  Exit;
ret_false:
  Result := False;
end;
{$endif}

// Result 0 means "equal"
function SetsCompare(const Left, Right: Pointer; const Size: Integer; const SubsetMode: Boolean): Integer; overload;
var
  Ret: Boolean;
begin
  if (SubsetMode) then
  begin
    case (Size) of
      1: Ret := (TSet1(Left^) <= TSet1(Right^));
      2: Ret := (TSet2(Left^) <= TSet2(Right^));
      4: Ret := (TSet4(Left^) <= TSet4(Right^));
    else
      Ret := _SetLe(Left, Right, Size);
    end;
  end else
  begin
    case (Size) of
      1: Ret := (TSet1(Left^) = TSet1(Right^));
      2: Ret := (TSet2(Left^) = TSet2(Right^));
      4: Ret := (TSet4(Left^) = TSet4(Right^));
    else
      Ret := _SetEq(Left, Right, Size);
    end;
  end;

  Result := Ord(not Ret);
end;

function SetsCompare(const Left: Pointer; const Bit, Size: Integer; const SubsetMode: Boolean): Integer; overload;
var
  Right: TSetBuffer;
  Ret: Boolean;
begin
  if (SubsetMode) then
  begin
    case (Size) of
      {$ifNdef FPC}
      1: Ret := (TSet1(Left^) <= TSet1(Byte(1 shl Bit)));
      2: Ret := (TSet2(Left^) <= TSet2(Word(1 shl Bit)));
      {$endif}
      4: Ret := (TSet4(Left^) <= TSet4(Integer(1 shl Bit)));
    else
      Right := SetBitInitialize(Bit);
      Ret := _SetLe(Left, Pointer(@Right), Size);
    end;
  end else
  begin
    case (Size) of
      {$ifNdef FPC}
      1: Ret := (TSet1(Left^) = TSet1(Byte(1 shl Bit)));
      2: Ret := (TSet2(Left^) = TSet2(Word(1 shl Bit)));
      {$endif}
      4: Ret := (TSet4(Left^) = TSet4(Integer(1 shl Bit)));
    else
      Right := SetBitInitialize(Bit);
      Ret := _SetEq(Left, Pointer(@Right), Size);
    end;
  end;

  Result := Ord(not Ret);
end;

procedure SetsUnion(Dest, Left, Right: PByte; ASize: Integer); overload;
{$ifdef CPUX86}
asm
  push ebx
  mov ebp, ASize
  jmp @@start

@@dwords:
  mov ebx, [edx]
  add edx, 4
  or  ebx, [ecx]
  add ecx, 4
  mov [eax], ebx
  add eax, 4
@@start:
  sub ebp, 4
  jg @@dwords
  je @@4

@@bytes:
  inc ebp
  jz @@3
  inc ebp
  jz @@2

@@1:
  movzx ebx, byte ptr [edx]
  or  bl, [ecx]
  mov [eax], bl
  jmp @@exit
@@2:
  movzx ebx, word ptr [edx]
  or  bx, [ecx]
  mov [eax], bx
  jmp @@exit
@@3:
  movzx ebx, word ptr [edx]
  or  bx, [ecx]
  mov [eax], bx
  movzx ebx, byte ptr [edx + 2]
  or  bl, [ecx + 2]
  mov [eax + 2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  or  ebx, [ecx]
  mov [eax], ebx
@@exit:
  pop ebx
end;
{$else}
var
  Size: Integer;
begin
  Size := ASize;

  if (Size >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(Dest)^ := PNativeUInt(Left)^ or PNativeUInt(Right)^;
    Dec(Size, SizeOf(NativeUInt));
    Inc(Dest, SizeOf(NativeUInt));
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));
  until (Size < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (Size >= SizeOf(Cardinal)) then
  begin
    PCardinal(Dest)^ := PCardinal(Left)^ or PCardinal(Right)^;
    Dec(Size, SizeOf(Cardinal));
    Inc(Dest, SizeOf(Cardinal));
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Size >= SizeOf(Byte)) then
  repeat
    PByte(Dest)^ := PByte(Left)^ or PByte(Right)^;
    Dec(Size, SizeOf(Byte));
    Inc(Dest, SizeOf(Byte));
    Inc(Left, SizeOf(Byte));
    Inc(Right, SizeOf(Byte));
  until (Size < SizeOf(Byte));
end;
{$endif}

procedure SetsUnion(Dest, Left: Pointer; Bit, Size: Integer); overload;
var
  Right: TSetBuffer;
begin
  case Size of
    {$ifNdef FPC}
    1: TSet1(Dest^) := TSet1(Left^) + TSet1(Byte(1 shl Bit));
    2: TSet2(Dest^) := TSet2(Left^) + TSet2(Word(1 shl Bit));
    {$endif}
    4: TSet4(Dest^) := TSet4(Left^) + TSet4(Integer(1 shl Bit));
  else
    Right := SetBitInitialize(Bit);
    SetsUnion(Dest, Left, Pointer(@Right), Size);
  end;
end;

procedure SetsDifference(Dest, Left, Right: PByte; ASize: Integer); overload;
{$ifdef CPUX86}
asm
  push ebx
  mov ebp, ASize
  jmp @@start

@@dwords:
  mov ebx, [ecx]
  add ecx, 4
  not ebx
  and ebx, [edx]
  add edx, 4
  mov [eax], ebx
  add eax, 4
@@start:
  sub ebp, 4
  jg @@dwords
  je @@4

@@bytes:
  inc ebp
  jz @@3
  inc ebp
  jz @@2

@@1:
  movzx ebx, byte ptr [ecx]
  not ebx
  movzx ebp, byte ptr [edx]
  and ebx, ebp
  mov [eax], bl
  jmp @@exit
@@2:
  movzx ebx, word ptr [ecx]
  not ebx
  movzx ebp, word ptr [edx]
  and ebx, ebp
  mov [eax], bx
  jmp @@exit
@@3:
  movzx ebx, word ptr [ecx]
  not ebx
  movzx ebp, word ptr [edx]
  and ebx, ebp
  mov [eax], bx
  movzx ebx, byte ptr [ecx + 2]
  not ebx
  movzx ebp, byte ptr [edx + 2]
  and ebx, ebp
  mov [eax + 2], bl
  jmp @@exit
@@4:
  mov ebx, [ecx]
  not ebx
  and ebx, [edx]
  mov [eax], ebx
@@exit:
  pop ebx
end;
{$else}
var
  Size: Integer;
begin
  Size := ASize;

  if (Size >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(Dest)^ := PNativeUInt(Left)^ and (not PNativeUInt(Right)^);
    Dec(Size, SizeOf(NativeUInt));
    Inc(Dest, SizeOf(NativeUInt));
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));
  until (Size < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (Size >= SizeOf(Cardinal)) then
  begin
    PCardinal(Dest)^ := PCardinal(Left)^ and (not PCardinal(Right)^);
    Dec(Size, SizeOf(Cardinal));
    Inc(Dest, SizeOf(Cardinal));
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Size >= SizeOf(Byte)) then
  repeat
    PByte(Dest)^ := PByte(Left)^ and (not PByte(Right)^);
    Dec(Size, SizeOf(Byte));
    Inc(Dest, SizeOf(Byte));
    Inc(Left, SizeOf(Byte));
    Inc(Right, SizeOf(Byte));
  until (Size < SizeOf(Byte));
end;
{$endif}

procedure SetsDifference(Dest, Left: Pointer; Bit, Size: Integer; Exchange: Boolean); overload;
var
  Right: TSetBuffer;
begin
  if (not Exchange) then
  begin
    case Size of
      {$ifNdef FPC}
      1: TSet1(Dest^) := TSet1(Left^) - TSet1(Byte(1 shl Bit));
      2: TSet2(Dest^) := TSet2(Left^) - TSet2(Word(1 shl Bit));
      {$endif}
      4: TSet4(Dest^) := TSet4(Left^) - TSet4(Integer(1 shl Bit));
    else
      Right := SetBitInitialize(Bit);
      SetsDifference(Dest, Left, Pointer(@Right), Size);
    end;
  end else
  begin
    case Size of
      {$ifNdef FPC}
      1: TSet1(Dest^) := TSet1(Byte(1 shl Bit)) - TSet1(Left^);
      2: TSet2(Dest^) := TSet2(Word(1 shl Bit)) - TSet2(Left^);
      {$endif}
      4: TSet4(Dest^) := TSet4(Integer(1 shl Bit))- TSet4(Left^);
    else
      Right := SetBitInitialize(Bit);
      SetsDifference(Dest, Pointer(@Right), Left, Size);
    end;
  end;
end;

procedure SetsIntersection(Dest, Left, Right: PByte; ASize: Integer); overload;
{$ifdef CPUX86}
asm
  push ebx
  mov ebp, ASize
  jmp @@start

@@dwords:
  mov ebx, [edx]
  add edx, 4
  and ebx, [ecx]
  add ecx, 4
  mov [eax], ebx
  add eax, 4
@@start:
  sub ebp, 4
  jg @@dwords
  je @@4

@@bytes:
  inc ebp
  jz @@3
  inc ebp
  jz @@2

@@1:
  movzx ebx, byte ptr [edx]
  and bl, [ecx]
  mov [eax], bl
  jmp @@exit
@@2:
  movzx ebx, word ptr [edx]
  and bx, [ecx]
  mov [eax], bx
  jmp @@exit
@@3:
  movzx ebx, word ptr [edx]
  and bx, [ecx]
  mov [eax], bx
  movzx ebx, byte ptr [edx + 2]
  and bl, [ecx + 2]
  mov [eax + 2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  and ebx, [ecx]
  mov [eax], ebx
@@exit:
  pop ebx
end;
{$else}
var
  Size: Integer;
begin
  Size := ASize;

  if (Size >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(Dest)^ := PNativeUInt(Left)^ and PNativeUInt(Right)^;
    Dec(Size, SizeOf(NativeUInt));
    Inc(Dest, SizeOf(NativeUInt));
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));
  until (Size < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (Size >= SizeOf(Cardinal)) then
  begin
    PCardinal(Dest)^ := PCardinal(Left)^ and PCardinal(Right)^;
    Dec(Size, SizeOf(Cardinal));
    Inc(Dest, SizeOf(Cardinal));
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Size >= SizeOf(Byte)) then
  repeat
    PByte(Dest)^ := PByte(Left)^ and PByte(Right)^;
    Dec(Size, SizeOf(Byte));
    Inc(Dest, SizeOf(Byte));
    Inc(Left, SizeOf(Byte));
    Inc(Right, SizeOf(Byte));
  until (Size < SizeOf(Byte));
end;
{$endif}

procedure SetsIntersection(Dest, Left: Pointer; Bit, Size: Integer); overload;
var
  Right: TSetBuffer;
begin
  case Size of
    {$ifNdef FPC}
    1: TSet1(Dest^) := TSet1(Left^) * TSet1(Byte(1 shl Bit));
    2: TSet2(Dest^) := TSet2(Left^) * TSet2(Word(1 shl Bit));
    {$endif}
    4: TSet4(Dest^) := TSet4(Left^) * TSet4(Integer(1 shl Bit));
  else
    Right := SetBitInitialize(Bit);
    SetsIntersection(Dest, Left, Pointer(@Right), Size);
  end;
end;

procedure SetInvert(Dest, Left: PByte; AndMasks, ASize: Integer);
{$ifdef CPUX86}
asm
  push eax
  push ebx
  mov ebp, ASize
  jmp @@start

@@dwords:
  mov ebx, [edx]
  add eax, 4
  not ebx
  add edx, 4
  mov [eax-4], ebx
@@start:
  sub ebp, 4
  jg @@dwords
  je @@4

@@bytes:
  inc ebp
  jz @@3
  inc ebp
  jz @@2

@@1:
  movzx ebx, byte ptr [edx]
  not ebx
  and ebx, ecx
  mov [eax], bl
  jmp @@exit
@@2:
  movzx ebx, word ptr [edx]
  not ebx
  and ebx, ecx
  mov [eax], bx
  jmp @@exit
@@3:
  movzx ebx, byte ptr [edx]
  not ebx
  mov [eax], bx
  movzx ebx, byte ptr [edx + 2]
  not ebx
  and ebx, ecx
  mov [eax + 2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  not ebx
  mov [eax], ebx
@@exit:
  pop ebx
  pop eax
  and [eax], ch
end;
{$else}
var
  Size: Integer;
begin
  Size := ASize;

  if (Size >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(Dest)^ := not PNativeUInt(Left)^;
    Dec(Size, SizeOf(NativeUInt));
    Inc(Dest, SizeOf(NativeUInt));
    Inc(Left, SizeOf(NativeUInt));
  until (Size < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (Size >= SizeOf(Cardinal)) then
  begin
    PCardinal(Dest)^ := not PCardinal(Left)^;
    Dec(Size, SizeOf(Cardinal));
    Inc(Dest, SizeOf(Cardinal));
    Inc(Left, SizeOf(Cardinal));
  end;
  {$endif}

  case Size of
    3:
    begin
      PWord(Dest)^ := (not Integer(PWord(Left)^));
      Inc(Left, SizeOf(Word));
      Inc(Dest, SizeOf(Word));
      PByte(Dest)^ := (not Integer(PByte(Left)^)) and AndMasks;
    end;
    2:
    begin
      PWord(Dest)^ := (not Integer(PWord(Left)^)) and AndMasks;
    end;
    1:
    begin
      PByte(Dest)^ := (not Integer(PByte(Left)^)) and AndMasks;
    end;
  end;
end;
{$endif}


{ RTTI routine }

type
  HugeByteArray = array[0..High(Integer) div SizeOf(Byte) - 1] of Byte;
  HugeWordArray = array[0..High(Integer) div SizeOf(Word) - 1] of Word;
  HugeCardinalArray = array[0..High(Integer) div SizeOf(Cardinal) - 1] of Cardinal;
  HugeNativeUIntArray = array[0..High(Integer) div SizeOf(NativeUInt) - 1] of NativeUInt;

  PMemoryItems = ^TMemoryItems;
  TMemoryItems = packed record
  case Integer of
    0: (Bytes: HugeByteArray);
    1: (Words: HugeWordArray);
    2: (Cardinals: HugeCardinalArray);
    3: (NativeUInts: HugeNativeUIntArray);
    4: (A1: array[1..1] of Byte; Words1: HugeWordArray);
    5: (A1C: array[1..1] of Byte; Cardinals1: HugeCardinalArray);
    6: (A1N: array[1..1] of Byte; NativeUInts1: HugeNativeUIntArray);
    7: (A2: array[1..2] of Byte; Cardinals2: HugeCardinalArray);
    8: (A2N: array[1..2] of Byte; NativeUInts2: HugeNativeUIntArray);
    9: (A3: array[1..3] of Byte; Cardinals3: HugeCardinalArray);
   10: (A3N: array[1..3] of Byte; NativeUInts3: HugeNativeUIntArray);
  {$ifdef LARGEINT}
   11: (A4: array[1..4] of Byte; NativeUInts4: HugeNativeUIntArray);
   12: (A5: array[1..5] of Byte; NativeUInts5: HugeNativeUIntArray);
   13: (A6: array[1..6] of Byte; NativeUInts6: HugeNativeUIntArray);
   14: (A7: array[1..7] of Byte; NativeUInts7: HugeNativeUIntArray);
  {$endif}
  end;

const
  NULL_CHAR: {Byte/}Word = 0;
  RECORD_TYPES: set of TTypeKind = [tkRecord{$ifdef FPC}, tkObject{$endif}];
  varDeepData = $BFE8;

  {$if Defined(MSWINDOWS) or Defined(FPC) or (CompilerVersion < 22)}
    {$define WIDE_STR_SHIFT}
  {$else}
    {$undef WIDE_STR_SHIFT}
  {$ifend}

  {$if not Defined(FPC) and (CompilerVersion >= 20)}
    {$define INTERNALSTRFLAGS}
  {$ifend}

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$if Defined(LARGEINT) and not Defined(FPC)}
    _Padding: Integer;
  {$ifend}
    RefCount: Integer;
    Length: NativeInt;
  end;

  PAnsiStrRec = ^TAnsiStrRec;
  {$if Defined(FPC) or (not Defined(UNICODE))}
    TAnsiStrRec = packed record
      RefCount: Integer;
      Length: Integer;
    end;
    const ASTR_OFFSET_LENGTH = 4{SizeOf(Integer), inline bug fix};
  {$else}
    {$ifdef NEXTGEN}
      TAnsiStrRec = TDynArrayRec;
      const ASTR_OFFSET_LENGTH = {$ifdef SMALLINT}4{$else}8{$endif}{SizeOf(NativeInt), inline bug fix};
    {$else}
      TAnsiStrRec = packed record
      {$ifdef LARGEINT}
        _Padding: Integer;
      {$endif}
        CodePageElemSize: Integer;
        RefCount: Integer;
        Length: Integer;
      end;
      const ASTR_OFFSET_LENGTH = 4{SizeOf(Integer), inline bug fix};
    {$endif}
  {$ifend}

{$ifdef UNICODE}
type
  PUnicodeStrRec = ^TUnicodeStrRec;
  {$ifdef FPC}
    TUnicodeStrRec = TAnsiStrRec;
    const USTR_OFFSET_LENGTH = ASTR_OFFSET_LENGTH;
  {$else}
    TUnicodeStrRec = packed record
    {$ifdef LARGEINT}
      _Padding: Integer;
    {$endif}
      CodePageElemSize: Integer;
      RefCount: Integer;
      Length: Integer;
    end;
    const USTR_OFFSET_LENGTH = 4{SizeOf(Integer), inline bug fix};
  {$endif}
{$endif}

type
  PWideStrRec = ^TWideStrRec;
  {$ifdef MSWINDOWS}
    TWideStrRec = packed record
      Length: Integer; // *2: windows BSTR
    end;
    const WSTR_OFFSET_LENGTH = 4{SizeOf(Integer), inline bug fix};
  {$else}
  {$ifdef FPC}
    TWideStrRec = TAnsiStrRec;
    const WSTR_OFFSET_LENGTH = ASTR_OFFSET_LENGTH;
  {$else}
  {$ifdef NEXTGEN}
    TWideStrRec = TDynArrayRec;
    const WSTR_OFFSET_LENGTH = {$ifdef SMALLINT}4{$else}8{$endif}{SizeOf(NativeInt), inline bug fix};
  {$else}
    {$if CompilerVersion >= 22}
       TWideStrRec = TUnicodeStrRec;
       const WSTR_OFFSET_LENGTH = USTR_OFFSET_LENGTH;
    {$else}
       TWideStrRec = TAnsiStrRec;
       const WSTR_OFFSET_LENGTH = ASTR_OFFSET_LENGTH;
    {$ifend}
  {$endif}
  {$endif}
  {$endif}

{$ifNdef UNICODE}
type
  PUnicodeStrRec = ^TUnicodeStrRec;
  TUnicodeStrRec = TWideStrRec;
const
  USTR_OFFSET_LENGTH = WSTR_OFFSET_LENGTH;
{$endif}

const
  STR_REC_SIZE = 16;

type
  PStrRec = ^TStrRec;
  TStrRec = packed record
  case Integer of
    0: (Data: array[1..STR_REC_SIZE] of Byte);
    1: (  {$if SizeOf(Byte) <> STR_REC_SIZE}
          SAlign: array[1..16-SizeOf(Byte)] of Byte;
          {$ifend}
          ShortLength: Byte;
       );
    2: (  {$if SizeOf(TAnsiStrRec) <> STR_REC_SIZE}
          AAlign: array[1..16-SizeOf(TAnsiStrRec)] of Byte;
          {$ifend}
          Ansi: TAnsiStrRec;
       );
    3: (  {$if SizeOf(TWideStrRec) <> STR_REC_SIZE}
          WAlign: array[1..16-SizeOf(TWideStrRec)] of Byte;
          {$ifend}
          Wide: TWideStrRec;
       );
    4: (  {$if SizeOf(TUnicodeStrRec) <> STR_REC_SIZE}
          UAlign: array[1..16-SizeOf(TUnicodeStrRec)] of Byte;
          {$ifend}
          Unicode: TUnicodeStrRec;
       );
    5: (  {$if SizeOf(TDynArrayRec) <> STR_REC_SIZE}
          UCS4Align: array[1..16-SizeOf(TDynArrayRec)] of Byte;
          {$ifend}
          UCS4: TDynArrayRec;
       );
  end;

const
  ASTR_OFFSET_REFCOUNT = ASTR_OFFSET_LENGTH + SizeOf(Integer);
  {$ifNdef MSWINDOWS} WSTR_OFFSET_REFCOUNT = WSTR_OFFSET_LENGTH + SizeOf(Integer); {$endif}
  {$ifdef UNICODE} USTR_OFFSET_REFCOUNT = USTR_OFFSET_LENGTH + SizeOf(Integer); {$endif}

  {$ifdef INTERNALCODEPAGE}
    ASTR_OFFSET_CODEPAGE = ASTR_OFFSET_REFCOUNT + {ElemSize}SizeOf(Word) + {CodePage}SizeOf(Word);
  {$endif}

  ASTR_REFCOUNT_LITERAL = {$ifdef NEXTGEN}0{$else}-1{$endif};
  {$ifNdef WINDOWS}
  WSTR_REFCOUNT_LITERAL = {$ifdef NEXTGEN}0{$else}-1{$endif};
  {$endif}
  {$ifdef UNICODE}
  USTR_REFCOUNT_LITERAL = -1;
  {$endif}
  UCS4STR_REFCOUNT_LITERAL = 0;

type
  PFieldInfo = ^TFieldInfo;
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
    {$ifdef LARGEINT}
    _Padding: Integer;
    {$endif}
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

function GetTail(var Name): Pointer; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
begin
  Result := @Name;
  Inc(NativeUInt(Result), PByte(Result)^);
  Inc(NativeUInt(Result));
end;

function GetTypeData(const TypeInfo: PTypeInfo): PTypeData; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
begin
  Result := GetTail(TypeInfo.Name);
end;

function GetTypeInfo(const Value: PPTypeInfo): PTypeInfo; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
begin
  Result := nil;
  if (Assigned(Value)) then
    Result := Value^;
end;

procedure GetTypeKindName(var Result: string; const Kind: TTypeKind);
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Byte(Kind));
end;

function SkipAttributes(AttrData: Pointer): Pointer;
var
  P: PByte;
begin
  P := AttrData;

  {$ifdef EXTENDEDRTTI}
    Inc(P, TAttrData(Pointer(P)^).Len);
  {$endif}

  Result := P;
end;

function IsBooleanTypeInfo(const Value: PTypeInfo): Boolean;
var
  Base: PTypeInfo;
  pBase: ^PTypeInfo;
  TypeData: PTypeData;
  S: PByte;
begin
  Result := Assigned(Value) and (Value.Kind = tkEnumeration);
  if (not Result) then
    Exit;

  Base := Value;
  repeat
    pBase := GetTypeData(Base)^.BaseType;
    if (pBase = nil) or (pBase^ = nil) or (pBase^ = Base) then Break;
    Base := pBase^;
  until (False);

  Result := (Base = System.TypeInfo(Boolean)) or
    (Base = System.TypeInfo(ByteBool)) or
    (Base = System.TypeInfo(WordBool)) or
    (Base = System.TypeInfo(LongBool));

  if (not Result) then
  begin
    TypeData := GetTypeData(Base);
    if (TypeData.MinValue = 0) and (TypeData.MaxValue = 1) then // match C++ bool
    begin
      S := Pointer(@Base.Name);
      if (S^ <> 4) then Exit;
      Inc(S);
      if (S^ <> Ord('b')) then Exit;
      Inc(S);
      if (S^ <> Ord('o')) then Exit;
      Inc(S);
      if (S^ <> Ord('o')) then Exit;
      Inc(S);
      if (S^ <> Ord('l')) then Exit;

      Result := True;
    end;
  end;
end;

function IsReferenceTypeInfo(const Value: PTypeInfo): Boolean;
var
  TypeData: PTypeData;
  Table: PIntfMethodTable;
  MethodEntry: PIntfMethodEntry;
  Name: ^HugeByteArray;
begin
  Result := False;
  if (not Assigned(Value)) or (Value.Kind <> tkInterface) then
    Exit;

  TypeData := GetTypeData(Value);
  if (not Assigned(TypeData.IntfParent)) or (TypeData.IntfParent^ <> TypeInfo(IInterface)) then
    Exit;

  Table := GetTail(TypeData.IntfUnit);
  MethodEntry := Pointer(NativeUInt(Table) + SizeOf(TIntfMethodTable));
  if (Table.Count <> 1) then
    Exit;

  case Table.RttiCount of
    $ffff: Result := (PByte(@MethodEntry.Name)^ = 2){not RTTI-reference};
    1:
    begin
      Name := Pointer(@MethodEntry.Name);
      Result := (Name[0] = 6) and (Name[1] = Ord('I')) and (Name[2] = Ord('n')) and
        (Name[3] = Ord('v')) and (Name[4] = Ord('o')) and (Name[5] = Ord('k')) and (Name[6] = Ord('e'));
    end;
  else
    Exit;
  end;
end;

function IsManagedTypeInfo(const Value: PTypeInfo): Boolean;
var
  i: Cardinal;
  {$ifdef WEAKREF}
  WeakMode: Boolean;
  {$endif}
  FieldTable: PFieldTable;
begin
  Result := False;

  if Assigned(Value) then
  case Value.Kind of
    tkVariant,
    {$ifdef AUTOREFCOUNT}
    tkClass,
    {$endif}
    {$ifdef WEAKINSTREF}
    tkMethod,
    {$endif}
    {$ifdef FPC}
    tkAString,
    {$endif}
    tkWString, tkLString, {$ifdef UNICODE}tkUString,{$endif} tkInterface, tkDynArray:
    begin
      Result := True;
      Exit;
    end;
    tkArray{static array}:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if (FieldTable.Fields[0].TypeInfo <> nil) then
        Result := IsManagedTypeInfo(FieldTable.Fields[0].TypeInfo^);
    end;
    tkRecord:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if FieldTable.Count > 0 then
      begin
        {$ifdef WEAKREF}
        WeakMode := False;
        {$endif}
        for i := 0 to FieldTable.Count - 1 do
        begin
          {$ifdef WEAKREF}
          if FieldTable.Fields[i].TypeInfo = nil then
          begin
            WeakMode := True;
            Continue;
          end;
          if (not WeakMode) then
          begin
          {$endif}
            if (IsManagedTypeInfo(FieldTable.Fields[i].TypeInfo^)) then
            begin
              Result := True;
              Exit;
            end;
          {$ifdef WEAKREF}
          end else
          begin
            Result := True;
            Exit;
          end;
          {$endif}
        end;
      end;
    end;
  end;
end;


{$if Defined(FPC)}
function fpc_Copy_internal(Src, Dest, TypeInfo: Pointer): SizeInt; [external name 'FPC_COPY'];
procedure CopyRecord(const Dest, Source, TypeInfo: Pointer); inline;
begin
  fpc_Copy_internal(Source, Dest, TypeInfo);
end;
{$elseif Defined(CPUINTEL)}
procedure CopyRecord(const Dest, Source, TypeInfo: Pointer);
asm
  jmp System.@CopyRecord
end;
{$else}
procedure CopyRecord(const Dest, Source, TypeInfo: Pointer); inline;
begin
  System.CopyArray(Dest, Source, TypeInfo, 1);
end;
{$ifend}

procedure CopyObject(const Dest, Src: TObject);
var
  InitTable: Pointer;
  BaseSize, DestSize: NativeInt;
  BaseClass, DestClass, SrcClass: TClass;
begin
  if (Dest = nil) or (Src = nil) then Exit;

  DestClass := TClass(Pointer(Dest)^);
  SrcClass := TClass(Pointer(Src)^);

  if (DestClass = SrcClass) then BaseClass := DestClass
  else
  if (DestClass.InheritsFrom(SrcClass)) then BaseClass := SrcClass
  else
  if (SrcClass.InheritsFrom(DestClass)) then BaseClass := DestClass
  else
  begin
    BaseClass := DestClass;

    while (BaseClass <> nil) and (not SrcClass.InheritsFrom(BaseClass)) do
    begin
      BaseClass := BaseClass.ClassParent;
    end;

    if (BaseClass = nil) then Exit;
  end;

  DestSize := BaseClass.InstanceSize;
  while (BaseClass <> TObject) do
  begin
    InitTable := PPointer(Integer(BaseClass) + vmtInitTable)^;
    if (InitTable <> nil) then
    begin
      CopyRecord(Pointer(Dest), Pointer(Src), InitTable);
      Break;
    end;
    BaseClass := BaseClass.ClassParent;
  end;

  BaseSize := BaseClass.InstanceSize;
  if (BaseSize <> DestSize) then
  begin
    System.Move(Pointer(NativeInt(Src) + BaseSize)^,
      Pointer(NativeInt(Dest) + BaseSize)^, DestSize - BaseSize);
  end;
end;

{$if Defined(FPC)}
procedure CopyArray(const Dest, Source: Pointer; const TypeInfo: PTypeInfo; const Count: NativeInt);
var
  ItemDest, ItemSrc: Pointer;
  ItemSize, i: NativeInt;
begin
  ItemDest := Dest;
  ItemSrc := Source;

  case TypeInfo.Kind of
    tkVariant: ItemSize := SizeOf(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: ItemSize := SizeOf(Pointer);
      tkArray, tkRecord, tkObject: ItemSize := PFieldTable(NativeUInt(TypeInfo) + PByte(@TypeInfo.Name)^).Size;
  else
    Exit;
  end;

  for i := 1 to Count do
  begin
    fpc_Copy_internal(ItemSrc, ItemDest, TypeInfo);

    Inc(NativeInt(ItemDest), ItemSize);
    Inc(NativeInt(ItemSrc), ItemSize);
  end;
end;
{$elseif (CompilerVersion <= 20)}
procedure CopyArray(const Dest, Source: Pointer; const TypeInfo: PTypeInfo; const Count: NativeInt);
asm
  cmp byte ptr [ecx], tkArray
  jne @1
  push eax
  push edx
    movzx edx, [ecx + TTypeInfo.Name]
    mov eax, [ecx + edx + 6]
    mov ecx, [ecx + edx + 10]
    mul Count
    mov ecx, [ecx]
    mov Count, eax
  pop edx
  pop eax
  @1:

  pop ebp
  jmp System.@CopyArray
end;
{$ifend}

{$if Defined(FPC)}
procedure int_Initialize(Data, TypeInfo: Pointer); [external name 'FPC_INITIALIZE'];
procedure InitializeArray(const Item: Pointer; const TypeInfo: PTypeInfo; const Count: NativeInt);
var
  ItemPtr: Pointer;
  ItemSize, i: NativeInt;
begin
  ItemPtr := Item;

  case TypeInfo.Kind of
    tkVariant: ItemSize := SizeOf(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: ItemSize := SizeOf(Pointer);
      tkArray, tkRecord, tkObject: ItemSize := PFieldTable(NativeUInt(TypeInfo) + PByte(@TypeInfo.Name)^).Size;
  else
    Exit;
  end;

  for i := 1 to Count do
  begin
    int_Initialize(ItemPtr, TypeInfo);
    Inc(NativeInt(ItemPtr), ItemSize);
  end;
end;
{$elseif (CompilerVersion <= 20)}
procedure InitializeArray(const Item: Pointer; const TypeInfo: PTypeInfo; const Count: NativeInt);
asm
  jmp System.@InitializeArray
end;
{$ifend}

{$if Defined(FPC)}
procedure int_Finalize(Data, TypeInfo: Pointer); [external name 'FPC_FINALIZE'];
procedure FinalizeArray(const Item: Pointer; const TypeInfo: PTypeInfo; const Count: NativeInt);
var
  ItemPtr: Pointer;
  ItemSize, i: NativeInt;
begin
  ItemPtr := Item;

  case TypeInfo.Kind of
    tkVariant: ItemSize := SizeOf(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: ItemSize := SizeOf(Pointer);
      tkArray, tkRecord, tkObject: ItemSize := PFieldTable(NativeUInt(TypeInfo) + PByte(@TypeInfo.Name)^).Size;
  else
    Exit;
  end;

  for i := 1 to Count do
  begin
    int_Finalize(ItemPtr, TypeInfo);
    Inc(NativeInt(ItemPtr), ItemSize);
  end;
end;
{$elseif (CompilerVersion <= 20)}
procedure FinalizeArray(const Item: Pointer; const TypeInfo: PTypeInfo; const Count: NativeInt);
asm
  jmp System.@FinalizeArray
end;
{$ifend}

function DynArrayLength(P: Pointer): NativeInt;
begin
  Result := NativeInt(P);
  if (Result <> 0) then
  begin
    Dec(Result, SizeOf(NativeInt));
    Result := PNativeInt(Result)^;
    {$ifdef FPC}
      Inc(Result);
    {$endif}
  end;
end;

{$if Defined(FPC)}
procedure fpc_dynarray_incr_ref(P: Pointer); [external name 'FPC_DYNARRAY_INCR_REF'];
procedure DynArrayAddRef(P: Pointer); inline;
begin
  fpc_dynarray_incr_ref(P);
end;
{$elseif Defined(CPUINTEL)}
procedure DynArrayAddRef(P: Pointer);
asm
  jmp System.@DynArrayAddRef
end;
{$else}
procedure DynArrayAddRef(P: Pointer);
var
  Rec: PDynArrayRec;
begin
  if (P <> nil) then
  begin
    Rec := P;
    Dec(Rec);
    if (Rec.RefCnt >= 0) then
    begin
      AtomicIncrement(Rec.RefCnt);
    end;
  end;
end;
{$ifend}

{$ifdef FPC}
procedure fpc_dynarray_clear (var P: Pointer; TypeInfo: Pointer); external name 'FPC_DYNARRAY_CLEAR';
procedure DynArrayClear(var P: Pointer; TypeInfo: Pointer); inline;
begin
  fpc_dynarray_clear(P, TypeInfo);
end;
{$endif}

function UniqueLuaBuffer(var Data: __luabuffer): NativeInt;
begin
  Result := NativeInt(Data);
  if (Result = 0) then Exit;

  {$ifdef NEXTGEN}
    if (PDynArrayRec(Result - SizeOf(TDynArrayRec)).RefCount <> 1) then
      Data := System.Copy(Data, Low(Data), Length(Data));
  {$else}
    UniqueString(AnsiString(Data));
  {$endif}

  Result := NativeInt(Data) - Result;
end;


{ Low level helpers }

function LStrLen(S: PAnsiChar): NativeUInt;
label
  done;
const
  CHARS_IN_CARDINAL = SizeOf(Cardinal) div SizeOf(Byte);
  SUB_MASK  = Integer(-$01010101);
  OVERFLOW_MASK = Integer($80808080);
var
  Start: PAnsiChar;
  Align: NativeInt;
  X, V: NativeInt;
begin
  Start := S;
  if (S = nil) then goto done;

  Align := NativeInt(S) and (CHARS_IN_CARDINAL - 1);
  if (Align <> 0) then
  repeat
    if (Byte(S^) = 0) then goto done;
    Inc(Align);
    Inc(S);
  until (Align = CHARS_IN_CARDINAL);

  repeat
    X := PCardinal(S)^;
    Inc(S, CHARS_IN_CARDINAL);

    V := X + SUB_MASK;
    X := not X;
    X := X and V;

    if (X and OVERFLOW_MASK = 0) then Continue;
    Dec(S, CHARS_IN_CARDINAL);
    Inc(S, Byte(Byte(X and $80 = 0) + Byte(X and $8080 = 0) + Byte(X and $808080 = 0)));
    Break;
  until (False);

done:
  Result := NativeUInt(S) - NativeUInt(Start);
end;

function WStrLen(S: PWideChar): NativeUInt;
label
  done;
var
  Start: PWideChar;
  X: Cardinal;
begin
  Start := S;
  if (S = nil) then goto done;

  if (NativeInt(S) and 1 <> 0) then
  begin
    while (S^ <> #0) do Inc(S);
    goto done;
  end;

  if (S^ = #0) then goto done;
  Inc(NativeInt(S), NativeInt(S) and 2);

  repeat
    X := PCardinal(S)^;
    if (X and $ffff = 0) then Break;
    Inc(S);
    if (X <= $ffff) then Break;
    Inc(S);
  until (False);

done:
  Result := (NativeUInt(S) - NativeUInt(Start)) shr 1;
end;

{$ifNdef NEXTGEN}
procedure FillShortString(var AResult: ShortString; AChars: PAnsiChar; ALength, AMaxLength: Integer);
var
  S: PByte;
begin
  if (ALength > AMaxLength) then ALength := AMaxLength;
  S := Pointer(@AResult);
  S^ := ALength;
  Inc(S);
  if (ALength <> 0) then
    System.Move(AChars^, S^, ALength);
end;
{$endif}

{ Generated in CachedSerializer: https://github.com/d-mozulyov/CachedTexts#cachedserializer }
{ 0: False, 1: True, -1: Failure }
function CastStringAsBoolean(const AValue: PAnsiChar; const ALength: Integer): Integer; overload;
begin
  Result := -1;

  // byte ascii, ignore case
  with PMemoryItems(AValue)^ do
  case ALength of
    0: Result := 0; // empty string
    1: case (Bytes[0]) of // "0", "1"
         $30: Result := 0; // "0"
         $31: Result := 1; // "1"
       end;
    2: case (Words[0] or $2020) of // "no", "ok"
         $6F6E: Result := 0; // "no"
         $6B6F: Result := 1; // "ok"
       end;
    3: if (Words[0] + Bytes[2] shl 16 or $202020 = $736579) then Result := 1; // "yes"
    4: if (Cardinals[0] or $20202020 = $65757274) then Result := 1; // "true"
    5: if (Cardinals[0] or $20202020 = $736C6166) and (Bytes[4] or $20 = $65) then
       Result := 0; // "false"
    6: if (Cardinals[0] or $20202020 = $636E6163) and (Words[2] or $2020 = $6C65) then
       Result := 0; // "cancel"
  end;
end;

{ Generated in CachedSerializer: https://github.com/d-mozulyov/CachedTexts#cachedserializer }
{ 0: False, 1: True, -1: Failure }
function CastStringAsBoolean(const AValue: PWideChar; const ALength: Integer): Integer; overload;
begin
  Result := -1;

  // utf16 ascii, ignore case
  with PMemoryItems(AValue)^ do
  case ALength of
    0: Result := 0; // empty string
    1: case (Words[0]) of // "0", "1"
         $0030: Result := 0; // "0"
         $0031: Result := 1; // "1"
       end;
    2: case (Cardinals[0] or $00200020) of // "no", "ok"
         $006F006E: Result := 0; // "no"
         $006B006F: Result := 1; // "ok"
       end;
    3: if (Cardinals[0] or $00200020 = $00650079) and (Words[2] or $0020 = $0073) then
       Result := 1; // "yes"
    4: if (Cardinals[0] or $00200020 = $00720074) and
       (Cardinals[1] or $00200020 = $00650075) then Result := 1; // "true"
    5: if (Cardinals[0] or $00200020 = $00610066) and
       (Cardinals[1] or $00200020 = $0073006C) and (Words[4] or $0020 = $0065) then
       Result := 0; // "false"
    6: if (Cardinals[0] or $00200020 = $00610063) and
       (Cardinals[1] or $00200020 = $0063006E) and
       (Cardinals[2] or $00200020 = $006C0065) then Result := 0; // "cancel"
  end;
end;

function NumberToInteger({$ifdef CPUX86}var{$else}const{$endif} VNumber: Double;
  var VInteger: Integer): Boolean;
const
  DBLROUND_CONST{$ifdef CPUX86}: Double{$endif} = 6755399441055744.0;
var
  Temp: record
  case Integer of
    0: (VDouble: Double);
    1: (VInteger: Integer);
  end;
begin
  Temp.VDouble := VNumber + DBLROUND_CONST;
  if (Temp.VInteger <> VNumber) then
  begin
    Result := False;
    Exit;
  end else
  begin
    VInteger := Temp.VInteger;
    Result := True;
  end;
end;

// 0: TDateTime, 1: TDate, 2: TTime
function InspectDateTime({$ifdef CPUX86}var{$else}const{$endif} Value: TDateTime): Integer;
const
  DBLROUND_CONST{$ifdef CPUX86}: Double{$endif} = 6755399441055744.0;
var
  Temp: record
  case Integer of
    0: (VDouble: Double);
    1: (VInteger: Integer);
  end;
begin
  // check 2: TTime
  {$ifdef CPUX86}
  if (PPoint(@Value)^.Y >= 0) and (PInt64(@Value)^ < 4607182418800017408) then
  {$else}
  if (Value >= 0) and (Value < 1.0) then
  {$endif}
  begin
    Result := 2;
    Exit;
  end;

  // 0: TDateTime, 1: TDate
  Temp.VDouble := Value + DBLROUND_CONST;
  Result := Byte(Temp.VInteger = Value);
end;

function IsValidIdent(const Ident: LuaString): Boolean;
{$if (not Defined(LUA_UNICODE)) and (not Defined(NEXTGEN))}
const
  Alpha: set of AnsiChar = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric: set of AnsiChar = ['A'..'Z', 'a'..'z', '_', '0'..'9'{$ifdef UNITSCOPENAMES}, '.'{$endif}];
{$ifend}
var
  i, Count: NativeInt;
  S: PLuaChar;
begin
  Result := False;
  Count := Length(Ident);
  if (Count = 0) then Exit;
  S := Pointer(Ident);

  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    if (S^ <> '_') and (not
      {$if not Defined(UNICODE)}
        IsCharAlphaW(S^)
      {$elseif (CompilerVersion < 25)}
        IsLetter(S^)
      {$else}
        S^.IsLetter
      {$ifend}
    ) then Exit;

    Inc(S);
    for i := 1 to Count - 1 do
    begin
      case S^ of
        '_'{$ifdef UNITSCOPENAMES}, '.'{$endif}: ;
      else
        if (not
          {$if not Defined(UNICODE)}
            IsCharAlphaNumericW(S^)
          {$elseif (CompilerVersion < 25)}
            IsLetterOrDigit(S^)
          {$else}
            S^.IsLetterOrDigit
          {$ifend}
        ) then Exit;
      end;

      Inc(S);
    end;
  {$else .ANSI}
    if (not (S^ in Alpha)) then Exit;
    Inc(S);
    for i := 1 to Count - 1 do
    begin
      if (not (S^ in AlphaNumeric)) then Exit;
      Inc(S);
    end;
  {$ifend}

  Result := True;
end;


{ LuaCFunction routine }

{$ifdef LUA_NATIVEFUNC}
const
  MEMORY_PAGE_SIZE = 4 * 1024;
  MEMORY_PAGE_CLEAR = -MEMORY_PAGE_SIZE;
  MEMORY_PAGE_TEST = MEMORY_PAGE_SIZE - 1;
  MEMORY_BLOCK_SIZE = 64 * 1024;
  MEMORY_BLOCK_CLEAR = -MEMORY_BLOCK_SIZE;
  MEMORY_BLOCK_TEST = MEMORY_BLOCK_SIZE - 1;
  MEMORY_PAGE_MARKER_LOW = Ord('C') + Ord('r') shl 8 + Ord('y') shl 16 + Ord('s') shl 24;
  MEMORY_PAGE_MARKER_HIGH = Ord('C') + Ord('F') shl 8 + Ord('u') shl 16 + Ord('n') shl 24;

type
  PLuaCFunctionData = ^TLuaCFunctionData;
  TLuaCFunctionData = object
    {$ifdef CPUX86}
      Bytes: array[0..19] of Byte;
    {$else .CPUX64}
      Bytes: array[0..33] of Byte;
    {$endif}
    procedure Init(const Lua: TLua; const P1, P2: __luapointer; const Callback: Pointer);
  end;

  PLuaCFunctionPage = ^TLuaCFunctionPage;
  TLuaCFunctionPage = object
    MarkerLow: Integer;
    MarkerHigh: Integer;
    Items: PLuaCFunctionData;
    Allocated: Cardinal;

    procedure Init;
    function Alloc: Pointer;
    function Free(const P: Pointer): Boolean;
  end;

  PLuaCFunctionBlock = ^TLuaCFunctionBlock;
  TLuaCFunctionBlock = object(TLuaCFunctionPage)
    // er
    // 00 - decommited page
    // 01 - full page
    // 10 - impossible case
    // 11 - contains empties
    Reserved: Word;
    Empties: Word;
    // single linked list
    Next: PLuaCFunctionBlock;
  end;

  TLuaCFunctionHeap = object
  public
    {$WARNINGS OFF}
    FakeField: NativeUInt;
    {$WARNINGS ON}
    Blocks: PLuaCFunctionBlock;

    function Alloc(const Lua: TLua; const Callback: Pointer; const P1, P2: __luapointer): Pointer;
    procedure Free(const LuaCFunction: Pointer);
    procedure Clear;
  end;

function BitScan16(const Value: Integer{Word}): NativeInt;
begin
  if (Value <> 0) then
  begin
    if (Value and $ff <> 0) then
    begin
      Result := BIT_SCANS[Byte(Value)];
    end else
    begin
      Result := 8 + BIT_SCANS[Value shr 8];
    end;
  end else
  begin
    Result := -1;
  end;
end;

procedure TLuaCFunctionData.Init(const Lua: TLua; const P1, P2: __luapointer; const Callback: Pointer);
var
  Offset: NativeInt;
begin
  {$ifdef CPUX86}
    // mov eax, Lua
    Bytes[0] := $B8;
    PPointer(@Bytes[1])^ := Lua;
    // mov edx, P1
    Bytes[5] := $BA;
    PInteger(@Bytes[6])^ := P1;
    // mov ecx, P2
    Bytes[10] := $B9;
    PInteger(@Bytes[11])^ := P2;
    // jmp Callback
    Offset := NativeInt(Callback) - (NativeInt(@Bytes[15]) + 5);
    Bytes[15] := $E9;
    PInteger(@Bytes[16])^ := Offset;
  {$endif}

  {$ifdef CPUX64}
    // mov rcx, Lua
    Bytes[0] := $48;
    Bytes[1] := $B9;
    PPointer(@Bytes[2])^ := Lua;
    // mov edx, P1
    Bytes[10] := $BA;
    PInteger(@Bytes[11])^ := P1;
    // mov r8d, P2
    Bytes[15] := $41;
    Bytes[16] := $B8;
    PInteger(@Bytes[17])^ := P2;
    // jump
    Offset := NativeInt(Callback) - (NativeInt(@Bytes[21]) + 5);
    case Integer(Offset shr 32) of
      -1, 0:
      begin
        // jmp Callback
        Bytes[21] := $E9;
        PInteger(@Bytes[22])^ := Offset;
      end;
    else
      // mov rax, Callback
      Bytes[21] := $48;
      Bytes[22] := $B8;
      PPointer(@Bytes[23])^ := Callback;
      // jmp rax
      Bytes[31] := $48;
      Bytes[32] := $FF;
      Bytes[33] := $E0;
    end;
  {$endif}
end;

procedure TLuaCFunctionPage.Init;
type
  TList = array[0..MEMORY_PAGE_SIZE div SizeOf(TLuaCFunctionData) - 1] of TLuaCFunctionData;
var
  i, Count: NativeInt;
  List: ^TList;
begin
  MarkerLow := MEMORY_PAGE_MARKER_LOW;
  MarkerHigh := MEMORY_PAGE_MARKER_HIGH;

  List := Pointer(@Self);
  if (NativeInt(List) and MEMORY_BLOCK_TEST <> 0) then
  begin
    Inc(NativeInt(List), SizeOf(TLuaCFunctionPage));
    Count := (MEMORY_PAGE_SIZE - SizeOf(TLuaCFunctionPage)) div SizeOf(TLuaCFunctionData);
  end else
  begin
    Inc(NativeInt(List), SizeOf(TLuaCFunctionBlock));
    Count := (MEMORY_PAGE_SIZE - SizeOf(TLuaCFunctionBlock)) div SizeOf(TLuaCFunctionData);
  end;

  for i := 0 to Count - 2 do
  begin
    PPointer(@List[i])^ := @List[i + 1];
  end;
  PPointer(@List[Count - 1])^ := nil;

  Items := @List[0];
  Allocated := 0;
end;

function TLuaCFunctionPage.Alloc: Pointer;
begin
  Result := Items;
  if (Result <> nil) then
  begin
    Inc(Allocated);
    Items := PPointer(Result)^;
  end;
end;

function TLuaCFunctionPage.Free(const P: Pointer): Boolean;
var
  Count: Cardinal;
begin
  PPointer(P)^ := Items;
  Items := P;

  Count := Allocated - 1;
  Allocated := Count;
  Result := (Count = 0);
end;

procedure TLuaCFunctionHeap.Clear;
var
  Block, Next: PLuaCFunctionBlock;
begin
  Block := Blocks;
  Blocks := nil;

  while (Block <> nil) do
  begin
    Next := Block.Next;
    VirtualFree(Block, 0, MEM_RELEASE);
    Block := Next;
  end;
end;

function TLuaCFunctionHeap.Alloc(const Lua: TLua; const Callback: Pointer; const P1, P2: __luapointer): Pointer;
var
  Index: NativeInt;
  Block: PLuaCFunctionBlock;
  Page: PLuaCFunctionPage;

  function CommitPage(const ABlock: PLuaCFunctionBlock; const AIndex: NativeInt): PLuaCFunctionPage; far;
  begin
    Result := Pointer(NativeInt(ABlock) + AIndex * MEMORY_PAGE_SIZE);
    VirtualAlloc(Result, MEMORY_PAGE_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

    ABlock.Empties := ABlock.Empties or (1 shl AIndex);
    ABlock.Reserved := ABlock.Reserved or (1 shl AIndex);

    Result.Init;
  end;

begin
  Result := nil;

  Block := Self.Blocks;
  while (Block <> nil) do
  begin
    Index := BitScan16(Block.Empties);
    if (Index >= 0) then
    begin
      Page := Pointer(NativeInt(Block) + Index * MEMORY_PAGE_SIZE);
      Result := Page.Alloc;
      if (Page.Items = nil) then
      begin
        Block.Empties := Block.Empties and (not (1 shl Index));
      end;
      Break;
    end else
    if (Block.Reserved <> $ffff) then
    begin
      Index := BitScan16(not Block.Reserved);
      Result := CommitPage(Block, Index).Alloc;
      Break;
    end;

    Block := Block.Next;
  end;

  if (Result = nil) then
  begin
    Block := VirtualAlloc(nil, MEMORY_BLOCK_SIZE, MEM_RESERVE, PAGE_EXECUTE_READWRITE);

    if (Block <> nil) then
    begin
      Result := CommitPage(Block, 0).Alloc;
      Block.Next := Self.Blocks;
      Self.Blocks := Block;
    end;
  end;

  if (Result = nil) then System.Error(reOutOfMemory);
  PLuaCFunctionData(Result).Init(Lua, P1, P2, Callback);
end;

procedure TLuaCFunctionHeap.Free(const LuaCFunction: Pointer);
var
  Index: NativeInt;
  Page: PLuaCFunctionPage;
  Block, Item: PLuaCFunctionBlock;
  Parent: ^PLuaCFunctionBlock;
begin
  Page := Pointer(NativeInt(LuaCFunction) and MEMORY_PAGE_CLEAR);
  Block := Pointer(NativeInt(Page) and MEMORY_BLOCK_CLEAR);
  Index := NativeInt(NativeUInt(Page) div MEMORY_PAGE_SIZE) and 15;
  if (not Page.Free(LuaCFunction)) then
  begin
    Block.Empties := Block.Empties or (1 shl Index);
    Exit;
  end;

  // decommit page (not first)
  if (Index <> 0) then
  begin
    Block.Empties := Block.Empties and (not (1 shl Index));
    Block.Reserved := Block.Reserved and (not (1 shl Index));
    VirtualFree(Page, MEMORY_PAGE_SIZE, MEM_DECOMMIT);
  end;

  // check empty block
  if (Block.Empties <> 1) or (Block.Allocated <> 0) then
  begin
    Exit;
  end;

  // remove block
  Parent := @Blocks;
  repeat
    Item := Parent^;
    if (Item = Block) then
    begin
      Parent^ := Item.Next;
      VirtualFree(Item, 0, MEM_RELEASE);
      Break;
    end;

    Parent := @Item.Next;
  until (False);
end;
{$endif}


{ Containers }

const
  HEAP_BUFFER_SHIFT = 13{8Kb};
  HEAP_BUFFER_SIZE = 1 shl HEAP_BUFFER_SHIFT;
  HEAP_BUFFER_MASK = HEAP_BUFFER_SIZE - 1;

  LUA_POINTER_INVALID = __luapointer(-1);

type
  TLuaStack = object
  private
    FItems: TIntegerDynArray;
    FCount: NativeInt;
    FCapacity: NativeInt;
  public
    procedure Clear;
    procedure Push(const Value: Integer);
    function Pop: Integer;

    property Items: TIntegerDynArray read FItems;
    property Count: NativeInt read FCount;
    property Capacity: NativeInt read FCapacity;
  end;

  TLuaMemoryHeap = object
  private
    FBuffers: array of TBytes;
    FCurrent: NativeInt; {Pointer/Index+Offset}
    FMargin: NativeInt;

    function GrowAlloc(const ASize: NativeInt): __luapointer;
    {$ifdef LARGEINT}
    function ExtendedUnpack(const APtr: __luapointer): Pointer;
    {$endif}
  public
    procedure Clear;
    function Alloc(const ASize: NativeInt): __luapointer;
    function Unpack(const APtr: __luapointer): Pointer; {$ifdef INLINESUPPORT}inline;{$endif}
  end;

  TLuaBuffer = object
  private
    FBytes: TBytes;
    FCapacity: NativeInt;
    FSize: NativeInt;

    procedure SetCapacity(const Value: NativeInt);
    function GrowAlloc(const ASize: NativeInt): Pointer;
  public
    procedure Clear;
    function Alloc(const ASize: NativeInt): Pointer;

    property Bytes: TBytes read FBytes;
    property Capacity: NativeInt read FCapacity write SetCapacity;
    property Size: NativeInt read FSize write FSize;
  end;

  PLuaDictionaryItem = ^TLuaDictionaryItem;
  TLuaDictionaryItem = packed record
    Key: Pointer;
    Value: __luapointer;
    Next: Integer;
  end;

  TLuaDictionary = object{<Pointer,__luapointer>}
  private
    FItems: array of TLuaDictionaryItem;
    FHashes: array of Integer;
    FHashesMask: NativeInt;
    FCapacity: NativeInt;
    FCount: NativeInt;

    procedure Grow;
    function NewItem(const Key: Pointer): PLuaDictionaryItem;
    function InternalFind(const Key: Pointer; const ModeCreate: Boolean): PLuaDictionaryItem;
  public
    procedure Clear;
    procedure TrimExcess;
    procedure Assign(const Value: TLuaDictionary);
    function Find(const Key: Pointer): PLuaDictionaryItem; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure Add(const Key: Pointer; const Value: __luapointer);
    function FindValue(const Key: Pointer): __luapointer;

    property Capacity: NativeInt read FCapacity;
    property Count: NativeInt read FCount;
  end;

  PLuaNameItem = ^TLuaNameItem;
  TLuaNameItem = packed record
    HashCode: Integer;
    Value: __luaname;
    Next: Integer;
  end;

  TLuaNames = object{TDictionary<__luaname,__luaname>}
  private
    FLua: TLua;
    FItems: array of TLuaNameItem;
    FHashes: array of Integer;
    FHashesMask: NativeInt;
    FCapacity: NativeInt;
    FCount: NativeInt;

    procedure Grow;
    function NewItem(const HashCode: Integer): PLuaNameItem;
    function InternalFind(const ModeCreate: Boolean): PLuaNameItem;
    procedure InitBuffer(const Key: LuaString); overload;
    procedure InitBuffer(const RttiName: ShortString); overload;
  public
    procedure Clear;
    procedure TrimExcess;
    function Add(const Key: LuaString): __luaname; overload;
    function Add(const RttiName: ShortString): __luaname; overload;
    function FindValue(const Key: LuaString): __luaname; overload;
    function FindValue(const RttiName: ShortString): __luaname; overload;

    property Lua: TLua read FLua write FLua;
    property Capacity: NativeInt read FCapacity;
    property Count: NativeInt read FCount;
  end;

// x86 architecture compatibility
{$ifNdef CPUX86}
function Swap(const X: NativeUInt): NativeUInt; inline;
begin
  Result := (Byte(X) shl 8) + Byte(X shr 8);
end;
{$endif}

procedure SwapPtr(var Left, Right: Pointer);
var
  Temp: Pointer;
begin
  Temp := Left;
  Left := Right;
  Right := Temp;
end;

procedure TLuaStack.Clear;
begin
  FItems := nil;
  FCount := 0;
  FCapacity := 0;
end;

procedure TLuaStack.Push(const Value: Integer);
label
  start;
var
  C: NativeInt;
begin
start:
  C := FCount;
  if (C = FCapacity) then
  begin
    Inc(C);
    FCount := C;
    Dec(C);
    FItems[C] := Value;
    Exit;
  end else
  begin
    if (C = 0) then
    begin
      C := 16;
    end else
    begin
      Inc(C, C);
    end;
    FCapacity := C;
    SetLength(FItems, C);
    goto start;
  end;
end;

function TLuaStack.Pop: Integer;
var
  C: NativeInt;
begin
  C := FCount;
  if (C > 0) then
  begin
    Dec(C);
    FCount := C;
    Result := FItems[C];
    Exit;
  end else
  begin
    raise ELua.CreateFmt('TLuaStack.Pop: invalid count %d', [C]);
  end;
end;

procedure TLuaMemoryHeap.Clear;
begin
  FBuffers := nil;
  FCurrent := 0;
  FMargin := 0;
end;

function TLuaMemoryHeap.GrowAlloc(const ASize: NativeInt): __luapointer;
var
  BufferSize, Count: NativeInt;
begin
  // buffer size
  BufferSize := (ASize + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer);
  if (BufferSize < HEAP_BUFFER_SIZE) then BufferSize := HEAP_BUFFER_SIZE;

  // allocate buffer
  Count := Length(FBuffers);
  SetLength(FBuffers, Count + 1);
  SetLength(FBuffers[Count], BufferSize);

  // new current pointer
  FCurrent := NativeInt(FBuffers[Count]);
  {$ifdef LARGEINT}
  if (NativeUInt(FCurrent) > NativeUInt(High(Integer))) or
    (NativeUInt(FCurrent + BufferSize - 1) > NativeUInt(High(Integer))) then
    FCurrent := (Count shl HEAP_BUFFER_SHIFT) + (NativeInt(1) shl 31);
  {$endif}

  // allocate size
  FMargin := BufferSize;
  Result := Alloc(ASize);
end;

function TLuaMemoryHeap.Alloc(const ASize: NativeInt): __luapointer;
var
  Size, NewMargin, NewCurrent: NativeInt;
begin
  Size := (ASize + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer);
  NewMargin := FMargin - Size;
  if (NewMargin < 0) then
  begin
    Result := GrowAlloc(Size);
  end else
  begin
    FMargin := NewMargin;
    NewCurrent := FCurrent + Size;
    FCurrent := NewCurrent;
    Result := __luapointer(NewCurrent - Size);
  end;
end;

{$ifdef LARGEINT}
function TLuaMemoryHeap.ExtendedUnpack(const APtr: __luapointer): Pointer;
var
  Value: NativeInt;
begin
  Value := NativeInt(Cardinal(APtr xor (1 shl 31)));
  Result := Pointer(FBuffers[Value shr HEAP_BUFFER_SHIFT]);
  Inc(NativeInt(Result), Value and HEAP_BUFFER_MASK);
end;
{$endif}

function TLuaMemoryHeap.Unpack(const APtr: __luapointer): Pointer;
begin
  Result := Pointer(NativeUInt(Cardinal(APtr)));
  {$ifdef LARGEINT}
  if (APtr < 0) then
    Result := ExtendedUnpack(APtr);
  {$endif}
end;

procedure TLuaBuffer.Clear;
begin
  FBytes := nil;
  FCapacity := 0;
  FSize := 0;
end;

procedure TLuaBuffer.SetCapacity(const Value: NativeInt);
var
  NewBytes: TBytes;
begin
  if (Value = FCapacity) then
    Exit;
  if (Value < FSize) then
    raise ELua.CreateFmt('Invalid capacity value %d, items count: %d', [Value, FSize]);

  if (Value < FSize) then
  begin
    SetLength(NewBytes, Value);
    System.Move(Pointer(FBytes)^, Pointer(NewBytes)^, FSize);
    FBytes := NewBytes;
  end else
  begin
    SetLength(FBytes, Value);
  end;

  FCapacity := Value;
end;

function TLuaBuffer.Alloc(const ASize: NativeInt): Pointer;
var
  NewSize: NativeInt;

  function EInvalidSize(const ASize: NativeInt): ELua; far;
  begin
    Result := ELua.CreateFmt('Invalid allocated size: %d', [ASize]);
  end;
begin
  NewSize := Self.FSize + ASize;

  if (ASize > 0) then
  begin
    if (NewSize <= FCapacity) then
    begin
      FSize := NewSize;
      Dec(NewSize, ASize);
      Result := @FBytes[NewSize];
      Exit;
    end else
    begin
      Result := GrowAlloc(ASize);
    end;
  end else
  begin
    raise EInvalidSize(ASize);
  end;
end;

function TLuaBuffer.GrowAlloc(const ASize: NativeInt): Pointer;
var
  NewSize, NewCapacity: NativeInt;
begin
  NewSize := Self.FSize + ASize;
  NewCapacity := Self.FCapacity;
  if (NewCapacity = 0) then NewCapacity := 16;

  while (NewCapacity < NewSize) do NewCapacity := NewCapacity * 2;
  Self.Capacity := NewCapacity;

  FSize := NewSize;
  Dec(NewSize, ASize);
  Result := @FBytes[NewSize];
end;

procedure TLuaDictionary.Clear;
begin
  FItems := nil;
  FHashes := nil;
  FHashesMask := 0;
  FCapacity := 0;
  FCount := 0;
end;

procedure TLuaDictionary.TrimExcess;
var
  NewItems: array of TLuaDictionaryItem;
begin
  SetLength(NewItems, FCount);
  System.Move(Pointer(FItems)^, Pointer(NewItems)^, FCount * SizeOf(TLuaDictionaryItem));
  SwapPtr(Pointer(FItems), Pointer(NewItems));
  FCapacity := FCount;
end;

procedure TLuaDictionary.Assign(const Value: TLuaDictionary);
var
  NewCount: Integer;
  NewItems: array of TLuaDictionaryItem;
  NewHashes: array of Integer;
begin
  NewCount := Length(Value.FItems);
  SetLength(NewItems, NewCount);
  System.Move(Pointer(Value.FItems)^, Pointer(NewItems)^, NewCount * SizeOf(TLuaDictionaryItem));

  NewCount := Length(Value.FHashes);
  SetLength(NewHashes, NewCount);
  System.Move(Pointer(Value.FHashes)^, Pointer(NewHashes)^, NewCount * SizeOf(Integer));

  SwapPtr(Pointer(Self.FItems), Pointer(NewItems));
  SwapPtr(Pointer(Self.FHashes), Pointer(NewHashes));
  Self.FHashesMask := Value.FHashesMask;
  Self.FCapacity := Value.FCapacity;
  Self.FCount := Value.FCount;
end;

procedure TLuaDictionary.Grow;
var
  i: NativeInt;
  Item: PLuaDictionaryItem;
  HashCode: Integer;
  Parent: PInteger;
  Pow2, NewHashesMask, NewCapacity: NativeInt;
  NewHashes: array of Integer;
begin
  Pow2 := FHashesMask;
  if (Pow2 <> 0) then
  begin
    Inc(Pow2);
    NewCapacity := (Pow2 shr 2) + (Pow2 shr 1);
    if (NewCapacity = Count) then
    begin
      Pow2 := Pow2 * 2;
      SetLength(NewHashes, Pow2);
      FillChar(Pointer(NewHashes)^, Pow2 * SizeOf(Integer), $ff);

      NewHashesMask := (Pow2 - 1);
      Item := Pointer(FItems);
      for i := 0 to Count - 1 do
      begin
        {$ifdef LARGEINT}
          HashCode := (NativeInt(Item.Key) shr 4) xor (NativeInt(Item.Key) shr 32);
        {$else .SMALLINT}
          HashCode := NativeInt(Item.Key) shr 4;
        {$endif}
        Inc(HashCode, (HashCode shr 16) * -1660269137);

        Parent := @NewHashes[NativeInt(HashCode) and NewHashesMask];
        Item.Next := Parent^;
        Parent^ := i;

        Inc(Item);
      end;

      FHashesMask := NewHashesMask;
      NewCapacity := (Pow2 shr 2) + (Pow2 shr 1);
      SwapPtr(Pointer(FHashes), Pointer(NewHashes));
    end;
    SetLength(FItems, NewCapacity);
    FCapacity := NewCapacity;
  end else
  begin
    SetLength(FItems, 3);
    SetLength(FHashes, 4);
    System.FillChar(Pointer(FHashes)^, 4 * SizeOf(Integer), $ff);
    FHashesMask := 3;
    FCapacity := 3;
  end;
end;

function TLuaDictionary.NewItem(const Key: Pointer): PLuaDictionaryItem;
label
  start;
var
  Index: NativeInt;
  HashCode: Integer;
  Parent: PInteger;
begin
start:
  Index := FCount;
  if (Index <> FCapacity) then
  begin
    Inc(Index);
    FCount := Index;
    Dec(Index);

    {$ifdef LARGEINT}
      HashCode := (NativeInt(Key) shr 4) xor (NativeInt(Key) shr 32);
    {$else .SMALLINT}
      HashCode := NativeInt(Key) shr 4;
    {$endif}
    Inc(HashCode, (HashCode shr 16) * -1660269137);

    Parent := @FHashes[NativeInt(HashCode) and FHashesMask];
    Result := @FItems[Index];
    Result.Key := Key;
    Result.Value := LUA_POINTER_INVALID;
    Result.Next := Parent^;
    Parent^ := Index;
  end else
  begin
    Grow;
    goto start;
  end;
end;

function TLuaDictionary.InternalFind(const Key: Pointer; const ModeCreate: Boolean): PLuaDictionaryItem;
var
  HashCode: Integer;
  HashesMask: NativeInt;
  Index: NativeInt;
begin
  {$ifdef LARGEINT}
    HashCode := (NativeInt(Key) shr 4) xor (NativeInt(Key) shr 32);
  {$else .SMALLINT}
    HashCode := NativeInt(Key) shr 4;
  {$endif}
  Inc(HashCode, (HashCode shr 16) * -1660269137);

  HashesMask := FHashesMask;
  if (HashesMask <> 0) then
  begin
    Index := FHashes[NativeInt(HashCode) and HashesMask];
    if (Index >= 0) then
    repeat
      Result := @FItems[Index];
      if (Result.Key = Key) then Exit;
      Index := Result.Next;
    until (Index < 0);
  end;

  if (ModeCreate) then
  begin
    Result := NewItem(Key);
  end else
  begin
    Result := nil;
  end;
end;

function TLuaDictionary.Find(const Key: Pointer): PLuaDictionaryItem;
{$ifdef INLINESUPPORT}
begin
  Result := InternalFind(Key, False);
end;
{$else}
asm
  xor ecx, ecx
  jmp TLuaDictionary.InternalFind
end;
{$endif}

procedure TLuaDictionary.Add(const Key: Pointer; const Value: __luapointer);
begin
  InternalFind(Key, True).Value := Value;
end;

function TLuaDictionary.FindValue(const Key: Pointer): __luapointer;
var
  Item: PLuaDictionaryItem;
begin
  Item := InternalFind(Key, False);
  Result := LUA_POINTER_INVALID;
  if (Assigned(Item)) then
    Result := Item.Value;
end;

procedure TLuaNames.Clear;
begin
  FItems := nil;
  FHashes := nil;
  FHashesMask := 0;
  FCapacity := 0;
  FCount := 0;
end;

procedure TLuaNames.TrimExcess;
var
  NewItems: array of TLuaNameItem;
begin
  SetLength(NewItems, FCount);
  System.Move(Pointer(FItems)^, Pointer(NewItems)^, FCount * SizeOf(TLuaNameItem));
  System.FillChar(Pointer(FItems)^, FCount * SizeOf(TLuaNameItem), #0);
  SwapPtr(Pointer(FItems), Pointer(NewItems));
  FCapacity := FCount;
end;

procedure TLuaNames.Grow;
var
  i: NativeInt;
  Item: PLuaNameItem;
  Parent: PInteger;
  Pow2, NewHashesMask, NewCapacity: NativeInt;
  NewHashes: array of Integer;
begin
  Pow2 := FHashesMask;
  if (Pow2 <> 0) then
  begin
    Inc(Pow2);
    NewCapacity := (Pow2 shr 2) + (Pow2 shr 1);
    if (NewCapacity = Count) then
    begin
      Pow2 := Pow2 * 2;
      SetLength(NewHashes, Pow2);
      FillChar(Pointer(NewHashes)^, Pow2 * SizeOf(Integer), $ff);

      NewHashesMask := (Pow2 - 1);
      Item := Pointer(FItems);
      for i := 0 to Count - 1 do
      begin
        Parent := @NewHashes[NativeInt(Item.HashCode) and NewHashesMask];
        Item.Next := Parent^;
        Parent^ := i;

        Inc(Item);
      end;

      FHashesMask := NewHashesMask;
      NewCapacity := (Pow2 shr 2) + (Pow2 shr 1);
      SwapPtr(Pointer(FHashes), Pointer(NewHashes));
    end;
    SetLength(FItems, NewCapacity);
    FCapacity := NewCapacity;
  end else
  begin
    SetLength(FItems, 3);
    SetLength(FHashes, 4);
    System.FillChar(Pointer(FHashes)^, 4 * SizeOf(Integer), $ff);
    FHashesMask := 3;
    FCapacity := 3;
  end;
end;

function TLuaNames.NewItem(const HashCode: Integer): PLuaNameItem;
label
  start;
var
  Index: NativeInt;
  Parent: PInteger;
  Buffer: ^TLuaBuffer;
begin
start:
  Index := FCount;
  if (Index <> FCapacity) then
  begin
    Inc(Index);
    FCount := Index;
    Dec(Index);

    Parent := @FHashes[NativeInt(HashCode) and FHashesMask];
    Result := @FItems[Index];
    Result.HashCode := HashCode;
    Result.Value := nil;
    Result.Next := Parent^;
    Parent^ := Index;

    Buffer := @TLuaBuffer(FLua.FInternalBuffer);
    lua_pushlstring(FLua.Handle, Pointer(Buffer.FBytes), Buffer.Size);
    Result.Value := lua_tolstring(FLua.Handle, -1, nil);
    FLua.global_fill_value(FLua.global_alloc_ref);
  end else
  begin
    Grow;
    goto start;
  end;
end;

function TLuaNames.InternalFind(const ModeCreate: Boolean): PLuaNameItem;
var
  Key: PByte;
  HashCode, Length, X, i: Integer;
  HashesMask: NativeInt;
  Index: NativeInt;
begin
  // specific characters fix
  with TLuaBuffer(FLua.FInternalBuffer) do
  begin
    Key := Pointer(FBytes);
    HashCode := Size;
  end;
  {$if CompilerVersion >= 20}
    for i := 1 to HashCode{Length} do
    begin
      case Key^ of
        Ord('<'), Ord('>') {$ifdef UNITSCOPENAMES}, Ord('.'){$endif}:
        begin
          Key^ := Ord('_');
        end;
      end;
      Inc(Key);
    end;
    Key := Pointer(TLuaBuffer(FLua.FInternalBuffer).FBytes);
  {$ifend}

  // hash code
  X := 63689;
  Length := HashCode{Length};
  for i := 1 to Length do
  begin
    HashCode := HashCode * X + Integer(Key^);
    Inc(Key);
    X := X * 378551;
  end;
  Length := TLuaBuffer(FLua.FInternalBuffer).Size;
  if (Length > 255) then Length := 255;
  HashCode := (HashCode and $00ffffff) + (Length shl 24);
  Key := Pointer(TLuaBuffer(FLua.FInternalBuffer).FBytes);

  // find
  HashesMask := FHashesMask;
  if (HashesMask <> 0) then
  begin
    Index := FHashes[NativeInt(HashCode) and HashesMask];
    if (Index >= 0) then
    repeat
      Result := @FItems[Index];
      if (Result.HashCode = HashCode) and (CompareMem(Result.Value, Key, HashCode shr 24)) then Exit;
      Index := Result.Next;
    until (Index < 0);
  end;

  // not found
  if (ModeCreate) then
  begin
    Result := NewItem(HashCode);
  end else
  begin
    Result := nil;
  end;
end;

function Utf8FromUnicode(ADest: PAnsiChar; ASource: PWideChar; ALength: Integer): Integer; forward;

procedure TLuaNames.InitBuffer(const Key: LuaString);
var
  Length, Size: Integer;
  Buffer: ^TLuaBuffer;
  {$if (not Defined(LUA_UNICODE)) and (not Defined(NEXTGEN)) and Defined(INTERNALCODEPAGE)}
  CodePage: Word;
  {$ifend}
begin
  Length := System.Length(Key);
  Buffer := Pointer(@FLua.FInternalBuffer);
  Buffer.Size := 0;

  if (Pointer(Key) <> nil) then
  begin
    {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
      // Ansi/Utf8 <-- Utf16
      Size := Length * 3 + 1;
      if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
      {$ifdef LUA_ANSI}
        Buffer.Size := FLua.AnsiFromUnicode(Pointer(Buffer.FBytes), 0, Pointer(Key), Length);
      {$else .LUA_UNICODE}
        Buffer.Size := Utf8FromUnicode(Pointer(Buffer.FBytes), Pointer(Key), Length);
      {$endif}
    {$else .ANSISTRING}
      // Ansi/Utf8 <-- Ansi/Utf8
      Size := Length * 6 + 1;
      if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
      {$ifdef INTERNALCODEPAGE}
      CodePage := PWord(NativeInt(Key) - ASTR_OFFSET_CODEPAGE)^;
      if (CodePage = CODEPAGE_UTF8) then
      begin
        {$ifdef LUA_ANSI}
          Buffer.Size := FLua.AnsiFromUtf8(Pointer(Buffer.FBytes), 0, Pointer(Key), Length);
        {$else .LUA_UNICODE}
          System.Move(Pointer(Key)^, Pointer(Buffer.FBytes)^, Length);
          Buffer.Size := Length;
        {$endif}
      end else
      {$endif}
      begin
        {$ifdef LUA_ANSI}
          Buffer.Size := FLua.AnsiFromAnsi(Pointer(Buffer.FBytes), 0, Pointer(Key),
            {$ifdef INTERNALCODEPAGE}CodePage{$else}0{$endif}, Length);
        {$else .LUA_UNICODE}
          Buffer.Size := FLua.Utf8FromAnsi(Pointer(Buffer.FBytes), Pointer(Key),
            {$ifdef INTERNALCODEPAGE}CodePage{$else}0{$endif}, Length);
        {$endif}
      end;
    {$ifend}
  end;
end;

procedure TLuaNames.InitBuffer(const RttiName: ShortString);
var
  Length, Size: Integer;
  Buffer: ^TLuaBuffer;
begin
  Length := PByte(@RttiName)^;
  Buffer := Pointer(@FLua.FInternalBuffer);
  Buffer.Size := 0;

  if (Length <> 0) then
  begin
    {$ifdef UNICODE}
      Size := Length + 1;
      if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
      {$ifdef LUA_UNICODE}
        Buffer.Size := FLua.AnsiFromUtf8(Pointer(Buffer.FBytes), 0, Pointer(@RttiName[1]), Length);
      {$else .LUA_ANSI}
        Buffer.Size := Length;
        System.Move(RttiName[1], Pointer(Buffer.FBytes)^, Length);
      {$endif}
    {$else .ANSI.ASCII}
      Size := Length;
      if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
      Buffer.Size := Length;
      System.Move(RttiName[1], Pointer(Buffer.FBytes)^, Length);
    {$endif}
  end;
end;

function TLuaNames.Add(const Key: LuaString): __luaname;
begin
  InitBuffer(Key);
  Result := InternalFind(True).Value;
end;

function TLuaNames.Add(const RttiName: ShortString): __luaname;
begin
  InitBuffer(RttiName);
  Result := InternalFind(True).Value;
end;

function TLuaNames.FindValue(const Key: LuaString): __luaname;
begin
  InitBuffer(Key);
  Result := Pointer(InternalFind(False));
  if (Assigned(Result)) then
    Result := PLuaNameItem(Result).Value;
end;

function TLuaNames.FindValue(const RttiName: ShortString): __luaname;
begin
  InitBuffer(RttiName);
  Result := Pointer(InternalFind(False));
  if (Assigned(Result)) then
    Result := PLuaNameItem(Result).Value;
end;


{ TLuaResult }

{$if Defined(FPC) or (CompilerVersion < 24)}
function AtomicDecrement(var Target: Integer): Integer;
asm
  {$ifdef CPUX86}
    or edx, -1
    lock xadd [eax], edx
    lea eax, [edx - 1]
  {$else .CPUX64} .NOFRAME
    or eax, -1
    lock xadd [rcx], eax
    sub eax, 1
  {$endif}
end;
{$ifend}

procedure TLuaResult.GrowCapacity(var List: PLuaArgList; var Capacity: Integer; const Value: Integer);
label
  new, resize;
var
  i, LastCapacity, NewCapacity: Integer;
  Rec: PDynArrayRec;
begin
  if (Capacity or Value < 0) then raise EInvalidArgument;

  NewCapacity := Capacity;
  if (NewCapacity = 0) then NewCapacity := 16;
  while (NewCapacity < Value) do
  begin
    NewCapacity := NewCapacity shl 1;
  end;

  Rec := Pointer(List);
  if (Rec = nil) then
  begin
  new:
    GetMem(Rec, SizeOf(TDynArrayRec) + NewCapacity * SizeOf(TLuaArg));
    Capacity := 0;
  end else
  begin
    Dec(Rec);
    if (Rec.RefCount = 1) then
    begin
    resize:
      ReallocMem(Rec, SizeOf(TDynArrayRec) + NewCapacity * SizeOf(TLuaArg));
    end else
    begin
      if (AtomicDecrement(Rec.RefCount) = 0) then
      begin
        Rec.RefCount := 1;
        goto resize;
      end;
      goto new;
    end;
  end;

  Rec.RefCount := 1;
  Rec.Length := NewCapacity;
  Inc(Rec);
  Pointer(List) := Rec;
  LastCapacity := Capacity;
  Capacity := NewCapacity;

  for i := LastCapacity to NewCapacity - 1 do
  begin
    Pointer(PLuaArgList(Rec)[i].FStringValue) := nil;
  end;
end;

procedure TLuaResult.SetCount(const Value: Integer);
var
  Rec: PDynArrayRec;
begin
  if (FCount = Value) then Exit;
  if (Cardinal(Value) > Cardinal(FCapacity)) then
  begin
    GrowCapacity(FItems, FCapacity, Value);
  end;

  FCount := Value;
  if (Value <> 0) then
  begin
    Rec := Pointer(FItems);
    Dec(Rec);
    Rec.Length := Value;
  end;
end;

procedure TLuaResult.SetParamCount(const Value: Integer);
var
  Rec: PDynArrayRec;
begin
  if (FParamCount = Value) then Exit;
  if (Cardinal(Value) > Cardinal(FParamCapacity)) then
  begin
    GrowCapacity(FParamItems, FParamCapacity, Value);
  end;

  FParamCount := Value;
  if (Value <> 0) then
  begin
    Rec := Pointer(FParamItems);
    Dec(Rec);
    Rec.Length := Value;
  end;
end;

function TLuaResult.GrowAlloc(Size: NativeUInt): Pointer;
var
  Heap: ^TLuaMemoryHeap;
  First: Boolean;
  Ptr: NativeUInt;
begin
  Heap := Pointer(@FHeap);
  First := (Heap.FBuffers = nil);
  Result := Heap.Unpack(Heap.GrowAlloc(Size));

  Ptr := NativeUInt(Result) + Size;
  NativeUInt(FHeapCurrent) := Ptr;
  NativeUInt(FHeapOverflow) := Ptr + NativeUInt(Heap.FMargin);

  if (First) then
  begin
    FHeapEmpty := Heap.FCurrent - NativeInt(Size);
  end;
end;

function TLuaResult.Alloc(Size: NativeUInt): Pointer;
var
  Ptr: NativeUInt;
begin
  if (Size <> 0) then
  begin
    Size := (Size + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer);
    Ptr := NativeUInt(FHeapCurrent);
    Inc(Ptr, Size);
    if (Ptr <= NativeUInt(FHeapOverflow)) then
    begin
      NativeUInt(FHeapOverflow) := Ptr;
      Dec(Ptr, Size);
      Result := Pointer(Ptr);
      Exit;
    end else
    begin
      Result := GrowAlloc(Size);
    end;
  end else
  begin
    Result := nil;
  end;
end;


type
  PMetaStruct = ^TMetaStruct;
  TMetaStruct = record
    MetaType: PLuaMetaType;
    Next: PMetaStruct;
  end;

function TLuaResult.AllocMetaType(const MetaType: PLuaMetaType): Pointer;
var
  Size: NativeUInt;
  MetaStruct: PMetaStruct;
begin
  if (MetaType = nil) then
  begin
    Result := nil;
    Exit;
  end;

  case MetaType.F.Kind of
    mtRecord, mtArray, mtSet:
    begin
      Size := MetaType.Size;
    end;
  else
    raise EInvalidArgument;
  end;

  Size := (Size + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer);
  MetaStruct := Alloc(SizeOf(TMetaStruct) + Size);
  MetaStruct.MetaType := MetaType;
  MetaStruct.Next := FMetaStructs;
  FMetaStructs := MetaStruct;

  Inc(MetaStruct);
  if (Size = SizeOf(Pointer)) then
  begin
    PNativeUInt(MetaStruct)^ := 0;
  end else
  begin
    FillChar(MetaStruct^, Size, #0);
  end;
  Result := MetaStruct;
end;

procedure TLuaResult.Clear;
var
  MetaStruct: PMetaStruct;
  MetaType: PLuaMetaType;
  TypeInfo: PTypeInfo;
  ItemsCount: Integer;
  Heap: ^TLuaMemoryHeap;
  Rec: PDynArrayRec;
begin
  repeat
    MetaStruct := FMetaStructs;
    if (MetaStruct = nil) then Break;
    FMetaStructs := MetaStruct.Next;

    TypeInfo := nil;
    ItemsCount := 1;
    MetaType := MetaStruct.MetaType;
    case MetaType.F.Kind of
      mtRecord:
      begin
        TypeInfo := PLuaRecordInfo(MetaType).F.TypeInfo;
      end;
      mtArray:
      begin
        TypeInfo := PLuaArrayInfo(MetaType).FFinalTypeInfo;
        ItemsCount := PLuaArrayInfo(MetaType).FFinalItemsCount;
      end;
    end;

    if (TypeInfo <> nil) then
    begin
      Inc(MetaStruct);
      FinalizeArray(MetaStruct, TypeInfo, ItemsCount);
    end;
  until (False);

  if (FCount <> 1) then SetCount(1);
  FParamCount := 0;
  Heap := Pointer(@FHeap);
  if (Heap.FCurrent <> FHeapEmpty) then
  begin
    Rec := Pointer(NativeUInt(Heap.FBuffers) - SizeOf(TDynArrayRec));
    if (Rec.RefCount <> 1) or (Rec.Length <> 1) then SetLength(Heap.FBuffers, 1);
    Rec := Pointer(NativeUInt(Heap.FBuffers[0]) - SizeOf(TDynArrayRec));
    Heap.FCurrent := FHeapEmpty;
    Heap.FMargin := Rec.Length;
    FHeapCurrent := Pointer(Heap.FBuffers[0]);
    NativeUInt(FHeapOverflow) := NativeUInt(FHeapCurrent) + NativeUInt(Heap.FMargin);
  end;
end;

procedure TLuaResult.FinalizeArgs(var List: PLuaArgList; var Capacity, ACount: Integer);
var
  i, ItemsCount: Integer;
  Rec: PDynArrayRec;
begin
  Rec := Pointer(List);
  ItemsCount := Capacity;
  List := nil;
  Capacity := 0;
  Count := 0;

  if (Rec <> nil) then
  begin
    Dec(Rec);
    if (Rec.RefCount = 1) or (AtomicDecrement(Rec.RefCount) = 0) then
    begin
      Inc(Rec);

      for i := 0 to ItemsCount - 1 do
      with PLuaArgList(Rec)[i] do
      begin
        if (Pointer(FStringValue) <> nil) then
          FStringValue := '';
      end;

      Dec(Rec);
      FreeMem(Rec);
    end;
  end;
end;

procedure TLuaResult.Finalize;
begin
  Clear;
  TLuaMemoryHeap(FHeap).FBuffers := nil;
  FinalizeArgs(FItems, FCapacity, FCount);
  FinalizeArgs(FParamItems, FParamCapacity, FParamCount);
end;

{ TLuaResultBuffer }

function  TLuaResultBuffer.AllocRecord(const RecordInfo: PLuaRecordInfo): Pointer;
begin
  Result := AllocMetaType(RecordInfo);
end;

function  TLuaResultBuffer.AllocArray(const ArrayInfo: PLuaArrayInfo): Pointer;
begin
  Result := AllocMetaType(ArrayInfo);
end;

function  TLuaResultBuffer.AllocSet(const SetInfo: PLuaSetInfo): Pointer;
begin
  Result := AllocMetaType(SetInfo);
end;


{ TLuaArg }

function LuaArgTypeToString(const Value: TLuaArgType; const Prefixes: Boolean): string;
begin
  Result := GetEnumName(TypeInfo(TLuaArgType), Ord(Value));
  if (not Prefixes) and (Ord(Value) <= Ord(High(TLuaArgType))) then
    Delete(Result, 1, 2);
end;

function TLuaArg.TypeException(const AType: TLuaArgType): ELua;
begin
  Result := ELua.CreateFmt('Argument can''t be getted as %s because current type is "%s"',
    [LuaArgTypeToString(AType, True), LuaArgTypeToString(LuaType, False)]);
end;

function TLuaArg.VariantException: ELua;
begin
  Result := ELua.CreateFmt('Argument can''t be getted as Variant because current type is "%s"',
    [LuaArgTypeToString(LuaType, False)]);
end;

procedure TLuaArg.CheckDifficultSetter(const Value: TLuaDifficultType; const Name: PChar;
  const ReturnAddress: Pointer);
var
  P: Pointer;
  B: Byte;
begin
  P := Value.Data;
  if (NativeUInt(P) <= High(Word)) then
  raise ELua.CreateFmt('%s.Data field not defined ($%p)', [Name, P]) at ReturnAddress;

  P := PLuaRecord(@Value).Info;
  if (NativeUInt(P) <= High(Word)) then
  raise ELua.CreateFmt('%s.Info field not defined ($%p)', [Name, P]) at ReturnAddress;

  B := Byte(Value.FIsRef);
  if (B > 1) then
  raise ELua.CreateFmt('%s.IsRef field not defined (%d)', [Name, B]) at ReturnAddress;

  B := Byte(Value.FIsConst);
  if (B > 1) then
  raise ELua.CreateFmt('%s.IsConst field not defined (%d)', [Name, B]) at ReturnAddress;
end;

function TLuaArg.GetLuaTypeName: string;
begin
  Result := GetEnumName(TypeInfo(TLuaArgType), Ord(F.LuaType));
end;

function TLuaArg.GetEmpty: Boolean;
begin
  GetEmpty := (F.LuaType = ltEmpty);
end;

procedure TLuaArg.SetEmpty(const Value: Boolean);
begin
  if (Value) then
  begin
    FStringValue := '';
    PInteger(@F.LuaType)^ := 0;
    F.VDouble := 0;
  end;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetBoolean: Boolean;
{$else}
function TLuaArgGetBoolean(const Self: TLuaArg; const ReturnAddress: Pointer): Boolean;
{$endif}
begin
  Result := Self.F.VBoolean;
  if (Self.LuaType <> ltBoolean) then
    raise Self.TypeException(ltBoolean) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetBoolean: Boolean;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetBoolean
end;
{$endif}

procedure TLuaArg.SetBoolean(const Value: Boolean);
begin
  F.LuaType := ltBoolean;
  F.VBoolean := Value;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetInteger: Integer;
{$else}
function TLuaArgGetInteger(const Self: TLuaArg; const ReturnAddress: Pointer): Integer;
{$endif}
begin
  Result := Self.F.VInteger;
  if (Self.LuaType <> ltInteger) then
    raise Self.TypeException(ltInteger) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetInteger: Integer;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetInteger
end;
{$endif}

procedure TLuaArg.SetInteger(const Value: Integer);
begin
  F.LuaType := ltInteger;
  F.VInteger := Value;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetDouble: Double;
{$else}
function TLuaArgGetDouble(const Self: TLuaArg; const ReturnAddress: Pointer): Double;
{$endif}
begin
  case Self.LuaType of
   ltInteger: Result := Self.F.VInteger;
    ltDouble: Result := Self.F.VDouble;
  else
    raise Self.TypeException(ltDouble) at ReturnAddress;
  end;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetDouble: Double;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetDouble
end;
{$endif}

procedure TLuaArg.SetDouble(Value: Double);
begin
  if (NumberToInteger(Value, F.VInteger)) then
  begin
    F.LuaType := ltInteger;
  end else
  begin
    F.LuaType := ltDouble;
    F.VDouble := Value;
  end;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetString: LuaString;
{$else}
procedure TLuaArgGetString(const Self: TLuaArg; var Result: LuaString; const ReturnAddress: Pointer);
{$endif}
begin
  Result := Self.FStringValue;
  if (Self.LuaType <> ltString) then
    raise Self.TypeException(ltString) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetString: LuaString;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetString
end;
{$endif}

procedure TLuaArg.SetString(const Value: LuaString);
begin
  F.LuaType := ltString;
  FStringValue := Value;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetPointer: Pointer;
{$else}
function TLuaArgGetPointer(const Self: TLuaArg; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  case Self.LuaType of
      ltEmpty: Result := nil;
    ltPointer: Result := Self.F.VPointer;
  else
    raise Self.TypeException(ltPointer) at ReturnAddress;
  end;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetPointer: Pointer;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetPointer
end;
{$endif}

procedure TLuaArg.SetPointer(const Value: Pointer);
begin
  if (Value = nil) then
  begin
    F.LuaType := ltEmpty;
  end else
  begin
    F.LuaType := ltPointer;
    F.VPointer := Value;
  end;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetVariant: Variant;
{$else}
procedure TLuaArgGetVariant(const Self: TLuaArg; var Result: Variant; const ReturnAddress: Pointer);
{$endif}
begin
  if (Self.F.LuaType in [ltEmpty, ltBoolean, ltInteger, ltDouble, ltString]) then
  begin
    Result := Self.ForceVariant;
  end else
  begin
    raise Self.VariantException at ReturnAddress;
  end;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetVariant: Variant;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetVariant
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetVariant(const Value: Variant);
{$else}
procedure TLuaArgSetVariant(var Self: TLuaArg; const Value: Variant; const ReturnAddress: Pointer);
{$endif}

  procedure DateTimeValue(const Self: PLuaArg; Value: TDateTime); far;
  begin
    case InspectDateTime(Value) of
      0: Self.FStringValue := LuaString(DateTimeToStr(Value));
      1: Self.FStringValue := LuaString(DateToStr(Value));
      2: Self.FStringValue := LuaString(TimeToStr(Value));
    else
      Self.FStringValue := '';
    end;
  end;

  {$ifdef UNICODE}
  procedure UnicodeValue(const Self: PLuaArg; const Value: UnicodeString); far;
  begin
    Self.FStringValue := LuaString(Value);
  end;
  {$endif}

  {$ifNdef NEXTGEN}
  procedure AnsiValue(const Self: PLuaArg; const Value: AnsiString); far;
  begin
    Self.FStringValue := LuaString(Value);
  end;

  procedure WideValue(const Self: PLuaArg; const Value: WideString); far;
  begin
    Self.FStringValue := LuaString(Value);
  end;
  {$endif}
begin
  with TVarData(Value) do
  case (VType) of
    varEmpty, varNull: Self.F.LuaType := ltEmpty;
    varSmallint: Self.SetInteger(VSmallInt);
    varInteger : Self.SetInteger(VInteger);
    varSingle  : Self.SetDouble(VSingle);
    varDouble  : Self.SetDouble(VDouble);
    varCurrency: Self.SetDouble(VCurrency);
    varDate    : DateTimeValue(@Self, VDate);
    varBoolean : Self.SetBoolean(VBoolean);
    varShortInt: Self.SetInteger(VShortInt);
    varByte    : Self.SetInteger(VByte);
    varWord    : Self.SetInteger(VWord);
    varLongWord: Self.SetInteger(VLongWord);
    varInt64   : Self.SetDouble(VInt64);
    {$ifdef UNICODE}
    varUString : UnicodeValue(@Self, UnicodeString(VUString));
    {$endif}
    {$ifNdef NEXTGEN}
    varString  : AnsiValue(@Self, AnsiString(VString));
    varOleStr  : WideValue(@Self, WideString(VOleStr));
    {$endif}
  else
    raise ELua.CreateFmt('Unknown variant type %d in "TLuaArg.SetVariant" procedure', [VType])
      at ReturnAddress;
  end;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetVariant(const Value: Variant);
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgSetVariant
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArg.GetClass: TClass;
{$else}
function TLuaArgGetClass(const Self: TLuaArg; const ReturnAddress: Pointer): TClass;
{$endif}
begin
  case Self.LuaType of
    ltEmpty: Result := nil;
    ltClass: Result := TClass(Self.F.VPointer);
  else
    raise Self.TypeException(ltClass) at ReturnAddress;
  end;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetClass: TClass;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetClass
end;
{$endif}

procedure TLuaArg.SetClass(const Value: TClass);
begin
  if (Value = nil) then
  begin
    F.LuaType := ltEmpty;
  end else
  begin
    F.LuaType := ltClass;
    F.VPointer := Pointer(Value);
  end;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetObject: TObject;
{$else}
function TLuaArgGetObject(const Self: TLuaArg; const ReturnAddress: Pointer): TObject;
{$endif}
begin
  case Self.LuaType of
    ltEmpty: Result := nil;
    ltObject: Result := TObject(Self.F.VPointer);
  else
    raise Self.TypeException(ltObject) at ReturnAddress;
  end;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetObject: TObject;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetObject
end;
{$endif}

procedure TLuaArg.SetObject(const Value: TObject);
begin
  if (Value = nil) then
  begin
    F.LuaType := ltEmpty;
  end else
  begin
    F.LuaType := ltObject;
    F.VPointer := Pointer(Value);
  end;
end;

{$ifdef RETURNADDRESS}
function TLuaArg.GetRecord: TLuaRecord;
{$else}
procedure TLuaArgGetRecord(const Self: TLuaArg; var Result: TLuaRecord; const ReturnAddress: Pointer);
{$endif}
begin
  Result := PLuaRecord(@Self.F)^;
  if (Self.LuaType <> ltRecord) then
    raise Self.TypeException(ltRecord) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetRecord: TLuaRecord;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetRecord
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetRecord(const Value: TLuaRecord);
{$else}
procedure TLuaArgSetRecord(var Self: TLuaArg; const Value: TLuaRecord; const ReturnAddress: Pointer);
{$endif}
begin
  Self.CheckDifficultSetter(Value, 'LuaRecord', ReturnAddress);
  PLuaRecord(@Self.F)^ := Value;
  Self.F.LuaType := ltRecord;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetRecord(const Value: TLuaRecord);
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgSetRecord
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArg.GetArray: TLuaArray;
{$else}
procedure TLuaArgGetArray(const Self: TLuaArg; var Result: TLuaArray; const ReturnAddress: Pointer);
{$endif}
begin
  Result := PLuaArray(@Self.F)^;
  if (Self.LuaType <> ltArray) then
    raise Self.TypeException(ltArray) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetArray: TLuaArray;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetArray
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetArray(const Value: TLuaArray);
{$else}
procedure TLuaArgSetArray(var Self: TLuaArg; const Value: TLuaArray; const ReturnAddress: Pointer);
{$endif}
begin
  Self.CheckDifficultSetter(Value, 'LuaArray', ReturnAddress);
  PLuaArray(@Self.F)^ := Value;
  Self.F.LuaType := ltArray;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetArray(const Value: TLuaArray);
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgSetArray
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArg.GetSet: TLuaSet;
{$else}
procedure TLuaArgGetSet(const Self: TLuaArg; var Result: TLuaSet; const ReturnAddress: Pointer);
{$endif}
begin
  Result := PLuaSet(@Self.F)^;
  if (Self.LuaType <> ltSet) then
    raise Self.TypeException(ltSet) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetSet: TLuaSet;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetSet
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetSet(const Value: TLuaSet);
{$else}
procedure TLuaArgSetSet(var Self: TLuaArg; const Value: TLuaSet; const ReturnAddress: Pointer);
{$endif}
begin
  Self.CheckDifficultSetter(Value, 'LuaSet', ReturnAddress);
  PLuaSet(@Self.F)^ := Value;
  Self.F.LuaType := ltSet;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetSet(const Value: TLuaSet);
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgSetSet
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArg.GetTable: PLuaTable;
{$else}
function TLuaArgGetTable(const Self: TLuaArg; const ReturnAddress: Pointer): PLuaTable;
{$endif}
begin
  Result := Self.F.VPointer;
  if (Self.LuaType <> ltTable) then
    raise Self.TypeException(ltTable) at ReturnAddress;
end;

{$ifNdef RETURNADDRESS}
function TLuaArg.GetTable: PLuaTable;
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} .NOFRAME
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetTable
end;
{$endif}

function TLuaArg.ForceBoolean: Boolean;
begin
  case LuaType of
      ltEmpty: Result := False;
    ltBoolean: Result := F.VBoolean;
    ltInteger: Result := (F.VInteger > 0);
     ltDouble: Result := (F.VDouble > 0);
     ltString:
     begin
       Result := (Pointer(FStringValue) <> nil) and
         (CastStringAsBoolean(PLuaChar(Pointer(FStringValue)),
          PInteger(NativeInt(FStringValue) - SizeOf(Integer))^ {$ifdef LUA_LENGTH_SHIFT}shr 1{$endif}) > 0);
     end;
  else
    Result := (F.VPointer <> nil);
  end;
end;

function TLuaArg.ForceInteger: NativeInt;

  function StringDefault(const Value: LuaString): NativeInt; far;
  begin
    Result := {$ifdef LARGEINT}StrToInt64Def{$else}StrToIntDef{$endif}(string(Value), 0);
  end;

begin
  case LuaType of
      ltEmpty: Result := 0;
     ltDouble: Result := Trunc(F.VDouble);
     ltObject: Result := TObject(F.VPointer).InstanceSize;
     ltRecord: Result := PLuaRecord(@F).Info.Size;
      ltTable: Result := PLuaTable(@F).Length;
     ltString: Result := StringDefault(FStringValue);
  else
    Result := NativeInt(F.VPointer);
  end;
end;

function TLuaArg.ForceDouble: Double;

  function StringDefault(const Value: LuaString): Double; far;
  begin
    Result := StrToFloatDef(string(Value), 0);
  end;

begin
  case LuaType of
      ltEmpty: Result := 0;
    ltBoolean:
         begin
           PInteger(@F.VInfo)^ := Byte(F.VBoolean);
           Result := PInteger(@F.VInfo)^;
         end;
    ltInteger: Result := F.VInteger;
     ltDouble: Result := F.VDouble;
     ltString: Result := StringDefault(FStringValue);
  else
    Result := 0;
  end;
end;

function TLuaArg.ForceString: LuaString;
const
  BOOLEANS: array[Boolean] of LuaString = ('False', 'True');

  function IntegerValue(const Value: Integer): LuaString; far;
  begin
    Result := LuaString(IntToStr(Value));
  end;

  function DoubleValue(const Value: Double): LuaString; far;
  begin
    Result := LuaString(FloatToStr(Value));
  end;

  function PointerValue(const Value: Pointer): LuaString; far;
  begin
    Result := LuaString(IntToHex(NativeInt(Value), {$ifdef SMALLINT}8{$else .LARGEINT}16{$endif}));
  end;

  function ClassValue(const Value: TClass): LuaString; far;
  begin
    Result := LuaString(Value.ClassName);
  end;

  function ObjectValue(const Value: TObject): LuaString; far;
  begin
    {$ifNdef LUA_NOCLASSES}
    if (Value is TComponent) then Result := LuaString(TComponent(Value).Name)
    else
    {$endif}
    Result := LuaString(Value.ClassName);
  end;

begin
  case LuaType of
    ltBoolean: Result := BOOLEANS[F.VBoolean];
    ltInteger: Result := IntegerValue(F.VInteger);
     ltDouble: Result := DoubleValue(F.VDouble);
     ltString: Result := FStringValue;
    ltPointer: Result := PointerValue(F.VPointer);
      ltClass: Result := ClassValue(TClass(F.VPointer));
     ltObject: Result := ObjectValue(TObject(F.VPointer));
     ltRecord,
      ltArray: Result := PLuaRecordInfo(@F.VInfo).Name;
        ltSet: with PLuaSet(@F)^ do Result := Info.Description(Data);
      ltTable: Result := 'LuaTable';
  else
    {ltEmpty:}
    Result := 'nil';
  end;
end;

function TLuaArg.ForcePointer: pointer;
begin
  case LuaType of
    ltPointer, ltClass, ltObject, ltRecord, ltArray, ltSet: Result := F.VPointer;
    ltString: Result := Pointer(FStringValue);
     ltTable: Result := PLuaTable(@F);
  else
    Result := nil;
  end;
end;

function TLuaArg.ForceVariant: Variant;
var
  VType: Integer;
  VarData: TVarData absolute Result;
begin
  VType := VarData.VType;
  if (VType and varDeepData <> 0) then
  case VType of
    varBoolean, varUnknown+1..$15{varUInt64}: ;
  else
    System.VarClear(Result);
  end;

  case (LuaType) of
    ltBoolean: begin
                 VarData.VType := varBoolean;
                 VarData.VBoolean := F.VBoolean;
               end;
    ltInteger: begin
                 VarData.VType := varInteger;
                 VarData.VInteger := F.VInteger;
               end;
     ltDouble: begin
                 VarData.VType := varDouble;
                 VarData.VDouble := F.VDouble;
               end;
     ltString: begin
                {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
                  {$ifdef UNICODE}
                    VarData.VType := varUString;
                   {$else}
                    VarData.VType := varOleStr;
                  {$endif}
                {$else}
                  VarData.VType := varString;
                {$ifend}
                 VarData.VPointer := nil;

                 if (Pointer(FStringValue) <> nil) then
                 begin
                  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
                    {$ifdef UNICODE}
                      UnicodeString(VarData.VUString) := FStringValue;
                     {$else}
                      WideString(VarData.VOleStr) := FStringValue;
                    {$endif}
                  {$else}
                    AnsiString(VarData.VString) := FStringValue;
                  {$ifend}
                 end;
               end;
   else
     {ltEmpty и др:}
     VarData.VType := varEmpty;
  end;
end;

function TLuaArg.ForceClass: TClass;
begin
  case LuaType of
    ltClass: Result := TClass(F.VPointer);
   ltObject: Result := TClass(F.VPointer^);
  else
    Result := nil;
  end;
end;

function TLuaArg.ForceObject: TObject;
begin
  case LuaType of
    ltObject: Result := TObject(F.VPointer);
  else
    Result := nil;
  end;
end;

function TLuaArg.ForceRecord: TLuaRecord;
begin
  case LuaType of
    ltRecord: Result := PLuaRecord(@F)^;
  else
    FillChar(Result, SizeOf(Result), #0);
  end;
end;

function TLuaArg.ForceArray: TLuaArray;
begin
  case LuaType of
    ltArray: Result := PLuaArray(@F)^;
  else
    FillChar(Result, SizeOf(Result), #0);
  end;
end;

function TLuaArg.ForceSet: TLuaSet;
begin
  case LuaType of
    ltSet: Result := PLuaSet(@F)^;
  else
    FillChar(Result, SizeOf(Result), #0);
  end;
end;

function TLuaArg.ForceTable: PLuaTable;
begin
  case LuaType of
    ltTable: Result := PLuaTable(@F);
  else
    Result := nil;
  end;
end;

function LuaArgs(const Count: Integer): TLuaArgs;
var
  i: Integer;
begin
  if (Count < 0) then
    raise ELua.CreateFmt('Can''t create an array lenght of %d arguments', [Count])
    {$ifdef RETURNADDRESS} at ReturnAddress{$endif};

  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i].F.LuaType := ltEmpty;
end;

function LuaArg(const Value: Boolean): TLuaArg;
begin
  Result.AsBoolean := Value;
end;

function LuaArg(const Value: Integer): TLuaArg;
begin
  Result.AsInteger := Value;
end;

function LuaArg(const Value: Double): TLuaArg;
begin
  Result.AsDouble := Value;
end;

function LuaArg(const Value: LuaString): TLuaArg;
begin
  Result.AsString := Value;
end;

function LuaArg(const Value: Pointer): TLuaArg;
begin
  Result.AsPointer := Value;
end;

function LuaArg(const Value: TClass): TLuaArg;
begin
  Result.AsClass := Value;
end;

function LuaArg(const Value: TObject): TLuaArg;
begin
  Result.AsObject := Value;
end;

function LuaArg(const Value: TLuaRecord): TLuaArg;
begin
  Result.AsRecord := Value;
end;

function LuaArg(const Value: TLuaArray): TLuaArg;
begin
  Result.AsArray := Value;
end;

function LuaArg(const Value: TLuaSet): TLuaArg;
begin
  Result.AsSet := Value;
end;

function LuaArg(const Value: Variant): TLuaArg;
begin
  Result.AsVariant := Value;
end;

     (*
function LuaRecord(const Data: pointer; const Info: PLuaRecordInfo; const IsRef, IsConst: boolean): TLuaRecord;
begin
  Result.Data := Data;
  Result.Info := Info;
  Result.FIsRef := IsRef;
  Result.FIsConst := IsConst;
end;

function LuaArray(const Data: pointer; const Info: PLuaArrayInfo; const IsRef, IsConst: boolean): TLuaArray;
begin
  Result.Data := Data;
  Result.Info := Info;
  Result.FIsRef := IsRef;
  Result.FIsConst := IsConst;
end;

function LuaSet(const Data: pointer; const Info: PLuaSetInfo; const IsRef, IsConst: boolean): TLuaSet;
begin
  Result.Data := Data;
  Result.Info := Info;
  Result.FIsRef := IsRef;
  Result.FIsConst := IsConst;
end;     *)

function LuaProcCallback(const Address: Pointer): TLuaProcCallback;
begin
  Result := Address;
end;

function LuaMethodCallback(const Address: Pointer): TLuaMethodCallback;
begin
  Result := Address;
end;

function LuaProcParam(const Name: LuaString; const TypeInfo: PTypeInfo;
  const Flags: TParamFlags; const PointerDepth: Integer): TLuaProcParam;
begin
  Result.Name := Name;
  Result.TypeInfo := TypeInfo;
  Result.Flags := Flags;
  Result.PointerDepth := PointerDepth;
end;


                         (*
{ TLuaTableItem }

const
  PAIRS_ITERATING = high(integer)-1;
  PAIRS_BROKEN = high(integer)-0;

procedure TLuaPair.ThrowNotInitialized(const CodeAddr: pointer);
begin
  ELua.Assert('TLuaTableItem operation is not available, because an item is not initialized', [], CodeAddr);
end;

procedure TLuaPair.ThrowValueType(const CodeAddr: pointer; const pop: boolean);
begin
  if (pop) then Lua.stack_pop();
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
end;

procedure TLuaPair.ThrowBroken(const CodeAddr: pointer; const Action: string);
begin
  ELua.Assert('Can''t %s, because the Item is broken', [Action], CodeAddr);
end;

function TLuaPair.Initialize(const ALua: TLua; const AIndex: integer; const UseKey: boolean): boolean;
const
  MODES: array[boolean] of integer = (PAIRS_BROKEN, PAIRS_ITERATING);
begin
  Lua := ALua;
  Handle := ALua.Handle;
  KeyIndex := lua_gettop(Handle);
  ValueIndex := KeyIndex+1;

  Index := AIndex;
  if (AIndex < 0) then Index := KeyIndex+AIndex; // 1 прибавлять не надо, потому что он уже в стеке

  // запушить первую пару (в зависимости от запушенного ключа)
  if (not UseKey) then
  begin
    Result := (lua_next(Handle, Index) <> 0);
  end else
  begin
    lua_pushvalue(Handle, -1);
    lua_rawget(Handle, Index);
    Result := (lua_type(Handle, -1) <> LUA_TNIL);

    if (not Result) then lua_settop(Handle, -1 -2);
  end;

  // инициализация в зависимости от результата 
  FIteration := ord(Result);
  Mode := MODES[Result];
end;

function __TLuaPairGetBroken(const Self: TLuaPair; const ReturnAddr: pointer): boolean;
begin
  case (Self.Mode) of
    PAIRS_ITERATING: Result := false;
    PAIRS_BROKEN: Result := true;
  else
    Result := false;
    Self.ThrowNotInitialized(ReturnAddr);
  end;
end;

function TLuaPair.GetBroken: boolean;
asm
  mov edx, [esp]
  jmp __TLuaPairGetBroken
end;

procedure __TLuaPairGetKey(const Self: TLuaPair; var Result: string; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get key');
    PAIRS_ITERATING:
    begin
      if (lua_type(Handle, KeyIndex) = LUA_TSTRING) then
      begin
        lua_to_pascalstring(Result, Handle, KeyIndex);
      end else
      with Lua do
      begin
        stack_luaarg(FBufferArg, KeyIndex, false);
        TForceString(@TLuaArg.ForceString)(FBufferArg, Result);
      end;
    end;
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

function TLuaPair.GetKey: string;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetKey
end;


procedure __TLuaPairGetKeyEx(const Self: TLuaPair; var Result: Variant; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get key');
    PAIRS_ITERATING: Lua.stack_variant(Result, KeyIndex);
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

function TLuaPair.GetKeyEx: Variant;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetKeyEx
end;

procedure __TLuaPairGetValue(const Self: TLuaPair; var Result: Variant; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get value');
    PAIRS_ITERATING: Lua.stack_variant(Result, ValueIndex);
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

function TLuaPair.GetValue: Variant;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetValue
end;

procedure __TLuaPairSetValue(const Self: TLuaPair; const AValue: Variant; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'change value');
 PAIRS_ITERATING: begin
                    // <Key, Value>
                    lua_pushvalue(Handle, KeyIndex);
                    if (not Lua.push_variant(AValue)) then ThrowValueType(ReturnAddr, true);

                    // скопировать в KeyValue
                    lua_remove(Handle, ValueIndex);
                    lua_pushvalue(Handle, -1);
                    lua_insert(Handle, ValueIndex);

                    // занести в таблицу
                    lua_rawset(Handle, Index);
                  end;
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

procedure TLuaPair.SetValue(const AValue: Variant);
asm
  mov ecx, [esp]
  jmp __TLuaPairSetValue
end;

procedure __TLuaPairGetValueEx(const Self: TLuaPair; var Result: TLuaArg; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get value');
    PAIRS_ITERATING: Lua.stack_luaarg(Result, ValueIndex, true);
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

function TLuaPair.GetValueEx: TLuaArg;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetValueEx
end;

procedure __TLuaPairSetValueEx(const Self: TLuaPair; const AValue: TLuaArg; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'change value');
 PAIRS_ITERATING: begin
                    // <Key, Value>
                    lua_pushvalue(Handle, KeyIndex);
                    if (not Lua.push_luaarg(AValue)) then ThrowValueType(ReturnAddr, true);

                    // скопировать в KeyValue
                    lua_remove(Handle, ValueIndex);
                    lua_pushvalue(Handle, -1);
                    lua_insert(Handle, ValueIndex);

                    // занести в таблицу
                    lua_rawset(Handle, Index);
                  end;
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

procedure TLuaPair.SetValueEx(const AValue: TLuaArg);
asm
  mov ecx, [esp]
  jmp __TLuaPairSetValueEx
end;

function __TLuaPairNext(var Self: TLuaPair; const ReturnAddr: pointer): boolean;
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: Result := false;
 PAIRS_ITERATING: begin
                    lua_settop(Handle, -1 -1);
                    Result := (lua_next(Handle, Index) <> 0);
                    inc(FIteration);
                    if (not Result) then Mode := PAIRS_BROKEN;
                  end;
  else
    Result := false;
    ThrowNotInitialized(ReturnAddr);
  end;
end;

function TLuaPair.Next(): boolean;
asm
  mov edx, [esp]
  jmp __TLuaPairNext
end;

procedure __TLuaPairBreak(var Self: TLuaPair; const ReturnAddr: pointer);
begin
  with Self do
  case (Mode) of
    PAIRS_BROKEN: ;
 PAIRS_ITERATING: Mode := PAIRS_BROKEN;
  else
    ThrowNotInitialized(ReturnAddr);
  end;
end;

procedure TLuaPair.Break();
asm
  mov edx, [esp]
  jmp __TLuaPairBreak
end;
       *)

          (*
{ TLuaTable }

procedure TLuaTable.ThrowValueType(const CodeAddr: pointer; const pop: boolean);
begin
  if (pop) then Lua.stack_pop();
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
end;
               *)
// максимальный индекс целочисленного индекса
function TLuaTable.GetLength: Integer;
begin
  Result := lua_objlen(FLua.FHandle, FIndex);
end;
           (*
// количество элементов в таблице
function TLuaTable.GetCount: integer;
var
  Handle: pointer;
  Index: integer;  
begin
  Result := 0;
  Handle := Lua.Handle;
  Index := Index_;
  if (Index < 0) then Index := lua_gettop(Handle)+1+Index;

  lua_pushnil(Handle);
  while (lua_next(Handle, Index) <> 0) do
  begin
    inc(Result);
    lua_settop(Handle, -1-1);
  end;
end;

function TLuaTable.Pairs(var Pair: TLuaPair): boolean;
begin
  lua_pushnil(Lua.Handle);
  Result := Pair.Initialize(Lua, Index_, false);
end;

function __TLuaTablePairs(const Self: TLuaTable; var Pair: TLuaPair; const FromKey: Variant; const ReturnAddr: pointer): boolean;
begin
  if (not Self.Lua.push_variant(FromKey)) then Self.ThrowValueType(ReturnAddr);
  Result := Pair.Initialize(Self.Lua, Self.Index_, true);
end;

function TLuaTable.Pairs(var Pair: TLuaPair; const FromKey: Variant): boolean;
asm
  push [esp]
  jmp __TLuaTablePairs
end;

procedure __TLuaTableGetValue(const Self: TLuaTable; const AIndex: integer; var Result: Variant; const ReturnAddr: pointer);
begin
  with Self do
  begin
    lua_rawgeti(Lua.Handle, Index_, AIndex);
    if (not Lua.stack_variant(Result, -1)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
end;

function  TLuaTable.GetValue(const AIndex: integer): Variant;
asm
  push [esp]
  jmp  __TLuaTableGetValue
end;

procedure __TLuaTableSetValue(const Self: TLuaTable; const AIndex: integer; const NewValue: Variant; const ReturnAddr: pointer);
begin
  if (not Self.Lua.push_variant(NewValue)) then Self.ThrowValueType(ReturnAddr);
  lua_rawseti(Self.Lua.Handle, Self.Index_, AIndex);
end;

procedure TLuaTable.SetValue(const AIndex: integer; const NewValue: Variant);
asm
  push [esp]
  jmp __TLuaTableSetValue
end;

procedure __TLuaTableGetValueEx(const Self: TLuaTable; const AIndex: integer; var Result: TLuaArg; const ReturnAddr: pointer);
begin
  with Self do
  begin
    lua_rawgeti(Lua.Handle, Index_, AIndex);
    if (not Lua.stack_luaarg(Result, -1, false)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
end;

function  TLuaTable.GetValueEx(const AIndex: integer): TLuaArg;
asm
  push [esp]
  jmp __TLuaTableGetValueEx
end;

procedure __TLuaTableSetValueEx(const Self: TLuaTable; const AIndex: integer; const NewValue: TLuaArg; const ReturnAddr: pointer);
begin
  if (not Self.Lua.push_luaarg(NewValue)) then Self.ThrowValueType(ReturnAddr);
  lua_rawseti(Self.Lua.Handle, Self.Index_, AIndex);
end;

procedure TLuaTable.SetValueEx(const AIndex: integer; const NewValue: TLuaArg);
asm
  push [esp]
  jmp __TLuaTableSetValueEx
end;

procedure __TLuaTableGetKeyValue(const Self: TLuaTable; const Key: string; var Result: Variant; const ReturnAddr: pointer);
begin
  with Self do
  begin
    lua_push_pascalstring(Lua.Handle, Key);
    lua_rawget(Lua.Handle, Index_);
    if (not Lua.stack_variant(Result, -1)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
end;

function  TLuaTable.GetKeyValue(const Key: string): Variant;
asm
  push [esp]
  jmp __TLuaTableGetKeyValue
end;

procedure __TLuaTableSetKeyValue(const Self: TLuaTable; const Key: string; const NewValue: Variant; const ReturnAddr: pointer);
begin
  with Self do
  begin
    lua_push_pascalstring(Lua.Handle, Key);
    if (not Lua.push_variant(NewValue)) then ThrowValueType(ReturnAddr, true);
    lua_rawset(Lua.Handle, Index_);
  end;
end;

procedure TLuaTable.SetKeyValue(const Key: string; const NewValue: Variant);
asm
  push [esp]
  jmp __TLuaTableSetKeyValue
end;

procedure __TLuaTableGetKeyValueEx(const Self: TLuaTable; const Key: Variant; var Result: TLuaArg; const ReturnAddr: pointer);
begin
  with Self do
  begin
    if (not Lua.push_variant(Key)) then ThrowValueType(ReturnAddr);
    lua_rawget(Lua.Handle, Index_);
    if (not Lua.stack_luaarg(Result, -1, false)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
end;

function  TLuaTable.GetKeyValueEx(const Key: Variant): TLuaArg;
asm
  push [esp]
  jmp __TLuaTableGetKeyValueEx
end;         

procedure __TLuaTableSetKeyValueEx(const Self: TLuaTable; const Key: Variant; const NewValue: TLuaArg; const ReturnAddr: pointer);
begin
  with Self do
  begin
    if (not Lua.push_variant(Key)) then ThrowValueType(ReturnAddr);
    if (not Lua.push_luaarg(NewValue)) then ThrowValueType(ReturnAddr, true);
    lua_rawset(Lua.Handle, Index_);
  end;
end;

procedure TLuaTable.SetKeyValueEx(const Key: Variant; const NewValue: TLuaArg);
asm
  push [esp]
  jmp __TLuaTableSetKeyValueEx
end;  *)

        (*

{ TLuaReference }

destructor TLuaReference.Destroy;
var
  P, Len: integer;
begin
  if (Locked) then ThrowLocked('destroy reference', nil {здесь адрес обратного вызова сложно найти});

  if (Lua <> nil) then
  with Lua do
  begin
    // удаление из Lua
    global_free_ref(Index); //luaL_unref(Handle, LUA_REGISTRYINDEX, Index);

    // удаление из списка
    Len := Length(FReferences);
    P := IntPos(integer(Self), pinteger(FReferences), Len);
    if (P >= 0) then
    begin
      dec(Len);
      if (P <> Len) then FReferences[P] := FReferences[Len];
      SetLength(FReferences, Len);
    end;
  end;

  Lua := nil;
  Index := 0;
  inherited;
end;

// на вершине стека находится значение, которое надо зареференить
procedure TLuaReference.Initialize(const ALua: TLua);
var
  Len: integer;
begin
  Lua := ALua;

  // занять индекс, присвоить значение
  with Lua do
  if (lua_type(Handle, -1) = LUA_TNIL) then
  begin
    lua_pushboolean(Handle, true);
    global_alloc_ref(Index); //Index := luaL_ref(Handle, LUA_REGISTRYINDEX);
    lua_rawseti(Handle, LUA_REGISTRYINDEX, Index); // nil, который уже в стеке
  end else
  begin
    global_alloc_ref(Index); //Index := luaL_ref(Handle, LUA_REGISTRYINDEX);
  end;

  // добавить себя в список Lua.FReferences
  Len := Length(Lua.FReferences);
  SetLength(Lua.FReferences, Len+1);
  Lua.FReferences[Len] := Self;
end;

procedure TLuaReference.ThrowLocked(const Operation: string; const CodeAddr: pointer);
begin
  ELua.Assert('Can''t %s, because the reference is locked by table value', [Operation], CodeAddr);
end;

procedure TLuaReference.ThrowValueType(const CodeAddr: pointer);
begin
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
end;

procedure __TLuaReferenceGetValue(const Self: TLuaReference; var Result: Variant; const ReturnAddr: pointer);
begin
  if (Self.Locked) then Self.ThrowLocked('get value', ReturnAddr);

  with Self.Lua do
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Self.Index);
    stack_variant(Result, -1);
    lua_settop(Handle, -1-1);
  end;
end;

function  TLuaReference.GetValue(): Variant;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceGetValue
end;

procedure __TLuaReferenceSetValue(const Self: TLuaReference; const NewValue: Variant; const ReturnAddr: pointer);
begin
  with Self do
  begin
    if (Locked) then ThrowLocked('change value', ReturnAddr);
    if (not Lua.push_variant(NewValue)) then ThrowValueType(ReturnAddr);
    lua_rawseti(Lua.Handle, LUA_REGISTRYINDEX, Index);
  end;
end;

procedure TLuaReference.SetValue(const NewValue: Variant);
asm
  mov ecx, [esp]
  jmp __TLuaReferenceSetValue
end;

procedure __TLuaReferenceGetValueEx(const Self: TLuaReference; var Result: TLuaArg; const ReturnAddr: pointer);
begin
  if (Self.Locked) then Self.ThrowLocked('get value', ReturnAddr);
  with Self.Lua do
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Self.Index);
    stack_luaarg(Result, -1, false);
    lua_settop(Handle, -1-1);
  end;
end;

function  TLuaReference.GetValueEx(): TLuaArg;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceGetValueEx
end;

procedure __TLuaReferenceSetValueEx(const Self: TLuaReference; const NewValue: TLuaArg; const ReturnAddr: pointer);
begin
  with Self do
  begin
    if (Locked) then ThrowLocked('change value', ReturnAddr);

    // Push
    if (NewValue.LuaType = ltTable) then
    begin
      lua_pushvalue(Lua.Handle, PLuaTable(@NewValue.FLuaType).Index_);
    end else
    if (not Lua.push_luaarg(NewValue)) then ThrowValueType(ReturnAddr);

    // SetValue
    lua_rawseti(Lua.Handle, LUA_REGISTRYINDEX, Index);
  end;
end;

procedure TLuaReference.SetValueEx(const NewValue: TLuaArg);
asm
  mov ecx, [esp]
  jmp __TLuaReferenceSetValueEx
end;
           *)
             (*
function __TLuaReferenceAsTableBegin(const Self: TLuaReference; var Table: PLuaTable; const ReturnAddr: pointer): boolean;
begin
  with Self do
  begin
    if (Locked) then
    ELua.Assert('Can''t lock table value, because the reference is already locked', [], ReturnAddr);

    // посмотреть на данные, вернуть результат
    with Lua do
    begin
      lua_rawgeti(Handle, LUA_REGISTRYINDEX, Index);
      Result := (lua_type(Handle, -1) = LUA_TTABLE);
      if (Result) then
      begin
        Data.Index_ := lua_gettop(Handle);
        Table := @Data;
      end else
      begin
        Table := nil;
        lua_settop(Handle, -1 -1);
      end;
    end;

    // флаг "locked"
    FLocked := Result;
  end;
end;

function TLuaReference.AsTableBegin(var Table: PLuaTable): boolean;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceAsTableBegin
end;

function __TLuaReferenceAsTableEnd(const Self: TLuaReference; var Table: PLuaTable; const ReturnAddr: pointer): boolean;
var
  Delta: integer;
begin
  with Self do
  begin
    Result := (Table <> nil);
    if (not Result) then exit;

    if (not Locked) then
    ELua.Assert('Can''t unlock table value, because the reference is not locked', [], ReturnAddr);

    if (Table <> @Data) then
    ELua.Assert('Can''t unlock table value: incorrect parameter "Table"', [], ReturnAddr);

    // подсчёт расхождения стека
    Delta := abs(lua_gettop(Lua.Handle)-Data.Index_);
    if (Delta <> 0) then
    ELua.Assert('Can''t unlock table value: lua stack size difference = %d', [Delta], ReturnAddr);

    // финализация
    Table := nil;
    FLocked := false;
    lua_settop(Lua.Handle, -1 -1);
  end;
end;

function TLuaReference.AsTableEnd(var Table: PLuaTable): boolean;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceAsTableEnd
end;
          *)


{ TLuaMetaType }

{$ifdef SMALLINT}
function TLuaMetaType.GetPtr: __luapointer;
begin
  Result := __luapointer(@Self);
end;
{$endif}

function TLuaMetaType.GetManaged: Boolean;
begin
  case F.Kind of
    {$ifdef AUTOREFCOUNT}
    mtClass: Result := True;
    {$endif}
    mtRecord:
    begin
      Result := (Assigned(F.TypeInfo)) and (IsManagedTypeInfo(F.TypeInfo));
    end;
    mtArray:
    begin
      Result := Assigned(PLuaArrayInfo(@Self).FFinalTypeInfo);
    end;
  else
    Result := False;
  end;
end;

function TLuaMetaType.GetHFA: Boolean;
{$ifdef CPUARM64}
  function GetRecordHFA(const ATypeInfo: PTypeInfo): Boolean; far;
  var
    Context: TRTTIContext;
  begin
    Result := Context.GetType(ATypeInfo).IsHFA;
  end;
begin
  Result := (F.Kind = mtRecord) and (Assigned(F.TypeInfo)) and GetRecordHFA(F.TypeInfo);
end;
{$else}
begin
  Result := False;
end;
{$endif}

function TLuaMetaType.GetHFAElementType: TFloatType;
{$ifdef CPUARM64}
  function GetRecordHFAElementType(const ATypeInfo: PTypeInfo): TFloatType; far;
  var
    Context: TRTTIContext;
    ElementType: TRttiFloatType;
  begin
    Result := High(TFloatType);
    ElementType := Context.GetType(ATypeInfo).HFAElementType;
    if (Assigned(ElementType)) then
      Result := ElementType.FloatType;
  end;
{$endif}
begin
  Result := High(TFloatType);
  {$ifdef CPUARM64}
  if (F.Kind = mtRecord) and (Assigned(F.TypeInfo)) then
    Result := GetRecordHFAElementType(F.TypeInfo);
  {$endif}
end;

function TLuaMetaType.GetHFAElementCount: Integer;
{$ifdef CPUARM64}
  function GetRecordHFAElementCount(const ATypeInfo: PTypeInfo): Integer; far;
  var
    Context: TRTTIContext;
  begin
    Result := Context.GetType(ATypeInfo).HFAElementCount;
  end;
{$endif}
begin
  Result := 0;
  {$ifdef CPUARM64}
  if (F.Kind = mtRecord) and (Assigned(F.TypeInfo)) then
    Result := GetRecordHFAElementCount(F.TypeInfo);
  {$endif}
end;

procedure TLuaMetaType.CheckInstance(const AKind: TLuaMetaKind; const ReturnAddress: Pointer);
const
  KINDS: array[TLuaMetaKind] of string = ('class', 'record', 'array', 'set');
begin
  if (NativeUInt(@Self) <= $FFFF) or (NativeInt(@Self) and (SizeOf(Pointer) - 1) <> 0) or
    (F.Marker <> LUA_METATYPE_MARKER) or (F.Kind <> AKind) then
    raise ELua.CreateFmt('Invalid %s instance: %p', [KINDS[AKind], @Self]) at ReturnAddress;
end;

procedure TLuaMetaType.Clear(const Instance: Pointer{/TObject}; const FillZero: Boolean);
begin
  case F.Kind of
    mtClass:
    begin
      TObject(Instance).CleanupInstance;
      if (FillZero) then
      begin
        FillChar(PPointer(NativeInt(Instance) + SizeOf(Pointer))^,
          TObject(Instance).InstanceSize - SizeOf(Pointer), #0);
      end;
      Exit;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
        FinalizeArray(Instance, F.TypeInfo, 1);
    end;
    mtArray:
    begin
      if (Self.Managed) then
        FinalizeArray(Instance, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
    end;
    mtSet: ;
  end;

  if (FillZero) then
  begin
    FillChar(Instance^, F.Size, #0);
  end;
end;

procedure TLuaMetaType.Assign(const Instance, Value: Pointer{/TObject});
begin
  case F.Kind of
    mtClass:
    begin
      CopyObject(TObject(Instance), TObject(Value));
      Exit;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
      begin
        CopyArray(Instance, Value, F.TypeInfo, 1);
        Exit;
      end;
    end;
    mtArray:
    begin
      if (Self.Managed) then
      begin
        CopyArray(Instance, Value, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
        Exit;
      end;
    end;
    mtSet: ;
  end;

  System.Move(Value^, Instance^, F.Size);
end;


{ TLuaRecordInfo }

procedure TLuaRecordInfo.InternalRegField(const FieldName: LuaString; const FieldOffset: NativeInt;
  const TypeInfo: Pointer);
const
  FLAGS = 1{Ord(pmField)} + 1{Ord(pmField)} shl 3{PROP_SLOTSETTER_SHIFT};
begin
  CheckInstance(mtRecord, FLua.FReturnAddress);
  if (NativeUInt(FieldOffset) > (High(NativeUInt) shr 8)) then
    raise EInvalidFieldOffset(FieldOffset) at FLua.FReturnAddress;

  FLua.InternalAddProperty(@Self, Name, TypeInfo, False, False, FieldOffset, FieldOffset,
    FLAGS, Low(Integer));
end;

procedure TLuaRecordInfo.RegField(const FieldName: LuaString; const FieldOffset: Integer;
  const TypeInfo: Pointer);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  InternalRegField(FieldName, FieldOffset, TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  movsxd r8, r8d
  {$endif}
  jmp TLuaRecordInfo.InternalRegField
end;
{$endif}

procedure TLuaRecordInfo.RegField(const FieldName: LuaString; const FieldPointer: Pointer;
  const TypeInfo: Pointer; const RecordInstance: Pointer);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  InternalRegField(FieldName, NativeInt(FieldPointer) - NativeInt(RecordInstance), TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  sub ecx, RecordInstance
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  pop ebp
  {$else .CPUX64} .NOFRAME
  sub r8, RecordInstance
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  movsxd r8, r8d
  {$endif}
  jmp TLuaRecordInfo.InternalRegField
end;
{$endif}

const
  RECORD_METHOD_KINDS: array[Boolean{IsStatic}] of TLuaMethodKind = (mkInstance, mkStatic);

procedure __TLuaRecordInfoRegUniversalMethod(const Self: TLuaRecordInfo; const MethodName: LuaString;
  const Method: TLuaMethodCallback; const ArgsCount: Integer; const IsStatic: Boolean);
begin
  Self.FLua.InternalAddMethod(@Self, MethodName, @Method, RECORD_METHOD_KINDS[IsStatic],
    LUA_POINTER_INVALID, ArgsCount);
end;

procedure TLuaRecordInfo.RegMethod(const MethodName: LuaString; const Callback: TLuaMethodCallback;
  const ArgsCount: Integer; const IsStatic: Boolean);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  __TLuaRecordInfoRegUniversalMethod(Self, MethodName, Callback, ArgsCount, IsStatic);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRecordInfoRegUniversalMethod
end;
{$endif}

procedure __TLuaRecordInfoRegTypeInfoMethod(const Self: TLuaRecordInfo; const MethodName: LuaString;
  const Address: Pointer; const TypeInfo: PTypeInfo; const IsStatic: Boolean);
begin
  Self.FLua.InternalAddMethod(@Self, MethodName, Address, RECORD_METHOD_KINDS[IsStatic],
    Self.FLua.InternalBuildInvokable(TypeInfo, RECORD_METHOD_KINDS[IsStatic]));
end;

procedure TLuaRecordInfo.RegMethod(const MethodName: LuaString; const Address: Pointer;
  const TypeInfo: PTypeInfo; const IsStatic: Boolean);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  __TLuaRecordInfoRegTypeInfoMethod(Self, MethodName, Address, TypeInfo, IsStatic);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRecordInfoRegUniversalMethod
end;
{$endif}

procedure __TLuaRecordInfoRegCustomMethod(const Self: TLuaRecordInfo; const MethodName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const IsStatic: Boolean; const CallConv: TCallConv);
begin
  Self.FLua.InternalAddMethod(@Self, MethodName, Address, RECORD_METHOD_KINDS[IsStatic],
    Self.FLua.InternalBuildInvokable(Params, ResultType, RECORD_METHOD_KINDS[IsStatic], CallConv));
end;

procedure TLuaRecordInfo.RegMethod(const MethodName: LuaString; const Address: Pointer;
  const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const IsStatic: Boolean; const CallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  __TLuaRecordInfoRegCustomMethod(Self, MethodName, Address, Params, ResultType, IsStatic, CallConv);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRecordInfoRegCustomMethod
end;
{$endif}

procedure TLuaRecordInfo.SetOperators(const Value: TLuaOperators);
begin
  if (FOperators <> Value) then
  begin
    FOperators := Value;
    //FLua.FInitialized := false;
  end;
end;

procedure TLuaRecordInfo.SetOperatorCallback(const Value: TLuaOperatorCallback);
begin
  if (Pointer(@FOperatorCallback) <> Pointer(@Value)) then
  begin
    FOperatorCallback := Value;
    //FLua.FInitialized := false;
  end;
end;


{ TLuaSetInfo }

function TLuaSetInfo.Description(const Value: Pointer): LuaString;
var
  P1, P2, i: integer;
  Buffer: ^string;

  procedure Add();
  const
    CHARS: array[Boolean] of string = (', ', '..');
  var
    S: string;
  begin
    if (P1 < 0) then Exit;
    if (Buffer^ <> '') then Buffer^ := Buffer^ + ', ';
    S := GetEnumName(FItemTypeInfo, P1);
    Buffer^ := Buffer^ + S;
    if (P2 <> P1) then
    begin
      S := GetEnumName(FItemTypeInfo, P2);
      Buffer^ := Buffer^ + CHARS[P2 > (P1 + 1)] + S;
    end;

    P1 := -1;
    P2 := -1;
  end;
begin
  Buffer := @FLua.FStringBuffer.Default;
  Buffer^ := '';

  P1 := -1;
  P2 := -1;
  for i := FLow to FHigh do
  if (SetBitContains(Value, i - FCorrection)) then
  begin
    if (P1 < 0) then P1 := i;
    P2 := i;
  end else
  begin
    if (P1 >= 0) then Add;
  end;

  Add;
  Buffer^ := '['+ Buffer^ + ']';

  Result := LuaString(Buffer^);
end;


{ TLuaClassInfo }

function TLuaClassInfo.VmtOffset(const Proc: Pointer): NativeInt;
var
  VMT, Top: NativeUInt;
  Start, Finish, Value: NativeUInt;
begin
  if (Assigned(Proc)) then
  begin
    VMT := NativeUInt(AClass);
    Start := VMT;
    Finish := VMT;
    Inc(Start, {$ifdef FPC}vmtClassName{$else .DELPHI}(vmtSelfPtr + SizeOf(Pointer)){$endif});
    Inc(Finish, {$ifdef FPC}vmtMsgStrPtr{$else .DELPHI}vmtClassName{$endif});
    {$ifdef FPC}Inc(NativeInt(VMT), (vmtToString + SizeOf(Pointer)));{$endif}

    Top := High(NativeUInt);
    repeat
      Value := PNativeUInt(Start)^;
      Inc(Start, SizeOf(Pointer));
      if (Value >= VMT) and (Value < Top) then Top := Value;
    until (Start > Finish);

    Top := NativeUInt(NativeInt(Top) and -SizeOf(Pointer));
    Start := VMT;
    Dec(VMT, SizeOf(Pointer));
    repeat
      Inc(VMT, SizeOf(Pointer));
      if (VMT = Top) then Break;
      if (PPointer(VMT)^ = Proc) then
      begin
        Result := NativeInt(VMT) - NativeInt(Start);
        Exit;
      end;
    until (False);
  end;

  Result := -1;
end;


{ MetaType script instances (user data) }

type
  TLuaUserDataKind = (ukInstance, ukArray, ukSet, ukProperty);
  TLuaUserData = object
  public
    Instance: Pointer;

    Kind: TLuaUserDataKind;
    ArrayParams: Byte; // Count(4bits) and Filled(4bits) for difficult properties
    ConstMode: Boolean;
    GcDestroy: Boolean; // automatically call finalizer/destructor

    MetaType: __luapointer;
    (*case Integer of
//      0: (ClassIndex: integer); // integer, потому что наиболее неустойчивый user data
      1: (ArrayInfo: PLuaArrayInfo);
      2: (SetInfo: PLuaSetInfo);
      3: (PropertyInfo: PLuaPropertyInfo); *)

    procedure GetDescription(var Result: string; const Lua: TLua);
  end;
  PLuaUserData = ^TLuaUserData;


procedure TLuaUserData.GetDescription(var Result: string; const Lua: TLua);
begin
  if (@Self = nil) then
  begin
    Result := 'nil userdata';
  end else
  if (Byte(Kind) > Byte(ukSet)) then
  begin
    Result := 'unknown userdata';
  end else
  if (Instance = nil) then
  begin
    Result := 'already destroyed';
  end else
  begin
    Result := 'ToDo';

    (*
    case Kind of
      ukInstance: Result := Lua.ClassesInfo[userdata.ClassIndex]._ClassName;
         ukArray: Result := userdata.ArrayInfo.Name;
           ukSet: Result := userdata.SetInfo.Name;
      ukProperty: Result := Format('difficult property ''%s''', [userdata.PropertyInfo.PropertyName]);
    end; *)
  end;
end;


{ Internal types management, like Rtti.TValue routine }

type
  TLuaParamKind = (pkUnknown, pkBoolean, pkInteger, pkInt64, pkFloat,
                   pkObject, pkWeakObject, pkString, pkVariant, pkInterface,
                   pkPointer, pkClass, pkRecord, pkArray, pkSet,
                   pkClosure, pkUniversal);

  TLuaBoolType = (btBoolean, btByteBool, btWordBool, btLongBool);

  TLuaStringType = (stShortString, stAnsiString, stWideString, stUnicodeString,
    stAnsiChar, stWideChar, stPAnsiChar, stPWideChar);

  TLuaClosureKind = (ckStatic, ckClassStatic, ckInstance, ckClassIntance,
    ckConstructor, ckDestructor, ckMethod, ckReference {Operator?});

  TLuaInstanceKind = (ikNone, ikRecord{, ikArray, ikSet,}, ikObject, ikWeakObject,
    ikClass, ikInterface);

  TLuaClosureHeader = packed record
    Lua: Pointer;
    Marker: Byte;
    Kind: TLuaClosureKind;
    InstanceKind: TLuaInstanceKind;
    InvokableMode: Boolean;
    Name: __luaname;
  end;

  TLuaClosure = packed record
    Header: TLuaClosureHeader;
    Instance: Pointer;
    Address: Pointer;
    case Integer of
      0: (Invokable: __luapointer{PLuaInvokable});
      1: (ArgsCount: Integer);
  end;
  PLuaClosure = ^TLuaClosure;

  TLuaObjectClosure = packed record
    Header: TLuaClosureHeader;
    Instance: TObject;
  end;
  PLuaObjectClosure = ^TLuaObjectClosure;

  TLuaWeakClosure = packed record
    Header: TLuaClosureHeader;
    {$ifdef WEAKREF}[Weak]{$endif} Instance: TObject;
  end;
  PLuaWeakClosure = ^TLuaWeakClosure;

  TLuaInterfaceClosure = packed record
    Header: TLuaClosureHeader;
    Instance: IInterface;
  end;
  PLuaInterfaceClosure = ^TLuaInterfaceClosure;

  TLuaCustomParam = object
  private
    F: packed record
    case Integer of
      0: (
          TypeInfo: PTypeInfo;
          Flags: Byte;
          Kind: TLuaParamKind;
          case Integer of
            0: (OrdType: TOrdType);
            1: (FloatType: TFloatType);
            2: (BoolType: TLuaBoolType);
            3: (StringType: TLuaStringType; MaxShortLength: Byte);
            4: (VarOle: Boolean);
            5: (ClosureKind: TLuaClosureKind);
         );
      1: (MetaType: PLuaMetaType; FlagsEx: Cardinal);
      2: (LuaClosure: PLuaClosure);
    end;

    function InternalSetTypeInfo(const Value: PTypeInfo; const Lua: TLua;
      const IsAutoRegister: Boolean; const IsWeakObject: Boolean = False): Boolean;
    procedure SetTypeInfo(const Value: PTypeInfo);
    procedure SetMetaType(const Value: PLuaMetaType);
    function GetSize: Integer;
  public
    property TypeInfo: PTypeInfo read F.TypeInfo write SetTypeInfo;
    property MetaType: PLuaMetaType read F.MetaType write SetMetaType;
    property LuaClosure: PLuaClosure read F.LuaClosure write F.LuaClosure;
    property Flags: Byte read F.Flags write F.Flags;
    property FlagsEx: Cardinal read F.FlagsEx write F.FlagsEx;
    property Size: Integer read GetSize;

    property Kind: TLuaParamKind read F.Kind write F.Kind;
    property OrdType: TOrdType read F.OrdType write F.OrdType;
    property FloatType: TFloatType read F.FloatType write F.FloatType;
    property BoolType: TLuaBoolType read F.BoolType write F.BoolType;
    property StringType: TLuaStringType read F.StringType write F.StringType;
    property MaxShortLength: Byte read F.MaxShortLength write F.MaxShortLength;
    property VarOle: Boolean read F.VarOle write F.VarOle;
    property ClosureKind: TLuaClosureKind read F.ClosureKind write F.ClosureKind;

    procedure GetValue(const Src; const Lua: TLua; const AFlags: Cardinal{IsRef:1, IsConst:1});
    function SetValue(var Dest; const Lua: TLua; const StackIndex: Integer; LuaType: Integer = -1): Boolean;
  end;
  PLuaCustomParam = ^TLuaCustomParam;

function TLuaCustomParam.InternalSetTypeInfo(const Value: PTypeInfo; const Lua: TLua;
  const IsAutoRegister: Boolean; const IsWeakObject: Boolean): Boolean;
begin
  F.TypeInfo := Value;
  F.FlagsEx := 0;

  if (Value = TypeInfoTClass) then
  begin
    F.Kind := pkClass;
    {$ifdef EXTENDEDRTTI}
    F.TypeInfo := System.TypeInfo(TClass);
    {$endif}
  end else
  if (Value = TypeInfoPointer) then
  begin
    F.Kind := pkPointer;
    {$ifdef EXTENDEDRTTI}
    F.TypeInfo := System.TypeInfo(Pointer);
    {$endif}
  end else
  if (Value = TypeInfoUniversal) then
  begin
    F.Kind := pkUniversal;
  end else
  if (Value = TypeInfoPWideChar) then
  begin
    F.Kind := pkString;
    F.StringType := stPWideChar;
  end else
  if (NativeUInt(Value) >= NativeUInt(TypeInfoPAnsiChar)) and
    (NativeUInt(Value) <= NativeUInt(TypeInfoPAnsiChar) or $FFFF) then
  begin
    F.Kind := pkString;
    F.StringType := stPAnsiChar;
  end;
  if (IsBooleanTypeInfo(Value)) then
  begin
    F.Kind := pkBoolean;
    case (GetTypeData(Value).OrdType) of
      otUByte: F.BoolType := btBoolean;
      otSByte: F.BoolType := btByteBool;
      otSWord, otUWord: F.BoolType := btWordBool;
    else
      // otSLong, otULong:
      F.BoolType := btLongBool;
    end;
  end else
  case Value.Kind of
    {$ifdef FPC}tkBool:
    begin
      F.Kind := pkBoolean;
      F.BoolType := btBoolean;
    end;
    {$endif}
    tkInteger:
    begin
      F.Kind := pkInteger;
      F.OrdType := GetTypeData(Value).OrdType;
    end;
    tkEnumeration:
     begin
       F.Kind := pkInteger;
       F.OrdType := GetTypeData(Value).OrdType;
       if (Assigned(Lua)) and (IsAutoRegister) then
       begin
         Lua.RegEnum(Value);
       end;
     end;
    {$ifdef FPC}tkQWord,{$endif}
    tkInt64: F.Kind := pkInt64;
    tkFloat:
    begin
      F.Kind := pkFloat;
      F.FloatType := GetTypeData(Value).FloatType;
    end;
    tkChar:
    begin
      F.Kind := pkString;
      F.StringType := stAnsiChar;
    end;
    tkWChar:
    begin
      F.Kind := pkString;
      F.StringType := stWideChar;
    end;
    tkString:
    begin
      F.Kind := pkString;
      F.StringType := stShortString;
      F.MaxShortLength := GetTypeData(Value).MaxLength;
    end;
    {$ifdef FPC}tkAString,{$endif}
    tkLString:
    begin
      F.Kind := pkString;
      F.StringType := stAnsiString;
    end;
    {$ifdef UNICODE}
    tkUString:
    begin
      F.Kind := pkString;
      F.StringType := stUnicodeString;
    end;
    {$endif}
    tkWString:
    begin
      F.Kind := pkString;
      F.StringType := stWideString;
    end;
    tkVariant:
    begin
      F.Kind := pkVariant;
      F.VarOle := (Value = System.TypeInfo(OleVariant));
    end;
    tkInterface: F.Kind := pkInterface;
    tkClass:
    begin
      if (IsWeakObject) then
      begin
        F.Kind := pkWeakObject;
      end else
      begin
        F.Kind := pkObject;
      end;
    end;
    tkMethod:
    begin
      if (Assigned(Lua)) then
      begin     // IsAutoRegister ?
        F.Kind := pkClosure;
        F.ClosureKind := ckMethod;
        F.LuaClosure := nil{ToDo !};
      end;
    end;
    tkRecord{$ifdef FPC}, tkObject{$endif}:
    begin
      if (Assigned(Lua)) and (IsAutoRegister) then
      begin
        Lua.unpack_lua_string(Lua.FStringBuffer.Lua, PShortString(@PTypeInfo(TypeInfo).Name)^);
        Self.MetaType := Lua.InternalAddRecord(Lua.FStringBuffer.Lua, Value, True);
      end;
    end;
    tkArray:
    begin
      if (Assigned(Lua)) and (IsAutoRegister) then
      begin
        //ToDo Self.MetaType := Lua.InternalAddArray(
      end;
    end;
    tkDynArray:
    begin
      if (Assigned(Lua)) and (IsAutoRegister) then
      begin
        Self.MetaType := Lua.InternalAddArray(Value, GetTypeData(Value).elType^, []);
      end;
    end;
    tkSet:
    begin
      if (Assigned(Lua)) and (IsAutoRegister) then
      begin
        Self.MetaType := Lua.InternalAddSet(Value);
      end;
    end;
    {$ifdef EXTENDEDRTTI}
    tkClassRef:
    begin
      F.Kind := pkClass;
    end;
    tkPointer:
    begin
      F.Kind := pkPointer;
    end;
    tkProcedure:
    begin
      F.Kind := pkPointer;
      // ToDo: registration?
    end;
    {$endif}
  end;

  Result := (Self.F.Kind <> pkUnknown);
  if (not Result) and (Assigned(Lua)) then
  begin
    Lua.unpack_lua_string(Lua.FStringBuffer.Lua, PShortString(@PTypeInfo(TypeInfo).Name)^);
    GetTypeKindName(Lua.FStringBuffer.Default, PTypeInfo(TypeInfo).Kind);
  end;
end;

procedure TLuaCustomParam.SetTypeInfo(const Value: PTypeInfo);
begin
  InternalSetTypeInfo(Value, nil, False);
end;

procedure TLuaCustomParam.SetMetaType(const Value: PLuaMetaType);
begin
  F.MetaType := Value;
  F.FlagsEx := (Ord(Value.F.Kind) + Ord(pkClass)) shl 8;
end;

function TLuaCustomParam.GetSize: Integer;
begin
  Result := 0;

  case F.Kind of
    pkBoolean:
    begin
      case F.BoolType of
        btBoolean,
        btByteBool: Result := SizeOf(ByteBool);
        btWordBool: Result := SizeOf(WordBool);
        btLongBool: Result := SizeOf(LongBool);
      end;
    end;
    pkInteger:
    begin
      case (F.OrdType) of
        otSByte,
        otUByte: Result := SizeOf(Byte);
        otSWord,
        otUWord: Result := SizeOf(Word);
        otSLong,
        otULong: Result := SizeOf(Cardinal);
      end;
    end;
    pkInt64:
    begin
      Result := SizeOf(Int64);
    end;
    pkFloat:
    begin
      case (F.FloatType) of
        ftSingle: Result := SizeOf(Single);
        ftDouble: Result := SizeOf(Double);
      ftExtended: Result := SizeOf(Extended);
          ftComp: Result := SizeOf(Comp);
          ftCurr: Result := SizeOf(Currency);
      end;
    end;
    pkObject, pkWeakObject, pkPointer, pkClass, pkInterface:
    begin
      Result := SizeOf(Pointer);
    end;
    pkVariant:
    begin
      Result := SizeOf(Variant);
    end;
    pkString:
    begin
      case (F.StringType) of
       stShortString: Result := SizeOf(Byte) + F.MaxShortLength;
        stAnsiString,
        stWideString,
     stUnicodeString,
         stPAnsiChar,
         stPWideChar: Result := SizeOf(Pointer);
          stAnsiChar: Result := SizeOf(Byte);
          stWideChar: Result := SizeOf(WideChar);
      end;
    end;
    pkClosure:
    begin
      case F.ClosureKind of
        ckMethod: Result := SizeOf(TMethod);
      else
        Result := SizeOf(Pointer);
      end;
    end;
    pkRecord, pkArray, pkSet:
    begin
      Result := F.MetaType.Size;
    end;
  end;
end;

procedure TLuaCustomParam.GetValue(const Src; const Lua: TLua; const AFlags: Cardinal{IsRef:1, IsConst:1});
label
  int64_value;
var
  Value: Pointer;
  UserData: PLuaUserData;
begin
  Value := @Src;

  with Lua do
  case F.Kind of
    pkBoolean:
    begin
      if (F.BoolType <> btBoolean) then
      begin
        case F.BoolType of
          btByteBool: FTempBuffer.B := ByteBool(Value^);
          btWordBool: FTempBuffer.B := WordBool(Value^);
        else
          // btLongBool
          FTempBuffer.B := LongBool(Value^);
        end;
        Value := @FTempBuffer.B;
      end;

      lua_pushboolean(Handle, PBoolean(Value)^);
    end;
    pkInteger:
    begin
      if (F.OrdType <> otSLong) then
      begin
        case F.OrdType of
          otSByte: FTempBuffer.I := PShortInt(Value)^;
          otUByte: FTempBuffer.I := PByte(Value)^;
          otSWord: FTempBuffer.I := PSmallInt(Value)^;
          otUWord: FTempBuffer.I := PWord(Value)^;
        else
          // otULong
          {$ifdef LARGEINT}
            lua_pushinteger(Handle, PCardinal(Value)^);
          {$else .SMALLINT}
            if (PInteger(Value)^ < 0) then
            begin
              lua_pushnumber(Handle, PCardinal(Value)^);
              Exit;
            end else
            begin
              FTempBuffer.I := PInteger(Value)^;
            end;
          {$endif}
        end;
        Value := @FTempBuffer.I;
      end;

      lua_pushinteger(Handle, PInteger(Value)^);
    end;
    pkInt64:
    begin
    int64_value:
      {$ifdef LARGEINT}
        lua_pushinteger(Handle, PInt64(Value)^);
      {$else .SMALLINT}
        if (-(TPoint(Value^).X shr 31) = TPoint(Value^).Y) then
        begin
          lua_pushinteger(Handle, PInteger(Value)^);
        end else
        begin
          lua_pushnumber(Handle, PInt64(Value)^);
        end;
      {$endif}
    end;
    pkFloat:
    begin
      if (F.FloatType <> ftDouble) then
      begin
        case F.FloatType of
           ftSingle: FTempBuffer.D := PSingle(Value)^;
         ftExtended: FTempBuffer.D := PExtended(Value)^;
             ftComp: goto int64_value;
        else
          // ftCurr
          FTempBuffer.D := PCurrency(Value)^;
        end;
        Value := @FTempBuffer.D;
      end;

      lua_pushnumber(Handle, PDouble(Value)^);
    end;
    pkString:
    begin
      case F.StringType of
        {$ifNdef NEXTGEN}
        stShortString:
        begin
          push_short_string(PShortString(Value)^);
        end;
        stAnsiString:
        begin
          push_ansi_string(PAnsiString(Value)^);
        end;
        stWideString:
        begin
          push_wide_string(PWideString(Value)^);
        end;
        {$endif}
        stUnicodeString:
        begin
          push_unicode_string(PUnicodeString(Value)^);
        end;
        stAnsiChar:
        begin
          push_ansi_chars(Value, 0, 1);
        end;
        stWideChar:
        begin
          push_wide_chars(Value, 1);
        end;
        stPAnsiChar:
        begin
          Value := PPointer(Value)^;
          push_ansi_chars(Value, 0, LStrLen(Value));
        end;
        stPWideChar:
        begin
          Value := PPointer(Value)^;
          push_wide_chars(Value, WStrLen(Value));
        end;
      else
        Lua.InternalErrorFmt('Unknown string kind: %d', [Ord(F.StringType)]);
      end;
    end;
    pkVariant:
    begin
      if (not push_variant(PVariant(Value)^)) then lua_pushnil(Handle);
    end;
    pkInterface,
    pkPointer,
    pkClass,
    pkWeakObject,
    pkObject:
    begin
      Value := PPointer(Value);

      if (Value = nil) then lua_pushnil(Handle)
      else
      if (F.Kind in [pkInterface, pkPointer]) then lua_pushlightuserdata(Handle, Value)
      else
      if (F.Kind = pkClass) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, InternalNearestClass(TClass(Value)).Ref)
      else
      // pkWeakObject/pkObject
      if (TClass(Value^) = TLuaReference) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaReference(Value).Index)
      else
      push_userdata(InternalNearestClass(TClass(Value^)), Value, False);
    end;
  else
    // pkRecord, pkArray, pkSet, pkUniversal
    UserData := push_userdata(F.MetaType, Value, AFlags and 1 = 0);
    UserData.ConstMode := (AFlags and 2 <> 0);
  end;
end;

function TLuaCustomParam.SetValue(var Dest; const Lua: TLua; const StackIndex: Integer;
  LuaType: Integer): Boolean;

  function CastAsNumber(const Lua: TLua; const StackIndex: Integer; const ModeInt64: Boolean): Boolean; far;
  var
    LuaType: Integer;
    Value, i: NativeInt;
    Default: ^string;
    Separator: Char;
  begin
    Result := True;
    Lua.FTempBuffer.I64 := 0;

    LuaType := lua_type(Lua.Handle, StackIndex);
    case LuaType of
      LUA_TNIL: {already 0};
      LUA_TBOOLEAN:
      begin
        if (lua_toboolean(Lua.Handle, StackIndex)) then
        begin
          Lua.FTempBuffer.B := True;
          if (not ModeInt64) then
            Lua.FTempBuffer.D := 1;
        end;
      end;
      LUA_TLIGHTUSERDATA, LUA_TUSERDATA:
      begin
        Result := ModeInt64;
        if (Result) then
        begin
          Value := NativeInt(lua_touserdata(Lua.Handle, StackIndex));
          if (LuaType = LUA_TUSERDATA) and (Value <> 0) then
          begin
            Value := NativeInt(PLuaUserData(Value).Instance);
          end;
          Lua.FTempBuffer.N := Value;
        end;
      end;
      LUA_TSTRING:
      begin
        Default := @Lua.FStringBuffer.Default;
        {$ifdef UNICODE}
          Lua.stack_unicode_string(Default^, StackIndex);
        {$else .ANSI}
          Lua.stack_ansi_string(Default^, StackIndex, 0);
        {$endif}

        if (ModeInt64) then
        begin
          Result := TryStrToInt64(Default^, Lua.FTempBuffer.I64);
        end else
        begin
          {$if Defined(FPC) or (CompilerVersion <= 21)}
            Separator := DecimalSeparator;
          {$else}
            Separator := FormatSettings.DecimalSeparator;
          {$ifend}

          for i := 1 to Length(Default^) do
          case Default^[i] of
            '.', ',':
            begin
              if (Default^[i] <> Separator) then
                Default^[i] := Separator;

              Break;
            end;
          end;

          Result := TryStrToFloat(Default^, Lua.FTempBuffer.D);
        end;
      end;
    else
      Result := False;
    end;
  end;

  procedure CastAsString(const Lua: TLua; const StackIndex: Integer; const Instance: Pointer; const Self: TLuaCustomParam); far;
  begin
    with Lua do
    case Self.StringType of
      {$ifNdef NEXTGEN}
      stShortString:
      begin
        stack_short_string(PShortString(Instance)^, StackIndex, Self.MaxShortLength);
      end;
      stAnsiString:
      begin
        stack_ansi_string(PAnsiString(Instance)^, StackIndex,
          {$ifdef INTERNALCODEPAGE}GetTypeData(Self.TypeInfo).CodePage{$else}0{$endif});
      end;
      stWideString:
      begin
        stack_wide_string(PWideString(Instance)^, StackIndex);
      end;
      stAnsiChar:
      begin
        stack_ansi_string(FStringBuffer.Ansi, StackIndex, 0);
        PByte(Instance)^ := 0;
        if (Pointer(FStringBuffer.Ansi) <> nil) then PByte(Instance)^ := PByte(FStringBuffer.Ansi)^;
      end;
      stPAnsiChar:
      begin
        stack_ansi_string(FStringBuffer.Ansi, StackIndex, Word(NativeUInt(Self.F.TypeInfo)));
        PPointer(Instance)^ := @NULL_CHAR;
        if (Pointer(FStringBuffer.Ansi) <> nil) then PPointer(Instance)^ := Pointer(FStringBuffer.Ansi);
      end;
      {$endif}
      stUnicodeString:
      begin
        stack_unicode_string(PUnicodeString(Instance)^, StackIndex);
      end;
      stWideChar:
      begin
        stack_unicode_string(FStringBuffer.Unicode, StackIndex);
        PWord(Instance)^ := 0;
        if (Pointer(FStringBuffer.Unicode) <> nil) then PWord(Instance)^ := PWord(FStringBuffer.Unicode)^;
      end;
      stPWideChar:
      begin
        stack_unicode_string(FStringBuffer.Unicode, StackIndex);
        PPointer(Instance)^ := @NULL_CHAR;
        if (Pointer(FStringBuffer.Unicode) <> nil) then PPointer(Instance)^ := Pointer(FStringBuffer.Unicode);
      end;
    else
      Lua.InternalErrorFmt('Unknown string kind: %d', [Ord(Self.StringType)]);
    end;
  end;

  function CastAsTObject(const Lua: TLua; const StackIndex: Integer): Pointer; far;
  var
    UserData: PLuaUserData;
    MetaType: PLuaMetaType;
  begin
    UserData := lua_touserdata(Lua.Handle, StackIndex);

    if (Assigned(UserData)) and (UserData.Kind = ukInstance) then
    begin
      MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(Lua.FMemoryHeap).Unpack{$endif}(UserData.MetaType);
      if (MetaType.F.Kind = mtClass) then
      begin
        Result := UserData.Instance;
        Exit;
      end;
    end;

    Result := nil;
  end;

  function AssignMetaType(const Lua: TLua; const StackIndex: Integer; const Instance: Pointer;
    const Self: TLuaCustomParam): Boolean; far;
  label
    failure;
  var
    UserData: PLuaUserData;
    MetaType: PLuaMetaType;
  begin
    MetaType := Self.MetaType;

    case lua_type(Lua.Handle, StackIndex) of
      LUA_TNIL:
      begin
        MetaType.Clear(Instance, True);
      end;
      LUA_TUSERDATA:
      begin
        UserData := lua_touserdata(Lua.Handle, StackIndex);
        if (UserData = nil) or (UserData.Instance = nil) or
          (UserData.MetaType <> {$ifdef SMALLINT}__luapointer(MetaType){$else}MetaType.Ptr{$endif}) then
          goto failure;

        MetaType.Assign(Instance, UserData.Instance);
      end;
    else
    failure:
      Result := False;
      Exit;
    end;

    Result := True;
  end;

label
  set_integer, set_int64, set_float, unpack_int_int64, failure, set_native, done;
var
  Instance: Pointer;
  Value: NativeInt;
begin
  Instance := @Dest;
  if (LuaType = -1) then
    LuaType := lua_type(Lua.Handle, StackIndex);

  with Lua do
  case F.Kind of
    pkBoolean:
    begin
      case (LuaType) of
            LUA_TNIL: Value := 0;
        LUA_TBOOLEAN: Value := NativeInt(lua_toboolean(Handle, StackIndex)) and 1;
         LUA_TSTRING: begin
                        Value := CastStringAsBoolean(PAnsiChar(lua_tolstring(Handle, StackIndex, Pointer(@FTempBuffer.N))), FTempBuffer.N);
                        if (Value < 0) then goto failure;
                      end;
      else
        goto failure;
      end;
      if (F.BoolType <> btBoolean) then Value := -Value;

      case F.BoolType of
        btWordBool: PWord(Instance)^ := Value;
        btLongBool: PCardinal(Instance)^ := Value;
      else
        // btBoolean, btByteBool
        PByte(Instance)^ := Value;
      end;

      goto done;
    end;
    pkInteger:
    begin
      if (LuaType = LUA_TNUMBER) then
      begin
        {$ifNdef LARGEINT}
        if (F.OrdType <> otULong) then
        {$endif}
        begin
          Value := lua_tointeger(Handle, StackIndex);

        set_integer:
          case Cardinal(F.OrdType) shr 1 of
            0: PByte(Instance)^ := Value;
            1: PWord(Instance)^ := Value;
          else
            PCardinal(Instance)^ := Value;
          end;

          goto done;
        end;
      end;

      goto unpack_int_int64;
    end;
    pkInt64:
    begin
    unpack_int_int64:
      if (LuaType = LUA_TNUMBER) then
      begin
        {$ifdef LARGEINT}
          FTempBuffer.I64 := lua_tointeger(Handle, StackIndex);
        {$else .SMALLINT}
          FTempBuffer.I64 := lua_toint64(Handle, StackIndex);
        {$endif}

      set_int64:
        if (F.Kind = pkInt64) then
        begin
          PInt64(Instance)^ := FTempBuffer.I64;
          goto done;
        end else
        begin
          Value := PCardinal(@FTempBuffer.I64)^;
          goto set_integer;
        end;
      end else
      begin
        if (CastAsNumber(Lua, StackIndex, True)) then goto set_int64;
        goto failure;
      end;
    end;
    pkFloat:
    begin
      if (F.FloatType = ftComp) then goto unpack_int_int64;
      if (LuaType = LUA_TNUMBER) then
      begin
        FTempBuffer.D := lua_tonumber(Handle, StackIndex);
      set_float:
        case F.FloatType of
          ftSingle: PSingle(Instance)^ := FTempBuffer.D;
          ftDouble: PDouble(Instance)^ := FTempBuffer.D;
        ftExtended: PExtended(Instance)^ := FTempBuffer.D;
        else
          // ftCurr
          PCurrency(Instance)^ := FTempBuffer.D;
        end;

        goto done;
      end else
      begin
        if (CastAsNumber(Lua, StackIndex, True)) then goto set_float;
        goto failure;
      end;
    end;
    pkString:
    begin
      CastAsString(Lua, StackIndex, Instance, Self);
      goto done;
    end;
    pkVariant:
    begin
      if (not stack_variant(PVariant(Instance)^, StackIndex, F.VarOle)) then goto failure;
      goto done;
    end;
    pkInterface{ToDo}:
    begin
      case LuaType of
        LUA_TNIL: Value := 0;
        LUA_TLIGHTUSERDATA: Value := NativeInt(lua_touserdata(Handle, StackIndex));
      else
        goto failure;
      end;

      FTempBuffer.N := Value;
      IInterface(PPointer(Instance)^) := IInterface(FTempBuffer.N);
      goto done;
    end;
    pkPointer:
    begin
      case (LuaType) of
        LUA_TNIL: Value := 0;
        LUA_TSTRING:
        begin
          {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
            stack_lua_string(FStringBuffer.Lua, StackIndex);
            Value := NativeInt(FStringBuffer.Lua);
          {$else}
            Value := NativeInt(lua_tolstring(Handle, StackIndex, nil));
          {$ifend}
        end;
        LUA_TLIGHTUSERDATA: Value := NativeInt(lua_touserdata(Handle, StackIndex));
        LUA_TUSERDATA:
        begin
          Value := NativeInt(lua_touserdata(Handle, StackIndex));
          if (Value <> 0) then Value := NativeInt(PLuaUserData(Value).Instance);
        end;
        LUA_TFUNCTION:
        begin
          Value := NativeInt(function_topointer(StackIndex));
        end;
        LUA_TTABLE:
        begin
          Value := NativeInt(InternalTableToMetaType(StackIndex));
          if (Value = 0) then goto failure;
          if (PLuaMetaType(Value).F.Kind = mtClass) then
            Value := NativeInt(PLuaMetaType(Value).F.AClass);
        end;
      else
        goto failure;
      end;

      goto set_native;
    end;
    pkWeakObject, {ToDo}
    pkObject:
    begin
      case (LuaType) of
        LUA_TNIL: Value := 0;
        LUA_TUSERDATA:
        begin
          Value := NativeInt(CastAsTObject(Lua, StackIndex));
          if (Value = 0) then goto failure;
        end;
      else
        goto failure;
      end;

      // ToDo?
      goto set_native; 
    end;
    pkClass:
    begin
      case (LuaType) of
        LUA_TNIL: Value := 0;
        LUA_TTABLE:
        begin
          Value := NativeInt(InternalTableToMetaType(StackIndex));
          if (Value = 0) then goto failure;
          if (PLuaMetaType(Value).F.Kind <> mtClass) then goto failure;
          Value := NativeInt(PLuaMetaType(Value).F.AClass);
        end;
        LUA_TUSERDATA:
        begin
          Value := NativeInt(CastAsTObject(Lua, StackIndex));
          if (Value = 0) then goto failure;
          Value := PNativeInt(Value)^;
        end;
      else
        goto failure;
      end;

      goto set_native;
    end;
  else
    // pkRecord, pkArray, pkSet, pkUniversal
    if (AssignMetaType(Lua, StackIndex, Instance, Self)) then goto done;
  failure:
    stack_force_unicode_string(FStringBuffer.Unicode, StackIndex);
    GetLuaTypeName(FStringBuffer.Default, lua_type(Handle, StackIndex));
    Result := False;
    Exit;
  end;

set_native:
  PNativeInt(Instance)^ := Value;

done:
  Result := True;
end;

const
  PARAM_POINTERDEPTH_MASK = (1 shl 3 - 1);
  PARAM_MAX_POINTERDEPTH = PARAM_POINTERDEPTH_MASK - 1;
  PARAM_REFERENCE_MODE = 1 shl 3;
  PARAM_ARRAY_MODE = 1 shl 4;
  PARAM_ADDRESS_MODE = 1 shl 5;
  PARAM_INSURANCE_MODE = 1 shl 6;
  PARAM_ARMPARTIAL_MODE = 1 shl 7;

type
  TLuaInvokableParam = object(TLuaCustomParam)
  private
    {
      Flags:
      RefCount: Byte:3;
      IsReference: Boolean:1;
      IsArray: Boolean:1;
      IsAddress: Boolean:1;
      IsInsurance: Boolean:1;
      IsARMPartial: Byte:1;
    }
    function GetIsReference: Boolean;
    procedure SetIsReference(const AValue: Boolean);
    function GetIsArray: Boolean;
    procedure SetIsArray(const AValue: Boolean);
    function GetIsAddress: Boolean;
    procedure SetIsAddress(const AValue: Boolean);
    function GetIsInsurance: Boolean;
    procedure SetIsInsurance(const AValue: Boolean);
    function GetIsARMPartial: Boolean;
    procedure SetIsARMPartial(const AValue: Boolean);
    function GetPointerDepth: Byte;
    procedure SetPointerDepth(const AValue: Byte);
  public
    Name: PShortString;
    Value: Integer;
    DataValue: Integer;
    InsuranceValue: Integer;

    property IsReference: Boolean read GetIsReference write SetIsReference;
    property IsArray: Boolean read GetIsArray write SetIsArray;
    property IsAddress: Boolean read GetIsAddress write SetIsAddress;
    property IsInsurance: Boolean read GetIsInsurance write SetIsInsurance;
    property IsARMPartial: Boolean read GetIsARMPartial write SetIsARMPartial;
    property PointerDepth: Byte read GetPointerDepth write SetPointerDepth;
  end;
  PLuaInvokableParam = ^TLuaInvokableParam;
  TLuaInvokableParams = array[0..0] of TLuaInvokableParam;
  PLuaInvokableParams = ^TLuaInvokableParams;

function TLuaInvokableParam.GetIsReference: Boolean;
begin
  Result := (F.FlagsEx and PARAM_REFERENCE_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsReference(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.FlagsEx := F.FlagsEx or PARAM_REFERENCE_MODE;
  end else
  begin
    F.FlagsEx := F.FlagsEx and (not PARAM_REFERENCE_MODE);
  end;
end;

function TLuaInvokableParam.GetIsArray: Boolean;
begin
  Result := (F.FlagsEx and PARAM_ARRAY_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsArray(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.FlagsEx := F.FlagsEx or PARAM_ARRAY_MODE;
  end else
  begin
    F.FlagsEx := F.FlagsEx and (not PARAM_ARRAY_MODE);
  end;
end;

function TLuaInvokableParam.GetIsAddress: Boolean;
begin
  Result := (F.FlagsEx and PARAM_ADDRESS_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsAddress(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.FlagsEx := F.FlagsEx or PARAM_ADDRESS_MODE;
  end else
  begin
    F.FlagsEx := F.FlagsEx and (not PARAM_ADDRESS_MODE);
  end;
end;

function TLuaInvokableParam.GetIsInsurance: Boolean;
begin
  Result := (F.FlagsEx and PARAM_INSURANCE_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsInsurance(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.FlagsEx := F.FlagsEx or PARAM_INSURANCE_MODE;
  end else
  begin
    F.FlagsEx := F.FlagsEx and (not PARAM_INSURANCE_MODE);
  end;
end;

function TLuaInvokableParam.GetIsARMPartial: Boolean;
begin
  Result := (F.FlagsEx and PARAM_ARMPARTIAL_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsARMPartial(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.FlagsEx := F.FlagsEx or PARAM_ARMPARTIAL_MODE;
  end else
  begin
    F.FlagsEx := F.FlagsEx and (not PARAM_ARMPARTIAL_MODE);
  end;
end;

function TLuaInvokableParam.GetPointerDepth: Byte;
begin
  Result := F.FlagsEx and PARAM_POINTERDEPTH_MASK;
end;

procedure TLuaInvokableParam.SetPointerDepth(const AValue: Byte);
begin
  if (AValue >= PARAM_MAX_POINTERDEPTH) then
  begin
    F.FlagsEx := (F.FlagsEx and (not PARAM_POINTERDEPTH_MASK)) + PARAM_MAX_POINTERDEPTH;
  end else
  begin
    F.FlagsEx := (F.FlagsEx and (not PARAM_POINTERDEPTH_MASK)) + AValue;
  end;
end;


type
  {$if Defined(IOS) and Defined(CPUARM32) and (CompilerVersion < 28)}
    {$define ARM_NO_VFP_USE}
  {$ifend}

  {$if Defined(CPUX86)}
  TParamRegsGen = array[0..2] of Integer;
  TParamRegsExt = packed record end;
  TParamBlock = record
  case Integer of
    0:
    (
      RegEAX: Integer;
      RegEDX: Integer;
      RegECX: Integer;
      case Integer of
        0: (OutEAX, OutEDX: Integer);
        1: (OutInt64: Int64);
        2: (OutFPU: Extended; OutFPUAlign: Word);
        3: (OutSafeCall: HRESULT);
    );
    1:
    (
      RegsGen: array[0..2] of Integer;
      RegsExt: packed record end;
    );
  end;
  {$elseif Defined(CPUX64) and Defined(MSWINDOWS)}
  TParamRegsGen = array[0..3] of Int64;
  TParamRegsExt = array[0..3] of Double;
  TParamBlock = record
  case Integer of
    0:
    (
      RegRCX: Int64;
      RegRDX: Int64;
      RegR8: Int64;
      RegR9: Int64;
      OutXMM0: Double;
      case Integer of
        0: (OutEAX: Integer);
        1: (OutRAX: Int64);
        2: (OutSafeCall: HRESULT);
    );
    1: (RegsGen: array[0..3] of Int64);
    2: (RegsExt: array[0..3] of Double);
  end;
  {$elseif Defined(CPUX64) and (not Defined(MSWINDOWS))}
  TParamRegsGen = array[0..5] of Int64;
  TParamRegsExt = array[0..7] of Double;
  TParamBlock = record
  case Integer of
    0:
    (
      RegXMM: array [0..7] of Double; {XMM0-XMM7}
      RegR: array [0..5] of Int64;    {RDI, RSI, RDX, RCX, R8, R9}
      OutRAX: Int64;
      OutRDX: Int64;
      OutFPU: Extended;
      StackData: PByte;
      StackDataSize: Integer;
      OutXMM0: Double;
    );
    1:
    (
      RegsExt: array[0..7] of Double;
      RegsGen: array[0..5] of Int64;
      OutSafeCall: HRESULT;
    );
  end;
  {$elseif Defined(CPUARM32)}
  TParamRegsGen = array[0..3] of Integer;
  TParamRegsExt = {$ifdef ARM_NO_VFP_USE}packed record end{$else}array[0..15] of Single{$endif};
  TParamBlock = record
  case Integer of
    0:
    (
      RegCR: array[0..3] of Integer;
      StackData: PByte;
      StackDataSize: Integer;
      case Integer of
        0: (RegD: array[0..7] of Double);
        1: (RegS: array[0..15] of Single);
        2: (RegsExt: array[0..15] of Single;
            case Integer of
              0: (OutCR: Integer; OutD: Double);
              1: (OutSafeCall: HRESULT);
           );
    );
    1: (RegsGen: array[0..3] of Integer);
  end;
  {$elseif Defined(CPUARM64)}
  m128 = record
  case Integer of
    1: (Lo, Hi: UInt64);
    2: (LL, LH, HL, HH: UInt32);
  end align 16;
  m128x4 = array[0..3] of m128;
  TParamRegsHFA = packed record
  case Integer of
    0: (Singles: array[0..3] of UInt32);
    1: (Doubles: array[0..3] of UInt64);
  end;
  TParamRegsGen = array[0..7] of Int64;
  TParamRegsExt = array[0..7] of m128;
  TParamBlock = record
  case Integer of
    0:
    (
      RegX: array[0..7] of Int64;
      RegQ: array[0..7] of m128;
      RegX8: Int64;
      StackData: PByte;
      StackDataSize: Integer;
      OutHFA: TParamRegsHFA;
      case Integer of
        0: (OutX: Int64; OutQ: m128);
        1: (OutSafeCall: HRESULT);
    );
    1:
    (
      RegsGen: array[0..7] of Int64;
      case Integer of
        0: (RegsExt: array[0..7] of m128);
        1: (Regs128x4: m128x4);
    );
  end;
  {$else}
    {$MESSAGE ERROR 'Unknown compiler'}
  {$ifend}
  PParamBlock = ^TParamBlock;

const
  REGGEN_SIZE = SizeOf(Pointer);
  {$if Defined(CPUARM32)}
  REGEXT_SIZE = SizeOf(Single);
  {$elseif Defined(CPUARM32)}
  REGEXT_SIZE = SizeOf(m128);
  {$else}
  REGEXT_SIZE = SizeOf(Double);
  {$ifend}
  REGGEN_COUNT = SizeOf(TParamRegsGen) div REGGEN_SIZE;
  REGEXT_COUNT = SizeOf(TParamRegsExt) div REGEXT_SIZE;

  VALUE_NOT_DEFINED = -1;

type
  TLuaResultKind = (rkNone, rkRegister, rkHFASingle, rkHFADouble, rkBuffer, rkManagedBuffer, rkUserData);
  TLuaStackCleaner = (scCaller, scInvokable, scInvokableEx);

  PLuaInvokable = ^TLuaInvokable;
  TLuaInvokable = object
  public
    MethodKind: TLuaMethodKind;
    CallConv: TCallConv;
    ResultKind: TLuaResultKind;
    StackCleaner: TLuaStackCleaner;
    StackDataSize: Integer;
    TotalDataSize: Integer;

    InitialCount: Integer;
    Initials: PInteger;
    FinalCount: Integer;
    Finals: PInteger;
    {$ifdef CPUARM}
    ARMPartialSize: Integer;
    {$endif}
    Instance: Integer;
    ConstructorFlag: Integer;
    Result: TLuaInvokableParam;
    ParamCount: Integer;
    Params: TLuaInvokableParams;

    procedure Initialize(const ParamBlock: PParamBlock);
    procedure Finalize(const ParamBlock: PParamBlock);
    {$ifdef CPUARM}
    procedure FillARMPartialData(const Param: PLuaInvokableParam; const Value: Pointer; const ParamBlock: PParamBlock);
    {$endif}
    procedure Invoke(const CodeAddress: Pointer; const ParamBlock: PParamBlock);
  end;

{$if (Defined(CPUX64) and (not Defined(MSWINDOWS))) or Defined(CPUARM)}
const
  RTLHelperLibName =
  {$if Defined(PIC) and Defined(LINUX)}
    'librtlhelper_PIC.a';
  {$else}
    'librtlhelper.a';
  {$ifend}

procedure RawInvoke(CodeAddress: Pointer; ParamBlock: PParamBlock);
  external RTLHelperLibName name 'rtti_raw_invoke';

function CheckAutoResult(ResultCode: HResult): HResult;
begin
  if ResultCode < 0 then
  begin
    if Assigned(SafeCallErrorProc) then
      SafeCallErrorProc(ResultCode, ReturnAddress);
    System.Error(reSafeCallError);
  end;
  Result := ResultCode;
end;
{$ifend}

procedure TLuaInvokable.Initialize(const ParamBlock: PParamBlock);
var
  i, PointerDepth, FlagsEx: Integer;
  InitialPtr: PInteger;
  Param: PLuaInvokableParam;
  Ptr: Pointer;
begin
  InitialPtr := Initials;

  for i := 1 to InitialCount do
  begin
    Param := Pointer(NativeInt(@Self) + InitialPtr^);
    Ptr := Pointer(NativeInt(ParamBlock) + Param.DataValue);

    PNativeInt(Ptr)^ := 0;
    FlagsEx := Param.F.FlagsEx;
    if (FlagsEx and (PARAM_INSURANCE_MODE or PARAM_POINTERDEPTH_MASK) <> 0) then
    begin
      // insurance
      if (FlagsEx and PARAM_INSURANCE_MODE <> 0) then
      begin
        Dec(NativeUInt(Ptr), SizeOf(Pointer));
        PNativeInt(Ptr)^ := 0;
      end;

      // references
      PointerDepth := FlagsEx and PARAM_POINTERDEPTH_MASK;
      if (PointerDepth <> 0) then
      begin
        if (PointerDepth <> 1) then
        repeat
          Dec(NativeInt(Ptr), SizeOf(Pointer));
          PNativeInt(Ptr)^ := NativeInt(Ptr) + SizeOf(Pointer);
          Dec(PointerDepth);
        until (PointerDepth = 1);

        PPointer(NativeInt(ParamBlock) + Param.Value)^ := Ptr;
      end;
    end;

    Inc(InitialPtr);
  end;
end;

procedure TLuaInvokable.Finalize(const ParamBlock: PParamBlock);
label
  failure;
var
  i, FlagsEx, VType: Integer;
  FinalPtr: PInteger;
  Param: PLuaInvokableParam;
  Ptr: Pointer;

  procedure _FinalizeArray(Param: PLuaInvokableParam; Ptr: Pointer; Count: Integer); far;
  var
    i: Integer;
    MetaType: PLuaMetaType;
  begin
    case (Param.F.Kind) of
      {$ifdef AUTOREFCOUNT}
      pkObject:
      begin
        FinalizeArray(Ptr, TypeInfo(TObject), Count);
      end;
      {$endif}
      pkVariant:
      begin
        FinalizeArray(Ptr, TypeInfo(Variant), Count);
      end;
      pkInterface:
      begin
        FinalizeArray(Ptr, TypeInfo(IInterface), Count);
      end;
      pkString:
      begin
        case Param.F.StringType of
            stAnsiString: FinalizeArray(Ptr, TypeInfo(AnsiString), Count);
            stWideString: FinalizeArray(Ptr, TypeInfo(WideString), Count);
         stUnicodeString: FinalizeArray(Ptr, TypeInfo(UnicodeString), Count);
        end;
      end;
      pkClosure:
      begin
        case Param.F.ClosureKind of
          ckReference:
          begin
            FinalizeArray(Ptr, TypeInfo(IInterface), Count);
          end;
          {$ifdef WEAKINSTREF}
          ckMethod:
          begin
            FinalizeArray(Ptr, TypeInfo(TLuaOnPreprocessScript{TMethodProc}), Count);
          end;
          {$endif}
        end;
      end;
      pkRecord, pkArray:
      begin
        MetaType := Param.F.MetaType;

        for i := 1 to Count do
        begin
          MetaType.Clear(Ptr, False);
          Inc(NativeInt(Ptr), MetaType.Size);
        end;

        Dec(NativeInt(Ptr), Count * MetaType.Size);
      end;
      // TVarArg?
    end;

    FreeMem(Ptr);
  end;
begin
  FinalPtr := Finals;

  for i := 1 to FinalCount do
  begin
    Param := Pointer(NativeInt(@Self) + FinalPtr^);
    Ptr := Pointer(NativeInt(ParamBlock) + Param.DataValue);

    FlagsEx := Param.F.FlagsEx;
    if (FlagsEx and PARAM_INSURANCE_MODE = 0) then
    begin
      case ((FlagsEx shr 8) and $7f) of
        Ord(pkString):
        begin
          case Param.F.StringType of
            {$ifNdef NEXTGEN}
            stAnsiString:
            begin
              if (PNativeInt(Ptr)^ <> 0) then
                AnsiString(Ptr^) := '';
            end;
            stWideString:
            begin
              if (PNativeInt(Ptr)^ <> 0) then
                WideString(Ptr^) := '';
            end;
            {$endif}
            stUnicodeString:
            begin
              if (PNativeInt(Ptr)^ <> 0) then
                UnicodeString(Ptr^) := '';
            end;
          else
            goto failure;
          end;
        end;
        Ord(pkVariant):
        begin
          VType := PVarData(Ptr).VType;
          if (VType and varDeepData <> 0) then
          case VType of
            varBoolean, varUnknown+1..$15{varUInt64}: ;
          else
            System.VarClear(PVariant(Ptr)^);
          end;
        end;
      else
      failure:
        raise ELua.CreateFmt('Error finalize argument %d', [FinalPtr^]);
      end;
    end else
    begin
      // Insurance: array/PAnsiChar/PWideChar
      Ptr := Pointer(NativeInt(ParamBlock) + Param.InsuranceValue);
      if (FlagsEx and PARAM_ARRAY_MODE = 0) then
      begin
        case Param.F.Kind of
          pkString:
          begin
            case Param.F.StringType of
              stPAnsiChar:
              begin
                if (PNativeInt(Ptr)^ <> 0) then
                  AnsiString(Ptr^) := '';
              end;
              stPWideChar:
              begin
                if (PNativeInt(Ptr)^ <> 0) then
                  UnicodeString(Ptr^) := '';
              end;
            else
              goto failure;
            end;
          end;
        else
          goto failure;
        end;
      end else
      begin
        // insurance array
        Ptr := PPointer(Ptr)^;
        if (Ptr <> nil) then
        begin
          Inc(Param);
          FlagsEx{Count} := PInteger(NativeInt(ParamBlock) + Param.DataValue)^ {$ifNdef FPC} + 1{$endif};
          Dec(Param);
          _FinalizeArray(Param, Ptr, FlagsEx{Count});
        end;
      end;
    end;

    Inc(FinalPtr);
  end;
end;

{$ifdef CPUARM}
procedure TLuaInvokable.FillARMPartialData(const Param: PLuaInvokableParam;
  const Value: Pointer; const ParamBlock: PParamBlock);
var
  i: Integer;
  PartialSize: Integer;
  Src, Dest: PByte;
begin
  Src := Value;
  Dest := Pointer(NativeInt(ParamBlock) + Param.Value);
  PartialSize := Self.ARMPartialSize;

  {$ifdef CPUX64}
  if (Param.MetaType.HFA) then
  begin
    if (Param.MetaType.HFAElementType = ftSingle) then
    begin
      for i := 1 to PartialSize shr 2 do
      begin
        PCardinal(Dest)^ := PCardinal(Src)^;
        Inc(Src, SizeOf(Cardinal));
        Inc(Dest, 16);
      end;
    end else
    begin
      for i := 1 to PartialSize shr 3 do
      begin
        PInt64(Dest)^ := PInt64(Src)^;
        Inc(Src, SizeOf(Int64));
        Inc(Dest, 16);
      end;
    end;
    Exit;
  end else
  begin
    for i := 1 to PartialSize shr 3 do
    begin
      PInt64(Dest)^ := PInt64(Src)^;
      Inc(Src, SizeOf(Int64));
      Inc(Dest, 16);
    end;
  end;
  {$else .CPUARM32}
  begin
    System.Move(Src^, Dest^, PartialSize);
    Inc(Src, PartialSize);
    Inc(Dest, PartialSize);
  end;
  {$endif}

  Dest := Pointer(NativeInt(ParamBlock) + Param.DataValue);
  System.Move(Src^, Dest^, Param.MetaType.Size - PartialSize);
end;
{$endif}

procedure TLuaInvokable.Invoke(const CodeAddress: Pointer; const ParamBlock: PParamBlock);
{$if Defined(CPUX86)}
const
  PARAMBLOCK_SIZE = SizeOf(TParamBlock);
asm
      PUSH  EBP
      MOV   EBP, ESP

      PUSH  edx // EBP - 4 = CodeAddress
      PUSH  EBX
      MOV   EBX, ecx // EBX = ParamBlock
      push esi
      mov esi, eax // ESI = LuaInvokable

      // Copy block to stack (Native Aligned!)
      MOV   ECX, [esi].TLuaInvokable.StackDataSize
      TEST  ECX, ECX
      JZ    @@skip_push
      cmp ecx, 16
      ja @@do_push

      // small block
      sub esp, ecx
      shr ecx, 2
      jmp [offset @move_cases + ecx * 4 - 4]
      @move_cases: DD @move_4, @move_8, @move_12, @move_16

@@do_push:
      test ecx, ecx
{$IFDEF ALIGN_STACK}
      MOV   EAX, ECX
      AND   EAX, $F
      JZ    @@no_align
      SUB   EAX, 16
      ADD   ESP, EAX
@@no_align:
{$ENDIF ALIGN_STACK}
      // touch stack pages in case it needs to grow
      // while (count > 0) { touch(stack); stack -= 4096; count -= 4096; }
      MOV   EAX, ECX
      JMP   @@touch_loop_begin

@@touch_loop:
      MOV   [ESP],0
@@touch_loop_begin:
      SUB   ESP, 4096
      SUB   EAX, 4096
      JNS   @@touch_loop
      SUB   ESP, EAX

      lea   EAX, [EBX + PARAMBLOCK_SIZE]
      MOV   EDX, ESP
      push offset @@skip_push
      jmp System.Move // EAX=source, EDX=dest, ECX=count

@move_16:
      mov edx, [EBX + PARAMBLOCK_SIZE + 12]
      mov [esp + 12], edx
@move_12:
      mov edx, [EBX + PARAMBLOCK_SIZE + 8]
      mov [esp + 8], edx
@move_8:
      mov edx, [EBX + PARAMBLOCK_SIZE + 4]
      mov [esp + 4], edx
@move_4:
      mov edx, [EBX + PARAMBLOCK_SIZE]
      mov [esp], edx

@@skip_push:
      MOV   EAX, [EBX].TParamBlock.RegEAX
      MOV   EDX, [EBX].TParamBlock.RegEDX
      MOV   ECX, [EBX].TParamBlock.RegECX
      CALL  [EBP - 4]
      MOV   [EBX].TParamBlock.OutEAX, eax
      MOV   [EBX].TParamBlock.OutEDX, edx

      cmp byte ptr [esi].TLuaInvokable.Result.F.Kind, pkFloat
      jne @@done
      cmp byte ptr [esi].TLuaInvokable.Result.F.FloatType, ftExtended
      jne @fpu_to_int64

      fstp tbyte ptr [EBX].TParamBlock.OutFPU
      jmp @@done

@fpu_to_int64:
      fistp qword ptr [EBX].TParamBlock.OutInt64

@@done:
      mov ecx, esi
      lea esp, [ebp - 12]
      pop esi
      POP   EBX
      POP   edx
      POP   EBP

      // safe call check
      test eax, eax
      jge @ret
      cmp [ECX].TLuaInvokable.CallConv, ccSafeCall
      je System.@CheckAutoResult
@ret:
end;
{$elseif Defined(CPUX64) and Defined(MSWINDOWS)}
const
  PARAMBLOCK_SIZE = SizeOf(TParamBlock);

  procedure InvokeError(const ReturnAddress: Pointer); far;
  begin
    raise EInvocationError.CreateRes(Pointer(@SParameterCountExceeded)) at ReturnAddress;
  end;
asm
      // .PARAMS 62 // There's actually room for 64, assembler is saving locals for Self, CodeAddress & ParamBlock
      push rbp
      sub rsp, $1f0
      mov rbp, rsp

      mov [rbp + $208], rcx
      MOV     [RBP+$210], CodeAddress
      MOV     [RBP+$218], ParamBlock
      xchg rdx, r8 // rdx := ParamBlock

      // Copy block to stack (Native Aligned!)
      MOV     EAX, [rcx].TLuaInvokable.StackDataSize
      TEST    EAX, EAX
      JZ      @@skip_push

      // small/Sytem.Move
      cmp eax, 32
      ja @@do_move
      shr rax, 3
      lea rcx, [@move_cases - 8]
      mov rcx, [rcx + rax * 8]
      jmp rcx
      @move_cases: DQ @move_8, @move_16, @move_24, @move_32

@@do_move:
      CMP     EAX, 480 // (64-4) params * 8 bytes.
      Ja      @@invalid_frame
      lea     rcx, [RDX + PARAMBLOCK_SIZE]
      LEA     RDX, [RBP+$20]
      xchg    r8, rax
      CALL    System.Move
      MOV     RDX, [RBP+$218]
      jmp @@skip_push

@@invalid_frame:
      lea rsp, [rbp + $000001f0]
      pop rbp
      mov rcx, [rsp]
      jmp InvokeError

@move_32:
      mov rax, [RDX + PARAMBLOCK_SIZE + 24]
      mov [rbp + $20 + 24], rax
@move_24:
      mov rax, [RDX + PARAMBLOCK_SIZE + 16]
      mov [rbp + $20 + 16], rax
@move_16:
      mov rax, [RDX + PARAMBLOCK_SIZE + 8]
      mov [rbp + $20 + 8], rax
@move_8:
      mov rax, [RDX + PARAMBLOCK_SIZE]
      mov [rbp + $20], rax

@@skip_push:
      MOV     RCX, [RDX].TParamBlock.RegRCX
      MOV     R8,  [RDX].TParamBlock.RegR8
      MOV     R9,  [RDX].TParamBlock.RegR9
      MOV     RDX, [RDX].TParamBlock.RegRDX
      movq xmm0, rcx
      movq xmm1, rdx
      movq xmm2, r8
      movq xmm3, r9

      CALL    [RBP+$210]

      MOV     RDX, [RBP+$218]
      MOV     [RDX].TParamBlock.OutRAX, RAX
      MOVSD   [RDX].TParamBlock.OutXMM0, XMM0

      mov rdx, [rbp + $208]
      lea rsp, [rbp + $1f0]
      pop rbp

      // safe call check
      test eax, eax
      jge @ret
      xchg rax, rcx
      cmp [RDX].TLuaInvokable.CallConv, ccSafeCall
      je System.@CheckAutoResult
@ret:
end;
{$elseif Defined(CPUX64)} {!MSWINDOWS}
var
  StoredXMM0: Double;
begin
  StoredXMM0 := ParamBlock.RegXMM[0];
  try
    ParamBlock.StackData := Pointer(NativeInt(ParamBlock) + SizeOf(TParamBlock));
    ParamBlock.StackDataSize := Self.StackDataSize;
    RawInvoke(CodeAddress, ParamBlock);

    ParamBlock.OutXMM0 := ParamBlock.RegXMM[0];
  finally
    ParamBlock.RegXMM[0] := StoredXMM0;
  end;

  if (Self.CallConv = ccSafeCall) and (ParamBlock.OutSafeCall < 0) then
    CheckAutoResult(ParamBlock.OutSafeCall);
end;
{$elseif Defined(CPUARM32)}
var
  StoredCR: Integer;
  StoredD: Double;
begin
  StoredCR := ParamBlock.RegCR[0];
  StoredD := ParamBlock.RegD[0];
  try
    ParamBlock.StackData := Pointer(NativeInt(ParamBlock) + SizeOf(TParamBlock));
    ParamBlock.StackDataSize := Self.StackDataSize;
    RawInvoke(CodeAddress, ParamBlock);

    ParamBlock.OutCR := ParamBlock.RegCR[0];
    ParamBlock.OutD := ParamBlock.RegD[0];
  finally
    ParamBlock.RegCR[0] := StoredCR;
    ParamBlock.RegD[0] := StoredD;
  end;

  if (Self.CallConv = ccSafeCall) and (ParamBlock.OutSafeCall < 0) then
    CheckAutoResult(ParamBlock.OutSafeCall);
end;
{$else .CPUARM64}
var
  StoredX: Int64;
  Stored128x4: m128x4;
begin
  StoredX := ParamBlock.RegX[0];
  Stored128x4 := ParamBlock.Regs128x4;
  try
    ParamBlock.StackData := Pointer(NativeInt(ParamBlock) + SizeOf(TParamBlock));
    ParamBlock.StackDataSize := Self.StackDataSize;
    RawInvoke(CodeAddress, ParamBlock);

    ParamBlock.OutX := ParamBlock.RegX[0];
    ParamBlock.OutQ := ParamBlock.RegQ[0];
    if (Self.ResultKind in [rkHFASingle, rkHFADouble]) then
    begin
      if (Self.ResultKind = rkHFASingle) then
      begin
        ParamBlock.OutHFA.Singles[0] := ParamBlock.RegQ[0].LL;
        ParamBlock.OutHFA.Singles[1] := ParamBlock.RegQ[1].LL;
        ParamBlock.OutHFA.Singles[2] := ParamBlock.RegQ[2].LL;
        ParamBlock.OutHFA.Singles[3] := ParamBlock.RegQ[3].LL;
      end else
      begin
        ParamBlock.OutHFA.Doubles[0] := ParamBlock.RegQ[0].Lo;
        ParamBlock.OutHFA.Doubles[1] := ParamBlock.RegQ[1].Lo;
        ParamBlock.OutHFA.Doubles[2] := ParamBlock.RegQ[2].Lo;
        ParamBlock.OutHFA.Doubles[3] := ParamBlock.RegQ[3].Lo;
      end;
    end;
  finally
    ParamBlock.RegX[0] := StoredX;
    ParamBlock.Regs128x4 := Stored128x4;
  end;

  if (Self.CallConv = ccSafeCall) and (ParamBlock.OutSafeCall < 0) then
    CheckAutoResult(ParamBlock.OutSafeCall);
end;
{$ifend}


type
  TLuaInvokableBuilder = object
  private
    FLua: TLua;
    FBuffer: TLuaBuffer;
    FAdvanced: TLuaBuffer;

    function NewInvokable(const MethodKind: TLuaMethodKind; const CallConv: TCallConv; const ResultType: PTypeInfo): PLuaInvokable;
    function InternalAddName(const Name: LuaString): NativeInt;
    function InternalAddParam(const Name: PShortString; const TypeInfo: Pointer;
      const PointerDepth: Integer; const ParamFlags: TParamFlags): PLuaInvokableParam; overload;
    function InternalAddParam(const Param: TLuaProcParam): PLuaInvokableParam; overload;
    procedure InternalInitParams(var Invokable: TLuaInvokable);
    function InternalBuildDone: __luapointer;
  public
    procedure Clear;
    function BuildMethod(const AMethod: PTypeInfo; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    {$ifdef EXTENDEDRTTI}
    function BuildProcedure(const AProcedure: PTypeInfo; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    {$endif}
    // rtti?
    function BuildReference(const AReference: PTypeInfo; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildInterfaceMethod(const AMethodEntry: PIntfMethodEntry; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;

    // ToDo class/record method?

    function BuildCustom(const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
      const MethodKind: TLuaMethodKind; const CallConv: TCallConv): __luapointer;
  end;

const
  LUA_METHOD_KINDS: array[{$ifdef UNITSCOPENAMES}System.{$endif}TypInfo.TMethodKind] of TLuaMethodKind = (
    {mkProcedure} mkStatic,
    {mkFunction} mkStatic,
    {mkConstructor} mkConstructor,
    {mkDestructor} mkDestructor,
    {mkClassProcedure} mkClass,
    {mkClassFunction} mkClass,
    {$if Defined(EXTENDEDRTTI) or Defined(FPC)}
    {mkClassConstructor} mkStatic,
    {mkClassDestructor} mkStatic,
    {mkOperatorOverload} mkOperator,
    {$ifend}
    {mkSafeProcedure} mkStatic,
    {mkSafeFunction} mkStatic
  );
  PARAMNAME_RESULT: array[0..6] of Byte = (6, Ord('R'), Ord('e'), Ord('s'),
    Ord('u'), Ord('l'), Ord('t'));
  PARAMNAME_HIGH_ARRAY: array[0..11] of Byte = (11, Ord('H'), Ord('i'), Ord('g'),
    Ord('h'), Ord('('), Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'), Ord(')'));

procedure TLuaInvokableBuilder.Clear;
begin
  FBuffer.Clear;
  FAdvanced.Clear;
end;

function TLuaInvokableBuilder.NewInvokable(const MethodKind: TLuaMethodKind;
  const CallConv: TCallConv; const ResultType: PTypeInfo): PLuaInvokable;
begin
  FBuffer.Size := 0;
  FAdvanced.Size := 0;

  Result := FBuffer.Alloc(SizeOf(TLuaInvokable) - SizeOf(TLuaInvokableParams));
  FillChar(Result^, SizeOf(TLuaInvokable) - SizeOf(TLuaInvokableParams), #0);

  Result.MethodKind := MethodKind;
  Result.CallConv := CallConv;
  Result.Instance := VALUE_NOT_DEFINED;
  Result.ConstructorFlag := VALUE_NOT_DEFINED;
  Result.Result.Value := VALUE_NOT_DEFINED;
  Result.Result.DataValue := VALUE_NOT_DEFINED;
  Result.Result.InsuranceValue := VALUE_NOT_DEFINED;

  if (Assigned(ResultType)) then
  begin
    Result.Result.InternalSetTypeInfo(ResultType, FLua, True);
    if (Result.Result.Kind = pkUnknown) then
    begin
      FLua.FStringBuffer.Default := Format('Invalid result type "%s" (%s)', [
        FLua.FStringBuffer.Lua, FLua.FStringBuffer.Default]);
      Result := nil;
      Exit;
    end
    {$ifdef CPUX86}
    else
    if (Result.Result.Kind = pkFloat) and (CallConv <> ccSafeCall) and
      (Result.Result.F.FloatType in [ftSingle, ftDouble]) then
    begin
      Result.Result.F.FloatType := ftExtended;
    end
    {$endif}
    ;
  end;

  {$ifdef CPUX86}
  if CallConv = ccCdecl then
  begin
    Result.StackCleaner := scInvokable;

    {$ifdef MACOS}
    if (Result.Result.F.Kind = pkRecord) then
    case Result.Result.F.MetaType.Size of
      0,1,2,4,8: ;
    else
      Result.StackCleaner := scInvokableEx;
    end;
    {$endif}
  end;
  {$endif}
end;

function TLuaInvokableBuilder.InternalAddName(const Name: LuaString): NativeInt;
var
  Length, Size: Integer;
  Buffer: ^TLuaBuffer;
  {$if (not Defined(LUA_UNICODE)) and (not Defined(NEXTGEN)) and Defined(INTERNALCODEPAGE)}
  CodePage: Word;
  {$ifend}
  S: PByte;
begin
  Length := System.Length(Name);
  Buffer := Pointer(@FLua.FInternalBuffer);
  Buffer.Size := 0;

  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    // Ansi/Utf8 <-- Utf16
    Size := Length * 3 + 1;
    if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
    {$ifdef UNICODE}
      Buffer.Size := Utf8FromUnicode(Pointer(Buffer.FBytes), Pointer(Name), Length);
    {$else .ANSI}
      Buffer.Size := FLua.AnsiFromUnicode(Pointer(Buffer.FBytes), 0, Pointer(Name), Length);
    {$endif}
  {$else .ANSISTRING}
    // Ansi/Utf8 <-- Ansi/Utf8
    Size := Length * 6 + 1;
    if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
    {$ifdef INTERNALCODEPAGE}
    CodePage := PWord(NativeInt(Name) - ASTR_OFFSET_CODEPAGE)^;
    if (CodePage = CODEPAGE_UTF8) then
    begin
      {$ifdef UNICODE}
        System.Move(Pointer(Name)^, Pointer(Buffer.FBytes)^, Length);
        Buffer.Size := Length;
      {$else .ANSI}
        Buffer.Size := FLua.AnsiFromUtf8(Pointer(Buffer.FBytes), 0, Pointer(Name), Length);
      {$endif}
    end else
    {$endif}
    begin
      {$ifdef UNICODE}
        Buffer.Size := FLua.Utf8FromAnsi(Pointer(Buffer.FBytes), Pointer(Name),
          {$ifdef INTERNALCODEPAGE}CodePage{$else}0{$endif}, Length);
      {$else .ANSI}
        Buffer.Size := FLua.AnsiFromAnsi(Pointer(Buffer.FBytes), 0, Pointer(Name),
          {$ifdef INTERNALCODEPAGE}CodePage{$else}0{$endif}, Length);
      {$endif}
    end;
  {$ifend}

  Size := Buffer.Size;
  if (Size > High(Byte)) then Size := High(Byte);

  Result := FAdvanced.Size;
  S := FAdvanced.Alloc(SizeOf(Byte) + Size);
  PByte(S)^ := Size;
  Inc(S);
  System.Move(Pointer(Buffer.FBytes)^, S^, Size);
end;

function TLuaInvokableBuilder.InternalAddParam(const Name: PShortString; const TypeInfo: Pointer;
  const PointerDepth: Integer; const ParamFlags: TParamFlags): PLuaInvokableParam;
var
  CustomParam: TLuaCustomParam;
  HighArray: PLuaInvokableParam;

  {$if (not Defined(CPUX64)) or (not Defined(MSWINDOWS))}
  function CallConv: TCallConv;
  begin
    Result := PLuaInvokable(FBuffer.FBytes).CallConv;
  end;
  {$ifend}

  {$if Defined(CPUX86)}
  function IsConst: Boolean;
  begin
    Result := (pfConst in ParamFlags);
  end;
  {$ifend}
begin
  // TVarArg?
  if (not CustomParam.InternalSetTypeInfo(TypeInfo, FLua, True)) then
  begin
    Result := nil;
    Exit;
  end;

  Result := FBuffer.Alloc(SizeOf(TLuaInvokableParam));
  Inc(PLuaInvokable(FBuffer.FBytes).ParamCount);

  PLuaCustomParam(Result)^ := CustomParam;
  Result.Name := Name;
  Result.Value := VALUE_NOT_DEFINED;
  Result.DataValue := VALUE_NOT_DEFINED;
  Result.InsuranceValue := VALUE_NOT_DEFINED;

  Result.IsArray := (pfArray in ParamFlags);
  Result.IsAddress := (pfAddress in ParamFlags); // ?
  Result.IsReference := ([pfVar, pfReference, pfOut] * ParamFlags <> []) and (not Result.IsAddress);
  if (PointerDepth <> 0) then
  begin
    if (Result.IsAddress) then
    begin
      // error ToDo
      Result := nil;
      Exit;
    end;
  end else
  if (not (Result.IsArray or Result.IsAddress or Result.IsReference)) then
  begin
    case Result.F.Kind of
      pkVariant:
      begin
        Result.IsReference :=
        {$if Defined(CPUX86)}
          IsConst or (not (CallConv in [ccCdecl, ccStdCall, ccSafeCall]))
        {$else}
          True
        {$ifend};
      end;
      pkClosure:
      begin
        Result.IsReference :=
        {$if Defined(CPUX86)}
          False
        {$else}
          (Result.F.ClosureKind = ckMethod)
        {$ifend};
      end;
      pkString:
      begin
        Result.IsReference := (Result.F.StringType = stShortString);
      end;
      pkRecord:
      begin
        {$if Defined(CPUX86)}
        if (CallConv in [ccCdecl, ccStdCall, ccSafeCall]) and (not IsConst) then
          Result.IsReference := False
        else
        {$elseif (not Defined(CPUX64)) or (not Defined(MSWINDOWS))}
        if (CallConv in [ccReg, ccPascal]) then
          Result.IsReference := True
        else
        {$ifend}

        {$ifdef CPUARM64}
        if (Result.F.MetaType.HFA) then
          Result.IsReference := False
        else
        {$endif}

        case Result.F.MetaType.F.Size of
          {$if Defined(CPUX86)}
          1..SizeOf(Pointer)
          {$elseif Defined(CPUX64) and Defined(MSWINDOWS)}
          1, 2, 4, 8
          {$elseif Defined(CPUARM32)}
          -1{none}
          {$else}
          1..16
          {$ifend}
          : ;
        else
          Result.IsReference := True;
        end;
      end;
      pkArray:
      begin
        if (not PLuaArrayInfo(Result.F.MetaType).IsDynamic) then
        begin
          {$if (not Defined(CPUX86)) and ((not Defined(CPUX64)) or (not Defined(MSWINDOWS)))}
          if (CallConv in [ccReg, ccPascal]) then
            Result.IsReference := True
          else
          {$ifend}
          {$ifdef CPUARM64}
          if (Result.F.MetaType.HFA) then
            Result.IsReference := False
          else
          {$endif}

          case Result.F.MetaType.F.Size of
            {$if Defined(CPUX86) or (Defined(CPUX64) and Defined(MSWINDOWS))}
            1..SizeOf(Pointer)
            {$else}
            1..16
            {$ifend}
            : ;
          else
            Result.IsReference := True;
          end;
        end;
      end;
      pkSet:
      begin
        Result.IsReference := (Result.F.MetaType.F.Size > SizeOf(Pointer));
      end;
    end;
  end;

  Result.IsInsurance := Result.IsArray or
   ((Result.F.Kind = pkString) and (Result.F.StringType in [stPAnsiChar, stPWideChar]));

  Result.PointerDepth := PointerDepth + Ord(Result.IsReference);
  if (Result.IsArray) then
  begin
    HighArray := FBuffer.Alloc(SizeOf(TLuaInvokableParam));
    HighArray.InternalSetTypeInfo(System.TypeInfo(Integer), FLua, True);
    HighArray.Name := Pointer(@PARAMNAME_HIGH_ARRAY);
    HighArray.Value := VALUE_NOT_DEFINED;
    HighArray.DataValue := VALUE_NOT_DEFINED;
    HighArray.InsuranceValue := VALUE_NOT_DEFINED;
  end;
end;

function TLuaInvokableBuilder.InternalAddParam(const Param: TLuaProcParam): PLuaInvokableParam;
begin
  Result := InternalAddParam(Pointer(InternalAddName(Param.Name)), Param.TypeInfo,
    Param.PointerDepth, Param.Flags);
end;

function UseResultRef(const Param: TLuaInvokableParam; const CallConv: TCallConv;
  const IsConstructor: Boolean): Boolean;
begin
  if (CallConv = ccSafeCall) then
  begin
    Result := True;
  end else
  case Param.Kind of
    {$ifdef AUTOREFCOUNT}
    pkObject: Result := (not IsConstructor);
    {$endif}
    pkString: Result := (Param.F.StringType in [stShortString, stAnsiString, stWideString, stUnicodeString]);
    pkVariant,
    pkInterface: Result := True;
    pkClosure: Result := (Param.F.ClosureKind in [ckMethod, ckReference]);
    pkRecord:
    begin
      {$if Defined(CPUX64)}
      case Param.F.MetaType.Size of
        1, 2, 4: Result := False;
        8: Result := Param.F.MetaType.Managed;
      else
        Result := True;
      end;
      {$elseif Defined(CPUX86)}
      case Param.F.MetaType.Size of
        1, 2: Result := False;
        4: Result := Param.F.MetaType.Managed;
      else
        Result := True;
      end;
      {$elseif Defined(CPUARM32)}
      case Param.F.MetaType.Size of
        {$if Defined(ANDROID32)}
            1..4: Result := False;
        {$elseif Defined(IOS32)}
            1: Result := False;
        {$ifend}
      else
        Result := True;
      end;
      {$elseif Defined(CPUARM64)}
      if (Param.F.MetaType.HFA) then
        Result := False
      else
        Result := (Param.F.MetaType.Size < 1) or (Param.F.MetaType.Size > 16);
      {$ifend}
    end;
    pkArray, pkSet:
    begin
      Result := ((Param.F.Kind = pkArray) and (PLuaArrayInfo(Param.F.MetaType).IsDynamic)) or
        (Param.F.MetaType.Size > 255) or (not (Byte(Param.F.MetaType.Size) in [
        {$if Defined(CPUX64)}
          1, 2, 4, 8
        {$elseif Defined(CPUX86) or Defined(CPUARM32)}
          1, 2, 4
        {$elseif Defined(CPUARM64)}
          1..16
        {$ifend}
      ]));
    end;
  else
    Result := False;
  end;
end;

procedure TLuaInvokableBuilder.InternalInitParams(var Invokable: TLuaInvokable);
const
  MASK_BUFFERED = 1 shl 30;
  NATIVE_SHIFT = {$ifdef LARGEINT}3{$else}2{$endif};
type
  TResultMode = (rmNone, rmRegister, rmRefFirst, rmRefLast);
  TSelfMode = (smNone, smFirst, smSecond, smLast);
var
  i: Integer;
  IsStatic, IsConstructor, IsBackwardArg: Boolean;
  ResultMode: TResultMode;
  SelfMode: TSelfMode;
  Param: ^TLuaInvokableParam;
  RegGen, RegExt: Integer;
  TempParam: TLuaInvokableParam;

  function AllocRegGen(const Count: Integer = 1): Integer;
  begin
    if (RegGen >= REGGEN_COUNT) then
    begin
      Result := VALUE_NOT_DEFINED;
    end else
    begin
      Result := RegGen * REGGEN_SIZE + NativeInt(@PParamBlock(nil).RegsGen);
      Inc(RegGen, Count);
      {$if Defined(CPUX64) and Defined(MSWINDOWS)}
      RegExt := RegGen;
      {$ifend}
    end;
  end;

  function AllocRegExt(const Count: Integer = 1): Integer;
  begin
    if (RegExt >= REGEXT_COUNT) then
    begin
      Result := VALUE_NOT_DEFINED;
    end else
    begin
      Result := RegExt * REGEXT_SIZE + NativeInt(@PParamBlock(nil).RegsExt);
      Inc(RegExt, Count);
      {$if Defined(CPUX64) and Defined(MSWINDOWS)}
      RegGen := RegExt;
      {$ifend}
    end;
  end;

  function PutStack(var Value: Integer; ASize: Integer): Integer;
  begin
    ASize := (ASize + SizeOf(Pointer) - 1) and (-SizeOf(Pointer));

    if (IsBackwardArg) then
    begin
      Value := SizeOf(TParamBlock) + Invokable.StackDataSize;
      Inc(Invokable.StackDataSize, ASize);
    end else
    begin
      Inc(Invokable.StackDataSize, ASize);
      Value := -Invokable.StackDataSize;
    end;

    Result := Value;
  end;

  function PutBuffer(var Value: Integer; ASize: Integer): Integer;
  begin
    ASize := (ASize + SizeOf(Pointer) - 1) and (-SizeOf(Pointer));

    Value := MASK_BUFFERED or Invokable.TotalDataSize;
    Inc(Invokable.TotalDataSize, ASize);

    Result := Value;
  end;

  function PutGen(var Value: Integer): Integer;
  begin
    Value := AllocRegGen;
    Result := Value;
  end;

  function PutExt(var Value: Integer): Integer;
  begin
    Value := AllocRegExt;
    Result := Value;
  end;

  function PutPtr(var Value: Integer): Integer;
  begin
    Result := PutGen(Value);
    if (Result = VALUE_NOT_DEFINED) then
      Result := PutStack(Value, SizeOf(Pointer));
  end;

  procedure PutArg;
  var
    Size: Integer;
    {$ifdef CPUARM32}
    RegL, RegH: Integer;
    {$endif}
  begin
    Size := Param.Size;

    // array of ...
    if (Param.IsArray) then
    begin
      Param.DataValue := PutPtr(Param.Value);
      PutBuffer(Param.InsuranceValue, SizeOf(Pointer));
      Exit;
    end;

    // references: pointer + buffered data
    if (Param.PointerDepth <> 0) then
    begin
      Param.DataValue := PutPtr(Param.Value);

      if (Param.PointerDepth > 1) then
      begin
        PutBuffer(Param.DataValue, (Param.PointerDepth - 1) * SizeOf(Pointer));
        Inc(Param.DataValue, (Param.PointerDepth - 1) * SizeOf(Pointer));
      end;

      case Param.F.Kind of
        pkInterface, pkRecord, pkArray, pkSet, pkClosure:
        begin
          if (Param.PointerDepth > 1) then
            Dec(Param.DataValue, SizeOf(Pointer));

          Param.PointerDepth := Param.PointerDepth - 1;
        end;
      else
        PutBuffer(Param.DataValue, Size);
      end;

      if (Param.IsInsurance) then
      begin
        PutBuffer(Param.InsuranceValue, SizeOf(Pointer));
      end;

      Exit;
    end;

    // whole data: registers/stack
    if (Param.Kind = pkFloat) and (Param.FloatType in [ftSingle, ftDouble, ftExtended]) then
    begin
      case Size of
        4:
        begin
          Param.DataValue := PutExt(Param.Value);
          if (Param.Value <> VALUE_NOT_DEFINED) then
            Exit;
        end;
        8:
        begin
          {$ifdef CPUARM32}
          if (RegExt + 2 <= REGEXT_COUNT) then
          begin
            Param.Value := AllocRegExt(2);
            Param.DataValue := Param.Value;
            Exit;
          end;
          {$else}
          Param.DataValue := PutExt(Param.Value);
          if (Param.Value <> VALUE_NOT_DEFINED) then
            Exit;
          {$endif}
        end;
      else
        {$ifdef CPUARM64}
          Param.DataValue := PutExt(Param.Value);
          if (Param.Value <> VALUE_NOT_DEFINED) then
            Exit;
        {$endif}
      end;
    end
    {$ifdef CPUARM64}
    else
    if (Param.Kind = pkRecord) and (Param.MetaType.HFA) then
    begin
      Size := Param.MetaType.HFAElementCount * SizeOf(Single);
      if (Param.MetaType.HFAElementType <> ftSingle) then Size := Size * 2;

      if (RegExt + Param.MetaType.HFAElementCount <= REGEXT_COUNT) then
      begin
        Invokable.ARMPartialSize := Size;
        Param.IsARMPartial := True;
        Param.Value := AllocRegExt(Param.MetaType.HFAElementCount);
        Param.DataValue := Param.Value;
        Exit;
      end else
      begin
        RegExt := REGEXT_COUNT;
      end;
    end
    {$endif}
    {$ifdef CPUARM}
    else
    if (Param.Kind in [pkRecord{$ifdef CPUARM32}, pkArray{$endif}]) then
    begin
      if (RegGen + ((Size + SizeOf(Pointer) - 1) shr NATIVE_SHIFT) <= REGGEN_COUNT) then
      begin
        Param.Value := AllocRegGen((Size + SizeOf(Pointer) - 1) shr NATIVE_SHIFT);
        Param.DataValue := Param.Value;
        Exit;
      end else
      if (RegGen <> REGGEN_COUNT) then
      begin
        Invokable.ARMPartialSize := (REGGEN_COUNT - RegGen) shl NATIVE_SHIFT;
        Param.IsARMPartial := True;
        Param.Value := AllocRegExt(REGGEN_COUNT - RegGen);
        PutStack(Param.DataValue, Size - Invokable.ARMPartialSize);
        Exit;
      end;
    end
    {$endif};

    case Size of
      1, 2, 4{$ifdef LARGEINT},8{$ifNdef MSWINDOWS}3,5,6,7{$endif}{$endif}:
      begin
        Param.DataValue := PutPtr(Param.Value);
        Exit;
      end;
      {$if Defined(CPUX64) and (not Defined(MSWINDOWS))}
      9..16:
      begin
        if (PutGen(Param.Value)) and (AllocRegGen >= 0) then
        begin
          Param.DataValue := Param.Value;
          Exit;
        end;
      end;
      {$ifend}
      {$ifdef CPUARM32}
      8:
      begin
        {$ifNdef IOS}
        Inc(RegGen, RegGen and 1);
        {$endif}
        RegL := AllocRegGen;
        RegH := AllocRegGen;

        if (RegL >= 0) then
        begin
          if (RegH >= 0) then
          begin
            Param.Value := RegL;
            Param.DataValue := Param.Value;
            Exit;
          end else
          begin
            Invokable.ARMPartialSize := 4;
            Param.IsARMPartial := True;
            Param.Value := RegL;
            PutStack(Param.DataValue, 4);
            Exit;
          end;
        end else
        begin
          {$ifNdef IOS}
          Inc(Invokable.StackDataSize, Invokable.StackDataSize and 4);
          {$endif}
        end;
      end;
      {$endif}
    end;

    Param.DataValue := PutStack(Param.Value, Size);
    if (Param.IsInsurance{PAnsiChar, PWideChar}) then
      PutBuffer(Param.InsuranceValue, SizeOf(Pointer));
  end;

  procedure PutResult;
  var
    Ptr: Pointer;
  begin
    Param := @Invokable.Result;
    Param.Name := Pointer(@PARAMNAME_RESULT);

    if (ResultMode = rmRegister) then
    begin
      Invokable.ResultKind := rkRegister;
      Ptr :=
        {$if Defined(CPUINTEL)}
          @PParamBlock(nil).OutEAX
        {$elseif Defined(CPUARM32)}
          @PParamBlock(nil).OutCR
        {$else .CPUARM64}
          @PParamBlock(nil).OutX
        {$ifend}
        ;

      if (Param.F.Kind = pkFloat) then
      begin
        {$ifdef CPUX86}
        if (Param.F.FloatType = ftExtended{ftSingle, ftDouble}) then
        begin
          Ptr := @PParamBlock(nil).OutFPU;
        end else
        begin
          Ptr := @PParamBlock(nil).OutInt64{ftComp, ftCurr};
        end;
        {$endif}

        {$ifdef CPUX64}
        if (not (Param.F.FloatType in [ftComp, ftCurr])) then
        begin
          Ptr := @PParamBlock(nil).OutXMM0;

          {$ifNdef MSWINDOWS}
          if (Param.F.FloatType = ftExtended) then
          begin
            Ptr := @PParamBlock(nil).OutFPU;
          end;
          {$endif}
        end;
        {$endif}

        {$if (Defined(CPUARM32) and (not Defined(ARM_NO_VFP_USE))) or Defined(CPUARM64)}
         if {$ifdef CPUARM32}(Invokable.CallConv = ccReg) and {$endif}
           (Param.F.FloatType in [ftSingle, ftDouble, ftExtended]) then
         begin
           Ptr := @PParamBlock(nil).{$ifdef CPUARM32}OutD{$else .CPUARM64}OutQ{$endif};
         end;
         {$ifend}
      end;

      {$ifdef CPUARM64}
      if (Param.F.Kind = pkRecord) and (Param.MetaType.HFA) then
      begin
        Param.IsARMPartial := True;
        Ptr := @PParamBlock(nil).OutHFA;

        if (Param.MetaType.HFAElementType = ftSingle) then
        begin
          Invokable.ResultKind := rkHFASingle;
        end else
        begin
          Invokable.ResultKind := rkHFADouble;
        end;
      end;
      {$endif}

      Param.Value := NativeInt(Ptr);
      Param.DataValue := Param.Value;
      Exit;
    end;

    Param.IsReference := True;
    {$ifdef CPUARM64}
    if (ResultMode = rmRefFirst) then
    begin
      Param.Value := NativeInt(@PParamBlock(nil).RegX8);
    end else
    {$endif}
    begin
      PutPtr(Param.Value);
    end;

    if (Param.F.Kind in [pkObject, pkWeakObject, pkInterface,
      pkRecord, pkArray, pkSet, pkClosure]) then
    begin
      // user data
      Invokable.ResultKind := rkUserData;
    end else
    begin
      // buffer
      Param.PointerDepth := 1;
      Invokable.ResultKind := rkBuffer;
      PutBuffer(Param.DataValue, Param.Size);

      // is managed
      case Param.F.Kind of
        pkString:
        begin
          if (Param.F.StringType in [stAnsiString, stWideString, stUnicodeString]) then
            Invokable.ResultKind := rkManagedBuffer;
        end;
        pkVariant: Invokable.ResultKind := rkManagedBuffer;
      end;
    end;
  end;

  procedure PutSelf;
  begin
    PutPtr(Invokable.Instance);
  end;

  procedure ApplyOffset(var Value: Integer); overload;
  var
    FrameSize: Integer;
  begin
    FrameSize := SizeOf(TParamBlock) + Invokable.StackDataSize;

    if (Value < 0) then
    begin
      if (Value <> VALUE_NOT_DEFINED) then
        Inc(Value, FrameSize);
    end else
    if (Value and MASK_BUFFERED <> 0) then
    begin
      Value := (Value - MASK_BUFFERED) + FrameSize;
    end;
  end;

  procedure ApplyOffset; overload;
  begin
    ApplyOffset(Param.Value);
    ApplyOffset(Param.DataValue);
    ApplyOffset(Param.InsuranceValue);
  end;
begin
  // flags
  IsStatic := (Invokable.MethodKind = mkStatic);
  IsConstructor := (Invokable.MethodKind = mkConstructor);
  IsBackwardArg := {$ifdef CPUX86}(Invokable.CallConv in [ccCdecl, ccStdCall, ccSafeCall]){$else}True{$endif};

  // result mode
  ResultMode := rmNone;
  Param := @Invokable.Result;
  if (Param.F.Kind <> pkUnknown) then
  begin
    ResultMode := rmRegister;
    if (UseResultRef(Param^, Invokable.CallConv, IsConstructor)) then
    begin
      ResultMode := rmRefLast;

      {$if not Defined(CPUX86)}
      if (Invokable.CallConv <> ccSafeCall) then
      begin
        ResultMode := rmRefFirst;
      end;
      {$ifend}
    end;
  end;

  // self mode
  SelfMode := smNone;
  if (not IsStatic) then
  begin
    {$ifdef CPUX86}
    if (Invokable.CallConv = ccPascal) then
    begin
      SelfMode := smLast;
    end else
    {$endif}
    begin
      SelfMode := smFirst;

      {$if Defined(CPUARM)}
      if (ResultMode = rmRefFirst) then
      begin
        SelfMode := smSecond;
      end;
      {$ifend}
    end;
  end;

  // registers
  RegGen := 0;
  RegExt := 0;
  {$ifdef CPUX86}
  if (Invokable.CallConv <> ccReg) then
    RegGen := REGGEN_COUNT;
  {$endif}
  {$ifdef CPUARM32}
  if (Invokable.CallConv <> ccReg) then
    RegExt := REGEXT_COUNT;
  {$endif}

  // self, first result
  if (SelfMode = smFirst) then PutSelf;
  if (ResultMode = rmRefFirst) then PutResult;  
  if (SelfMode = smSecond) then PutSelf;

  // constructor flag
  if (IsConstructor) then
  begin
    TempParam.InternalSetTypeInfo(TypeInfo(Boolean), nil, False);
    Param := @TempParam;
    PutArg;
    Invokable.ConstructorFlag := TempParam.DataValue;
  end;

  // arguments
  Param := @Invokable.Params[Ord(SelfMode = smLast)];
  for i := 0 to Invokable.ParamCount - 1 do
  begin
    PutArg;

    if (Param.IsArray) then
    begin
      Inc(Param);
      PutArg;
    end;

    Inc(Param);
  end;

  // last result/self
  if (ResultMode = rmRefLast) then PutResult;
  if (SelfMode = smLast) then PutSelf;
  if (ResultMode = rmRegister) then PutResult;

  // total buffer (stack) size
  // calculate real offsets
  Inc(Invokable.TotalDataSize, SizeOf(TLuaInvokable) + Invokable.StackDataSize);
  Param := @Invokable.Params[0];
  for i := 0 to Invokable.ParamCount - 1 do
  begin
    ApplyOffset;

    if (Param.IsArray) then
    begin
      Inc(Param);
      ApplyOffset;
    end;

    Inc(Param);
  end;
  if (ResultMode in [rmRefFirst, rmRefLast]) then
  begin
    Param := @Invokable.Result;
    ApplyOffset;
  end;
  ApplyOffset(Invokable.Instance);
  ApplyOffset(Invokable.ConstructorFlag);
end;

function TLuaInvokableBuilder.InternalBuildDone: __luapointer;
var
  i: Integer;
  Offset: NativeInt;
  Invokable: PLuaInvokable;
  Param: ^TLuaInvokableParam;

  function IsParamHFAManaged: Boolean;
  begin
    Result := (Param.F.Kind in [pkRecord, pkArray]) and
      (Param.F.MetaType.HFA) and (Param.F.MetaType.Managed);
  end;

  function IsParamFinal: Boolean;
  begin
    Result := False;

    if (Param.IsInsurance) or (IsParamHFAManaged) then
    begin
      Result := True;
      Exit;
    end;

    case Param.F.Kind of
      pkVariant:
      begin
        Result := True;
      end;
      pkString:
      begin
        Result := (Param.F.StringType in [stAnsiString, stWideString, stUnicodeString]);
      end;
    end;
  end;

  function IsParamInitial: Boolean;
  begin
    Result := (Param.PointerDepth <> 0) or (Param.IsInsurance) or (IsParamFinal);
  end;
begin
  // value, data value, insurance, registers, stack
  Invokable := Pointer(FBuffer.FBytes);
  InternalInitParams(Invokable^);

  // align advanced buffer
  if (FAdvanced.Size and (SizeOf(Pointer) - 1) <> 0) then
  begin
    FAdvanced.Alloc(SizeOf(Pointer) - (FAdvanced.Size and (SizeOf(Pointer) - 1)));
  end;

  // initial params
  Invokable := Pointer(FBuffer.FBytes);
  Invokable.Initials := Pointer(FBuffer.Size + FAdvanced.Size);
  Param := @Invokable.Params[0];
  for i := 0 to Invokable.ParamCount - 1 do
  begin
    if (IsParamInitial) then
    begin
      PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(Param) - NativeInt(Invokable);
      Inc(Invokable.InitialCount);
    end;
    if (Param.IsArray) then
      Inc(Param);

    Inc(Param);
  end;
  Param := @Invokable.Result;
  if (Invokable.ResultKind in [rkBuffer, rkManagedBuffer]) or (IsParamHFAManaged) then
  begin
    PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(Param) - NativeInt(Invokable);
    Inc(Invokable.InitialCount);
  end;

  // final params
  Param := @Invokable.Params[0];
  Invokable.Finals := Pointer(FBuffer.Size + FAdvanced.Size);
  for i := 0 to Invokable.ParamCount - 1 do
  begin
    if (IsParamFinal) then
    begin
      PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(Param) - NativeInt(Invokable);
      Inc(Invokable.FinalCount);
    end;
    if (Param.IsArray) then
      Inc(Param);

    Inc(Param);
  end;
  Param := @Invokable.Result;
  if (Invokable.ResultKind = rkManagedBuffer) or (IsParamHFAManaged) then
  begin
    PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(Param) - NativeInt(Invokable);
    Inc(Invokable.FinalCount);
  end;

  // concatenate buffers, unpack internal names
  Offset := FBuffer.Size;
  System.Move(Pointer(FAdvanced.FBytes)^, FBuffer.Alloc(FAdvanced.Size)^, FAdvanced.Size);
  Invokable := Pointer(FBuffer.FBytes);
  Param := @Invokable.Params[0];
  Offset := Offset{Last FBuffer.Size} + NativeInt(Invokable);
  for i := 0 to Invokable.ParamCount - 1 do
  begin
    if (NativeUInt(Param.Name) <= $ffff) then
      Inc(NativeInt(Param.Name), Offset);

    if (Param.IsArray) then
      Inc(Param){"High(Array)"};

    Inc(Param);
  end;

  // hash comparison
  // ToDo

  // result
  Result := TLuaMemoryHeap(FLua.FMemoryHeap).Alloc(FBuffer.Size);
  Invokable := TLuaMemoryHeap(FLua.FMemoryHeap).Unpack(Result);
  System.Move(Pointer(FBuffer.FBytes)^, Invokable^, FBuffer.Size);
  Inc(NativeInt(Invokable.Initials), NativeInt(Invokable));
  Inc(NativeInt(Invokable.Finals), NativeInt(Invokable));

  // result names (internal)
  Param := @Invokable.Params[0];
  Offset := NativeInt(Invokable) - NativeInt(Pointer(FBuffer.FBytes));
  for i := 0 to Invokable.ParamCount - 1 do
  begin
    if (NativeUInt(Param.Name) >= NativeUInt(FBuffer.FBytes)) and
      (NativeUInt(Param.Name) < NativeUInt(FBuffer.FBytes) + NativeUInt(FBuffer.Size)) then
      Inc(NativeInt(Param.Name), Offset);

    if (Param.IsArray) then
      Inc(Param){"High(Array)"};

    Inc(Param);
  end;
end;

function TLuaInvokableBuilder.BuildMethod(const AMethod: PTypeInfo; MethodKind: TLuaMethodKind): __luapointer;
type
  TTypeRefs = array[0..0] of PPTypeInfo;

  TMethodInfo = record
    CC: TCallConv;
    ResultTypeName: PShortString;
    ResultTypeInfo: PTypeInfo;
    ParamTypeRefs: ^TTypeRefs;
  end;

  TMethodParam = record
    Flags: TParamFlags;
    Name: PShortString;
    TypeName: PShortString;
    TypeInfo: PTypeInfo;
  end;

var
  Ptr: PByte;
  TypeData: PTypeData;
  ParamCount, i: Integer;
  MethodInfo: TMethodInfo;
  Param: TMethodParam;

  procedure ReadParam(const Index: Integer);
  begin
    Param.Flags := TParamFlags(Pointer(Ptr)^);
    Inc(Ptr, SizeOf(TParamFlags));
    Param.Name := Pointer(Ptr);
    Ptr := GetTail(Ptr^);
    Param.TypeName := Pointer(Ptr);
    Ptr := GetTail(Ptr^);
    if (Index >= 0) then
    begin
      Param.TypeInfo := GetTypeInfo(MethodInfo.ParamTypeRefs[Index]);
    end;
  end;
begin
  Result := LUA_POINTER_INVALID;
  if (MethodKind = TLuaMethodKind($ff)) then
  begin
    MethodKind := mkInstance;
  end;

  // skip parameters, fill method information
  TypeData := GetTypeData(AMethod);
  ParamCount := TypeData.ParamCount;
  Ptr := Pointer(@TypeData.ParamList);
  for i := 0 to ParamCount - 1 do
  begin
    ReadParam(-1);
  end;
  if (TypeData.MethodKind = mkFunction) then
  begin
    MethodInfo.ResultTypeName := Pointer(Ptr);
    Ptr := GetTail(Ptr^);
    MethodInfo.ResultTypeInfo := GetTypeInfo(PPointer(Ptr)^);
    Inc(Ptr, SizeOf(Pointer));
  end else
  begin
    MethodInfo.ResultTypeName := nil;
    MethodInfo.ResultTypeInfo := nil;
  end;
  MethodInfo.CC := TCallConv(Pointer(Ptr)^);
  Inc(Ptr, SizeOf(TCallConv));
  MethodInfo.ParamTypeRefs := Pointer(Ptr);

  // initialization
  if (not Assigned(NewInvokable(MethodKind, MethodInfo.CC, MethodInfo.ResultTypeInfo))) then
    Exit;

  // parameters
  Ptr := Pointer(@TypeData.ParamList);
  for i := 0 to ParamCount - 1 do
  begin
    ReadParam(i);
    if (InternalAddParam(Param.Name, Param.TypeInfo, 0, Param.Flags) = nil) then
      Exit;
  end;

  // done
  Result := InternalBuildDone;
end;

{$ifdef EXTENDEDRTTI}
function TLuaInvokableBuilder.BuildProcedure(const AProcedure: PTypeInfo; MethodKind: TLuaMethodKind): __luapointer;
var
  i: Integer;
  CallConv: TCallConv;
  ProcSig: PProcedureSignature;
  ResultType: PTypeInfo;
  Param: PProcedureParam;
  TypeInfo: PTypeInfo;
  ParamFlags: TParamFlags;
begin
  Result := LUA_POINTER_INVALID;
  ProcSig := GetTypeData(AProcedure).ProcSig;
  if (not Assigned(ProcSig) or (ProcSig.Flags = 255)) then
  begin
    // error todo
    Exit;
  end;

  if (MethodKind = TLuaMethodKind($ff)) then
  begin
    MethodKind := mkStatic;
  end;
  CallConv := ProcSig.CC;
  ResultType := GetTypeInfo(ProcSig.ResultType);
  if (not Assigned(NewInvokable(MethodKind, CallConv, ResultType))) then
    Exit;

  // parameters
  Param := Pointer(NativeUInt(ProcSig) + SizeOf(TProcedureSignature));
  for i := 0 to Integer(ProcSig.ParamCount) - 1 do
  begin
    TypeInfo := GetTypeInfo(Param.ParamType);
    ParamFlags := TParamFlags(Param.Flags);
    if (InternalAddParam(Pointer(@Param.Name), TypeInfo, 0, ParamFlags) = nil) then
      Exit;

    Param := SkipAttributes(GetTail(Param.Name));
  end;

  // done
  Result := InternalBuildDone;
end;
{$endif}

function TLuaInvokableBuilder.BuildReference(const AReference: PTypeInfo; MethodKind: TLuaMethodKind): __luapointer;
var
  TypeData: PTypeData;
  Table: PIntfMethodTable;
  MethodEntry: PIntfMethodEntry;
begin
  TypeData := GetTypeData(AReference);
  Table := GetTail(TypeData.IntfUnit);
  MethodEntry := Pointer(NativeUInt(Table) + SizeOf(TIntfMethodTable));

  Result := BuildInterfaceMethod(MethodEntry, MethodKind);
end;

function TLuaInvokableBuilder.BuildInterfaceMethod(const AMethodEntry: PIntfMethodEntry;
  MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
type
  TMethodInfo = record
    IsFunc: Boolean;
    CC: TCallConv;
    ResultTypeName: PShortString;
    ResultTypeInfo: PTypeInfo;
  end;

  TMethodParam = record
    Flags: TParamFlags;
    Name: PShortString;
    TypeName: PShortString;
    TypeInfo: PTypeInfo;
  end;
var
  Ptr: PByte;
  ParamCount, i: Integer;
  MethodInfo: TMethodInfo;
  Param: TMethodParam;

  procedure ReadParam;
  begin
    Param.Flags := TParamFlags(Ptr^);
    Inc(Ptr);
    Param.Name := Pointer(Ptr);
    Ptr := GetTail(Ptr^);
    Param.TypeName := Pointer(Ptr);
    Ptr := GetTail(Ptr^);
    Param.TypeInfo := GetTypeInfo(PPointer(Ptr)^);
    Inc(Ptr, SizeOf(Pointer));
    Ptr := SkipAttributes(Ptr);
  end;
begin
  Result := LUA_POINTER_INVALID;
  if (MethodKind = TLuaMethodKind($ff)) then
  begin
    MethodKind := mkInstance;
  end;

  // skip parameters, fill method information
  Ptr := GetTail(AMethodEntry.Name);
  MethodInfo.IsFunc := (Ptr^ = 1);
  Inc(Ptr);
  MethodInfo.CC := TCallConv(Ptr^);
  Inc(Ptr);
  ParamCount := Ptr^;
  Inc(Ptr);
  MethodInfo.ResultTypeName := nil;
  MethodInfo.ResultTypeInfo := nil;
  if (MethodInfo.IsFunc) then
  begin
    ReadParam;
    for i := 1 to ParamCount - 1 do
    begin
      ReadParam;
    end;
    if (Ptr^ <> 0) then
    begin
      MethodInfo.ResultTypeName := Pointer(Ptr);
      Ptr := GetTail(Ptr^);
      MethodInfo.ResultTypeInfo := GetTypeInfo(PPointer(Ptr)^);
    end;
  end;

  if (not Assigned(NewInvokable(MethodKind, MethodInfo.CC, MethodInfo.ResultTypeInfo))) then
    Exit;

  // parameters
  Ptr := GetTail(AMethodEntry.Name);
  Inc(Ptr, SizeOf(Byte) + SizeOf(TCallConv) + SizeOf(Byte));
  ReadParam;
  for i := 1 to ParamCount - 1 do
  begin
    ReadParam;

    if (InternalAddParam(Pointer(@Param.Name), Param.TypeInfo, 0, Param.Flags) = nil) then
      Exit;
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildCustom(const Params: array of TLuaProcParam;
  const ResultType: PTypeInfo; const MethodKind: TLuaMethodKind; const CallConv: TCallConv): __luapointer;
var
  i: Integer;
begin
  Result := LUA_POINTER_INVALID;
  if (not Assigned(NewInvokable(MethodKind, CallConv, ResultType))) then
    Exit;

  for i := Low(Params) to High(Params) do
  if (not Assigned(InternalAddParam(Params[i]))) then
    Exit;

  Result := InternalBuildDone;
end;


{ Name space management structures  }

const
  {$ifdef SMALLINT}
    PROPSLOT_MASK    = $FF000000;
    PROPSLOT_FIELD   = $FF000000;
    PROPSLOT_VIRTUAL = $FE000000;
  {$else .LARGEINT}
    PROPSLOT_MASK    = $FF00000000000000;
    PROPSLOT_FIELD   = $FF00000000000000;
    PROPSLOT_VIRTUAL = $FE00000000000000;
  {$endif}
  PROPSLOT_CLEAR = (not PROPSLOT_MASK);

  PROP_SLOT_MASK = 7;
  PROP_SLOTSETTER_SHIFT = 3;
  PROP_CLASS_MODE = 1 shl 6;
  PROP_CONSTREF_MODE = 1 shl 7;

  NAMESPACE_FLAG_PROC = 1;
  NAMESPACE_FLAG_INHERITED = 2;
  NAMESPACE_FLAGS_CLEAR = -4;

type
  TLuaPropertyMode = (pmNone, pmField, pmStatic, pmStaticIndex, pmStaticParams,
    pmVirtual, pmVirtualIndex, pmVirtualParams);

  TLuaProperty = object(TLuaCustomParam)
  private
    {
      Flags:
      GetterMode: TLuaPropertyMode:3;
      SetterMode: TLuaPropertyMode:3;
      ClassMode: Boolean:1;
      ConstMode: Boolean:1;
    }
    FIndex: packed record
    case Integer of
      0: (Value: Integer);
      1: (ParamsPtr: __luapointer);
    end;
  public
    Getter: NativeUInt;
    Setter: NativeUInt;

    property Index: Integer read FIndex.Value write FIndex.Value;
    property ParamsPtr: __luapointer read FIndex.ParamsPtr write FIndex.ParamsPtr;
  end;
  PLuaProperty = ^TLuaProperty;

  // global proc, class/record method
  TLuaMethod = packed record
    Address: Pointer;
    MethodKind: TLuaMethodKind;
    InvokableMode: Boolean;
    Reserved: array[0..1] of Byte;
  case Integer of
    0: (Invokable: __luapointer{PLuaInvokable});
    1: (ArgsCount: Integer{Universal});
  end;
  PLuaMethod = ^TLuaMethod;

  // global entity
  TLuaGlobalKind = (gkMetaType, gkVariable, gkProc, gkConst{Script}, gkScriptVariable);
  TLuaGlobalEntity = record
    Kind: TLuaGlobalKind;
    ConstMode: Boolean; // True means can't be changed by native Variables[] property
    case Integer of
      0: (Ptr: __luapointer);
      1: (Ref: Integer); // script entity LUA_GLOBALSINDEX index
  end;
  PLuaGlobalEntity = ^TLuaGlobalEntity;

           (*
{ TLuaPropertyInfo }

const
  PROP_NONE_USE = pointer($80000000);
  TMETHOD_CLASS_INDEX = 1;
  TLUA_REFERENCE_CLASS_INDEX = 2;

  MODE_NONE_USE = integer(PROP_NONE_USE);
  MODE_PROC_USE = -1;


function IsTypeInfo_Boolean(const tpinfo: ptypeinfo): boolean;
begin
  Result := (tpinfo <> nil) and
  ((tpinfo = typeinfo(boolean)) or (tpinfo = typeinfo(bytebool)) or
   (tpinfo = typeinfo(wordbool)) or (tpinfo = typeinfo(longbool)) );
end;

function GetOrdinalTypeName(const tpinfo: ptypeinfo): string;
const
  INT_STRS: array[TOrdType] of string = ('shortint', 'byte', 'smallint', 'word', 'integer', 'dword');
var
  TypeData: ptypedata;
begin
  TypeData := GetTypeData(tpinfo);

  case (tpinfo.Kind) of
   tkInteger: Result := INT_STRS[TypeData.OrdType];
   tkEnumeration: if (IsTypeInfo_Boolean(tpinfo)) then
                  begin
                    case (TypeData.OrdType) of
                      otUByte: Result := 'boolean';
                      otSByte: Result := 'ByteBool';
                      otSWord, otUWord: Result := 'WordBool';
                      otSLong, otULong: Result := 'LongBool';
                    end;
                  end
                  else Result := tpinfo.Name;
  else
    Result := '';
  end;
end;


function GetLuaPropertyBase(const Lua: TLua; const Prefix, PropertyName: string; const tpinfo, CodeAddr: pointer; const auto_registrate: boolean=false): TLuaPropertyInfoBase;
var
  ClassIndex: integer;

  procedure ThrowUnknown();
  begin
    ELua.Assert('Can''t register "%s%s" because its type is unknown'#13+
                'typeinfo.Name = %s, typeinfo.Kind = %s',
                [Prefix, PropertyName, ptypeinfo(tpinfo).Name, TypeKindName(ptypeinfo(tpinfo).Kind)], CodeAddr);
  end;
begin
  Result.Information := tpinfo;
  Result.Kind := pkUnknown;
  Result.str_max_len := 0;
  byte(Result.OrdType) := 0;


  // зарегистрированные вещи
  ClassIndex := Lua.internal_class_index(tpinfo);
  if (ClassIndex >= 0) then
  begin
    with Lua.ClassesInfo[ClassIndex] do
    case _ClassKind of
      ckClass: begin
                 ELua.Assert('Can''t register "%s%s", because typeinfo is not correct - %s is defined', [Prefix, PropertyName, _ClassName], CodeAddr);
               end;
     ckRecord: begin
                 Result.Kind := pkRecord;
                 Result.Information := _Class;
               end;

      ckArray: begin
                 Result.Kind := pkArray;
                 Result.Information := _Class;
               end;

        ckSet: begin
                 Result.Kind := pkSet;
                 Result.Information := _Class;
               end;
    end;

    exit;
  end;


  if (tpinfo = typeinfoPointer) then
  begin
    Result.Kind := pkPointer;
    Result.Information := typeinfo(integer);
    exit;
  end;

  if (tpinfo = typeinfoTClass) then
  begin
    Result.Kind := pkClass;
    Result.Information := typeinfo(integer);
    exit;
  end;

  if (tpinfo = typeinfoUniversal) then
  begin
    Result.Kind := pkUniversal;
    Result.Information := typeinfo(TLuaArg);
    exit;
  end;


  if (IsTypeInfo_Boolean(tpinfo)) then
  begin
    Result.Kind := pkBoolean;

    case (GetTypeData(tpinfo).OrdType) of
      otUByte: Result.BoolType := btBoolean;
      otSByte: Result.BoolType := btByteBool;
      otSWord, otUWord: Result.BoolType := btWordBool;
      otSLong, otULong: Result.BoolType := btLongBool;
    end;

    exit;
  end;

  case ptypeinfo(tpinfo).Kind of
 {$ifdef fpc}tkBool: begin
                       Result.Kind := pkBoolean;
                       Result.BoolType := btBoolean; // todo проверить
                     end;
 {$endif}

          tkInteger: begin
                       Result.Kind := pkInteger;
                       Result.OrdType := GetTypeData(tpinfo).OrdType;
                     end;

      tkEnumeration: begin
                       Result.Kind := pkInteger;
                       Result.OrdType := GetTypeData(tpinfo).OrdType; 
                       if (auto_registrate) then Lua.RegEnum(tpinfo);
                     end;

{$ifdef fpc}tkQWord,{$endif}
            tkInt64: Result.Kind := pkInt64;

            tkFloat: begin
                       Result.Kind := pkFloat;
                       Result.FloatType := GetTypeData(tpinfo).FloatType;
                     end;

             tkChar: begin
                       Result.Kind := pkString;
                       Result.StringType := stAnsiChar;
                     end;

            tkWChar: begin
                       Result.Kind := pkString;
                       Result.StringType := stWideChar;
                     end;

           tkString: begin
                       Result.Kind := pkString;
                       Result.StringType := stShortString;
                       Result.str_max_len := GetTypeData(tpinfo).MaxLength;
                     end;

{$ifdef fpc}tkAString,{$endif}                     
          tkLString: begin
                       Result.Kind := pkString;
                       Result.StringType := stAnsiString;
                     end;

          tkWString: begin
                       Result.Kind := pkString;
                       Result.StringType := stWideString;
                     end;

          tkVariant: Result.Kind  := pkVariant;

        tkInterface: Result.Kind  := pkInterface;

            tkClass: Result.Kind  := pkObject;

           tkMethod: begin
                       Result.Kind := pkRecord;
                       Result.Information := Lua.ClassesInfo[TMETHOD_CLASS_INDEX]._Class;
                     end;

           tkRecord, {$ifdef fpc}tkObject,{$endif}
            tkArray,
         tkDynArray: begin
                       // Unregistered difficult type
                     end;

              tkSet: begin
                       // Unregistered difficult type
                       if (auto_registrate) then
                       begin
                         Result.Kind := pkSet;
                         Result.Information := Lua.ClassesInfo[Lua.InternalAddSet(tpinfo, CodeAddr)]._Class;
                       end;
                     end;
  end;

  if (Result.Kind = pkUnknown) then
  ThrowUnknown();
end;


// что-то типа InspectType
function GetLuaItemSize(const Base: TLuaPropertyInfoBase): integer;
begin
  Result := 0;
  if (Base.Information = nil) then exit;

  case Base.Kind of
   pkBoolean: case (Base.BoolType) of
                btBoolean: Result := sizeof(boolean);
                btWordBool: Result := sizeof(wordbool);
                btLongBool: Result := sizeof(longbool);
              end;

   pkInteger: case (Base.OrdType) of
                otSByte, otUByte: Result := sizeof(byte);
                otSWord, otUWord: Result := sizeof(word);
                otSLong, otULong: Result := sizeof(dword);
              end;

     pkFloat: case (Base.FloatType) of
                ftSingle: Result := sizeof(single);
                ftDouble: Result := sizeof(double);
                ftExtended: Result := sizeof(extended);
                ftComp: Result := sizeof(Comp);
                ftCurr: Result := sizeof(Currency);
              end;

    pkString: case (Base.StringType) of
                stShortString: Result := Base.str_max_len+1;

                {todo UnicodeString?,}
                stAnsiString,
                stWideString: Result := sizeof(pointer);

                stAnsiChar: Result := sizeof(AnsiChar);
                stWideChar: Result := sizeof(WideChar);
              end;

   pkVariant: Result := sizeof(Variant);
     pkInt64: Result := sizeof(int64);
    pkObject, pkInterface, pkPointer, pkClass: Result := sizeof(Pointer);
    pkRecord: Result := PLuaRecordInfo(Base.Information).FSize;
     pkArray: Result := PLuaArrayInfo(Base.Information).FSize;
       pkSet: Result := PLuaSetInfo(Base.Information).FSize;

       //pkUniversal? наверное не надо - ибо только в массивах
  end;          
end;

// имея проперти инфо, необходимо определить - является ли его тип сложным
// необходимо ли его инициализировать/финализировать
function GetLuaDifficultTypeInfo(const Base: TLuaPropertyInfoBase): ptypeinfo;
begin
  Result := nil;

  case Base.Kind of
    pkVariant: Result := typeinfo(Variant);
  pkInterface: Result := typeinfo(IInterface);
     pkRecord: Result := PLuaRecordInfo(Base.Information).FTypeInfo;
      pkArray: Result := PLuaArrayInfo(Base.Information).FTypeInfo;

     pkString: if (Base.StringType in [stAnsiString, stWideString {todo Unicode ?}]) then
               Result := Base.Information;
  end;
end;


// функция задумывалась как аналог TypInfo-функций для работы со свойствами, только более сложный вариант
// для свойств -структур/-массивов/-множеств
// сейчас функция получает свойство и делает ему push в стек Lua (user data)
procedure GetPushDifficultTypeProp(const Lua: TLua; const Instance: pointer; const IsConst: boolean; const Info: TLuaPropertyInfo);
type
  TGetterProc = procedure(const instance: pointer; var Result);
  TSimpleGetterProc = function(const instance: pointer): integer;
  TIndexedGetterProc = procedure(const instance: pointer; const index: integer; var Result);
  TIndexedSimpleGetterProc = function(const instance: pointer; const index: integer): integer;
var
  PValue: pointer;
  Value: dword absolute PValue;
  Data: pointer;
  IsSimple: boolean;
begin

  if (Info.read_mode >= 0) then
  begin
    // самый простой случай
    // Instance уже указывает на структуру/массив/множество 
    // в Lua будет отправлен только указатель (IsRef)
    Data := Instance;
  end else
  begin
    // получить через функцию и засунуть в ResultBuffer
    // сначала определяемся с адресом
    PValue := Info.PropInfo.GetProc;
    if (Value >= $FE000000) then PValue := pointer(pointer(dword(Instance^) + (Value and $00FFFFFF))^);

    // указатель на данные и признак простого результата (идёт в eax)
    case (Info.Base.Kind) of
      pkRecord: begin
                  Data := Lua.FResultBuffer.AllocRecord(PLuaRecordInfo(Info.Base.Information));

                  with PLuaRecordInfo(Info.Base.Information)^ do
                  IsSimple := (FSize <= 4{что если single?}) and (FTypeInfo = nil);
                end;
       pkArray: begin
                  Data := Lua.FResultBuffer.AllocArray(PLuaArrayInfo(Info.Base.Information));

                  with PLuaArrayInfo(Info.Base.Information)^ do
                  IsSimple := (FSize <= 4) and (FTypeInfo = nil);
                end;
    else
      Data := Lua.FResultBuffer.AllocSet(PLuaSetInfo(Info.Base.Information));

      with PLuaSetInfo(Info.Base.Information)^ do
      IsSimple := (FSize <= 4);
    end;

    // получить данные по функции
    // "в eax" или в структуру
    with Info.PropInfo^ do
    if (IsSimple) then
    begin
      if (Index = integer(PROP_NONE_USE)) then
        integer(Data^) := TSimpleGetterProc(PValue)(instance)
      else
        integer(Data^) := TIndexedSimpleGetterProc(PValue)(instance, Index);
    end else
    begin
      if (Index = integer(PROP_NONE_USE)) then
        TGetterProc(PValue)(Instance, Data^)
      else
        TIndexedGetterProc(PValue)(Instance, Index, Data^);
    end;
  end;


  {  финализация  }

  // TMethod
  if (Info.Base.Kind = pkRecord) and (PLuaRecordInfo(Info.Base.Information).FClassIndex = TMETHOD_CLASS_INDEX)
  and (pint64(Instance)^ = 0) then
  begin
    lua_pushnil(Lua.Handle);
    exit;
  end;

  // push-им значение
  Value := PLuaRecordInfo(Info.Base.Information).FClassIndex; // FClassIndex у всех по одному смещению!!!
  Lua.push_userdata(Lua.ClassesInfo[Value], (Info.read_mode < 0), Data).is_const := IsConst;
end;

// специальный калбек для универсальных свойств
//
// универсальные свойства характерны тем, что возвращают TLuaArg
// и кроме того содержат PropertyName в калбеке
procedure GetPushUniversalTypeProp(const Lua: TLua; Instance: pointer; const IsConst: boolean; const Info: TLuaPropertyInfo);
type
  TGetterProc = procedure(const instance: pointer; const PropertyName: string; var Result: TLuaArg);
  TIndexedGetterProc = procedure(const instance: pointer; const PropertyName: string; const index: integer; var Result: TLuaArg);

var
  PValue: pointer;
begin
  if (Info.read_mode >= 0) then
  begin
    // непосредственный указатель
    // ничего не делаем
  end else
  begin
    // через функцию
    PValue := Info.PropInfo.GetProc;
    if (dword(PValue) >= $FE000000) then PValue := pointer(pointer(dword(Instance^) + dword(PValue) and $00FFFFFF)^);

    // получить данные по функции
    with Info.PropInfo^ do
    begin
      if (Index = integer(PROP_NONE_USE)) then
        TGetterProc(PValue)(Instance, Info.PropertyName, Lua.FBufferArg)
      else
        TIndexedGetterProc(PValue)(Instance, Info.PropertyName, Index, Lua.FBufferArg);
    end;

    Instance := @Lua.FBufferArg;
  end;

  // пуш-им значение
  // если так вдруг получилось, что запушить не удалось
  // то пушим nil (хотя я не представляю ситуации когда push может не получиться)
  if (not Lua.push_luaarg(PLuaArg(Instance)^)) then
  begin
    lua_pushnil(Lua.Handle);
  end;
end;

// изменить универсальное свойство (через TLuaArg)
function PopSetUniversalTypeProp(const Lua: TLua; const instance: pointer; const stack_index: integer; const Info: TLuaPropertyInfo): boolean;
type
  TSetterProc = procedure(const instance: pointer; const PropertyName: string; const Value: TLuaArg);
  TIndexedSetterProc = procedure(const instance: pointer; const PropertyName: string; const index: integer; const Value: TLuaArg);

var
  PValue: pointer;
begin
  if (Info.write_mode >= 0) then
  begin
    Result := Lua.stack_luaarg(PLuaArg(instance)^, stack_index, false{?});
  end else
  begin
    // через функцию
    PValue := Info.PropInfo.SetProc;
    if (dword(PValue) >= $FE000000) then PValue := pointer(pointer(dword(Instance^) + dword(PValue) and $00FFFFFF)^);

    // получить значение
    Result := Lua.stack_luaarg(Lua.FBufferArg, stack_index, false{?});

    // вызвать сеттер данные по функции
    with Info.PropInfo^ do
    begin
      if (Index = integer(PROP_NONE_USE)) then
        TSetterProc(PValue)(Instance, Info.PropertyName, Lua.FBufferArg)
      else
        TIndexedSetterProc(PValue)(Instance, Info.PropertyName, Index, Lua.FBufferArg);
    end;
  end;
end;

function TLuaPropertyInfo.Description(): string;
const
  BOOL_STRS: array[TLuaPropBoolType] of string = ('boolean', 'ByteBool', 'WordBool', 'LongBool');
  FLOAT_STRS: array[TFloatType] of string = ('single', 'double', 'extended', 'Comp', 'currency');
  STRING_STRS: array[TLuaPropStringType] of string = ('string[%d]', 'AnsiString', 'WideString', {todo UnicodeString?,} 'AnsiChar', 'WideChar');
var
  i: integer;
  params, typename: string;
  Readable, Writable: boolean;
begin
  Result := PropertyName;

  // список параметров
  params := '';
  if (Parameters <> nil) then
  begin
    if (Parameters = INDEXED_PROPERTY) then params := 'index'
    else if (Parameters = NAMED_PROPERTY) then params := 'name'
    else
    with PLuaRecordInfo(Parameters).FLua.ClassesInfo[PLuaRecordInfo(Parameters).FClassIndex] do
    for i := 0 to Length(Properties)-1 do
    begin
      if (i <> 0) then params := params + ', ';
      params := params + Properties[i].PropertyName;
    end;
  end;
  if (params <> '') then Result := Result + '[' + params + ']';

  // имя типа
  case Base.Kind of
    pkUnknown: typename := 'ERROR';
  pkUniversal: typename := 'unknown';
    pkBoolean: typename := BOOL_STRS[Base.BoolType];
      pkInt64: typename := 'int64';
      pkFloat: typename := FLOAT_STRS[Base.FloatType];
     pkObject: typename := 'TObject';
     pkString: begin
                 typename := STRING_STRS[Base.StringType];
                 if (Base.StringType = stShortString) then typename := Format(typename, [Base.str_max_len]);
               end;
    pkVariant: typename := 'variant';
  pkInterface: typename := 'IInterface';
    pkPointer: typename := 'pointer';
      pkClass: typename := 'TClass';
      pkArray: typename := PLuaArrayInfo(Base.Information).Name;
        pkSet: typename := PLuaSetInfo(Base.Information).Name;
     pkRecord: with PLuaRecordInfo(Base.Information)^ do
               if (FClassIndex = TMETHOD_CLASS_INDEX) then typename := 'Event' else typename := Name;
  else
     // pkInteger {Enum}
     typename := GetOrdinalTypeName(PropInfo.PropType^);
  end;


  // финализация
  begin
    Readable := (read_mode <> MODE_NONE_USE);
    Writable := (write_mode <> MODE_NONE_USE);
    Result := Result + ': ' + typename + ' ';

    if (Readable and Writable) then Result := Result + 'R/W'
    else
    if (Readable) then Result := Result + 'R'
    else Result := Result + 'W';
  end;
end;   *)


{ TLuaUnit }

const
  BOM_INFO: array[TLuaScriptBOM] of
  record
    Data: Cardinal;
    Size: Cardinal;
  end = (
    (Data: $00000000; Size: 0 {ANSI}),
    (Data: $00BFBBEF; Size: 3 {UTF-8}),
    (Data: $0000FEFF; Size: 2 {UTF-16 LE}),
    (Data: $0000FFFE; Size: 2 {UTF-16 BE}),
    (Data: $0000FEFF; Size: 4 {UTF-32 LE}),
    (Data: $FFFE0000; Size: 4 {UTF-32 BE})
  );

function TLuaUnit.GetLine(Index: Integer): TLuaUnitLine;
begin
  Dec(Index);
  if (Cardinal(Index) >= Cardinal(FLinesCount)) then
    raise ELua.CreateFmt('Can''t get line %d from unit "%s". Lines count = %d', [Index + 1, Name, FLinesCount]);

  Result := FLines[Index];
end;

function TLuaUnit.GetItem(Index: Integer): LuaString;
var
  Line: TLuaUnitLine;
begin
  Line := Self.GetLine(Index);
  FLua.unpack_lua_string(Result, Line.Chars, Line.Count);
end;

procedure TLuaUnit.InitializeLines;
var
  Chars: PByte;
  X: NativeUInt;
  Index, Size: Integer;
begin
  // calculate lines count
  Index := 0;
  Size := Length(FData) - FDataOffset;
  NativeInt(Chars) := NativeInt(FData) + FDataOffset;
  if (Size > 0) then
  begin
    repeat
      if (Size = 0) then Break;
      X := Chars^;
      Inc(Chars);
      Dec(Size);

      if (X <= 13) then
      case X of
        10, 13:
        begin
          Inc(Index);
          if (Size > 0) and (Chars^ = X xor 7) then
          begin
            Inc(Chars);
            Dec(Size);
          end;
        end;
      end;
    until (False);
    Inc(Index);
  end;

  // fill lines
  FLinesCount := Index;
  SetLength(FLines, Index);
  Index := 0;
  Size := Length(FData) - FDataOffset;
  NativeInt(Chars) := NativeInt(FData) + FDataOffset;
  if (Size > 0) then
  begin
    FLines[Index].Chars := Pointer(Chars);
    repeat
      if (Size = 0) then Break;
      X := Chars^;
      Inc(Chars);
      Dec(Size);

      if (X <= 13) then
      case X of
        10, 13:
        begin
          FLines[Index].Count := NativeInt(Chars) - NativeInt(FLines[Index].Chars) - 1;
          Inc(Index);
          if (Size > 0) and (Chars^ = X xor 7) then
          begin
            Inc(Chars);
            Dec(Size);
          end;
          FLines[Index].Chars := Pointer(Chars);
        end;
      end;
    until (False);
    FLines[Index].Count := NativeInt(Chars) - NativeInt(FLines[Index].Chars);
  end;
end;

{$ifNdef LUA_NOCLASSES}
procedure TLuaUnit.SaveToStream(const Stream: TStream);
var
  Chars: PByte;
  Size: Integer;
begin
  Size := Length(FData) - FDataOffset;
  NativeInt(Chars) := NativeInt(FData) + FDataOffset;

  {$ifdef LUA_UNICODE}
  Stream.WriteBuffer(BOM_INFO[sbUTF8], 3);
  {$endif}

  if (Size > 0) then
    Stream.WriteBuffer(Chars^, Size);
end;
{$endif}

{$ifdef KOL}
procedure TLuaUnit.SaveToStream(const Stream: KOL.PStream);
var
  Chars: PByte;
  Size: Integer;
begin
  Size := Length(FData) - FDataOffset;
  NativeInt(Chars) := NativeInt(FData) + FDataOffset;

  {$ifdef LUA_UNICODE}
  Stream.WriteBuffer(BOM_INFO[sbUTF8], 3);
  {$endif}

  if (Size > 0) then
    Stream.WriteBuffer(Chars^, Size);
end;
{$endif}

procedure TLuaUnit.SaveToFile(const FileName: string);
var
  Chars: PByte;
  Size: Integer;
  Handle: THandle;
begin
  Size := Length(FData) - FDataOffset;
  NativeInt(Chars) := NativeInt(FData) + FDataOffset;

  Handle := FileCreate(FileName);
  if (Handle = INVALID_HANDLE_VALUE) then
  begin
    raise {$ifdef LUA_NOCLASSES}ELua{$else}EFCreateError{$endif}.CreateResFmt
      (Pointer(@SFCreateErrorEx), [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  end;

  try
    if {$ifdef LUA_UNICODE}(FileWrite(Handle, BOM_INFO[sbUTF8], 3) <> 3) or {$endif}
      ((Size > 0) and (FileWrite(Handle, Chars^, Size) <> Size)) then
    begin
      {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};
    end;
  finally
    FileClose(Handle);
  end;
end;

procedure TLuaUnit.SaveToFile;
begin
  if (FileName <> '') then
  begin
    SaveToFile(FileName);
  end else
  begin
    SaveToFile(string(Name));
  end;
end;


{ TLua }

const
  GLOBAL_NAME_SPACE: LuaString = 'GLOBAL_NAME_SPACE';
  INSTANCE_MODES: array[0..2] of string = ('instance', 'type', 'class');
  PROPERTY_MODES: array[0..2] of string = ('property', 'field', 'variable');

  // each userdata (except difficult properties)
  STD_TYPE = 0;
  STD_TYPE_NAME = 1;
  STD_TYPE_PARENT = 2;
  STD_INHERITS_FROM = 3;
  STD_ASSIGN = 4;
  STD_IS_REF = 5;
  STD_IS_CONST = 6;
  STD_IS_CLASS = 7;
  STD_IS_RECORD = 8;
  STD_IS_ARRAY = 9;
  STD_IS_SET = 10;
  STD_IS_EMPTY = 11;
  // classes
  STD_CREATE = 12;
  STD_FREE = 13;
  // arrays
  STD_LOW = 14;
  STD_HIGH = 15;
  STD_LENGTH = 16;
  STD_RESIZE = 17;
  // sets
  STD_INCLUDE = 18;
  STD_EXCLUDE = 19;
  STD_CONTAINS = 20;
  // TLuaReference
  STD_VALUE = 21;

  ID_TYPE: array[0..4] of Byte = (Ord('T'), Ord('y'), Ord('p'), Ord('e'), 0);
  ID_TYPENAME: array[0..8] of Byte = (Ord('T'), Ord('y'), Ord('p'), Ord('e'), Ord('N'), Ord('a'), Ord('m'), Ord('e'), 0);
  ID_TYPEPARENT: array[0..10] of Byte = (Ord('T'), Ord('y'), Ord('p'), Ord('e'), Ord('P'), Ord('a'), Ord('r'), Ord('e'), Ord('n'), Ord('t'), 0);
  ID_INHERITSFROM: array[0..12] of Byte = (Ord('I'), Ord('n'), Ord('h'), Ord('e'), Ord('r'), Ord('i'), Ord('t'), Ord('s'), Ord('F'), Ord('r'), Ord('o'), Ord('m'), 0);
  ID_ASSIGN: array[0..6] of Byte = (Ord('A'), Ord('s'), Ord('s'), Ord('i'), Ord('g'), Ord('n'), 0);
  ID_ISREF: array[0..5] of Byte = (Ord('I'), Ord('s'), Ord('R'), Ord('e'), Ord('f'), 0);
  ID_ISCONST: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('C'), Ord('o'), Ord('n'), Ord('s'), Ord('t'), 0);
  ID_ISCLASS: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('C'), Ord('l'), Ord('a'), Ord('s'), Ord('s'), 0);
  ID_ISRECORD: array[0..8] of Byte = (Ord('I'), Ord('s'), Ord('R'), Ord('e'), Ord('c'), Ord('o'), Ord('r'), Ord('d'), 0);
  ID_ISARRAY: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'), 0);
  ID_ISSET: array[0..5] of Byte = (Ord('I'), Ord('s'), Ord('S'), Ord('e'), Ord('t'), 0);
  ID_ISEMPTY: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('E'), Ord('m'), Ord('p'), Ord('t'), Ord('y'), 0);
  ID_CREATE: array[0..6] of Byte = (Ord('C'), Ord('r'), Ord('e'), Ord('a'), Ord('t'), Ord('e'), 0);
  ID_FREE: array[0..4] of Byte = (Ord('F'), Ord('r'), Ord('e'), Ord('e'), 0);
  ID_LOW: array[0..3] of Byte = (Ord('L'), Ord('o'), Ord('w'), 0);
  ID_HIGH: array[0..4] of Byte = (Ord('H'), Ord('i'), Ord('g'), Ord('h'), 0);
  ID_LENGTH: array[0..6] of Byte = (Ord('L'), Ord('e'), Ord('n'), Ord('g'), Ord('t'), Ord('h'), 0);
  ID_RESIZE: array[0..6] of Byte = (Ord('R'), Ord('e'), Ord('s'), Ord('i'), Ord('z'), Ord('e'), 0);
  ID_INCLUDE: array[0..7] of Byte = (Ord('I'), Ord('n'), Ord('c'), Ord('l'), Ord('u'), Ord('d'), Ord('e'), 0);
  ID_EXCLUDE: array[0..7] of Byte = (Ord('E'), Ord('x'), Ord('c'), Ord('l'), Ord('u'), Ord('d'), Ord('e'), 0);
  ID_CONTAINS: array[0..8] of Byte = (Ord('C'), Ord('o'), Ord('n'), Ord('t'), Ord('a'), Ord('i'), Ord('n'), Ord('s'), 0);
  ID_VALUE: array[0..5] of Byte = (Ord('V'), Ord('a'), Ord('l'), Ord('u'), Ord('e'), 0);

  ID_INDEX: array[0..7] of Byte = (Ord('_'), Ord('_'), Ord('i'), Ord('n'), Ord('d'), Ord('e'), Ord('x'), 0);
  ID_NEWINDEX: array[0..10] of Byte = (Ord('_'), Ord('_'), Ord('n'), Ord('e'), Ord('w'), Ord('i'), Ord('n'), Ord('d'), Ord('e'), Ord('x'), 0);
  ID_LEN: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('l'), Ord('e'), Ord('n'), 0);
  ID_CALL: array[0..6] of Byte = (Ord('_'), Ord('_'), Ord('c'), Ord('a'), Ord('l'), Ord('l'), 0);
  ID_GC: array[0..4] of Byte = (Ord('_'), Ord('_'), Ord('g'), Ord('c'), 0);
  ID_TOSTRING: array[0..10] of Byte = (Ord('_'), Ord('_'), Ord('t'), Ord('o'), Ord('s'), Ord('t'), Ord('r'), Ord('i'), Ord('n'), Ord('g'), 0);
  ID_UNM: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('u'), Ord('n'), Ord('m'), 0);
  ID_ADD: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('a'), Ord('d'), Ord('d'), 0);
  ID_SUB: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('s'), Ord('u'), Ord('b'), 0);
  ID_MUL: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('m'), Ord('u'), Ord('l'), 0);
  ID_DIV: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('d'), Ord('i'), Ord('v'), 0);
  ID_MOD: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('m'), Ord('o'), Ord('d'), 0);
  ID_POW: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('p'), Ord('o'), Ord('w'), 0);
  ID_EQ: array[0..4] of Byte = (Ord('_'), Ord('_'), Ord('e'), Ord('q'), 0);
  ID_LT: array[0..4] of Byte = (Ord('_'), Ord('_'), Ord('l'), Ord('t'), 0);
  ID_LE: array[0..4] of Byte = (Ord('_'), Ord('_'), Ord('l'), Ord('e'), 0);
  ID_CONCAT: array[0..8] of Byte = (Ord('_'), Ord('_'), Ord('c'), Ord('o'), Ord('n'), Ord('c'), Ord('a'), Ord('t'), 0);



                             (*
// низкий уровень. адрес функции lua.dll
function __TLuaGetProcAddress(const Self: TClass; const ProcName: pchar;
         const throw_exception: boolean; const ReturnAddr: pointer): pointer;
begin
  if (LoadLuaLibrary = 0) and (throw_exception) then
  ELua.Assert('Lua library not found'#13'"%s"', [LuaPath], ReturnAddr);

  // загрузить функции
  if (LuaLibrary = 0) then Result := nil
  else Result := {$ifdef NO_CRYSTAL}Windows{$else}SysUtilsEx{$endif}.GetProcAddress(LuaLibrary, ProcName);

  // если не найдена
  if (Result = nil) and (throw_exception) then
  ELua.Assert('Proc "%s" not found in library'#13'"%s"', [ProcName, LuaPath], ReturnAddr);
end;

class function TLua.GetProcAddress(const ProcName: pchar; const throw_exception: boolean = false): pointer;
asm
  push [esp]
  jmp __TLuaGetProcAddress
end;
                       *)
                           (*
procedure TMethodConstructor(var X; const Args: TLuaArgs);
var
  M: TMethod absolute X;
  i: integer;
begin
  for i := 0 to Length(Args)-1 do
  case i of
    0: M.Code := Args[0].ForcePointer;
    1: M.Data := Args[1].ForcePointer;
  else
    exit;
  end;
end;

const
  SIGNS: array[boolean] of integer = (-1, 1);

// фактически только сравнение на равенство
// сравнивается по аналогии с TPoint
procedure TMethodOperator(var _Result, _X1, _X2; const Kind: TLuaOperator);
var
  Result: integer absolute _Result;
  P1: TPoint absolute _X1;
  P2: TPoint absolute _X2;
begin
  if (P1.X <> P2.X) then Result := SIGNS[P1.X > P2.X]
  else
  if (P1.Y <> P2.Y) then Result := SIGNS[P1.Y > P2.Y]
  else
  Result := 0;
end;  
                *)


constructor TLua.Create();
const
  STANDARD_NAMES: array[STD_TYPE..STD_VALUE] of Pointer = (
    @ID_TYPE, @ID_TYPENAME, @ID_TYPEPARENT, @ID_INHERITSFROM, @ID_ASSIGN, @ID_ISREF,
    @ID_ISCONST, @ID_ISCLASS, @ID_ISRECORD, @ID_ISARRAY, @ID_ISSET, @ID_ISEMPTY,
    @ID_CREATE, @ID_FREE, @ID_LOW, @ID_HIGH, @ID_LENGTH, @ID_RESIZE, @ID_INCLUDE,
    @ID_EXCLUDE, @ID_CONTAINS, @ID_VALUE);

  // function _FORMATWRAPPER_(...) return string.format(...)  end
  FORMATWRAPPER_CODE: array[0..59] of Byte = (Ord('f'), Ord('u'), Ord('n'), Ord('c'), Ord('t'), Ord('i'), Ord('o'), Ord('n'),
    Ord(' '), Ord('_'), Ord('F'), Ord('O'), Ord('R'), Ord('M'), Ord('A'), Ord('T'), Ord('W'), Ord('R'), Ord('A'), Ord('P'),
    Ord('P'), Ord('E'), Ord('R'), Ord('_'), Ord('('), Ord('.'), Ord('.'), Ord('.'), Ord(')'), Ord(' '), Ord('r'), Ord('e'),
    Ord('t'), Ord('u'), Ord('r'), Ord('n'), Ord(' '), Ord('s'), Ord('t'), Ord('r'), Ord('i'), Ord('n'), Ord('g'), Ord('.'),
    Ord('f'), Ord('o'), Ord('r'), Ord('m'), Ord('a'), Ord('t'), Ord('('), Ord('.'), Ord('.'), Ord('.'), Ord(')'), Ord(' '),
    Ord(' '), Ord('e'), Ord('n'), Ord('d'));
var
  i: Integer;

  procedure AddSystemClosure(const Name: LuaString; const Callback: Pointer; const P1, P2: __luapointer);
  begin
    lua_pushnil(Handle);
    push_lua_string(Name);
    push_newfunction(Callback, P1, P2, False);
    __global_newindex(0, 0);
    lua_settop(Handle, 0);
    with PLuaGlobalEntity(GetGlobalEntity(Name, True))^ do
    begin
      Kind := gkConst;
      ConstMode := True;
    end;
  end;
begin
  // unicode
  for i := 0 to 127 do
  begin
    FUnicodeTable[i] := i;
    FUTF8Table[i] := i + $01000000;
  end;
  SetCodePage(0);

  // containers
  TLuaInvokableBuilder(FInvokableBuilder).FLua := Self;
  TLuaNames(FNames).FLua := Self;

  // Lua initialization
  if (not InitializeLua) then
    raise ELua.CreateFmt('Lua library was not initialized: "%s"', [LuaPath]);
  FHandle := lua_open;
  luaL_openlibs(Handle);

  // standard names
  for i := Low(STANDARD_NAMES) to High(STANDARD_NAMES) do
  begin
    lua_pushlstring(Handle, STANDARD_NAMES[i], LStrLen(STANDARD_NAMES[i]));
    TLuaDictionary(FStandardNames).Add(lua_tolstring(Handle, -1, nil), i);
    global_fill_value(global_alloc_ref);
  end;

  // global callbacks
  FGlobalMetaTable := InternalRegisterMetaTable;
  global_push_value(FGlobalMetaTable);
  InternalRegisterCallback(Pointer(@ID_INDEX), @TLua.__global_index, 0, 0);
  InternalRegisterCallback(Pointer(@ID_NEWINDEX), @TLua.__global_newindex, 0, 0);
  lua_settop(Handle, 0);

  // internal string.format() wrapper
  luaL_loadbuffer(Handle, Pointer(@FORMATWRAPPER_CODE), SizeOf(FORMATWRAPPER_CODE), nil);
  lua_pcall(Handle, 0, 0, 0);
  lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
  with TLuaDictionary(FGlobalEntities) do
    FFormatWrapper := PLuaGlobalEntity(TLuaMemoryHeap(FMemoryHeap).Unpack(FItems[Count - 1].Value)).Ref;

  // print(f) wrappers
  AddSystemClosure('_PNT_', @TLua.__print, 0, 0);
  AddSystemClosure('printf', @TLua.__printf, 0, 0);


  (*
  // метатаблица для сложных свойств
  mt_properties := internal_register_metatable(nil);

  // TObject
  InternalAddClass(TObject, false, nil);

  // TMethod: структура для хранения событий

  FMethodInfo := !!!....

  with RegRecord('TMethod', pointer(sizeof(TMethod)))^, TMethod(nil^) do
  begin
    RegField('Code', @Code, typeinfoPointer);
    RegField('Data', @Data, typeinfoPointer);
    RegProc(LUA_CONSTRUCTOR, LuaClassProc(TMethodConstructor));
    Operators := [loCompare];
    OperatorCallback := TMethodOperator;
  end;

  // TLuaReference
  InternalAddClass(TLuaReference, false, nil);   *)
end;

// деструктор
destructor TLua.Destroy;
var
  i: Integer;
  MetaType: PLuaMetaType;
begin
  // Lua
  if (FHandle <> nil) then lua_close(FHandle);

 (* // внутренние данные
  FArgs := nil;
  if (FHandle <> nil) then lua_close(FHandle);
  FResultBuffer.Finalize(true);
  DeleteCFunctionDumps(Self);

  // массив ссылок
  for i := 0 to Length(FReferences)-1 do FReferences[i].FreeInstance();
  FReferences := nil;

  // подчистить данные
  GlobalNative.Cleanup();
  for i := 0 to Length(ClassesInfo)-1 do ClassesInfo[i].Cleanup();

  // чанки
  for i := 0 to Length(FUnits)-1 do FUnits[i].Free;
  FUnits := nil;
  FUnitsCount := 0; *)

  // units
  for i := 0 to FUnitCount - 1 do
  begin
    FUnits[i].Free;
  end;
  FUnits := nil;

  // script stack
  Pointer(FArgs) := nil;
  FArgsCount := 0;
  for i := Low(FScriptStack) to High(FScriptStack) do
    FScriptStack[i].Finalize;

  // finalize names anddynamic arrays
  for i := 0 to TLuaDictionary(FMetaTypes).Count - 1 do
  begin
    MetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(TLuaDictionary(FMetaTypes).FItems[i].Value);
    MetaType.FName := '';
    case MetaType.Kind of
      mtRecord, mtClass:
      begin
        TLuaDictionary(PLuaRecordInfo(MetaType).FNameSpace).Clear;
      end;
    end;
  end;

  // containers
  TLuaStack(FEmptyRefs).Clear;
  TLuaMemoryHeap(FMemoryHeap).Clear;
  TLuaBuffer(FInternalBuffer).Clear;
  TLuaInvokableBuilder(FInvokableBuilder).Clear;
  TLuaDictionary(FStandardNames).Clear;
  TLuaNames(FNames).Clear;
  TLuaDictionary(FMetaTypes).Clear;
  TLuaDictionary(FGlobalEntities).Clear;
  {$ifdef LUA_NATIVEFUNC}
  TLuaCFunctionHeap(FCFunctionHeap).Clear;
  {$endif}
//  TLuaStack(FEmptyMethods).Clear;
//  TLuaStack(FEmptyProperties).Clear;


  inherited;
end;

procedure TLua.SetCodePage(Value: Word);
var
  i, X, Y: Integer;
  Dest, Src, TopSrc: Pointer;
  Buffer: array[128..255] of AnsiChar;
begin
  // code page
  if (Value = 0) then Value := CODEPAGE_DEFAULT;
  FCodePage := Value;

  // unicode table (128..255)
  if (Value = $ffff) then
  begin
    Dest := @FUnicodeTable[128];
    X := 128 + (129 shl 16);
    for i := 1 to (128 div (SizeOf(Integer) div SizeOf(WideChar))) do
    begin
      PInteger(Dest)^ := X;
      Inc(X, $00010001);
      Inc(NativeInt(Dest), SizeOf(Integer));
    end;
  end else
  begin
    Dest := @Buffer;
    X := 128 + (129 shl 8) + (130 shl 16) + (131 shl 24);
    for i := 1 to (128 div SizeOf(Integer)) do
    begin
      PInteger(Dest)^ := X;
      Inc(X, $01010101);
      Inc(NativeInt(Dest), SizeOf(Integer));
    end;

    {$ifdef MSWINDOWS}
      MultiByteToWideChar(Value, 0, Pointer(@Buffer), 128, Pointer(@FUnicodeTable[128]), 128);
    {$else}
      UnicodeFromLocaleChars(Value, 0, Pointer(@Buffer), 128, Pointer(@FUnicodeTable[128]), 128);
    {$endif}
  end;

  // utf8 table (128..255)
  Src := @FUnicodeTable[128];
  TopSrc := @FUnicodeTable[High(FUnicodeTable)];
  Dest := Pointer(@FUTF8Table[128]);
  Dec(NativeInt(Src), SizeOf(WideChar));
  Dec(NativeInt(Dest), SizeOf(Cardinal));
  repeat
    if (Src = TopSrc) then Break;
    Inc(NativeInt(Src), SizeOf(WideChar));
    Inc(NativeInt(Dest), SizeOf(Cardinal));

    X := PWord(Src)^;
    if (X <= $7ff) then
    begin
      if (X > $7f) then
      begin
        Y := (X and $3f) shl 8;
        X := (X shr 6) + $020080c0;
        Inc(X, Y);
        PCardinal(Dest)^ := X;
      end else
      begin
        X := X + $01000000;
        PCardinal(Dest)^ := X;
      end;
    end else
    begin
      Y := ((X and $3f) shl 16) + ((X and ($3f shl 6)) shl (8-6));
      X := (X shr 12) + $038080E0;
      Inc(X, Y);
      PCardinal(Dest)^ := X;
    end;
  until (False);
end;

function TLua.AnsiFromUnicode(ADest: PAnsiChar; ACodePage: Word; ASource: PWideChar; ALength: Integer): Integer;
const
  CHARS_PER_ITERATION = SizeOf(Integer) div SizeOf(WideChar);
var
  Dest: PAnsiChar;
  Source: PWideChar;
  Count, X: Integer;
begin
  Count := ALength;
  Dest := ADest;
  Source := ASource;

  if (Count >= CHARS_PER_ITERATION) then
  repeat
    X := PInteger(Source)^;
    if (X and $ff80ff80 <> 0) then Break;

    Inc(X, X shr 8);
    Dec(Count, CHARS_PER_ITERATION);
    PWord(Dest)^ := X;
    Inc(Source, CHARS_PER_ITERATION);
    Inc(Dest, CHARS_PER_ITERATION);
  until (Count < CHARS_PER_ITERATION);

  if (Count <> 0) then
  begin
    X := PWord(Source)^;
    if (X and $ff80 = 0) then
    begin
      PByte(Dest)^ := X;
      Dec(Count);
      Inc(Source);
      Inc(Dest);
    end;

    if (Count <> 0) then
    Inc(Dest,
      {$ifdef MSWINDOWS}
        WideCharToMultiByte(ACodePage, 0, Source, Count, Pointer(Dest), Count, nil, nil)
      {$else}
        LocaleCharsFromUnicode(ACodePage, 0, Source, Count, Pointer(Dest), Count, nil, nil)
      {$endif} );
  end;

  Result := NativeInt(Dest) - NativeInt(ADest);
end;

procedure TLua.UnicodeFromAnsi(ADest: PWideChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer);
const
  CHARS_PER_ITERATION = SizeOf(Integer) div SizeOf(AnsiChar);
type
  TUnicodeTable = array[Byte] of Word;
var
  Dest: PWideChar;
  Source: PAnsiChar;
  Count, X: Integer;
  UnicodeTable: ^TUnicodeTable;
begin
  if (ACodePage = 0) then ACodePage := CODEPAGE_DEFAULT;
  if (ACodePage <> FCodePage) then SetCodePage(ACodePage);

  Count := ALength;
  Dest := ADest;
  Source := ASource;
  UnicodeTable := Pointer(@FUnicodeTable);

  if (Count >= CHARS_PER_ITERATION) then
  repeat
    X := PInteger(Source)^;
    if (X and $8080 = 0) then
    begin
      if (X and $80808080 = 0) then
      begin
        PCardinal(Dest)^ := Byte(X) + (X and $ff00) shl 8;
        X := X shr 16;
        Dec(Count, 2);
        Inc(Source, 2);
        Inc(Dest, 2);
      end;

      PCardinal(Dest)^ := Byte(X) + (X and $ff00) shl 8;
      Dec(Count, 2);
      Inc(Source, 2);
      Inc(Dest, 2);

      if (Count < CHARS_PER_ITERATION) then Break;
    end else
    begin
      X := Byte(X);
      {$ifdef CPUX86}if (X > $7f) then{$endif} X := UnicodeTable[X];
      PWord(Dest)^ := X;

      Dec(Count);
      Inc(Source);
      Inc(Dest);
      if (Count < CHARS_PER_ITERATION) then Break;
    end;
  until (False);

  if (Count <> 0) then
  repeat
    X := PByte(Source)^;
    {$ifdef CPUX86}if (X > $7f) then{$endif} X := UnicodeTable[X];
    PWord(Dest)^ := X;

    Dec(Count);
    Inc(Source);
    Inc(Dest);
  until (Count = 0);
end;

function Utf8FromUnicode(ADest: PAnsiChar; ASource: PWideChar; ALength: Integer): Integer;
label
  process4, look_first, process_standard, process_character, unknown,
  small_length, done;
const
  MASK_FF80_SMALL = $FF80FF80;
  MASK_FF80_LARGE = $FF80FF80FF80FF80;
  UNKNOWN_CHARACTER = Ord('?');
var
  X, U, Count: NativeUInt;
  Dest: PAnsiChar;
  Source: PWideChar;
{$ifdef CPUX86}
const
  MASK_FF80 = MASK_FF80_SMALL;
{$else .CPUX64/.CPUARM}
var
  MASK_FF80: NativeUInt;
{$endif}
begin
  Count := ALength;
  Dest := ADest;
  Source := ASource;

  if (Count = 0) then goto done;
  Inc(Count, Count);
  Inc(Count, NativeUInt(Source));
  Dec(Count, (2 * SizeOf(Cardinal)));

  {$ifNdef CPUX86}
  MASK_FF80 := {$ifdef LARGEINT}MASK_FF80_LARGE{$else}MASK_FF80_SMALL{$endif};
  {$endif}

  // conversion loop
  if (NativeUInt(Source) > Count{TopSource}) then goto small_length;
  {$ifdef SMALLINT}
    X := PCardinal(@Source[0])^;
    U := PCardinal(@Source[2])^;
    if ((X or U) and Integer(MASK_FF80) = 0) then
  {$else}
    X := PNativeUInt(Source)^;
    if (X and MASK_FF80 = 0) then
  {$endif}
  begin
    repeat
    process4:
      {$ifdef LARGEINT}
      U := X shr 32;
      {$endif}
      X := X + (X shr 8);
      U := U + (U shr 8);
      X := Word(X);
      {$ifdef LARGEINT}
      U := Word(U);
      {$endif}
      U := U shl 16;
      Inc(X, U);

      Inc(Source, SizeOf(Cardinal));
      PCardinal(Dest)^ := X;
      Inc(Dest, SizeOf(Cardinal));

      if (NativeUInt(Source) > Count{TopSource}) then goto small_length;
    {$ifdef SMALLINT}
      X := PCardinal(@Source[0])^;
      U := PCardinal(@Source[2])^;
    until ((X or U) and Integer(MASK_FF80) <> 0);
    {$else}
      X := PNativeUInt(Source)^;
    until (X and MASK_FF80 <> 0);
    {$endif}
    goto look_first;
  end else
  begin
  look_first:
    {$ifdef LARGEINT}
    X := Cardinal(X);
    {$endif}
  process_standard:
    Inc(Source);
    U := Word(X);
    if (X and $FF80 = 0) then
    begin
      if (X and MASK_FF80 = 0) then
      begin
        // ascii_2
        X := X shr 8;
        Inc(Source);
        Inc(X, U);
        PWord(Dest)^ := X;
        Inc(Dest, 2);
      end else
      begin
        // ascii_1
        PByte(Dest)^ := X;
        Inc(Dest);
      end;
    end else
    if (U < $d800) then
    begin
    process_character:
      if (U <= $7ff) then
      begin
        if (U > $7f) then
        begin
          X := (U shr 6) + $80C0;
          U := (U and $3f) shl 8;
          Inc(X, U);
          PWord(Dest)^ := X;
          Inc(Dest, 2);
        end else
        begin
          PByte(Dest)^ := U;
          Inc(Dest);
        end;
      end else
      begin
        X := (U and $0fc0) shl 2;
        Inc(X, (U and $3f) shl 16);
        U := (U shr 12);
        Inc(X, $8080E0);
        Inc(X, U);

        PWord(Dest)^ := X;
        Inc(Dest, 2);
        X := X shr 16;
        PByte(Dest)^ := X;
        Inc(Dest);
      end;
    end else
    begin
      if (U >= $e000) then goto process_character;
      if (U >= $dc00) then
      begin
      unknown:
        PByte(Dest)^ := UNKNOWN_CHARACTER;
        Inc(Dest);
      end else
      begin
        Inc(Source);
        X := X shr 16;
        Dec(U, $d800);
        Dec(X, $dc00);
        if (X >= ($e000-$dc00)) then goto unknown;

        U := U shl 10;
        Inc(X, $10000);
        Inc(X, U);

        U := (X and $3f) shl 24;
        U := U + ((X and $0fc0) shl 10);
        U := U + (X shr 18);
        X := (X shr 4) and $3f00;
        Inc(U, Integer($808080F0));
        Inc(X, U);

        PCardinal(Dest)^ := X;
        Inc(Dest, 4);
      end;
    end;
  end;

  if (NativeUInt(Source) > Count{TopSource}) then goto small_length;
  {$ifdef SMALLINT}
    X := PCardinal(@Source[0])^;
    U := PCardinal(@Source[2])^;
    if ((X or U) and Integer(MASK_FF80) = 0) then goto process4;
  {$else}
    X := PNativeUInt(Source)^;
    if (X and MASK_FF80 = 0) then goto process4;
  {$endif}
  goto look_first;

small_length:
  U := Count{TopSource} + (2 * SizeOf(Cardinal));
  if (U = NativeUInt(Source)) then goto done;
  Dec(U, NativeUInt(Source));
  if (U >= SizeOf(Cardinal)) then
  begin
    X := PCardinal(Source)^;
    goto process_standard;
  end;
  U := PWord(Source)^;
  Inc(Source);
  if (U < $d800) then goto process_character;
  if (U >= $e000) then goto process_character;
  if (U >= $dc00) then goto unknown;

  PByte(Dest)^ := UNKNOWN_CHARACTER;
  Inc(Dest);

  // result
done:
  Result := NativeInt(Dest) - NativeInt(ADest);
end;

function UnicodeFromUtf8(ADest: PWideChar; ASource: PAnsiChar; ALength: Integer): Integer;
label
  process4, look_first, process1_3,
  ascii_1, ascii_2, ascii_3,
  process_standard, process_character, unknown,
  next_iteration, small_length, done;
const
  MASK_80_SMALL = $80808080;
  MAX_UTF8CHAR_SIZE = 6;
  UNKNOWN_CHARACTER = Ord('?');
var
  X, U, Count: NativeUInt;
  Dest: PWideChar;
  Source: PAnsiChar;
{$ifdef CPUINTEL}
const
  MASK_80 = MASK_80_SMALL;
{$else .CPUARM}
var
  MASK_80: NativeUInt;
{$endif}
begin
  Count := ALength;
  Dest := ADest;
  Source := ASource;

  if (Count = 0) then goto done;
  Inc(Count, NativeUInt(Source));
  Dec(Count, MAX_UTF8CHAR_SIZE);

  {$ifdef CPUARM}
    MASK_80 := MASK_80_SMALL;
  {$endif}

  // conversion loop
  if (NativeUInt(Source) > Count{TopSource}) then goto small_length;
  X := PCardinal(Source)^;
  if (X and Integer(MASK_80) = 0) then
  begin
    repeat
    process4:
      Inc(Source, SizeOf(Cardinal));

      {$ifNdef LARGEINT}
        PCardinal(Dest)^ := (X and $7f) + ((X and $7f00) shl 8);
        X := X shr 16;
        Inc(Dest, 2);
        PCardinal(Dest)^ := (X and $7f) + ((X and $7f00) shl 8);
        Inc(Dest, 2);
      {$else}
        PNativeUInt(Dest)^ := (X and $7f) + ((X and $7f00) shl 8) +
          ((X and $7f0000) shl 16) + ((X and $7f000000) shl 24);
        Inc(NativeUInt(Dest), SizeOf(NativeUInt));
      {$endif}

      if (NativeUInt(Source) > Count{TopSource}) then goto small_length;
      X := PCardinal(Source)^;
    until (X and Integer(MASK_80) <> 0);
    goto look_first;
  end else
  begin
  look_first:
    if (X and $80 = 0) then
    begin
    process1_3:
      {$ifNdef LARGEINT}
        PCardinal(Dest)^ := (X and $7f) + ((X and $7f00) shl 8);
        Inc(Dest, 2);
        PCardinal(Dest)^ := ((X shr 16) and $7f);
      {$else}
        PNativeUInt(Dest)^ := (X and $7f) + ((X and $7f00) shl 8) +
          ((X and $7f0000) shl 16);
      {$endif}

      if (X and $8000 <> 0) then goto ascii_1;
      if (X and $800000 <> 0) then goto ascii_2;
      ascii_3:
        Inc(Source, 3);
        Inc(Dest, 3{$ifdef SMALLINT}- 2{$endif});
        goto next_iteration;
      ascii_2:
        X := X shr 16;
        Inc(Source, 2);
        {$ifdef LARGEINT}Inc(Dest, 2);{$endif}
        if (UTF8CHAR_SIZE[Byte(X)] <= 2) then goto process_standard;
        goto next_iteration;
      ascii_1:
        X := X shr 8;
        Inc(Source);
        Inc(Dest, 1{$ifdef SMALLINT}- 2{$endif});
        if (UTF8CHAR_SIZE[Byte(X)] <= 3) then goto process_standard;
        // goto next_iteration;
    end else
    begin
    process_standard:
      if (X and $C0E0 = $80C0) then
      begin
        X := ((X and $1F) shl 6) + ((X shr 8) and $3F);
        Inc(Source, 2);

      process_character:
        PWord(Dest)^ := X;
        Inc(Dest);
      end else
      begin
        U := UTF8CHAR_SIZE[Byte(X)];
        case (U) of
          1:
          begin
            X := X and $7f;
            Inc(Source);
            PWord(Dest)^ := X;
            Inc(Dest);
          end;
          3:
          begin
            if (X and $C0C000 = $808000) then
            begin
              U := (X and $0F) shl 12;
              U := U + (X shr 16) and $3F;
              X := (X and $3F00) shr 2;
              Inc(Source, 3);
              Inc(X, U);
              if (U shr 11 = $1B) then X := $fffd;
              PWord(Dest)^ := X;
              Inc(Dest);
              goto next_iteration;
            end;
            goto unknown;
          end;
          4:
          begin
            if (X and $C0C0C000 = $80808000) then
            begin
              U := (X and $07) shl 18;
              U := U + (X and $3f00) shl 4;
              U := U + (X shr 10) and $0fc0;
              X := (X shr 24) and $3f;
              Inc(X, U);

              U := (X - $10000) shr 10 + $d800;
              X := (X - $10000) and $3ff + $dc00;
              X := (X shl 16) + U;

              PCardinal(Dest)^ := X;
              Inc(Dest, 2);
              goto next_iteration;
            end;
            goto unknown;
          end;
        else
        unknown:
          PWord(Dest)^ := UNKNOWN_CHARACTER;
          Inc(Dest);
          Inc(Source, U);
          Inc(Source, NativeUInt(U = 0));
        end;
      end;
    end;
  end;

next_iteration:
  if (NativeUInt(Source) > Count{TopSource}) then goto small_length;
  X := PCardinal(Source)^;
  if (X and Integer(MASK_80) = 0) then goto process4;
  if (X and $80 = 0) then goto process1_3;
  goto process_standard;

small_length:
  U := Count{TopSource} + MAX_UTF8CHAR_SIZE;
  if (U = NativeUInt(Source)) then goto done;
  X := PByte(Source)^;
  if (X <= $7f) then
  begin
    PWord(Dest)^ := X;
    Inc(Source);
    Inc(Dest);
    if (NativeUInt(Source) <> Count{TopSource}) then goto small_length;
    goto done;
  end;
  X := UTF8CHAR_SIZE[X];
  Dec(U, NativeUInt(Source));
  if (X{char size} > U{available source length}) then
  begin
    PWord(Dest)^ := UNKNOWN_CHARACTER;
    Inc(Dest);
    goto done;
  end;

  case X{char size} of
    2:
    begin
      X := PWord(Source)^;
      goto process_standard;
    end;
    3:
    begin
      Inc(Source, 2);
      X := Byte(Source^);
      Dec(Source, 2);
      X := (X shl 16) or PWord(Source)^;
      goto process_standard;
    end;
  else
    // 4..5
    goto unknown;
  end;

  // result
done:
  Result := (NativeInt(Dest) - NativeInt(ADest)) shr 1;
end;

function TLua.Utf8FromAnsi(ADest: PAnsiChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer): Integer;
label
  process4, look_first, process1_3,
  ascii_1, ascii_2, ascii_3,
  process_not_ascii,
  small_4, small_3, small_2, small_1,
  small_length, done;
const
  MASK_80_SMALL = $80808080;
type
  TUTF8Table = array[Byte] of Cardinal;
var
  {$ifdef CPUX86}
  Store: record
    TopSource: NativeUInt;
  end;
  {$endif}

  X, U, Count: NativeUInt;
  Dest: PAnsiChar;
  Source: PAnsiChar;
  {$ifNdef CPUX86}
  TopSource: NativeUInt;
  {$endif}
  UTF8Table: ^TUTF8Table;

{$ifdef CPUINTEL}
const
  MASK_80 = MASK_80_SMALL;
{$else .CPUARM}
var
  MASK_80: NativeUInt;
{$endif}
begin
  if (ACodePage = 0) then ACodePage := CODEPAGE_DEFAULT;
  if (ACodePage <> FCodePage) then SetCodePage(ACodePage);

  Count := ALength;
  Dest := ADest;
  Source := ASource;

  if (Count = 0) then goto done;
  Inc(Count, NativeUInt(Source));
  Dec(Count, SizeOf(Cardinal));
  {$ifdef CPUX86}Store.{$endif}TopSource := Count;
  UTF8Table := Pointer(@FUTF8Table);

  {$ifdef CPUARM}
    MASK_80 := MASK_80_SMALL;
  {$endif}

  // conversion loop
  if (NativeUInt(Source) > {$ifdef CPUX86}Store.{$endif}TopSource) then goto small_length;
  X := PCardinal(Source)^;
  if (X and Integer(MASK_80) = 0) then
  begin
    repeat
    process4:
      Inc(Source, SizeOf(Cardinal));
      PCardinal(Dest)^ := X;
      Inc(Dest, SizeOf(Cardinal));

      if (NativeUInt(Source) > {$ifdef CPUX86}Store.{$endif}TopSource) then goto small_length;
      X := PCardinal(Source)^;
    until (X and Integer(MASK_80) <> 0);
    goto look_first;
  end else
  begin
  look_first:
    if (X and $80 = 0) then
    begin
    process1_3:
      PCardinal(Dest)^ := X;
      if (X and $8000 <> 0) then goto ascii_1;
      if (X and $800000 <> 0) then goto ascii_2;
      ascii_3:
        X := X shr 24;
        Inc(Source, 3);
        Inc(Dest, 3);
        goto small_1;
      ascii_2:
        X := X shr 16;
        Inc(Source, 2);
        Inc(Dest, 2);
        goto small_2;
      ascii_1:
        X := X shr 8;
        Inc(Source);
        Inc(Dest);
        goto small_3;
    end else
    begin
    process_not_ascii:
      if (X and $8000 = 0) then goto small_1;
      if (X and $800000 = 0) then goto small_2;
      if (X and $80000000 = 0) then goto small_3;

      small_4:
        U := UTF8Table[Byte(X)];
        X := X shr 8;
        Inc(Source);
        PCardinal(Dest)^ := U;
        U := U shr 24;
        Inc(Dest, U);
      small_3:
        U := UTF8Table[Byte(X)];
        X := X shr 8;
        Inc(Source);
        PCardinal(Dest)^ := U;
        U := U shr 24;
        Inc(Dest, U);
      small_2:
        U := UTF8Table[Byte(X)];
        X := X shr 8;
        Inc(Source);
        PCardinal(Dest)^ := U;
        U := U shr 24;
        Inc(Dest, U);
      small_1:
        U := UTF8Table[Byte(X)];
        Inc(Source);
        X := U;
        PWord(Dest)^ := U;
        U := U shr 24;
        Inc(Dest, U);
        if (X >= (3 shl 24)) then
        begin
          Dec(Dest);
          X := X shr 16;
          PByte(Dest)^ := X;
          Inc(Dest);
        end;
    end;
  end;

  if (NativeUInt(Source) > {$ifdef CPUX86}Store.{$endif}TopSource) then goto small_length;
  X := PCardinal(Source)^;
  if (X and Integer(MASK_80) = 0) then goto process4;
  if (X and $80 = 0) then goto process1_3;
  goto process_not_ascii;

small_length:
  case (NativeUInt(Source) - {$ifdef CPUX86}Store.{$endif}TopSource) of
   3{1}: begin
           X := PByte(Source)^;
           goto small_1;
         end;
   2{2}: begin
           X := PWord(Source)^;
           goto small_2;
         end;
   1{3}: begin
           Inc(Source, 2);
           X := Byte(Source^);
           Dec(Source, 2);
           X := (X shl 16) or PWord(Source)^;
           goto small_3;
         end;
  end;

  // result
done:
  Result := NativeUInt(Dest) - NativeUInt(ADest);
end;


type
  TUnicodeConvertDesc = record
    CodePage: Word;
    Dest: Pointer;
    Source: Pointer;
    Count: NativeUInt;
  end;

function InternalAnsiFromUtf8(const ConvertDesc: TUnicodeConvertDesc): Integer;
label
  ascii, ascii_write, unicode_write, non_ascii, unknown;
type
  TUnicodeTable = array[Byte] of Word;
  PUnicodeTable = ^TUnicodeTable;
const
  MASK_80_SMALL = $80808080;
  UNKNOWN_CHARACTER = Ord('?');
  BUFFER_SIZE = 1024;
var
  X, U, Count: NativeUInt;
  Dest: PAnsiChar;
  Source: PAnsiChar;
  BufferDest: PWideChar;
  BufferCount: NativeUInt;
  Buffer: array[0..BUFFER_SIZE + 4] of WideChar;
  Stored: record
    CodePage: Word;
    X: NativeUInt;
    Dest: Pointer;
  end;
{$ifdef CPUINTEL}
const
  MASK_80 = MASK_80_SMALL;
{$else .CPUARM}
var
  MASK_80: NativeUInt;
{$endif}
begin
  Stored.CodePage := ConvertDesc.CodePage;
  Dest := ConvertDesc.Dest;
  Source := ConvertDesc.Source;
  Count := ConvertDesc.Count;
  Stored.Dest := Dest;

  BufferDest := @Buffer[0];
  {$ifdef CPUARM}
    MASK_80 := MASK_80_SMALL;
  {$endif}

  repeat
    if (Count = 0) then Break;
    if (Count >= SizeOf(Cardinal)) then
    begin
      X := PCardinal(Source)^;
      if (X and $80 = 0) then
      begin
      ascii:
        if (BufferDest = @Buffer[0]) then
        begin
        ascii_write:
          PCardinal(Dest)^ := X;
          if (X and MASK_80 = 0) then
          begin
            Dec(Count, SizeOf(Cardinal));
            Inc(Source, SizeOf(Cardinal));
            Inc(Dest, SizeOf(Cardinal));
          end else
          begin
            U := Byte(X and $8080 = 0);
            X := Byte(X and $808080 = 0);
            Inc(X, U);
            Dec(Count);
            Inc(Source);
            Inc(Dest);
            Dec(Count, X);
            Inc(Source, X);
            Inc(Dest, X);
            if (Count = 0) then Break;
            X := PByte(Source)^;
            goto non_ascii;
          end;
        end else
        begin
        unicode_write:
          Stored.X := X;
          begin
            BufferCount := (NativeUInt(BufferDest) - NativeUInt(@Buffer[0])) shr 1;
            Inc(Dest,
            {$ifdef MSWINDOWS}
              WideCharToMultiByte(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
            {$else}
              LocaleCharsFromUnicode(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
            {$endif} );
            BufferDest := @Buffer[0];
          end;
          X := Stored.X;
          if (X <> NativeUInt(-1)) then goto ascii_write;
        end;
      end else
      begin
        goto non_ascii;
      end;
    end else
    begin
      X := PByte(Source)^;
      Inc(X, $ffffff00);
      if (X and $80 = 0) then goto ascii;
    non_ascii:
      U := UTF8CHAR_SIZE[Byte(X)];
      if (U <= Count) then
      begin
        case U of
          2:
          begin
            X := PWord(Source)^;
            Dec(Count, 2);
            Inc(Source, 2);
            if (X and $C0E0 <> $80C0) then goto unknown;

            X := ((X and $1F) shl 6) + ((X shr 8) and $3F);
          end;
          3:
          begin
            Inc(Source, SizeOf(Word));
            X := PByte(Source)^;
            Dec(Source, SizeOf(Word));
            X := X shl 16;
            Inc(X, PWord(Source)^);
            Dec(Count, 3);
            Inc(Source, 3);
            if (X and $C0C000 <> $808000) then goto unknown;

            U := (X and $0F) shl 12;
            U := U + (X shr 16) and $3F;
            X := (X and $3F00) shr 2;
            Inc(X, U);
            if (U shr 11 = $1B) then goto unknown;
          end;
        else
          Inc(U, Byte(U = 0));
          Dec(Count, U);
          Inc(Source, U);
        unknown:
          X := UNKNOWN_CHARACTER;
        end;

        PWord(BufferDest)^ := X;
        Inc(BufferDest);
        X := NativeUInt(-1);
        if (NativeUInt(BufferDest) >= NativeUInt(@Buffer[BUFFER_SIZE])) then goto unicode_write;
      end else
      begin
        if (BufferDest <> @Buffer[0]) then
        begin
          PWord(BufferDest)^ := UNKNOWN_CHARACTER;
          Inc(BufferDest);
        end else
        begin
          PByte(Dest)^ := UNKNOWN_CHARACTER;
          Inc(Dest);
        end;
        Break;
      end;
    end;
  until (False);

  // last chars
  if (BufferDest <> @Buffer[0]) then
  begin
    BufferCount := (NativeUInt(BufferDest) - NativeUInt(@Buffer[0])) shr 1;
    Inc(Dest,
    {$ifdef MSWINDOWS}
      WideCharToMultiByte(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
    {$else}
      LocaleCharsFromUnicode(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
    {$endif} );
  end;

  // result
  Result := NativeInt(Dest) - NativeInt(Stored.Dest);
end;

function TLua.AnsiFromUtf8(ADest: PAnsiChar; ACodePage: Word; ASource: PAnsiChar; ALength: Integer): Integer;
var
  ConvertDesc: TUnicodeConvertDesc;
begin
  if (ACodePage = 0) then ACodePage := CODEPAGE_DEFAULT;
  ConvertDesc.CodePage := ACodePage;
  ConvertDesc.Dest := ADest;
  ConvertDesc.Source := ASource;
  ConvertDesc.Count := ALength;

  Result := InternalAnsiFromUtf8(ConvertDesc);
end;

function InternalAnsiFromAnsi(const ConvertDesc: TUnicodeConvertDesc; const UnicodeTable: Pointer): Integer;
label
  ascii, ascii_write, unicode_write, non_ascii;
type
  TUnicodeTable = array[Byte] of Word;
  PUnicodeTable = ^TUnicodeTable;
const
  MASK_80_SMALL = $80808080;
  UNKNOWN_CHARACTER = Ord('?');
  BUFFER_SIZE = 1024;
var
  X, U, Count: NativeUInt;
  Dest: PAnsiChar;
  Source: PAnsiChar;
  BufferDest: PWideChar;
  BufferCount: NativeUInt;
  Buffer: array[0..BUFFER_SIZE + 4] of WideChar;
  Stored: record
    CodePage: Word;
    X: NativeUInt;
    Dest: Pointer;
  end;
{$ifdef CPUINTEL}
const
  MASK_80 = MASK_80_SMALL;
{$else .CPUARM}
var
  MASK_80: NativeUInt;
{$endif}
begin
  Stored.CodePage := ConvertDesc.CodePage;
  Dest := ConvertDesc.Dest;
  Source := ConvertDesc.Source;
  Count := ConvertDesc.Count;
  Stored.Dest := Dest;

  BufferDest := @Buffer[0];
  {$ifdef CPUARM}
    MASK_80 := MASK_80_SMALL;
  {$endif}

  repeat
    if (Count = 0) then Break;
    if (Count <= SizeOf(Cardinal)) then
    begin
      X := PCardinal(Source)^;
      if (X and $80 = 0) then
      begin
      ascii:
        if (BufferDest = @Buffer[0]) then
        begin
        ascii_write:
          PCardinal(Dest)^ := X;
          if (X and MASK_80 = 0) then
          begin
            Dec(Count, SizeOf(Cardinal));
            Inc(Source, SizeOf(Cardinal));
            Inc(Dest, SizeOf(Cardinal));
          end else
          begin
            U := Byte(X and $8080 = 0);
            X := Byte(X and $808080 = 0);
            Inc(X, U);
            Dec(Count);
            Inc(Source);
            Inc(Dest);
            Dec(Count, X);
            Inc(Source, X);
            Inc(Dest, X);
            if (Count = 0) then Break;
            X := PByte(Source)^;
            goto non_ascii;
          end;
        end else
        begin
        unicode_write:
          Stored.X := X;
          begin
            BufferCount := (NativeUInt(BufferDest) - NativeUInt(@Buffer[0])) shr 1;
            Inc(Dest,
            {$ifdef MSWINDOWS}
              WideCharToMultiByte(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
            {$else}
              LocaleCharsFromUnicode(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
            {$endif} );
            BufferDest := @Buffer[0];
          end;
          X := Stored.X;
          if (X <> NativeUInt(-1)) then goto ascii_write;
        end;
      end else
      begin
        goto non_ascii;
      end;
    end else
    begin
      X := PByte(Source)^;
      Inc(X, $ffffff00);
      if (X and $80 = 0) then goto ascii;
    non_ascii:
      PWord(BufferDest)^ := PUnicodeTable(UnicodeTable)[Byte(X)];
      Inc(BufferDest);
      Inc(Source);
      Dec(Count);
      X := NativeUInt(-1);
      if (NativeUInt(BufferDest) >= NativeUInt(@Buffer[BUFFER_SIZE])) then goto unicode_write;
    end;
  until (False);

  // last chars
  if (BufferDest <> @Buffer[0]) then
  begin
    BufferCount := (NativeUInt(BufferDest) - NativeUInt(@Buffer[0])) shr 1;
    Inc(Dest,
    {$ifdef MSWINDOWS}
      WideCharToMultiByte(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
    {$else}
      LocaleCharsFromUnicode(Stored.CodePage, 0, Pointer(@Buffer[0]), BufferCount, Pointer(Dest), BufferCount, nil, nil)
    {$endif} );
  end;

  // result
  Result := NativeInt(Dest) - NativeInt(Stored.Dest);
end;

function TLua.AnsiFromAnsi(ADest: PAnsiChar; ADestCodePage: Word; ASource: PAnsiChar;
  ASourceCodePage: Word; ALength: Integer): Integer;
var
  ConvertDesc: TUnicodeConvertDesc;
  SourceCodePage: Word;
begin
  ConvertDesc.Dest := ADest;
  ConvertDesc.Source := ASource;
  ConvertDesc.Count := ALength;

  if (ADestCodePage = 0) then ADestCodePage := CODEPAGE_DEFAULT;
  SourceCodePage := ASourceCodePage;
  if (SourceCodePage = 0) then SourceCodePage := CODEPAGE_DEFAULT;

  if (ADestCodePage = SourceCodePage) then
  begin
    System.Move(ConvertDesc.Source^, ConvertDesc.Dest^, ConvertDesc.Count);
    Result := ConvertDesc.Count;
  end else
  begin
    ConvertDesc.CodePage := ADestCodePage;
    if (SourceCodePage <> FCodePage) then SetCodePage(SourceCodePage);
    Result := InternalAnsiFromAnsi(ConvertDesc, @Self.FUnicodeTable);
  end;
end;

procedure TLua.unpack_lua_string(var Result: LuaString; const RttiName: ShortString);
var
  Count: NativeInt;
  Chars: PLuaChar;
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN) or Defined(UNICODE)}
  Buffer: array[Byte] of LuaChar;
  {$ifend}
begin
  Count := PByte(@RttiName)^;
  Chars := Pointer(@RttiName[1]);

  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    {$ifdef UNICODE}
       Count := UnicodeFromUtf8(Pointer(@Buffer), Pointer(Chars), Count);
    {$else .ANSI}
       Count := UnicodeFromAnsi(Pointer(@Buffer), Pointer(Chars), 0, Count);
    {$endif}
    Chars := @Buffer[0];
  {$else .LUA_ANSI}
    {$ifdef UNICODE}
      Count := AnsiFromUtf8(Pointer(@Buffer), 0, Pointer(Chars), Count);
      Chars := @Buffer[0];
    {$else .ANSI}
      {none}
    {$endif}
  {$ifend}

  SetLength(Result, Count);
  System.Move(Chars^, Pointer(Result)^, Count * SizeOf(LuaChar));
end;

procedure TLua.unpack_lua_string(var Result: LuaString; const Chars: __luadata; const Count: Integer);
{$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
var
  Buffer: ^TLuaBuffer;
  Size: NativeInt;
{$ifend}
begin
  if (Pointer(Result) <> nil) then Result := '';
  if (Count = 0) then Exit;

  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    Buffer := @TLuaBuffer(FInternalBuffer);
    Size := Count * 2 + 2;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    Size{Count} := UnicodeFromUtf8(Pointer(Buffer.FBytes), Pointer(Chars), Count);
    SetLength(Result, Size{Count});
    System.Move(Pointer(Buffer.FBytes)^, Pointer(Result)^, Size{Count} * SizeOf(WideChar));
  {$else .LUA_ANSI}
    SetLength(Result, Count);
    System.Move(Pointer(Chars)^, Pointer(Result)^, Count);
  {$ifend}
end;

procedure TLua.unpack_lua_string(var Result: LuaString; const Name: __luaname);
begin
  unpack_lua_string(Result, Pointer(Name), LStrLen(Name));
end;

procedure TLua.unpack_lua_string(var Result: LuaString; const Buffer: __luabuffer);
begin
  unpack_lua_string(Result, Pointer(Buffer), Length(Buffer));
end;

procedure TLua.push_ansi_chars(const S: PAnsiChar; const CodePage: Word; const Count: Integer);
var
  Size: Integer;
  Buffer: ^TLuaBuffer;
begin
  if (Count <= 0) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  if (CodePage = CODEPAGE_UTF8) then
  begin
    push_utf8_chars(S, Count);
    Exit;
  end;

  Buffer := @TLuaBuffer(FInternalBuffer);
  {$ifdef LUA_ANSI}
    if (CodePage = 0) or (CodePage = CODEPAGE_DEFAULT) then
    begin
      lua_pushlstring(Handle, Pointer(S), Count);
    end else
    begin
      Size := Count + 1;
      if (Buffer.Capacity < Size) then
      begin
        Buffer.Size := 0;
        Buffer.Alloc(Size);
      end;

      lua_pushlstring(Handle, Pointer(Buffer.FBytes),
        AnsiFromAnsi(Pointer(Buffer.FBytes), 0, S, CodePage, Count) );
    end;
  {$else .LUA_UNICODE}
    Size := Count * 6 + 1;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    lua_pushlstring(Handle, Pointer(Buffer.FBytes),
      Utf8FromAnsi(Pointer(Buffer.FBytes), Pointer(S), CodePage, Count) );
  {$endif}
end;

procedure TLua.push_utf8_chars(const S: PAnsiChar; const Count: Integer);
{$ifdef LUA_ANSI}
var
  Size: Integer;
  Buffer: ^TLuaBuffer;
{$endif}
begin
  if (Count <= 0) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  {$ifdef LUA_ANSI}
    Buffer := @TLuaBuffer(FInternalBuffer);
    Size := Count + 1;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    lua_pushlstring(Handle, Pointer(Buffer.FBytes),
      AnsiFromUtf8(Pointer(Buffer.FBytes), 0, Pointer(S), Count) );
  {$else .LUA_UNICODE}
    lua_pushlstring(Handle, Pointer(S), Count);
  {$endif}
end;

procedure TLua.push_wide_chars(const S: PWideChar; const Count: Integer);
var
  Size: Integer;
  Buffer: ^TLuaBuffer;
begin
  if (Count <= 0) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  Buffer := @TLuaBuffer(FInternalBuffer);
  {$ifdef LUA_ANSI}
    Size := Count + 1;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    lua_pushlstring(Handle, Pointer(Buffer.FBytes),
      AnsiFromUnicode(Pointer(Buffer.FBytes), 0, Pointer(S), Count) );
  {$else .LUA_UNICODE}
    Size := Count * 3 + 1;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    lua_pushlstring(Handle, Pointer(Buffer.FBytes),
      Utf8FromUnicode(Pointer(Buffer.FBytes), Pointer(S), Count) );
  {$endif}
end;

{$ifNdef NEXTGEN}
procedure TLua.push_short_string(const S: ShortString);
begin
  push_ansi_chars(@S[1], 0, Length(S));
end;

procedure TLua.push_ansi_string(const S: AnsiString);
begin
  if (Pointer(S) = nil) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  push_ansi_chars(Pointer(S),
    {$ifdef INTERNALCODEPAGE}PWord(NativeInt(S) - ASTR_OFFSET_CODEPAGE)^{$else}0{$endif},
    PInteger(NativeInt(S) - SizeOf(Integer))^);
end;

procedure TLua.push_wide_string(const S: WideString);
begin
  if (Pointer(S) = nil) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

   push_wide_chars(Pointer(S), PInteger(NativeInt(S) - SizeOf(Integer))^ {$ifdef WIDE_STR_SHIFT}shr 1{$endif});
end;
{$endif}

procedure TLua.push_unicode_string(const S: UnicodeString);
{$ifdef UNICODE}
begin
  if (Pointer(S) = nil) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  push_wide_chars(Pointer(S), PInteger(NativeInt(S) - SizeOf(Integer))^);
end;
{$else .CPUX86}
asm
  jmp TLua.push_wide_string
end;
{$endif}

procedure TLua.push_lua_string(const S: LuaString);
{$ifdef INLINESUPPORTSIMPLE}
begin
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    {$ifdef UNICODE}
      push_unicode_string(S);
    {$else}
      push_wide_string(S);
    {$endif}
  {$else}
    push_ansi_string(S);
  {$ifend}
end;
{$else .CPUX86}
asm
  {$ifdef LUA_UNICODE}
    jmp push_wide_string
  {$else}
    jmp push_ansi_string
  {$endif}
end;
{$endif}

{$ifNdef NEXTGEN}
procedure TLua.stack_short_string(var S: ShortString; const StackIndex: Integer; const MaxLength: Integer);
var
  Chars: __luadata;
  Count: NativeInt;
  {$ifdef LUA_UNICODE}
  Size: NativeInt;
  Buffer: ^TLuaBuffer;
  {$endif}
begin
  Chars := lua_tolstring(Handle, StackIndex, Pointer(@Count));
  if (Count <> 0) then
  begin
    {$ifdef LUA_ANSI}
      FillShortString(S, Chars, Count, MaxLength);
    {$else .LUA_UNICODE}
      Buffer := @TLuaBuffer(FInternalBuffer);
      Size := Count + 1;
      if (Buffer.Capacity < Size) then
      begin
        Buffer.Size := 0;
        Buffer.Alloc(Size);
      end;

      FillShortString(S, Pointer(Buffer.FBytes),
        AnsiFromUtf8(Pointer(Buffer.FBytes), 0, Chars, Count),
        MaxLength);
    {$endif}
  end else
  begin
    PByte(@S)^ := 0;
  end;
end;

procedure TLua.stack_ansi_string(var S: AnsiString; const StackIndex: Integer; const CodePage: Word);
var
  Chars: __luadata;
  Count, Size: NativeInt;
  Buffer: ^TLuaBuffer;
begin
  if (Pointer(S) <> nil) then S := '';
  Chars := lua_tolstring(Handle, StackIndex, Pointer(@Count));
  if (Count = 0) then Exit;
  Buffer := @TLuaBuffer(FInternalBuffer);

  {$ifdef LUA_ANSI}
  if (CodePage <> 0) and (CodePage <> CODEPAGE_DEFAULT) then
  {$else .LUA_UNICODE}
  if (CodePage <> CODEPAGE_UTF8) then
  {$endif}
  begin
    Size := Count + 1;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    {$ifdef LUA_ANSI}
      Count := AnsiFromAnsi(Pointer(Buffer.FBytes), CodePage, Chars, 0, Count);
    {$else .LUA_UNICODE}
      Count := AnsiFromUtf8(Pointer(Buffer.FBytes), CodePage, Chars, Count);
    {$endif}

    Chars := Pointer(Buffer.FBytes);
  end;

  if (Count = 0) then Exit;
  SetLength(S, Count);
  {$ifdef INTERNALCODEPAGE}
    PWord(NativeInt(S) - ASTR_OFFSET_CODEPAGE)^ := CodePage;
  {$endif}
  System.Move(Chars^, Pointer(S)^, Count);
end;

procedure TLua.stack_wide_string(var S: WideString; const StackIndex: Integer);
var
  Chars: __luadata;
  Count: NativeInt;
  {$ifdef LUA_UNICODE}
  Size: NativeInt;
  Buffer: ^TLuaBuffer;
  {$endif}
begin
  if (Pointer(S) <> nil) then S := '';
  Chars := lua_tolstring(Handle, StackIndex, Pointer(@Count));
  if (Count = 0) then Exit;

  {$ifdef LUA_ANSI}
    SetLength(S, Count);
    UnicodeFromAnsi(Pointer(S), Chars, 0, Count);
  {$else .LUA_UNICODE}
    Buffer := @TLuaBuffer(FInternalBuffer);
    Size := Count * 2 + 2;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    Count := UnicodeFromUtf8(Pointer(Buffer.FBytes), Chars, Count);
    SetLength(S, Count);
    System.Move(Pointer(Buffer.FBytes)^, Pointer(S)^, Count * SizeOf(WideChar));
  {$endif}
end;
{$endif}

procedure TLua.stack_unicode_string(var S: UnicodeString; const StackIndex: Integer);
{$ifdef UNICODE}
var
  Chars: __luadata;
  Count: NativeInt;
  {$ifdef LUA_UNICODE}
  Size: NativeInt;
  Buffer: ^TLuaBuffer;
  {$endif}
begin
  if (Pointer(S) <> nil) then S := '';
  Chars := lua_tolstring(Handle, StackIndex, Pointer(@Count));
  if (Count = 0) then Exit;

  {$ifdef LUA_ANSI}
    SetLength(S, Count);
    UnicodeFromAnsi(Pointer(S), Chars, 0, Count);
  {$else .LUA_UNICODE}
    Buffer := @TLuaBuffer(FInternalBuffer);
    Size := Count * 2 + 2;
    if (Buffer.Capacity < Size) then
    begin
      Buffer.Size := 0;
      Buffer.Alloc(Size);
    end;

    Count := UnicodeFromUtf8(Pointer(Buffer.FBytes), Chars, Count);
    SetLength(S, Count);
    System.Move(Pointer(Buffer.FBytes)^, Pointer(S)^, Count * SizeOf(WideChar));
  {$endif}
end;
{$else .CPUX86}
asm
  jmp TLua.stack_wide_string
end;
{$endif}

procedure TLua.stack_lua_string(var S: LuaString; const StackIndex: Integer);
{$ifdef INLINESUPPORTSIMPLE}
begin
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    {$ifdef UNICODE}
      stack_unicode_string(S, StackIndex);
    {$else}
      stack_wide_string(S, StackIndex);
    {$endif}
  {$else}
    stack_ansi_string(S, StackIndex, 0);
  {$ifend}
end;
{$else .CPUX86}
asm
  {$ifdef LUA_UNICODE}
    jmp stack_wide_string
  {$else}
    push [esp]
    mov [esp + 4], 0
    jmp stack_ansi_string
  {$endif}
end;
{$endif}

procedure TLua.stack_force_unicode_string(var S: UnicodeString; const StackIndex: Integer;
  const ExtendedMode{ToDo}: Boolean);
const
  HEX_CHARS: array[0..15] of WideChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  TYPE_TEMPLATES: array[LUA_TTABLE..LUA_TUSERDATA] of UnicodeString = (
    'table: 01234567' {$ifdef LARGEINT}+ '89ABCDEF'{$endif},
    'function: 01234567' {$ifdef LARGEINT}+ '89ABCDEF'{$endif},
    'userdata: 01234567' {$ifdef LARGEINT}+ '89ABCDEF'{$endif}
  );
var
  j, LuaType: Integer;
  X: NativeUInt;
  C: PWideChar;
begin
  LuaType := lua_type(Handle, StackIndex);
  case LuaType of
    LUA_TNIL:
    begin
      S := 'nil';
    end;
    LUA_TBOOLEAN:
    begin
      if (lua_toboolean(Handle, StackIndex)) then
      begin
        S := 'true';
      end else
      begin
        S := 'false';
      end;
    end;
    LUA_TNUMBER, LUA_TSTRING:
    begin
      stack_unicode_string(S, StackIndex);
    end;
    LUA_TLIGHTUSERDATA, LUA_TTABLE, LUA_TFUNCTION, LUA_TUSERDATA:
    begin
      if (LuaType = LUA_TLIGHTUSERDATA) then LuaType := LUA_TUSERDATA;
      S := TYPE_TEMPLATES[LuaType];
      UniqueString(S);
      C := @S[Length(S)];
      X := NativeUInt(lua_topointer(Handle, StackIndex));
      for j := 1 to SizeOf(Pointer) * 2 do
      begin
        C^ := HEX_CHARS[X and 15];
        X := X shr 4;
        Dec(C);
      end;
    end;
  else
    S := 'no value';
  end;
end;

function TLua.push_userdata(const MetaType: PLuaMetaType; const Instance: Pointer; const DcDestroy: Boolean): Pointer{PLuaUserData};
begin
  Result := nil{ToDo};
end;
            (*
// основная функция пуша для сложных типов: объектов класса, структур, массивов и множеств
function  TLua.push_userdata(const ClassInfo: TLuaClassInfo; const gc_destroy: boolean; const Data: pointer): PLuaUserData;
var
  DataSize: integer;
  SimpleFill: boolean;
begin

  with ClassInfo do
  if (not gc_destroy) or (_ClassKind = ckClass) then
  begin
    // простой вариант. sizeof(Result^) = sizeof(TLuaUserData)
    Result := PLuaUserData(lua_newuserdata(Handle, sizeof(TLuaUserData)));
    Result.instance := Data;
    pinteger(@Result.kind)^ := 0; // все флаги

    case (_ClassKind) of
       ckArray: begin
                  Result.kind := ukArray;
                  Result.array_params := PLuaArrayInfo(_Class).Dimention shl 4;
                  Result.ArrayInfo := PLuaArrayInfo(_Class);
                end;
         ckSet: begin
                  Result.kind := ukSet;
                  Result.SetInfo := PLuaSetInfo(_Class);
                end;
    else
      Result.gc_destroy := gc_destroy; // может быть true при _ClassKind = ckClass
      Result.ClassIndex := _ClassIndex;
      { kind по умолчанию = ukInstance }
    end;
  end else
  begin
    // структура или массив и его "надо удалять"
    case (_ClassKind) of
      ckRecord: DataSize := PLuaRecordInfo(_Class).FSize;
       ckArray: DataSize := PLuaArrayInfo(_Class).FSize;
    else
      // ckSet
      DataSize := PLuaSetInfo(_Class).FSize;
    end;
    Result := PLuaUserData(lua_newuserdata(Handle, sizeof(TLuaUserData)+DataSize));
    Result.instance := pointer(integer(Result)+sizeof(TLuaUserData));
    pinteger(@Result.kind)^ := 0; // все флаги
    Result.gc_destroy := gc_destroy;

    // оптимизация для варианта, когда результат во внутреннем буфере
    if (Data = nil) then SimpleFill := true
    else
    if (Data = FResultBuffer.Memory) then
    begin
      SimpleFill := true;
      FResultBuffer.tpinfo := nil;
    end
    else
    SimpleFill := false;


    // флаги и копирование (кроме simple)
    case (_ClassKind) of
      ckRecord: begin
                  Result.kind := ukInstance;
                  Result.ClassIndex := _ClassIndex;

                  if (not SimpleFill) then
                  with PLuaRecordInfo(_Class)^ do
                  if (FTypeInfo = nil) then SimpleFill := true
                  else
                  begin
                    FillChar(Result.instance^, DataSize, #0);
                    CopyRecord(Result.instance, Data, FTypeInfo);
                  end;
                end;

       ckArray: begin
                  Result.kind := ukArray;
                  Result.array_params := PLuaArrayInfo(_Class).Dimention shl 4;
                  Result.ArrayInfo := _Class;

                  if (not SimpleFill) then
                  with PLuaArrayInfo(_Class)^ do
                  if (FTypeInfo = nil) then SimpleFill := true
                  else
                  begin
                    if (IsDynamic) then
                    begin
                      pinteger(Result.instance)^ := pinteger(Data)^;
                      if (pinteger(Result.instance)^ <> 0) then DynArrayAddRef(Result.instance^);
                    end else
                    begin
                      FillChar(Result.instance^, DataSize, #0);
                      CopyArray(Result.instance, Data, FTypeInfo, FItemsCount);
                    end;
                 end;
               end;
      else
        // ckSet
        Result.kind := ukSet;
        Result.SetInfo := _Class;
        SimpleFill := true;
    end;

    // обычное копирование
    if (SimpleFill) then
    begin
      if (Data <> nil) then Move(Data^, Result.instance^, DataSize)
      else FillChar(Result.instance^, DataSize, #0);
    end;
  end;


  // навесить метатаблицу
  lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassInfo.Ref); // global_push_value(Ref);
  lua_setmetatable(Handle, -2);
end;
        *)

function TLua.push_difficult_property(const PropertyInfo; const Instance: Pointer): Pointer{PLuaUserData};
begin
  Result := nil{ToDo};
end;
          (*
// запушить свойство
function  TLua.push_difficult_property(const Instance: pointer; const PropertyInfo: TLuaPropertyInfo): PLuaUserData;
var
  Size: integer;
  array_params: byte;
  gc_destroy: boolean;
  Parameters: PLuaRecordInfo;
begin
  // информация
  Parameters := PropertyInfo.Parameters;
  case (integer(Parameters)) of
    integer(INDEXED_PROPERTY): begin
                                 Size := sizeof(TLuaUserData) + sizeof(integer);
                                 gc_destroy := false;
                                 array_params := (1 shl 4);
                               end;
      integer(NAMED_PROPERTY): begin
                                 Size := sizeof(TLuaUserData) + sizeof(string);
                                 gc_destroy := true;
                                 array_params := (1 shl 4);
                               end;
  else
    Size := (sizeof(TLuaUserData) + Parameters.Size + 3) and (not 3);
    gc_destroy := (Parameters.FTypeInfo <> nil);

    array_params := Length(ClassesInfo[Parameters.FClassIndex].Properties);
    if (array_params > 15) then array_params := 15;
    array_params := (array_params shl 4);
  end;

  // заполнение
  Result := PLuaUserData(lua_newuserdata(Handle, Size));
  FillChar(Result^, Size, #0);
  Result.instance := Instance;
  Result.kind := ukProperty;
  Result.array_params := array_params;
  Result.gc_destroy := gc_destroy;
  Result.PropertyInfo := @PropertyInfo;

  // навесить метатаблицу
  lua_rawgeti(Handle, LUA_REGISTRYINDEX, mt_properties); // global_push_value(Ref);
  lua_setmetatable(Handle, -2);
end;   *)

function TLua.push_luaarg(const LuaArg: TLuaArg): Boolean;
type
  TIntToStr = procedure(const Value: Integer; var Result: string);
begin
  with LuaArg do
  case (LuaType) of
      ltEmpty: lua_pushnil(Handle);
    ltBoolean: lua_pushboolean(Handle, F.VBoolean);
    ltInteger: lua_pushinteger(Handle, F.VInteger);
     ltDouble: lua_pushnumber(Handle, F.VDouble);
     ltString: push_lua_string(FStringValue);
    ltPointer: lua_pushlightuserdata(Handle, F.VPointer);
  (*    ltClass: lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassesInfo[internal_class_index(pointer(Data[0]), true)].Ref);
     ltObject: begin
                 if (TClass(pointer(Data[0])^) = TLuaReference) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaReference(Data[0]).Index)
                 else
                 push_userdata(ClassesInfo[internal_class_index(TClass(pointer(Data[0])^), true)], false, pointer(Data[0]));
               end;
     ltRecord: begin
                 with PLuaRecord(@FLuaType)^, Info^ do
                 push_userdata(ClassesInfo[FClassIndex], not IsRef, Data).is_const := IsConst;
               end;
      ltArray: begin
                 with PLuaArray(@FLuaType)^, Info^ do
                 push_userdata(ClassesInfo[FClassIndex], not IsRef, Data).is_const := IsConst;
               end;
        ltSet: begin
                 with PLuaSet(@FLuaType)^, Info^ do
                 push_userdata(ClassesInfo[FClassIndex], not IsRef, Data).is_const := IsConst;
               end; *)
      ltTable: begin
                 FStringBuffer.Default := 'LuaTable';
                 Result := False;
                 Exit;
               end;
  else
    TIntToStr(@IntToStr)(Byte(F.LuaType), FStringBuffer.Default);
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TLua.push_variant(const Value: Variant): Boolean;
type
  TDateProc = procedure(const DateTime: TDateTime; var Result: string);
  TIntToStr = procedure(const Value: Integer; var Result: string);
var
  VType: Integer;
  PValue: Pointer;
begin
  // TVarType, Data
  VType := TVarData(Value).VType;
  PValue := @TVarData(Value).VWords[3];
  if (VType and varByRef <> 0) then
  begin
    VType := VType and (not varByRef);
    PValue := PPointer(PValue)^;
  end;

  // push
  case (VType) of
    varEmpty, varNull, varError{EmptyParam}: lua_pushnil(Handle);
    varSmallint: lua_pushinteger(Handle, PSmallInt(PValue)^);
    varInteger : lua_pushinteger(Handle, PInteger(PValue)^);
    varSingle  : lua_pushnumber(Handle, PSingle(PValue)^);
    varDouble  : lua_pushnumber(Handle, PDouble(PValue)^);
    varCurrency: lua_pushnumber(Handle, PCurrency(PValue)^);
    varDate    :
    begin
      if (Pointer(FStringBuffer.Default) <> nil) then FStringBuffer.Default := '';

      case InspectDateTime(PDateTime(PValue)^) of
        0: TDateProc(@DateTimeToStr)(PDouble(PValue)^, FStringBuffer.Default);
        1: TDateProc(@DateToStr)(PDouble(PValue)^, FStringBuffer.Default);
        2: TDateProc(@TimeToStr)(PDouble(PValue)^, FStringBuffer.Default);
      end;

      {$ifdef UNICODE}push_unicode_string{$else}push_ansi_string{$endif}(FStringBuffer.Default);
    end;
    {$ifNdef NEXTGEN}
    varOleStr  : push_wide_string(PWideString(PValue)^);
    {$endif}
    varBoolean : lua_pushboolean(Handle, PBoolean(PValue)^);
    varShortInt: lua_pushinteger(Handle, PShortInt(PValue)^);
    varByte    : lua_pushinteger(Handle, PByte(PValue)^);
    varWord    : lua_pushinteger(Handle, PWord(PValue)^);
    varLongWord: lua_pushnumber(Handle, PLongWord(PValue)^);
    varInt64   : lua_pushnumber(Handle, PInt64(PValue)^);
    $15{UInt64}: lua_pushnumber(Handle, PInt64(PValue)^);
    {$ifdef UNICODE}
    varUString : push_unicode_string(PUnicodeString(PValue)^);
    {$endif}
    {$ifNdef NEXTGEN}
    varString  : push_ansi_string(PAnsiString(PValue)^);
    {$endif}
  else
    TIntToStr(@IntToStr)(VType, FStringBuffer.Default);
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TLua.push_argument(const Value: TVarRec): Boolean;
type
  TIntToStr = procedure(const Value: Integer; var Result: string);
begin
  with Value do
  case (VType) of
    vtInteger:   lua_pushinteger(Handle, VInteger);
    vtBoolean:   lua_pushboolean(Handle, VBoolean);
    vtExtended:  lua_pushnumber(Handle, VExtended^);
    vtInt64:     lua_pushnumber(Handle, VInt64^);
    vtCurrency:  lua_pushnumber(Handle, VCurrency^);
    vtVariant:   begin
                   Result := push_variant(VVariant^);
                   Exit;
                 end;
    vtPointer,
    vtInterface: if (VPointer = nil) then lua_pushnil(Handle) else lua_pushlightuserdata(Handle, VPointer);
(*    vtObject:    if (VObject = nil) then lua_pushnil(Handle) else
                 begin
                   if (TClass(pointer(VObject)^) = TLuaReference) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaReference(VObject).Index)
                   else
                   push_userdata(ClassesInfo[internal_class_index(TClass(pointer(VObject)^), true)], false, pointer(VObject));
                 end;
    vtClass:     if (VClass = nil) then lua_pushnil(Handle) else lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassesInfo[internal_class_index(pointer(VClass), true)].Ref);
*)
    {$ifNdef NEXTGEN}
    vtChar:      push_ansi_chars(@VChar, 0, Ord(VChar <> #0));
    vtPChar:     push_ansi_chars(VPChar, 0, LStrLen(VPChar));
    vtString:    push_short_string(PShortString(VString)^);
    vtAnsiString:push_ansi_string(AnsiString(VAnsiString));
    vtWideString:push_wide_string(WideString(VWideString));
    {$endif}
    vtWideChar:  push_wide_chars(@VWideChar, Ord(VWideChar <> #0));
    vtPWideChar: push_wide_chars(VPWideChar, WStrLen(VPWideChar));
    {$ifdef UNICODE}
    vtUnicodeString: push_unicode_string(UnicodeString(VUnicodeString));
    {$endif}
  else
    TIntToStr(@IntToStr)(VType, FStringBuffer.Default);
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TLua.stack_pop(const Count: Integer);
begin
  lua_settop(Handle, not Count{-Count - 1}); // lua_pop(Handle, Count);
end;

function TLua.stack_variant(var Ret: Variant; const StackIndex: Integer; const OleMode: Boolean): Boolean;
const
  varDeepData = $BFE8;
var
  VType, LuaType: Integer;
  VarData: TVarData absolute Ret;
begin
  VType := VarData.VType;
  if (VType and varDeepData <> 0) then
  case VType of
    varBoolean, varUnknown+1..$15{varUInt64}: ;
  else
    System.VarClear(Ret);
  end;

  LuaType := lua_type(Handle, StackIndex);
  case (LuaType) of
        LUA_TNIL: begin
                    VarData.VType := varEmpty;
                  end;
    LUA_TBOOLEAN: begin
                    VarData.VType := varBoolean;
                    VarData.VBoolean := lua_toboolean(Handle, StackIndex);
                  end;
     LUA_TNUMBER: begin
                    VarData.VDouble := lua_tonumber(Handle, StackIndex);
                    if (NumberToInteger(VarData.VDouble, VarData.VInteger)) then
                    begin
                      VarData.VType := varInteger;
                     end else
                    begin
                      VarData.VType := varDouble;
                    end;
                  end;
     LUA_TSTRING: begin
                    VarData.VUnknown := nil;

                    {$ifdef MSWINDOWS}
                    if (not OleMode) then
                    {$endif}
                    begin
                      {$if Defined(NEXTGEN) or (Defined(LUA_UNICODE) and Defined(UNICODE))}
                        VarData.VType := varUString;
                        stack_unicode_string(UnicodeString(VarData.VUString), StackIndex);
                      {$elseif Defined(LUA_ANSI)}
                        VarData.VType := varString;
                        stack_ansi_string(AnsiString(VarData.VString), StackIndex, 0);
                      {$else .LUA_UNICODE}
                        VarData.VType := varOleStr;
                        stack_wide_string(WideString(Pointer(VarData.VOleStr)), StackIndex);
                      {$ifend}
                    end
                    {$ifdef MSWINDOWS}
                    else
                    begin
                      VarData.VType := varOleStr;
                      stack_wide_string(WideString(Pointer(VarData.VOleStr)), StackIndex);
                    end{$endif};
                  end;
  else
    VarData.VType := varEmpty;
    GetLuaTypeName(FStringBuffer.Default, LuaType);
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TLua.stack_luaarg(var Ret: TLuaArg; const StackIndex: integer; const AllowLuaTable: Boolean): Boolean;
label
  copy_meta_params, fail_user_data, fail_lua_type;
var
  LuaType: Integer;
  UserData: ^TLuaUserData;
  MetaType: ^TLuaMetaType;
  Table: PLuaTable;
begin
  Result := True;
  Ret.F.LuaType := ltEmpty;

  LuaType := lua_type(Handle, StackIndex);
  case (LuaType) of
    LUA_TNIL:    {done};
    LUA_TBOOLEAN:
    begin
      Ret.F.LuaType := ltBoolean;
      Ret.F.VBoolean := lua_toboolean(Handle, StackIndex);
    end;
    LUA_TNUMBER:
    begin
      Ret.F.VDouble := lua_tonumber(Handle, StackIndex);
      if (NumberToInteger(Ret.F.VDouble, Ret.F.VInteger)) then
      begin
        Ret.F.LuaType := ltInteger;
      end else
      begin
        Ret.F.LuaType := ltDouble;
      end;
    end;
    LUA_TSTRING:
    begin
      Ret.F.LuaType := ltString;
      stack_lua_string(Ret.FStringValue, StackIndex);
    end;
    LUA_TLIGHTUSERDATA:
    begin
      Ret.F.LuaType := ltPointer;
      Ret.F.VPointer := lua_touserdata(Handle, StackIndex);
    end;
    LUA_TFUNCTION:
    begin
      Ret.F.LuaType := ltPointer;
      Ret.F.VPointer := function_topointer(StackIndex);
    end;
    LUA_TUSERDATA:
    begin
      UserData := lua_touserdata(Handle, StackIndex);
      if (UserData = nil) or (UserData.Instance = nil) then goto fail_user_data;

      MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(UserData.MetaType);
      case (UserData.Kind) of
        ukInstance:
        begin
          if (MetaType.F.Kind = mtClass) then
          begin
            Ret.F.LuaType := ltObject;
            Ret.F.VPointer := UserData.Instance;
          end else
          // if (LuaMetaType.Kind = mtRecord) then
          begin
            Ret.F.LuaType := ltRecord;
            goto copy_meta_params;
          end;
        end;
        ukArray:
        begin
          if (UserData.ArrayParams and $f = 0) then
          begin
            Ret.F.LuaType := ltArray;
            goto copy_meta_params;
          end else
          begin
            Ret.F.LuaType := ltPointer;
            Ret.F.VPointer := UserData.Instance;
          end;
        end;
        ukSet:
        begin
          Ret.F.LuaType := ltSet;
        copy_meta_params:
          with PLuaRecord{PLuaArray/PLuaSet}(@Ret.F)^ do
          begin
            Data := UserData.Instance;
            Info := Pointer(MetaType);
            FIsRef := not UserData.GcDestroy;
            FIsConst := UserData.ConstMode;
          end;
        end;
        ukProperty:
        begin
        fail_user_data:
          UserData.GetDescription(FStringBuffer.Default, Self);
          Result := False;
        end;
      end;
    end;
    LUA_TTABLE:
    begin
      // TClass, Info or LuaTable
      MetaType := nil;
      lua_rawgeti(Handle, StackIndex, 0);
      if (lua_type(Handle, -1) = LUA_TUSERDATA) then
      begin
        MetaType := lua_touserdata(Handle, -1);
        if (NativeInt(MetaType) and 3 <> 0) or (MetaType.F.Marker <> LUA_METATYPE_MARKER) then
          MetaType := nil;
      end;
      lua_settop(Handle, -1-1);

      if (Assigned(MetaType)) then
      begin
        if (MetaType.F.Kind = mtClass) then
        begin
          Ret.F.LuaType := ltClass;
          Ret.F.VPointer := Pointer(PLuaClassInfo(MetaType).AClass);
        end else
        begin
          Ret.F.LuaType := ltPointer;
          Ret.F.VPointer := Pointer(MetaType);
        end;
      end else
      if (AllowLuaTable) then
      begin
        PInteger(@Ret.F)^ := {clear and set} Ord(ltTable);

        Table := PLuaTable(@Ret.F);
        Table.FLua := Self;
        Table.FIndex := StackIndex;
      end else
      begin
        goto fail_lua_type;
      end;
    end;
  else
  fail_lua_type:
    GetLuaTypeName(FStringBuffer.Default, LuaType);
    Result := False;
  end;
end;

              (*
// сохранить неймспейс в файл
type TLuaStringList = class(TStringList) public Lua: TLua; end;
procedure TLua.SaveNameSpace(const FileName: string);
const
  CHARS: array[0..5] of char = (#13, #10, #9, #32, #32, #32);
var
  NameSpace, M: TMemoryStream;

  // запись
  procedure Enter();
  begin NameSpace.Write(CHARS[0], 2*sizeof(char)); end;

  procedure Write(C: char); overload;
  begin NameSpace.Write(C, sizeof(C)); end;

  procedure Write(const S: string; use_enter: boolean=true; tabs_count: integer=1); overload;
  var
    i: integer;
  begin
    for i := 0 to tabs_count-1 do NameSpace.Write(CHARS[2], sizeof(char));
    if (S <> '') then NameSpace.Write(pointer(S)^, Length(S));
    if (use_enter) then NameSpace.Write(CHARS[0], 2*sizeof(char));
  end;

  procedure Write(Lines: TStringList; tabs_count: integer=1; const prefics: string=''; const postfix: string=''); overload;
  var
    i: integer;
  begin
    if (Lines.Count = 0) then exit;

    for i := 0 to Lines.Count-1 do
    begin
      // tab
      if (tabs_count > 0) then NameSpace.Write(CHARS[2], sizeof(char));
      // spaces
      if (tabs_count > 1) then NameSpace.Write(CHARS[3], 3*sizeof(char));

      // prefics
      if (prefics <> '') then
      begin
        Write(prefics, false, 0);
        NameSpace.Write(CHARS[3], sizeof(char)); // #32
      end;

      // string
      Write(Lines[i], (postfix=''), 0);

      // postfix
      if (postfix <> '') then Write(postfix, true, 0);
    end;
  end;

  procedure WriteIdent(const Ident: string);
  const
    RowWidth = 80;
  var
    S: string;
    Len, L: integer;
  begin
    if (NameSpace.Size <> 0) then
    begin
      Enter;
      Enter;
    end;

    SetLength(S, RowWidth);
    FillChar(pointer(S)^, RowWidth, ord('*'));

    pchar(pointer(S))[0] := '(';
    pchar(pointer(S))[RowWidth-1] := ')';

    Len := Length(Ident);
    L := (RowWidth-Len) div 2;
    CopyMemory(@pchar(pointer(S))[L], pointer(Ident), Len);

    pchar(pointer(S))[L-1] := #32;
    pchar(pointer(S))[L-2] := #32;
    pchar(pointer(S))[L+Len] := #32;
    pchar(pointer(S))[L+Len+1] := #32;

    Write(S, true, 0);
  end;

  procedure WriteToLine(Lines: TStringList);
  const
    Limit = 80- 20;
  var
    i, Count: integer;
    Buffer: string;
  begin
    Buffer := '';

    Count := Lines.Count;
    for i := 1 to Count do
    begin
      Buffer := Buffer + Lines[i-1];

      if (i = Count) then
      begin
        Write(Buffer);
      end else
      begin
        Buffer := Buffer + ', ';
        if (Length(Buffer) >= Limit) then
        begin
          Write(Buffer);
          Buffer := '';
        end;
      end;
    end;               
  end;

  function global_index_type(const Ref: integer): integer;
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
    Result := lua_type(Handle, -1);
    lua_settop(Handle, -1-1);
  end;

  function global_index_value(const Ref: integer): string;
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
    stack_luaarg(FBufferArg, -1, true);

    if (FBufferArg.LuaType = ltString) then Result := '"' + FBufferArg.str_data + '"'
    else Result := FBufferArg.ForceString;

    if (FBufferArg.LuaType = ltTable) then Result := Result + '(...)';

    lua_settop(Handle, -1-1);
  end;

var
  S, Cl: string;
  P, i, j, Len: integer;
  tpinfo: ptypeinfo;
  typedata: ptypedata;
  str: PShortString;
  native_consts, native_variables, native_methods, enumerates,
  lua_variables, lua_methods,
  sets, arrays,
  records, classes,
  type_base, type_properties, type_methods, type_events: TStringList;

  procedure create_lists();
    procedure create(var l: TStringList); begin l := TLuaStringList.Create; TLuaStringList(l).Lua := Self; end;
  begin
    create(native_consts); create(native_variables); create(native_methods);
    create(lua_variables); create(lua_methods);
    create(enumerates); create(sets);
    create(arrays); create(records); create(classes);
    create(type_base); create(type_properties); create(type_methods); create(type_events);
  end;

  procedure destroy_lists();
    procedure destroy(var l: TStringList); begin FreeAndNil(l); end;
  begin
    destroy(native_consts); destroy(native_variables); destroy(native_methods);
    destroy(lua_variables); destroy(lua_methods);
    destroy(enumerates); destroy(sets);
    destroy(arrays); destroy(records); destroy(classes);
    destroy(type_base); destroy(type_properties); destroy(type_methods); destroy(type_events);
  end;

  procedure enter_if_lists(const lists: array of TStringList);
  var i: integer;
  begin
    for i := 0 to High(lists) do
    if (lists[i].Count > 0) then begin Enter(); exit; end;
  end;

  procedure sort_lists(const lists: array of TStringList);
  var i: integer;
  begin
    for i := 0 to High(lists) do lists[i].Sort;
  end;

  function class_level(aclass: TClass): integer; far;
  asm
    mov edx, eax
    xor eax, eax

    { TClass(AClass) := TClass(AClass).ClassParent; }
    @loop:
      inc eax
      mov edx, [edx + vmtParent]
      {$ifndef fpc}
        test edx, edx
        jz @exit
        mov edx, [edx]
      {$endif}
    test edx, edx
    jnz @loop

    @exit:
  end;

  function classes_sort(list: TStringList; index1, index2: integer): integer; far;
  var
    info1, info2: ^TLuaClassInfo;
  begin
    info1 := pointer(list.Objects[index1]);
    info2 := pointer(list.Objects[index2]);

    Result := class_level(TClass(info1._Class))-class_level(TClass(info2._Class));
    if (Result = 0) then
    begin
      if (info1.ParentIndex <> info2.ParentIndex) then
      begin
        info1 := @TLuaStringList(list).Lua.ClassesInfo[info1.ParentIndex];
        info2 := @TLuaStringList(list).Lua.ClassesInfo[info2.ParentIndex];
      end;

      Result := CompareStr(info1._ClassName, info2._ClassName);
    end;
  end;

  function array_description(const ArrayInfo: TLuaArrayInfo): string;
  var
    i: integer;
    typename, desc: string;
  begin
    // получить тип
    typename := TLuaPropertyInfo(ArrayInfo.ItemInfo).Description;
    Delete(typename, 1, 2);
    SetLength(typename, CharPos(#32, typename)-1);

    if (ArrayInfo.IsDynamic) then
    begin
      for i := 0 to ArrayInfo.Dimention-1 do
      begin
        if (i = 0) then desc := 'array of'
        else desc := desc + ' array of';
      end;
    end else
    begin
      for i := 0 to ArrayInfo.Dimention-1 do
      begin
        if (i <> 0) then desc := desc + ', ';
        desc := Format('%s%d..%d', [desc, ArrayInfo.FBoundsData[i*2], ArrayInfo.FBoundsData[i*2+1]]);
      end;

      desc := '[' + desc + '] of';
    end;

    Result := ArrayInfo.Name + ' = ' + desc + ' ' + typename;
  end;

  // записать структуру или класс
  procedure write_type(const info: TLuaClassInfo);
  const
    OPERATORS_SIMBOLS: array[TLuaOperator] of string = ('neg','+','-','*','/','%','^','compare');
  var
    o: TLuaOperator;
    i, p, Index: integer;    
    is_class: boolean;
    record_info: PLuaRecordInfo;
    description, prop_prefix, operators, propdesc: string;
    class_info: ^TLuaClassInfo;
    proc_info: ^TLuaClosureInfo;
    property_info: ^TLuaPropertyInfo;
  begin
    type_base.Clear(); type_properties.Clear(); type_methods.Clear(); type_events.Clear();
    is_class := (info._ClassKind = ckClass);
    if (is_class) then
    begin
      prop_prefix := 'property';
      description := info._ClassName + ' = class';
      if (TClass(info._Class) <> TObject) then description := description + '(' + TClass(info._Class).ClassParent.ClassName + ')';
      record_info := nil;
    end else
    begin
      prop_prefix := '';
      description := info._ClassName + ' = record';
      record_info := PLuaRecordInfo(info._Class);
    end;  

    // type_base
    if (info.constructor_address <> nil) then type_base.Add('CONSTRUCTOR ()');
    if (not is_class) and (assigned(record_info.OperatorCallback)) and (record_info.Operators <> []) then
    begin
      operators := '';
      for o := Low(TLuaOperator) to High(TLuaOperator) do
      if (o in record_info.Operators) then
      begin
        if (operators = '') then operators := OPERATORS_SIMBOLS[o]
        else operators := operators + ', ' + OPERATORS_SIMBOLS[o];
      end;

      type_base.Add('OPERATORS: ' + operators);
    end;

    // свойства, методы, события
    for i := 0 to Length(info.NameSpace)-1 do
    begin
      Index := info.NameSpace[i].Index;
      class_info := @Self.ClassesInfo[word(Index)];

      if (Index >= 0) then
      begin
        proc_info := @class_info.Procs[Index shr 16];
        type_methods.Add(proc_info.ProcName);
      end else
      begin
        property_info := @class_info.Properties[not smallint(Index shr 16)];
        propdesc := property_info.Description;

        // убрать R/W для cтруктур
        if (not is_class) then
        begin
          for p := Length(propdesc) downto 1 do
          if (propdesc[p] = #32) then
          begin
            SetLength(propdesc, p-1);
            break;
          end;
        end;

        // default property
        if (info._DefaultProperty >= 0) and (word(info._DefaultProperty) = word(Index)) and
           (smallint(info._DefaultProperty shr 16) = not smallint(Index shr 16)) then
            propdesc := propdesc + ' default';

        // event или property
        if (is_class) and (property_info.Base.Kind = pkRecord) and (property_info.Parameters = nil) and
           (PLuaRecordInfo(property_info.Base.Information).FClassIndex = TMETHOD_CLASS_INDEX)
        then type_events.Add(propdesc)
        else type_properties.Add(propdesc);
      end;
    end;

    // запись
    Enter;
    Enter;
    Write(description);
    Write('public');
    sort_lists([type_base, type_properties, type_methods, type_events]);

    // базовая информация
    Write(type_base, 2);

    // поля(свойства), методы, события. порядок зависит от is_class
    if (is_class) then
    begin
      if (type_methods.Count <> 0) then
      begin
        enter_if_lists([type_base]);
        Write(type_methods, 2, 'method', '()');
      end;
      if (type_properties.Count <> 0) then
      begin
        enter_if_lists([type_base, type_methods]);
        Write(type_properties, 2, prop_prefix);
      end;
      if (type_events.Count <> 0) then
      begin
        enter_if_lists([type_base, type_methods, type_properties]);
        Write(type_events, 2);
      end;
    end else
    begin
      if (type_properties.Count <> 0) then
      begin
        enter_if_lists([type_base]);
        Write(type_properties, 2, prop_prefix);
      end;
      if (type_methods.Count <> 0) then
      begin
        enter_if_lists([type_base, type_properties]);
        Write(type_methods, 2, 'method', '()');
      end;
    end;

    // end
    if (type_base.Count = 0) and (type_properties.Count = 0) and
       (type_methods.Count = 0) and (type_events.Count = 0) then Enter();
    Write('end');
  end;

begin
  INITIALIZE_NAME_SPACE;
  NameSpace := TMemoryStream.Create;
  create_lists();

  // рассортировка глобальных переменных
  for i := 0 to Length(GlobalVariables)-1 do
  with GlobalVariables[i] do
  case _Kind of
    gkType: begin
              global_push_value(Ref);
              P := LuaTableToClass(Handle, -1);
              lua_settop(Handle, -1-1);

              if (P >= 0) then
              case ClassesInfo[P]._ClassKind of
                ckClass: classes.AddObject(_Name, TObject(@ClassesInfo[P]));
               ckRecord: records.AddObject(_Name, TObject(@ClassesInfo[P]));
                ckArray: arrays.Add(array_description(PLuaArrayInfo(ClassesInfo[P]._Class)^));
                  ckSet: sets.Add(_Name + ' = set of ' + GetOrdinalTypeName(PLuaSetInfo(ClassesInfo[P]._Class).FTypeInfo));
              end;
            end;
gkVariable: begin
              S := GlobalNative.Properties[not Index].Description();

              // замена типа TObject на точный
              with GlobalNative.Properties[not Index] do
              if (Base.Kind = pkObject) and (read_mode >= 0) then
              if (ppointer(read_mode)^ <> nil) then
              begin
                P := integer{TObject}(pointer(read_mode)^);
                if (P <> 0) and (TClass(pointer(P)^) <> TObject) then
                begin
                  Cl := TObject(P).ClassName;
                  P := Pos(' TObject ', S);
                  if (P <> 0) then
                  begin
                    Delete(S, P+1, 7);
                    Insert(Cl, S, P+1);
                  end;
                end;
              end;

              // подмена r/w на const
              for P := Length(S) downto 1 do
              if (S[P] = #32) then
              begin
                SetLength(S, P-1);
                break;
              end;
              if (IsConst) then S := S + ' const';

              native_variables.Add(S);
            end;
    gkProc: native_methods.Add(_Name);
   gkConst: native_consts.AddObject(_Name, TObject(Ref));
 gkLuaData: begin
              if (global_index_type(Ref) = LUA_TFUNCTION) then
                lua_methods.AddObject(_Name, TObject(Ref))
              else
                lua_variables.AddObject(_Name + ' = ' + global_index_value(Ref), TObject(Ref));
            end;  
  end;  

  // рассортировать Enumerations
  native_consts.Sorted := true;
  for i := 0 to Length(EnumerationList)-1 do
  begin
    tpinfo := ptypeinfo(EnumerationList[i]);
    S := tpinfo.Name + ' = (';

    // по каждому enum value
    typedata := GetTypeData(tpinfo);
    Len := typedata.MaxValue-typedata.MinValue+1;
    typedata := GetTypeData(typedata^.BaseType^);
    str := PShortString(@typedata.NameList);
    for j := 0 to Len-1 do
    begin
      Cl := str^;
      Inc(Integer(str), Length(str^) + 1);

      // удалить из native_consts
      P := native_consts.IndexOf(Cl);
      if (P >= 0) then native_consts.Delete(P);

      // добавить в S
      if (j = 0) then S := S + Cl
      else S := S + ', ' + Cl;
    end;

    S := S + ')';
    enumerates.Add(S);
  end;

  // проставить значения native констант
  native_consts.Sorted := false;
  for i := 0 to native_consts.Count-1 do
  native_consts[i] := native_consts[i] + ' = ' + global_index_value(integer(native_consts.Objects[i]));

  // запись простых вещей: native/lua name space, enumerates
  WriteIdent('NATIVE_NAME_SPACE');
    sort_lists([native_consts, native_consts, native_methods, native_consts]);

    Write(native_consts);
    if (native_variables.Count > 0) then
    begin
      enter_if_lists([native_consts]);
      Write(native_variables);
    end;
    if (native_methods.Count > 0) then
    begin
      enter_if_lists([native_consts, native_variables]);
      Write(native_methods, 1, 'method', '()');
    end;
    if (enumerates.Count > 0) then
    begin
      enter_if_lists([native_consts, native_variables, native_methods]);
      Write(enumerates);
    end;

  WriteIdent('LUA_NAME_SPACE');
    sort_lists([lua_variables, lua_methods]);
    Write(lua_variables);
    if (lua_methods.Count > 0) then
    begin
      if (lua_variables.Count > 0) then Enter;
      Write(lua_methods, 1, 'function', '()');
    end;

  // множества
  WriteIdent('SETS');
     sets.Sort;
     Write(sets);

  // массивы
  WriteIdent('ARRAYS');
     arrays.Sort;
     Write(arrays);

  // структуры (records)
  WriteIdent('STRUCTURES');
     records.Sort;
     WriteToLine(records);
     for i := 0 to records.Count-1 do write_type(TLuaClassInfo(pointer(records.Objects[i])^));

  // классы
  WriteIdent('CLASSES');
     classes.CustomSort(pointer(@classes_sort));
     WriteToLine(classes);
     for i := 0 to classes.Count-1 do write_type(TLuaClassInfo(pointer(classes.Objects[i])^));   



  destroy_lists();
  // сохранение, удаление необходимых данных
  // сравнение с предыдущей версией - чтобы не парить svn (или другую версионную прогу)
  try
    if (not FileExists(FileName)) then
    begin
      NameSpace.SaveToFile(FileName);
    end else
    begin
      M := TMemoryStream.Create;
      try
        M.LoadFromFile(FileName);

        if (NameSpace.Size <> M.Size) or (not CompareMem(NameSpace.Memory, M.Memory, NameSpace.Size)) then
        NameSpace.SaveToFile(FileName);
      finally
        M.Free;
      end;
    end;
  finally
    NameSpace.Free;
  end;
end;       *)
             (*

// если указано глобальное имя, то запушить значение в стек. если не указано - то запушить nil
// TLuaReference.Create/Initialize добавляется в список ссылок и производит инициализауию в Lua
function __TLuaCreateReference(const Self: TLua; const global_name: string{=''}; const ReturnAddr: pointer): TLuaReference;
var
  Ind: integer;
  prop_struct: TLuaPropertyStruct;
begin
  with Self do
  begin
    // запушить nil или глобальную переменную или Exception
    if (global_name = '') then
    begin
      lua_pushnil(Handle);
    end else
    if (GlobalVariablePos(pchar(global_name), Length(global_name), Ind)) then
    with GlobalVariables[Ind] do
    begin
      if (_Kind in GLOBAL_INDEX_KINDS) then
      begin
        lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
      end else
      if (Index >= 0) then
      begin
        lua_pushcclosure(Handle, GlobalNative.Procs[Index].lua_CFunction, 0);
      end else
      begin
        // поместить значение глобальной переменной в луа-стек
        prop_struct.PropertyInfo := @GlobalNative.Properties[not Index];
        prop_struct.Instance := nil;
        prop_struct.IsConst := IsConst;
        prop_struct.Index := nil;
        prop_struct.ReturnAddr := ReturnAddr;
        Self.__index_prop_push(GlobalNative, @prop_struct);
      end;
    end else
    ELua.Assert('Global variable "%s" not found', [global_name], ReturnAddr);


    // создать Instance и проинициализировать
    Result := TLuaReference.Create;
    Result.Initialize(Self);
  end;
end;    *)
                       (*
function TLua.CreateReference(const global_name: string=''): TLuaReference;
asm
  mov ecx, [esp]
  jmp __TLuaCreateReference
end;                  *)


procedure __TLuaGarbageCollection(const Self: TLua);
var
  ErrCode: Integer;
begin
  ErrCode := lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0);
  if (ErrCode <> 0) then Self.CheckScriptError(ErrCode);
end;

procedure TLua.GarbageCollection;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaGarbageCollection(Self);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaGarbageCollection
end;
{$endif}

procedure TLua.InternalError(const Text: LuaString);
const
  SLN_CONST: Cardinal = Ord('S') + (Ord('l') shl 8) + (Ord('n') shl 16);
var
  DebugInfo: lua_Debug;
begin
  // debug information
  FillChar(DebugInfo, SizeOf(DebugInfo), #0);
  lua_getstack(Handle, 1, @DebugInfo);
  lua_getinfo(Handle, __luaname(Pointer(@SLN_CONST)), @DebugInfo);

  // standard exception
  if (DebugInfo.currentline < 0) then
    raise ELua.Create(string(Text)) at FReturnAddress;

  // script exception, format error to parse in Check
  unpack_lua_string(FStringBuffer.Lua, __luaname(Pointer(@DebugInfo.short_src[4])));
  {$ifdef UNICODE}
    FmtStr(FStringBuffer.Unicode, '%s:%d: %s', [FStringBuffer.Lua, DebugInfo.currentline, Text]);
    push_unicode_string(FStringBuffer.Unicode);
  {$else}
    WideFmtStr(FStringBuffer.Wide, '%s:%d: %s', [FStringBuffer.Lua, DebugInfo.currentline, Text]);
    push_wide_string(FStringBuffer.Wide);
  {$endif}
  lua_error(Handle);
end;

procedure TLua.InternalErrorFmt(const FmtStr: LuaString; const Args: array of const);
var
  Fmt, Buffer: UnicodeString;
begin
  Fmt := UnicodeString(FmtStr);
  Buffer := {$ifdef UNICODE}Format{$else}WideFormat{$endif}(Fmt, Args);
  InternalError(LuaString(Buffer));
end;

procedure TLua.Error(const Text: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  InternalError(Text);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp TLua.InternalError
end;
{$endif}

procedure TLua.Error(const FmtStr: LuaString; const Args: array of const);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  InternalErrorFmt(FmtStr, Args);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp TLua.InternalErrorFmt
end;
{$endif}

procedure TLua.CheckScriptError(const ErrCode: Integer; AUnit: TLuaUnit);
const
  LINE_MARKS: array[Boolean] of string = ('', '-->> ');
var
  Err, UnitName: ^LuaString;
  P1, P2, Count, i: Integer;
  UnitLine: Integer;
  Text, LineFmt: ^string;
  MinLine, MaxLine: Integer;
  Line: TLuaUnitLine;
  S: PChar;

  function CharPos(const C: LuaChar; const Str: LuaString; const StartFrom: Integer = 1): Integer; far;
  var
    S: PLuaChar;
    Count: Integer;
  begin
    S := Pointer(Str);
    if (S <> nil) then
    begin
      Count := PInteger(NativeInt(S) - SizeOf(Integer))^ {$ifdef LUA_LENGTH_SHIFT}shr 1{$endif};
      Dec(Count, StartFrom - 1);
      Inc(S, StartFrom - 1);

      if (Count > 0) then
      repeat
        if (S^ = C) then
        begin
          Result := Integer((NativeUInt(S) - NativeUInt(Str)) div SizeOf(LuaChar)) + 1;
          Exit;
        end;

        Dec(Count);
        Inc(S);
      until (Count = 0);
    end;

    Result := 0;
  end;

  function CharsToInt(S: PLuaChar; Count: Integer): Integer;
  label
    fail;
  begin
    if (Count <= 0) then goto fail;

    Result := 0;
    repeat
      case S^ of
        '0'..'9': Result := Result * 10 + Integer(Integer(S^) - Ord('0'));
      else
        goto fail;
      end;

      Dec(Count);
      Inc(S);
    until (Count = 0);
    Exit;

  fail:
    Result := 0;
  end;

  function EmptyLine(const AUnit: TLuaUnit; const AUnitLine: Integer): Boolean; far;
  var
    S: __luadata;
    Count, i: Integer;
  begin
    Result := False;
    S := AUnit.FLines[AUnitLine - 1].Chars;
    Count := AUnit.FLines[AUnitLine - 1].Count;
    for i := 1 to Count do
    begin
      if (Byte(S^) > 32) then Exit;
      Inc(S);
    end;

    Result := True;
  end;

begin
  if (ErrCode = 0) then Exit;

  // error text
  Err := @FStringBuffer.Lua;
  stack_lua_string(Err^, -1);
  stack_pop;
  if (Pointer(Err^) = nil) then Exit;

  // unit name buffer
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    UnitName := @FStringBuffer.Unicode;
  {$else}
    UnitName := @FStringBuffer.Ansi;
  {$ifend}

  // change error text, detect chunk name, line number
  UnitLine := -1;
  if (Err^[1] = '[') then
  begin
    P1 := CharPos('"', Err^);
    P2 := CharPos(']', Err^);
    if (P1 <> 0) and (P2 <> 0) then
    begin
      // unit name
      Count := P2 - P1 - 2;
      SetLength(UnitName^, Count);
      System.Move(PLuaChar(@PLuaChar(Pointer(Err^))[P1 {+ 1 - 1}])^, Pointer(UnitName^)^, Count * SizeOf(LuaChar));
      Delete(Err^, 1, P2);

      // unit line
      if (Pointer(Err^) <> nil) and (Err^[1] = ':') then
      begin
        P1 := 1;
        P2 := CharPos(':', Err^, 2);
        if (P2 <> 0) then
        begin
          UnitLine := CharsToInt(PLuaChar(@PLuaChar(Pointer(Err^))[P1 {+ 1 - 1}]), P2 - P1 - 1);
          Delete(Err^, 1, P2);
          if (Pointer(Err^) <> nil) and (Err^[1] = #32) then Delete(Err^, 1, 1);
        end;
      end;
    end;
  end;

  // unit (chunk)
  if (Pointer(UnitName^) = nil) then
  begin
    UnitName := @GLOBAL_NAME_SPACE;
  end else
  if (AUnit = nil) then
  begin
    AUnit := GetUnitByName(UnitName^);
  end;
  if (AUnit <> nil) and (Cardinal(UnitLine) > Cardinal(AUnit.FLinesCount)) then AUnit := nil;

  // availiable: unit instance, unit name, unit line, error text
  // may be later it will be used in debug mode

  // base output text
  Text := @FStringBuffer.Default;
  FmtStr(Text^, 'unit "%s", line %d.'#10'%s', [UnitName^, UnitLine, Err^]);

  // unit output text lines
  if (AUnit <> nil) then
  begin
    // min/max
    MinLine := UnitLine - 2;
    if (MinLine < 1) then MinLine := 1;
    while (MinLine <> UnitLine) and (EmptyLine(AUnit, MinLine)) do Inc(MinLine);
    MaxLine := UnitLine + 2;
    if (MaxLine > AUnit.FLinesCount) then MaxLine := AUnit.FLinesCount;
    while (MaxLine <> UnitLine) and (EmptyLine(AUnit, MaxLine)) do Dec(MaxLine);

    // max digits
    i := MaxLine;
    Count := 0;
    repeat
      i := i div 10;
      Inc(Count);
    until (i = 0);

    // line format
    {$ifdef UNICODE}
      LineFmt := @FStringBuffer.Unicode;
    {$else}
      LineFmt := @FStringBuffer.Ansi;
    {$endif}
    FmtStr(LineFmt^, '%%s'#10'%%%dd:  %%s%%s', [Count]);

    // output text
    Text^ := Text^ + #10#10'Code:';
    for i := MinLine to MaxLine do
    begin
      Line := AUnit.GetLine(i);
      Self.unpack_lua_string(FStringBuffer.Lua, Line.Chars, Line.Count);
      FmtStr(Text^, LineFmt^, [Text^, i, LINE_MARKS[i = UnitLine], FStringBuffer.Lua]);
    end;
  end;

  // correct: #13 --> #10 (for console cases)
  S := Pointer(Text);
  for i := 0 to Length(Text^) - 1 do
  begin
    if (S[i] = #13) then S[i] := #10;
  end;

  // exception
  raise ELuaScript.Create(Text^) at FReturnAddress;
end;

function TLua.BeginScriptStack(const ReturnAddress: Pointer): Pointer;
var
  Index: NativeUInt;
  Buffer: PLuaResultBuffer;
begin
  Result := FReturnAddress;
  FReturnAddress := ReturnAddress;
  Index := FScriptStackIndex;
  if (Index >= High(FScriptStack)) then
    raise EInvalidScriptStackIndex(Index) at FReturnAddress;

  Inc(Index);
  FScriptStackIndex := Index;
  Pointer(FArgs) := nil;
  FArgsCount := 0;

  Buffer := @FScriptStack[Index];
  if (Buffer.Count <> 1) then Buffer.Count := 1;
end;

procedure TLua.EndScriptStack(const ReturnAddress: Pointer);
var
  Index: NativeUInt;
  Buffer: PLuaResultBuffer;
begin
  FReturnAddress := ReturnAddress;
  Index := FScriptStackIndex;
  Dec(Index);
  if (Index > High(FScriptStack) - 1{Low(FScriptStack)}) then
    raise EInvalidScriptStackIndex(Index + 1) at FReturnAddress;

  FScriptStackIndex := Index;
  Buffer := @FScriptStack[Index + 1];
  Buffer.FParamCount := 0;
  if (Buffer.FMetaStructs <> nil) or (Buffer.FCount <> 1) or
    (Buffer.FHeapEmpty <> TLuaMemoryHeap(Buffer.FHeap).FCurrent) then
    Buffer.Clear;

  Pointer(FArgs) := nil;
  if (Index = 0) then
  begin
    FArgsCount := 0;
  end else
  begin
    Dec(Buffer);
    FArgsCount := Buffer.FParamCount;
    if (Buffer.FParamCount <> 0) then
      Pointer(FArgs) := Buffer.FParamItems;
  end;
end;

function TLua.GetResultBuffer: PLuaResultBuffer;
var
  Index: NativeUInt;
begin
  Index := FScriptStackIndex;
  if (Index - 1{Low(FScriptStack)} > High(FScriptStack) - 1{Low(FScriptStack)}) then
    raise EInvalidScriptStackIndex(Index);

  Result := @FScriptStack[Index];
end;


                   (*
function  TLua.InternalCheckArgsCount(PArgs: pinteger; ArgsCount: integer; const ProcName: string; const AClass: TClass): integer;
var
  Arg: pinteger;

  procedure ThrowAssertation();
  var
    i: integer;
    S, Required: string;
  begin
    S := ProcName;
    if (S <> '') and (AClass <> nil) then S := AClass.ClassName + '.' + S;
    if (S <> '') then S := ' in Proc "' + S + '"';

    Arg := PArgs;
    for i := 0 to ArgsCount-1 do
    begin
      if (Arg^ >= 0) then
      begin
        if (Required = '') then Required := IntToStr(Arg^)
        else Required := Format('%s or %d', [Required, Arg^]);
      end;

      inc(Arg);
    end;

    // (UNKNOWN). Все указанные = -1
    if (Required = '') then exit;

    // вызов ошибки
    if (ArgsCount <> 1) then Required := '(' + Required + ')';
    ScriptAssert('Wrong arguments count (%d)%s. Required %s.', [Self.FArgsCount, S, Required]);
  end;

begin
  // поиск подходящего числа параметров
  Arg := PArgs;
  for Result := 0 to ArgsCount-1 do
  begin
    if (Arg^ = Self.FArgsCount) then exit;
    inc(Arg);
  end;  

  // случай ошибки
  Result := -1;
  ThrowAssertation();
end;                 *)
                           (*
// строковое описание аргумента
// в основном для описания ошибок
function  TLua.StackArgument(const Index: integer): string;
var
  Buf: TLuaArg;
begin
  if (not stack_luaarg(Buf, Index, true)) then Result := FBufferArg.str_data
  else Result := Buf.ForceString;

  if (Result = '') then Result := 'nil';
end;

    *)

function TLua.global_alloc_ref: Integer;
var
  Count: NativeInt;
begin
  Count := TLuaStack(FEmptyRefs).Count;
  if (Count <> 0) then
  begin
    Dec(Count);
    TLuaStack(FEmptyRefs).FCount := Count;
    Result := TLuaStack(FEmptyRefs).Items[Count];
  end else
  begin
    Result := FRef + 1;
    FRef := Result;
  end;
end;

procedure TLua.global_free_ref(var Ref: Integer);
var
  Value: Integer;
begin
  Value := Ref;
  Ref := 0;
  if (Value > 0) then
  begin
    if (Value = FRef) then
    begin
      FRef := Value - 1;
    end else
    begin
      TLuaStack(FEmptyRefs).Push(Value);
    end;

    lua_pushnil(Handle);
    lua_rawseti(Handle, LUA_REGISTRYINDEX, Value);
  end;
end;

procedure TLua.global_fill_value(const Ref: Integer);
begin
  if (Ref > 0) then
  begin
    lua_rawseti(Handle, LUA_REGISTRYINDEX, Ref);
  end else
  begin
    stack_pop;
  end;
end;

procedure TLua.global_push_value(const Ref: Integer);
begin
  if (Ref > 0) then
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref);
  end else
  begin
    lua_pushnil(Handle);
  end;
end;

function TLua.GetGlobalEntity(const Name: LuaString; const ModeCreate: Boolean): Pointer;
label
  not_found;
var
  EntityItem: ^TLuaDictionaryItem;
  LuaName: __luaname;
  Ptr: __luapointer;
begin
  if (ModeCreate) then
  begin
    LuaName := TLuaNames(FNames).Add(Name);
  end else
  begin
    LuaName := TLuaNames(FNames).FindValue(Name);
  end;

  if (Assigned(LuaName)) then
  begin
    EntityItem := TLuaDictionary(FGlobalEntities).InternalFind(LuaName, ModeCreate);
    if (not Assigned(EntityItem)) then goto not_found;
  end else
  begin
  not_found:
    Result := nil;
    Exit;
  end;

  // retreave or add empty global entity
  Ptr := EntityItem.Value;
  if (Ptr = LUA_POINTER_INVALID) then
  begin
    Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaGlobalEntity));
    EntityItem.Value := Ptr;
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
    PInteger(Result)^ := 0;
    PLuaGlobalEntity(Result).Ptr := LUA_POINTER_INVALID;
  end else
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
  end;
end;

function TLua.GetRecordInfo(const Name: LuaString): PLuaRecordInfo;
var
  Entity: PLuaGlobalEntity;
begin
  Entity := GetGlobalEntity(Name, False);
  if (Assigned(Entity)) and (Entity.Kind = gkMetaType) then
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
    if (Result.F.Kind = mtRecord) then Exit;
  end;

  Result := nil;
end;

function TLua.GetArrayInfo(const Name: LuaString): PLuaArrayInfo;
var
  Entity: PLuaGlobalEntity;
begin
  Entity := GetGlobalEntity(Name, False);
  if (Assigned(Entity)) and (Entity.Kind = gkMetaType) then
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
    if (Result.F.Kind = mtArray) then Exit;
  end;

  Result := nil;
end;

function TLua.GetSetInfo(const Name: LuaString): PLuaSetInfo;
var
  Entity: PLuaGlobalEntity;
begin
  Entity := GetGlobalEntity(Name, False);
  if (Assigned(Entity)) and (Entity.Kind = gkMetaType) then
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
    if (Result.F.Kind = mtSet) then Exit;
  end;

  Result := nil;
end;

type
  TLuaGlobalModify = record
    Name: ^LuaString;
    VariantMode: Boolean;
    case Boolean of
      False: (Arg: PLuaArg);
       True: (V: PVariant);
  end;

procedure __TLuaGetVariable(const Self: TLua; const Name: LuaString; var Result: Variant);
var
  Modify: TLuaGlobalModify;
  Ptr: NativeUInt;
begin
  Modify.Name := @Name;
  Modify.VariantMode := True;
  Modify.V := @Result;

  Ptr := NativeUInt(@Modify);
  Self.__global_index(Cardinal(Ptr), {$ifdef LARGEINT}Ptr shr 32{$else}0{$endif});
end;

function TLua.GetVariable(const Name: LuaString): Variant;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaGetVariable(Self, Name, Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaGetVariable
end;
{$endif}

procedure __TLuaSetVariable(const Self: TLua; const Name: LuaString; const Value: Variant);
var
  Modify: TLuaGlobalModify;
  Ptr: NativeUInt;
begin
  Modify.Name := @Name;
  Modify.VariantMode := True;
  Modify.V := @Value;

  Ptr := NativeUInt(@Modify);
  Self.__global_newindex(Cardinal(Ptr), {$ifdef LARGEINT}Ptr shr 32{$else}0{$endif});
end;

procedure TLua.SetVariable(const Name: LuaString; const Value: Variant);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaSetVariable(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaSetVariable
end;
{$endif}

procedure __TLuaGetVariableEx(const Self: TLua; const Name: LuaString; var Result: TLuaArg);
var
  Modify: TLuaGlobalModify;
  Ptr: NativeUInt;
begin
  Modify.Name := @Name;
  Modify.VariantMode := False;
  Modify.Arg := @Result;

  Ptr := NativeUInt(@Modify);
  Self.__global_index(Cardinal(Ptr), {$ifdef LARGEINT}Ptr shr 32{$else}0{$endif});
end;

function TLua.GetVariableEx(const Name: LuaString): TLuaArg;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaGetVariableEx(Self, Name, Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaGetVariableEx
end;
{$endif}

procedure __TLuaSetVariableEx(const Self: TLua; const Name: LuaString; const Value: TLuaArg);
var
  Modify: TLuaGlobalModify;
  Ptr: NativeUInt;
begin
  Modify.Name := @Name;
  Modify.VariantMode := False;
  Modify.Arg := @Value;

  Ptr := NativeUInt(@Modify);
  Self.__global_newindex(Cardinal(Ptr), {$ifdef LARGEINT}Ptr shr 32{$else}0{$endif});
end;

procedure TLua.SetVariableEx(const Name: LuaString; const Value: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaSetVariableEx(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaSetVariableEx
end;
{$endif}

function TLua.VariableExists(const Name: LuaString): Boolean;
var
  Entity: PLuaGlobalEntity;
  LuaType: Integer;
begin
  Entity := GetGlobalEntity(Name, False);
  if (Assigned(Entity)) then
  begin
    if (Entity.Kind in [gkMetaType, gkVariable, gkConst]) then
    begin
      Result := True;
      Exit;
    end else
    if (Entity.Kind = gkScriptVariable) then
    begin
      global_push_value(Entity.Ref);
      LuaType := lua_type(Handle, -1);
      stack_pop;

      Result := (LuaType <> LUA_TNONE) and (LuaType <> LUA_TFUNCTION);
      Exit;
    end;
  end;

  Result := False;
end;

function TLua.ProcExists(const Name: LuaString): Boolean;
var
  Entity: PLuaGlobalEntity;
  LuaType: Integer;
begin
  Entity := GetGlobalEntity(Name, False);
  if (Assigned(Entity)) then
  begin
    if (Entity.Kind = gkProc) then
    begin
      Result := True;
      Exit;
    end else
    if (Entity.Kind = gkScriptVariable) then
    begin
      global_push_value(Entity.Ref);
      LuaType := lua_type(Handle, -1);
      stack_pop;

      Result := (LuaType = LUA_TFUNCTION);
      Exit;
    end;
  end;

  Result := False;
end;

function InternalLuaTableVariable(const Self: TLua; const Table: LuaString; const Name: PLuaString): Boolean;
var
  Entity: PLuaGlobalEntity;
begin
  Entity := Self.GetGlobalEntity(Table, False);
  if (Assigned(Entity)) and (Entity.Kind = gkScriptVariable) then
  begin
    Self.global_push_value(Entity.Ref);
    if (lua_type(Self.Handle, -1) = LUA_TTABLE) then
    begin
      if (Assigned(Name)) then
      begin
        Self.push_lua_string(Name^);
        lua_rawget(Self.Handle, -2);
      end;

      Result := True;
      Exit;
    end;
    lua_settop(Self.Handle, 0);
  end;

  Result := False;
end;

procedure __TLuaGetTableVariable(const SelfVariantMode: NativeInt;
  const Table, Name: LuaString; const Result: Pointer);
var
  VariantMode: Boolean;
  Self: TLua;
  LuaType: Integer;
  Supported: Boolean;
begin
  VariantMode := (SelfVariantMode and 1 <> 0);
  Self := TLua(SelfVariantMode and -2);

  if (InternalLuaTableVariable(Self, Table, @Name)) then
  begin
    LuaType := lua_type(Self.Handle, -1);
    if (LuaType > LUA_TNIL) and (LuaType <> LUA_TFUNCTION) then
    begin
      if (VariantMode) then
      begin
        Supported := Self.stack_variant(PVariant(Result)^, -1, False);
      end else
      begin
        Supported := Self.stack_luaarg(PLuaArg(Result)^, -1, False);
      end;

      lua_settop(Self.Handle, 0);
      if (not Supported) then
      begin
        raise ELua.CreateFmt('Can''t get variable "%s.%s" of type "%s"', [Table, Name,
          Self.FStringBuffer.Default]) at Self.FReturnAddress;
      end;
      Exit;
    end else
    begin
      lua_settop(Self.Handle, 0);
    end;
  end;

  raise ELua.CreateFmt('Table variable "%s.%s" not found', [Table, Name]) at Self.FReturnAddress;
end;

function TLua.GetTableVariable(const Table, Name: LuaString): Variant;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaGetTableVariable(NativeInt(Self) or Ord(True), Table, Name, @Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  or eax, 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  or rcx, 1
  {$endif}
  jmp __TLuaGetTableVariable
end;
{$endif}

function TLua.GetTableVariableEx(const Table, Name: LuaString): TLuaArg;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaGetTableVariable(NativeInt(Self) or Ord(False), Table, Name, @Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaGetTableVariable
end;
{$endif}

procedure __TLuaSetTableVariable(const SelfVariantMode: NativeInt;
  const Table, Name: LuaString; const Value: Pointer);
var
  VariantMode: Boolean;
  Self: TLua;
  Supported: Boolean;
begin
  VariantMode := (SelfVariantMode and 1 <> 0);
  Self := TLua(SelfVariantMode and -2);

  if (InternalLuaTableVariable(Self, Table, nil)) then
  begin
    Self.push_lua_string(Name);

    if (VariantMode) then
    begin
      Supported := Self.push_variant(PVariant(Value)^);
    end else
    begin
      Supported := Self.push_luaarg(PLuaArg(Value)^);
    end;

    if (Supported) then
    begin
      lua_rawset(Self.Handle, -3);
      lua_settop(Self.Handle, 0);
    end else
    begin
      lua_settop(Self.Handle, 0);
      raise ELua.CreateFmt('Can''t set variable "%s.%s" of type "%s"', [Table, Name,
        Self.FStringBuffer.Default]) at Self.FReturnAddress;
    end;
    Exit;
  end;

  raise ELua.CreateFmt('Table "%s" not found', [Table]) at Self.FReturnAddress;
end;

procedure TLua.SetTableVariable(const Table, Name: LuaString; const Value: Variant);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaSetTableVariable(NativeInt(Self) or Ord(True), Table, Name, @Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  or eax, 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  or rcx, 1
  {$endif}
  jmp __TLuaSetTableVariable
end;
{$endif}

procedure TLua.SetTableVariableEx(const Table, Name: LuaString; const Value: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaSetTableVariable(NativeInt(Self) or Ord(False), Table, Name, @Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaSetTableVariable
end;
{$endif}

function TLua.VariableExists(const Table, Name: LuaString): Boolean;
var
  LuaType: Integer;
begin
  if (InternalLuaTableVariable(Self, Table, @Name)) then
  begin
    LuaType := lua_type(Handle, -1);
    lua_settop(Handle, 0);
    Result := (LuaType > LUA_TNIL) and (LuaType <> LUA_TFUNCTION);
    Exit;
  end;

  Result := False;
end;

function TLua.ProcExists(const Table, Name: LuaString): Boolean;
var
  LuaType: Integer;
begin
  if (InternalLuaTableVariable(Self, Table, @Name)) then
  begin
    LuaType := lua_type(Handle, -1);
    lua_settop(Handle, 0);
    Result := (LuaType = LUA_TFUNCTION);
    Exit;
  end;

  Result := False;
end;

type
  TLuaCallOptions = record
    Table: ^LuaString;
    ProcName: ^LuaString;
    ReturnAddress: Pointer;
    Count: Integer;
    VarRecMode: Boolean;
    case Boolean of
      False: (Arg: PLuaArg);
       True: (V: PVarRec);
  end;

function InternalLuaCall(const Self: TLua; const Options: TLuaCallOptions): PLuaResult;
label
  clear_not_found, not_found, fail_argument;
var
  LastReturnAddress: Pointer;
  Entity: PLuaGlobalEntity;
  Proc: ^TLuaClosure;
  Arg: PLuaArg;
  V: PVarRec;
  i, Count, ErrCode: Integer;
begin
  LastReturnAddress := Self.BeginScriptStack(Options.ReturnAddress);
  try
    if (Assigned(Options.Table)) then
    begin
      // push table function
      Entity := Self.GetGlobalEntity(Options.Table^, False);
      if (Assigned(Entity)) then
      begin
        if (Entity.Kind <> gkScriptVariable) then goto not_found;
        Self.global_push_value(Entity.Ref);
        if (lua_type(Self.Handle, -1) <> LUA_TTABLE) then goto clear_not_found;
        Self.push_lua_string(Options.ProcName^);
        lua_rawget(Self.Handle, -2);
        if (lua_type(Self.Handle, -1) <> LUA_TFUNCTION) then goto clear_not_found;
        lua_insert(Self.Handle, -2);
      end;
    end else
    begin
      // push global function
      Entity := Self.GetGlobalEntity(Options.ProcName^, False);
      if (Assigned(Entity)) then
      begin
        case Entity.Kind of
          gkProc:
          begin
            Proc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(Self.FMemoryHeap).Unpack{$endif}(Entity.Ptr);
            //Self.push_function(Proc.Value);
          end;
          gkScriptVariable:
          begin
            Self.global_push_value(Entity.Ref);
            if (lua_type(Self.Handle, -1) <> LUA_TFUNCTION) then
            begin
            clear_not_found:
              lua_settop(Self.Handle, 0);
              goto not_found;
            end;
          end;
        else
        not_found:
          Entity := nil;
        end;
      end;
    end;

    // check not found
    if (not Assigned(Entity)) then
    begin
      Result := nil;
      if (Assigned(Options.Table)) then
      begin
        raise ELua.CreateFmt('Lua table function "%s:%s" not found',
          [Options.Table^, Options.ProcName^]) at Options.ReturnAddress;
      end else
      begin
        raise ELua.CreateFmt('Lua function "%s" not found', [Options.ProcName^]) at Options.ReturnAddress;
      end;
    end;

    // push arguments
    if (Options.VarRecMode) then
    begin
      V := Options.V;
      for i := 1 to Options.Count do
      begin
        if (not Self.push_argument(V^)) then
        begin
          ErrCode := i;
          goto fail_argument;
        end;
        Inc(V);
      end;
    end else
    begin
      Arg := Options.Arg;
      for i := 1 to Options.Count do
      begin
        if (not Self.push_luaarg(Arg^)) then
        begin
          ErrCode := i;
        fail_argument:
          lua_settop(Self.Handle, 0);
          Result := nil;
          raise ELua.CreateFmt('Unknown argument type "%s" (arg N%d)', [Self.FStringBuffer.Default, ErrCode]) at Options.ReturnAddress;
        end;
        Inc(Arg);
      end;
    end;

    // call
    ErrCode := lua_pcall(Self.Handle, Options.Count + Ord(Assigned(Options.Table)), LUA_MULTRET, 0);
    if (ErrCode = 0) then ErrCode := lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0);
    if (ErrCode <> 0) then Self.CheckScriptError(ErrCode, nil);

    // fill buffer
    Result := @Self.FScriptStack[Self.FScriptStackIndex - 1];
    Count := lua_gettop(Self.Handle);
    Result.Count := Count;
    for i := 0 to Count - 1 do
    begin
      if (not Self.stack_luaarg(Result.Items[i], i + 1, False)) then
        Result.Items[i].F.LuaType := ltEmpty;
    end;

    // clear stack
    lua_settop(Self.Handle, 0);
  finally
    Self.EndScriptStack(LastReturnAddress);
  end;
end;

function __TLuaCall_luaargs(const Self: TLua; const ProcName: LuaString;
  const Args: array of TLuaArg): PLuaResult;
var
  Options: TLuaCallOptions;
begin
  Options.Table := nil;
  Options.ProcName := @ProcName;
  Options.ReturnAddress := Pointer(Self.FTempBuffer.N);
  Options.Count := Length(Args);
  Options.VarRecMode := False;
  Options.Arg := Pointer(@Args[0]);
  Result := InternalLuaCall(Self, Options);
end;

function TLua.Call(const ProcName: LuaString; const Args: array of TLuaArg): PLuaResult;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  Result := __TLuaCall_luaargs(Self, ProcName, Args);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FTempBuffer.N
  {$endif}
  jmp __TLuaCall_luaargs
end;
{$endif}

function __TLuaCall__arguments(const Self: TLua; const ProcName: LuaString;
  const Args: array of const): PLuaResult;
var
  Options: TLuaCallOptions;
begin
  Options.Table := nil;
  Options.ProcName := @ProcName;
  Options.ReturnAddress := Pointer(Self.FTempBuffer.N);
  Options.Count := Length(Args);
  Options.VarRecMode := True;
  Options.Arg := Pointer(@Args[0]);
  Result := InternalLuaCall(Self, Options);
end;

function TLua.Call(const ProcName: LuaString; const Args: array of const): PLuaResult;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  Result := __TLuaCall__arguments(Self, ProcName, Args);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FTempBuffer.N
  {$endif}
  jmp __TLuaCall__arguments
end;
{$endif}

function __TLuaTableCall_luaargs(const Self: TLua; const Table, ProcName: LuaString;
  const Args: array of TLuaArg): PLuaResult;
var
  Options: TLuaCallOptions;
begin
  Options.Table := @Table;
  Options.ProcName := @ProcName;
  Options.ReturnAddress := Pointer(Self.FTempBuffer.N);
  Options.Count := Length(Args);
  Options.VarRecMode := False;
  Options.Arg := Pointer(@Args[0]);
  Result := InternalLuaCall(Self, Options);
end;

function TLua.Call(const Table, ProcName: LuaString; const Args: array of TLuaArg): PLuaResult;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  Result := __TLuaTableCall_luaargs(Self, Table, ProcName, Args);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FTempBuffer.N
  {$endif}
  jmp __TLuaTableCall_luaargs
end;
{$endif}

function __TLuaTableCall__arguments(const Self: TLua; const Table, ProcName: LuaString;
  const Args: array of const): PLuaResult;
var
  Options: TLuaCallOptions;
begin
  Options.Table := @Table;
  Options.ProcName := @ProcName;
  Options.ReturnAddress := Pointer(Self.FTempBuffer.N);
  Options.Count := Length(Args);
  Options.VarRecMode := True;
  Options.Arg := Pointer(@Args[0]);
  Result := InternalLuaCall(Self, Options);
end;

function TLua.Call(const Table, ProcName: LuaString; const Args: array of const): PLuaResult;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  Result := __TLuaTableCall__arguments(Self, Table, ProcName, Args);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FTempBuffer.N
  {$endif}
  jmp __TLuaTableCall__arguments
end;
{$endif}

function TLua.InternalConvertScript(var Data: __luabuffer): Integer;
label
  fix_dest;
var
  Size, Offset: Integer;
  S, Dest: PByte;
  Marker: Cardinal;
  BOM: TLuaScriptBOM;
  X, Y: NativeUInt;
  {$ifdef LUA_UNICODE}
  Buffer: __luabuffer;
  {$endif}
begin
  // detect BOM
  Size := Length(Data);
  S := Pointer(Data);
  BOM := High(TLuaScriptBOM);
  Offset := BOM_INFO[BOM].Size;
  repeat
    if (Size >= Offset) then
    begin
      Marker := 0;
      System.Move(S^, Marker, Offset);
      if (Marker = BOM_INFO[BOM].Data) then Break;
    end;

    Dec(BOM);
    Offset := BOM_INFO[BOM].Size;
  until (BOM = sbNone);
  Inc(S, Offset);
  Dec(Size, Offset);

  // sbUTF16BE/cbUTF32/cbUTF32BE --> sbUTF16
  if (BOM in [sbUTF16BE, cbUTF32, cbUTF32BE]) then
  begin
    Dest := Pointer(Data);
    case BOM of
      sbUTF16BE:
      begin
        while (Size >= SizeOf(Word)) do
        begin
          X := PWord(S)^;
          Inc(S, SizeOf(Word));
          Dec(Size, SizeOf(Word));

          X := Swap(X);
          PWord(Dest)^ := X;
          Inc(Dest, SizeOf(Word));
        end;
      end;
      cbUTF32, cbUTF32BE:
      begin
        while (Size >= SizeOf(Cardinal)) do
        begin
          X := PCardinal(S)^;
          Inc(S, SizeOf(Cardinal));
          Dec(Size, SizeOf(Cardinal));

          if (BOM = cbUTF32BE) then
          begin
            X := (Swap(X) shl 16) + Swap(X shr 16);
          end;

          if (X <= $ffff) then
          begin
            if (X shr 11 = $1B) then PWord(Dest)^ := $fffd
            else PWord(Dest)^ := X;

            Inc(Dest, SizeOf(Word));
          end else
          begin
            Y := (X - $10000) shr 10 + $d800;
            X := (X - $10000) and $3ff + $dc00;
            X := (X shl 16) + Y;

            PCardinal(Dest)^ := X;
            Inc(Dest, SizeOf(Cardinal));
          end;
        end;
      end;
    end;

    Offset := 0;
    Size := NativeInt(Dest) - NativeInt(Data);
    S := Pointer(Data);
    BOM := sbUTF16;
  end;

  // final convert
  {$ifdef LUA_ANSI}
  case BOM of
    sbNone: ;
    sbUTF8:
    begin
      // UTF8 --> ANSI
      Size := AnsiFromUtf8(Pointer(Data), 0, Pointer(S), Size);
      goto fix_dest;
    end;
    sbUTF16:
    begin
      // UTF16 --> ANSI
      Size := AnsiFromUnicode(Pointer(Data), 0, Pointer(S), Size shr 1);
      goto fix_dest;
    end;
  end;
  {$else .LUA_UNICODE}
  case BOM of
    sbUTF8: ;
    sbNone:
    begin
      // ANSI --> UTF8
      SetLength(Buffer, Size * 3 + 1);
      Size := Utf8FromAnsi(Pointer(Buffer), Pointer(S), 0, Size);
      Data := Buffer;
      goto fix_dest;
    end;
    sbUTF16:
    begin
      // UTF16 --> UTF8
      SetLength(Buffer, (Size shr 1) * 3 + 1);
      Size := Utf8FromUnicode(Pointer(Buffer), Pointer(S), Size shr 1);
      Data := Buffer;
      goto fix_dest;
    end;
  end;
  {$endif}

  // result
  Result := Offset;
  Exit;
fix_dest:
  {$ifdef LUA_UNICODE}
  Buffer := {$ifdef NEXTGEN}nil{$else}''{$endif};
  {$endif}
  SetLength(Data, Size);
  {$ifNdef NEXTGEN}
  S := Pointer(Data);
  Inc(S, Size);
  S^ := 0;
  {$endif}
  Result := 0;
end;

const
  CHAR_NONE = 0;
  CHAR_MINUS = 1;
  CHAR_QUOTE = 2;
  CHAR_SLASH = 3;
  CHAR_BRACKET = 4;
  CHAR_POINT = 5;
  CHAR_COLON = 6;
  CHAR_PUNCT = 7;
  CHAR_CRLF = 8;
  CHAR_SPACE = 9;

var
  CHAR_TABLE: array[Byte] of Byte;

procedure InitCharacterTable;
begin
  FillChar(CHAR_TABLE, 33, CHAR_SPACE);
  CHAR_TABLE[13] := CHAR_CRLF;
  CHAR_TABLE[10] := CHAR_CRLF;

  CHAR_TABLE[Ord('-')] := CHAR_MINUS;
  CHAR_TABLE[Ord('"')] := CHAR_QUOTE;
  CHAR_TABLE[Ord('''')] := CHAR_QUOTE;
  CHAR_TABLE[Ord('\')] := CHAR_SLASH;
  CHAR_TABLE[Ord('(')] := CHAR_BRACKET;
  CHAR_TABLE[Ord('.')] := CHAR_POINT;
  CHAR_TABLE[Ord(':')] := CHAR_COLON;

  CHAR_TABLE[Ord('!')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('#')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('$')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('%')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('&')] := CHAR_PUNCT;
  CHAR_TABLE[Ord(')')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('*')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('+')] := CHAR_PUNCT;
  CHAR_TABLE[Ord(',')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('/')] := CHAR_PUNCT;
  CHAR_TABLE[Ord(';')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('<')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('=')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('>')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('?')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('@')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('[')] := CHAR_PUNCT;
  CHAR_TABLE[Ord(']')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('^')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('`')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('{')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('}')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('|')] := CHAR_PUNCT;
  CHAR_TABLE[Ord('~')] := CHAR_PUNCT;
end;

function SkipScriptStringIdentifier(S, Top: PByte): PByte;
var
  First, X: NativeUInt;
begin
  First := S^;
  Inc(S);

  repeat
    if (S = Top) then Break;
    X := CHAR_TABLE[S^];
    Inc(S);
    if (X = 0) then Continue;

    case X of
      CHAR_QUOTE:
      begin
        Dec(S);
        X := S^;
        Inc(S);
        if (X = First) then Break;
      end;
      CHAR_SLASH:
      begin
        if (S = Top) then Break;
        Inc(S);
      end;
      CHAR_CRLF: Break;
    end;
  until (False);

  Result := S;
end;

procedure TLua.InternalPreprocessScript(var Buffer: __luabuffer; const Offset: Integer);
label
  line_comment, clear_chars, replace, next_find;
var
  S, Start, Top, StoredS, StoredPrint: PByte;
  Unique: Boolean;
  Size: Integer;
  X: NativeUInt;
  PtrOffset: NativeInt;
begin
  // initialize buffer
  Unique := False;
  Size := Length(Buffer) - Offset;
  NativeInt(S) := NativeInt(Buffer) + Offset;
  NativeInt(Top) := NativeInt(S) + Size;

  // remove comments
  repeat
    if (S = Top) then Break;
    X := CHAR_TABLE[S^];
    Inc(S);
    if (X = 0) then Continue;

    case X of
      CHAR_QUOTE:
      begin
        Dec(S);
        S := SkipScriptStringIdentifier(S, Top);
      end;
      CHAR_SLASH:
      begin
        if (S = Top) then Break;
        Inc(S);
      end;
      CHAR_MINUS:
      begin
        Start := S;
        Dec(Start);
        if (S = Top) then Break;
        if (S^ <> Ord('-')) then Continue;
        Inc(S);
        if (S = Top) then goto clear_chars;

        if (S^ = Ord('[')) then
        begin
          // multy-line comment
          Inc(S);
          if (S = Top) or (S^ <> Ord('[')) then goto line_comment;
          repeat
            if (S = Top) then Break;
            X := S^;
            Inc(S);
            if (X = Ord(']')) then
            begin
              if (S = Top) then Break;
              X := S^;
              Inc(S);
              if (X = Ord(']')) then Break;
            end;
          until (False);
        end else
        begin
        line_comment:
          repeat
            if (S = Top) then Break;
            X := CHAR_TABLE[S^];
            Inc(S);
            if (X = CHAR_CRLF) then Break;
          until (False);
        end;

      clear_chars:
        if (not Unique) then
        begin
          PtrOffset := UniqueLuaBuffer(Buffer);
          Inc(NativeInt(Start), PtrOffset);
          Inc(NativeInt(S), PtrOffset);
          Inc(NativeInt(Top), PtrOffset);
          Unique := True;
        end;

        repeat
          if (CHAR_TABLE[Start^] <> CHAR_CRLF) then Start^ := 32;
          Inc(Start);
        until (Start = S);
      end;
    end;
  until (False);

  // replace print --> _PNT_
  NativeInt(Start) := NativeInt(Buffer) + Offset;
  S := Start;
  repeat
    if (S = Top) then Break;

    // find '(' character
    repeat
      if (S = Top) then Break;
      X := CHAR_TABLE[S^];
      Inc(S);
      if (X = 0) then Continue;

      case X of
        CHAR_QUOTE:
        begin
          Dec(S);
          S := SkipScriptStringIdentifier(S, Top);
        end;
        CHAR_SLASH:
        begin
          if (S = Top) then Exit;
          Inc(S);
        end;
        CHAR_BRACKET:
        begin
          Break;
        end;
      end;
    until (False);
    StoredS := S;
    Dec(S){'('};

    // skip space chars: func..(
    repeat
      if (S = Start) then goto next_find;
      Dec(S);
      X := CHAR_TABLE[S^];
    until (X <> CHAR_SPACE);
    if (X <> 0) then goto next_find;

    // detect "print" function
    if (S^ <> Ord('t')) then goto next_find;
    if (S = Start) then goto next_find;
    Dec(S);
    if (S^ <> Ord('n')) then goto next_find;
    if (S = Start) then goto next_find;
    Dec(S);
    if (S^ <> Ord('i')) then goto next_find;
    if (S = Start) then goto next_find;
    Dec(S);
    if (S^ <> Ord('r')) then goto next_find;
    if (S = Start) then goto next_find;
    Dec(S);
    if (S^ <> Ord('p')) then goto next_find;
    StoredPrint := S;

    // detect not "."/":" before "print"
    repeat
      if (S = Start) then goto replace;
      Dec(S);
      X := CHAR_TABLE[S^];
    until (X < CHAR_CRLF);
    if (X = CHAR_POINT) or (X = CHAR_COLON) then goto next_find;

  replace:
    if (not Unique) then
    begin
      PtrOffset := UniqueLuaBuffer(Buffer);
      Inc(NativeInt(Start), PtrOffset);
      Inc(NativeInt(Top), PtrOffset);
      Inc(NativeInt(StoredPrint), PtrOffset);
      Inc(NativeInt(StoredS), PtrOffset);
      Unique := True;
    end;
    S := StoredPrint;
    S^ := Ord('_');
    Inc(S);
    S^ := Ord('P');
    Inc(S);
    S^ := Ord('N');
    Inc(S);
    S^ := Ord('T');
    Inc(S);
    S^ := Ord('_');

  next_find:
    S := StoredS;
  until (False);
end;

function TLua.InternalRunScript(var Data: __luabuffer; const Offset: Integer; const AUnitName: __luaname;
  const AUnit: TLuaUnit; const MakeResult: Boolean; var ExceptionAddress: Pointer; const ReturnAddress: Pointer): Exception;
var
  Buffer: __luabuffer;
  BufferOffset, ErrCode, Size: Integer;
  LastReturnAddress: Pointer;
  Memory: __luadata;
  {$ifdef CPUINTEL}
  CW: Word;
  {$endif}
begin
  // preprocessing, compilation, execution
  Result := nil;
  try
    // preprocess
    Buffer := Data;
    InternalPreprocessScript(Buffer, Offset);

    // user preprocessing
    BufferOffset := Offset;
    if (Assigned(FOnPreprocessScript)) then
      FOnPreprocessScript(Buffer, Data, BufferOffset, AUnit.Name, UniqueLuaBuffer);

    // compilation, execution
    LastReturnAddress := BeginScriptStack(ReturnAddress);
    try
      {$ifdef CPUINTEL}
      {$WARN SYMBOL_PLATFORM OFF}
      CW := Get8087CW;
      Set8087CW($037F {default intel C++ mode});
      try
      {$endif}
      begin
        Size := Length(Buffer) - BufferOffset;
        NativeInt(Memory) := NativeInt(Buffer) + BufferOffset;
        ErrCode := luaL_loadbuffer(Handle, Memory, Size, AUnitName);
        if (ErrCode = 0) then ErrCode := lua_pcall(Handle, 0, 0, 0);
        if (ErrCode = 0) then ErrCode := lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
        if (ErrCode <> 0) and (MakeResult) then CheckScriptError(ErrCode, AUnit);
      end;
      {$ifdef CPUINTEL}
      finally
        Set8087CW(CW);
      end;
      {$WARN SYMBOL_PLATFORM ON}
      {$endif}
    finally
      EndScriptStack(LastReturnAddress);
    end;
  except
    on E: Exception do
    if (MakeResult) then
    begin
      AcquireExceptionObject;
      ExceptionAddress := ExceptAddr;
      Result := E;
    end;
  end;
end;

procedure TLua.InternalLoadScript(var Data: __luabuffer; const UnitName: LuaString;
  const FileName: string);
const
  MAX_UNITNAME_LENGTH = 255;
var
  Offset, Count: Integer;
  LuaUnitName: __luaname;
  LuaUnitNameBuffer: array[0..MAX_UNITNAME_LENGTH * 3] of AnsiChar;
  AUnit, ALastUnit: TLuaUnit;
  E: Exception;
  ReturnAddress, ExceptAddress, Temp: Pointer;
begin
  // convert script data
  Offset := InternalConvertScript(Data);

  // unit name
  Count := Length(UnitName);
  if (Count = 0) then
  begin
    LuaUnitName := Pointer(@NULL_CHAR);
  end else
  begin
    if (Ord(UnitName[1]) <= 32) or (Ord(UnitName[Count]) <= 32) or (Count > MAX_UNITNAME_LENGTH) then
      raise ELua.Create('Invalid unit name') at FReturnAddress;

    {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
      {$ifdef LUA_UNICODE}
        Count := Utf8FromUnicode(Pointer(@LuaUnitNameBuffer), Pointer(UnitName), Count);
      {$else .LUA_ANSI}
        Count := AnsiFromUnicode(Pointer(@LuaUnitNameBuffer), 0, Pointer(UnitName), Count);
      {$endif}
    {$else .LUA_ANSI.ANSI}
      System.Move(Pointer(UnitName)^, LuaUnitNameBuffer, Count);
    {$ifend}

    Byte(LuaUnitNameBuffer[Count]) := 0;
    LuaUnitName := Pointer(@LuaUnitNameBuffer);
  end;

  // unit, last unit
  if (Count = 0) then
  begin
    ALastUnit := nil;
  end else
  begin
    ALastUnit := Self.GetUnitByName(UnitName);
  end;
  AUnit := TLuaUnit.Create;
  AUnit.FLua := Self;
  AUnit.FIndex := -1;
  AUnit.FName := UnitName;
  AUnit.FNameLength := Length(UnitName);
  AUnit.FData := Data;
  AUnit.FDataOffset := Offset;
  AUnit.FFileName := FileName;
  AUnit.InitializeLines;

  // try run script
  ReturnAddress := FReturnAddress;
  E := InternalRunScript(Data, Offset, LuaUnitName, AUnit, True, ExceptAddress, ReturnAddress);

  // add/replace unit or cleanup and compile/run previous instance
  if (E = nil) then
  begin
    if (Assigned(ALastUnit)) then
    begin
      AUnit.FIndex := ALastUnit.FIndex;
      FUnits[AUnit.FIndex] := AUnit;
      ALastUnit.Free;
    end else
    begin
      AUnit.FIndex := FUnitCount;
      Inc(FUnitCount);
      SetLength(FUnits, FUnitCount);
      FUnits[AUnit.FIndex] := AUnit;
    end;
  end else
  begin
    AUnit.Free;

    if (Assigned(ALastUnit)) then
    begin
      InternalRunScript(ALastUnit.FData, ALastUnit.FDataOffset, LuaUnitName, ALastUnit,
        False, Temp, ReturnAddress);
    end;

    raise E at ExceptAddress;
  end;
end;

procedure CheckScriptSize(const Self: TLua; const Size: Int64);
begin
  if (Size < 0) or (Size > $800000) then
   raise ELuaScript.CreateFmt('Incorrect script size: %d', [Size])
     at Self.FReturnAddress;
end;

procedure __TLuaRunScript(const Self: TLua; const Script: LuaString);
const
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
    BOM: Cardinal = $0000FEFF;
    BOM_SIZE = 2;
  {$else}
    BOM: Cardinal = $00000000;
    BOM_SIZE = 0;
  {$ifend}
var
  BufferSize: Integer;
  Data: __luabuffer;
begin
  BufferSize := Length(Script) * SizeOf(LuaChar);
  CheckScriptSize(Self, BufferSize);

  SetLength(Data, BOM_SIZE + BufferSize);
  System.Move(BOM, Pointer(Data)^, BOM_SIZE);
  System.Move(Pointer(Script)^, Pointer(NativeInt(Data) + BOM_SIZE)^, BufferSize);

  Self.InternalLoadScript(Data, '', '');
end;

procedure TLua.RunScript(const Script: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRunScript(Self, Script);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRunScript
end;
{$endif}

procedure __TLuaLoadFileScript(const Self: TLua; const FileName: string);
var
  Handle: THandle;
  {$ifdef MSWINDOWS}
    P: TPoint;
  {$endif}
  {$ifdef POSIX}
    S: _stat;
  {$endif}
  Size64: Int64;
  Size: Integer;
  Data: __luabuffer;
begin
  if (not FileExists(FileName)) then
  begin
    raise ELua.CreateFmt('File "%s" not found', [FileName]) at Self.FReturnAddress;
  end;

  {$ifdef MSWINDOWS}
  Handle := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.CreateFile(PChar(FileName), $0001{FILE_READ_DATA}, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  {$else}
  Handle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  {$endif}
  if (Handle = INVALID_HANDLE_VALUE) then
    raise ELua.CreateFmt('Cannot open file "%s"', [FileName]) at Self.FReturnAddress;
  try
    {$ifdef MSWINDOWS}
      P.X := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.GetFileSize(Handle, @P.Y);
      if (P.Y = -1) then P.X := -1;
      Size64 := PInt64(@P)^;
    {$endif}
    {$ifdef POSIX}
      if (fstat(Handle, S) = 0) then
        Size64 := S.st_size
      else
        Size64 := -1;
    {$endif}

    CheckScriptSize(Self, Size64);
    Size := Size64;

    SetLength(Data, Size);
    Size := FileRead(Handle, Pointer(Data)^, Size);
    if (Size < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};
    SetLength(Data, Size);

    Self.InternalLoadScript(Data, LuaString(ExtractFileName(FileName)), FileName);
  finally
    FileClose(Handle);
  end;
end;

procedure TLua.LoadScript(const FileName: string);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaLoadFileScript(Self, FileName);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaLoadFileScript
end;
{$endif}

procedure __TLuaLoadBufferScript(const Self: TLua; const Buffer: Pointer;
  const BufferSize: Integer; const BOM: TLuaScriptBOM; const UnitName: LuaString);
var
  Size: Integer;
  Data: __luabuffer;
begin
  CheckScriptSize(Self, BufferSize);

  Size := BOM_INFO[BOM].Size;
  SetLength(Data, Size + BufferSize);
  System.Move(BOM_INFO[BOM].Data, Pointer(Data)^, Size);
  System.Move(Buffer^, Pointer(NativeInt(Data) + Size)^, BufferSize);

  Self.InternalLoadScript(Data, UnitName, '');
end;

procedure TLua.LoadScript(const Buffer: Pointer; const BufferSize: Integer;
  const BOM: TLuaScriptBOM; const UnitName: LuaString);
{$ifdef RETURNADDRESS}
begin
  FReturnAddress := ReturnAddress;
  __TLuaLoadBufferScript(Self, Buffer, BufferSize, BOM, UnitName);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaLoadBufferScript
end;
{$endif}

{$ifNdef LUA_NOCLASSES}
procedure __TLuaLoadStreamScript(const Self: TLua; const Stream: TStream;
  const UnitName: LuaString; const ASize: Integer);
var
  Size64: Int64;
  Size: Integer;
  Data: __luabuffer;
begin
  Size64 := ASize;
  if (ASize < 0) then Size64 := Stream.Size - Stream.Position;
  CheckScriptSize(Self, Size64);
  Size := Size64;

  SetLength(Data, Size);
  Stream.ReadBuffer(Pointer(Data)^, Size);
  Self.InternalLoadScript(Data, UnitName, '');
end;

procedure TLua.LoadScript(const Stream: TStream; const UnitName: LuaString; const ASize: Integer);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaLoadStreamScript(Self, Stream, UnitName, ASize);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaLoadStreamScript
end;
{$endif}
{$endif}

{$ifdef KOL}
procedure __TLuaLoadKOLStreamScript(const Self: TLua; const Stream: KOL.PStream;
  const UnitName: LuaString; const ASize: Integer);
var
  Size64: Int64;
  Size: Integer;
  Data: __luabuffer;
begin
  Size64 := ASize;
  if (ASize < 0) then Size64 := Stream.Size - Stream.Position;
  CheckScriptSize(Self, Size64);
  Size := Size64;

  SetLength(Data, Size);
  if (Stream.Read(Pointer(Data)^, Size) <> Size) then
    raise ELua.Create('Stream read error') at Self.FReturnAddress;
  Self.InternalLoadScript(Data, UnitName, '');
end;

procedure TLua.LoadScript(const Stream: KOL.PStream; const UnitName: LuaString; const ASize: Integer);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaLoadKOLStreamScript(Self, Stream, UnitName, ASize);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaLoadKOLStreamScript
end;
{$endif}
{$endif}

{$ifdef MSWINDOWS}
procedure __TLuaLoadResourceScript(const Self: TLua; const Instance: THandle;
  const Name, ResType: PChar; const UnitName: LuaString);

  procedure RaiseNotFound(const Name, ResType: PChar);
  var
    V: NativeUInt;
    N, T: string;
  begin
    V := NativeUInt(Name);
    if (V <= High(Word)) then N := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else N := '"' + string(Name) + '"';

    V := NativeUInt(ResType);
    if (V <= High(Word)) then T := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else T := '"' + string(ResType) + '"';

    raise ELua.CreateFmt('Resource %s (%s) not found', [N, T]) at Self.FReturnAddress;
  end;

var
  HResInfo: THandle;
  HGlobal: THandle;
  Size: Integer;
  Data: __luabuffer;
begin
  HResInfo := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.FindResource(Instance, Name, ResType);
  if (HResInfo = 0) then RaiseNotFound(Name, ResType);
  HGlobal := LoadResource(Instance, HResInfo);
  if (HGlobal = 0) then RaiseNotFound(Name, ResType);

  try
    Size := SizeOfResource(Instance, HResInfo);
    CheckScriptSize(Self, Size);

    SetLength(Data, Size);
    System.Move(LockResource(HGlobal)^, Pointer(Data)^, Size);

    Self.InternalLoadScript(Data, UnitName, '');
  finally
    FreeResource(HGlobal);
  end;
end;

procedure TLua.LoadScript(const Instance: THandle; const Name, ResType: PChar;
  const UnitName: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaLoadResourceScript(Self, Instance, Name, ResType, UnitName);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaLoadResourceScript
end;
{$endif}
{$endif}

const
  // TLuaGlobalKind = (gkMetaType, gkVariable, gkProc, gkConst, gkScriptVariable);
  NATIVE_GLOBAL_KINDS: set of TLuaGlobalKind = [gkMetaType, gkVariable, gkProc];
  GLOBAL_INDEX_KINDS: set of TLuaGlobalKind = [gkConst, gkScriptVariable];
  CONST_GLOBAL_KINDS: set of TLuaGlobalKind = [gkMetaType, gkProc, gkConst];


(*function TLua.AllocMethod: __luapointer;
begin
  if (TLuaStack(FEmptyMethods).Count <> 0) then
  begin
    Result := TLuaStack(FEmptyMethods).Pop;
  end else
  begin
    Result := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaMethod));
  end;
end;

procedure TLua.FreeMethod(const Value: __luapointer);
begin
  TLuaStack(FEmptyMethods).Push(Value);
end;

function TLua.AllocProperty: __luapointer;
begin
  if (TLuaStack(FEmptyProperties).Count <> 0) then
  begin
    Result := TLuaStack(FEmptyProperties).Pop;
  end else
  begin
    Result := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaProperty));
  end;
end;

procedure TLua.FreeProperty(const Value: __luapointer);
begin
  TLuaStack(FEmptyProperties).Push(Value);
end;    *)

{$ifNdef LUA_NATIVEFUNC}
type
  TLuaFunctionParams = packed record
    Lua: TLua;
    Callback: function(const Lua: TLua; const P1, P2: __luapointer): Integer;
    P1: __luapointer;
    P2: __luapointer;
  end;
  PLuaFunctionParams = ^TLuaFunctionParams;

function LuaFuntionWrapper(L: Plua_State): Integer; cdecl;
var
  Params: PLuaFunctionParams;
begin
  Params := lua_touserdata(L, LUA_UPVALUEINDEX);
  Result := Params.Callback(Params.Lua, Params.P1, Params.P2);
end;
{$endif}

function TLua.push_newfunction(const Callback: Pointer; const P1, P2: __luapointer;
  const StoreMode: Boolean): __luafunction;
{$ifdef LUA_NATIVEFUNC}
begin
  Result := TLuaCFunctionHeap(FCFunctionHeap).Alloc(Self, Callback, P1, P2);
  lua_pushcclosure(Handle, Result, 0);
end;
{$else}
var
  Params: PLuaFunctionParams;
begin
  Params := lua_newuserdata(Handle, SizeOf(TLuaFunctionParams));
  Params.Lua := Self;
  Params.Callback := Callback;
  Params.P1 := P1;
  Params.P2 := P2;
  lua_pushcclosure(Handle, LuaFuntionWrapper, 1);

  if (StoreMode) then
  begin
    Result := global_alloc_ref;
    lua_pushvalue(Handle, -1);
    global_fill_value(Result);
  end else
  begin
    Result := 0;
  end;
end;
{$endif}

procedure TLua.push_function(const Func: __luafunction);
begin
  {$ifdef LUA_NATIVEFUNC}
    lua_pushcclosure(Handle, Func, 0);
  {$else}
    global_push_value(Func);
  {$endif}
end;

function TLua.function_topointer(const StackIndex: Integer): Pointer;
{$ifdef LUA_NATIVEFUNC}
var
  Data: PLuaCFunctionData;
  Offset: NativeInt;
  {$ifdef LARGEINT}
  Heap: ^TLuaMemoryHeap;
  {$endif}
  MetaType: ^TLuaMetaType;
  Proc: ^TLuaClosure;
begin
  Result := Pointer(lua_tocfunction(Handle, StackIndex));

  if (not Assigned(Result)) then Exit;
  with PLuaCFunctionPage(NativeInt(Result) and MEMORY_PAGE_CLEAR)^ do
    if (MarkerLow <> MEMORY_PAGE_MARKER_LOW) or (MarkerHigh <> MEMORY_PAGE_MARKER_HIGH) then Exit;

  // callback address
  Data := Result;
  {$ifdef CPUX86}
    Offset := PInteger(@Data.Bytes[16])^;
    Result := Pointer(Offset + (NativeInt(@Data.Bytes[15]) + 5));
  {$endif}
  {$ifdef CPUX64}
    if (Data.Bytes[21] = $E9) then
    begin
      Offset := PInteger(@Data.Bytes[21])^;
      Result := Pointer(Offset + (NativeInt(@Data.Bytes[21]) + 5));
    end else
    begin
      Result := PPointer(@Data.Bytes[23])^;
    end;
  {$endif}
  if (Result <> @TLua.ProcCallback) then Exit;

  // parameters
  {$ifdef CPUX86}
    MetaType := PPointer(@Data.Bytes[6])^;
    Proc := PPointer(@Data.Bytes[11])^;
  {$endif}
  {$ifdef CPUX64}
    Heap := Pointer(@TLua(PPointer(@Data.Bytes[2])^).FMemoryHeap);
    MetaType := Heap.Unpack(PInteger(@Data.Bytes[11])^);
    Proc := Heap.Unpack(PInteger(@Data.Bytes[17])^);
  {$endif}

  // result (optional virtual)
  Result := Proc.Address;
  if (NativeUInt(Result) >= PROPSLOT_VIRTUAL) then
    Result := PPointer(NativeInt(MetaType.F.AClass) + NativeInt(Result) and PROPSLOT_CLEAR)^;
end;
{$else}
var
  Params: PLuaFunctionParams;
  {$ifdef LARGEINT}
  Heap: ^TLuaMemoryHeap;
  {$endif}
  MetaType: ^TLuaMetaType;
  Proc: ^TLuaClosure;
begin
  Result := Pointer(lua_tocfunction(Handle, StackIndex));
  if (not Assigned(Result)) or (Result <> @LuaFuntionWrapper) then Exit;

  // callback address
  lua_getupvalue(Handle, StackIndex, 1);
  Params := lua_touserdata(Handle, -1);
  stack_pop;
  Result := @Params.Callback;
  if (Result <> @TLua.ProcCallback) then Exit;

  // parameters
  {$ifdef CPUX86}
    MetaType := Pointer(Params.P1);
    Proc := Pointer(Params.P2);
  {$endif}
  {$ifdef CPUX64}
    Heap := Pointer(@Params.Lua.FMemoryHeap);
    MetaType := Heap.Unpack(Params.P1);
    Proc := Heap.Unpack(Params.P2);
  {$endif}

  // result (optional virtual)
  Result := Proc.Address;
  if (NativeUInt(Result) >= PROPSLOT_VIRTUAL) then
    Result := PPointer(NativeInt(MetaType.F.AClass) + NativeInt(Result) and PROPSLOT_CLEAR)^;
end;
{$endif}

const
  LUA_CLOSURE_MARKER = Byte((Ord('L') shr 2) + (Ord('C') shl 2) or (Ord('M') shr 4));

function LuaClosureWrapper(L: Plua_State): Integer; cdecl;
label
  invalid_args_count;
const
  INSTANCE_KINDS: array[TLuaInstanceKind] of string = (
     'global', 'record', 'class instance', 'class instance', 'class', 'interface'
    );
  CLOSURE_KINDS: array[TLuaClosureKind] of string = (
    'method', 'method', 'method', 'method',
    'constructor', 'destructor', 'method', 'reference'
  );
var
  Closure: PLuaClosure;
  ClosureArgsCount, ArgsCount: Integer;
  Invokable: PLuaInvokable;
  Buffer: array[0..2047] of Byte;
  InvokableData: Pointer;
  Name: ^LuaString;
begin
  Closure := lua_touserdata(L, LUA_UPVALUEINDEX);
  ClosureArgsCount := lua_gettop(TLua(Closure.Header.Lua).Handle);

  if (Closure.Header.InvokableMode) then
  begin
    Invokable := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(TLua(Closure.Header.Lua).FMemoryHeap).Unpack{$endif}(Closure.Invokable);
    ArgsCount := Invokable.ParamCount;
    if (ClosureArgsCount <> ArgsCount) then
      goto invalid_args_count;

    if (Invokable.TotalDataSize <= SizeOf(Buffer)) then
    begin
      if (Invokable.InitialCount <> 0) then
        Invokable.Initialize(Pointer(@Buffer));

      if (Invokable.FinalCount = 0) then
      begin
        Result := TLua(Closure.Header.Lua).InvokableCallback(Closure, Pointer(@Buffer));
      end else
      begin
        try
          Result := TLua(Closure.Header.Lua).InvokableCallback(Closure, Pointer(@Buffer));
        finally
          Invokable.Finalize(Pointer(@Buffer));
        end;
      end;
    end else
    begin
      GetMem(InvokableData, Invokable.TotalDataSize);
      try
        if (Invokable.InitialCount <> 0) then
          Invokable.Initialize(InvokableData);

        if (Invokable.FinalCount = 0) then
        begin
          Result := TLua(Closure.Header.Lua).InvokableCallback(Closure, InvokableData);
        end else
        begin
          try
            Result := TLua(Closure.Header.Lua).InvokableCallback(Closure, InvokableData);
          finally
            Invokable.Finalize(InvokableData);
          end;
        end;
      finally
        FreeMem(InvokableData);
      end;
    end;
  end else
  begin
    ArgsCount := Closure.ArgsCount;
    if (ArgsCount <> -1) and (ClosureArgsCount <> ArgsCount) then
      goto invalid_args_count;

    Result := TLua(Closure.Header.Lua).ClosureCallback(Closure, ClosureArgsCount);
  end;

  Exit;
invalid_args_count:
  Name := @TLua(Closure.Header.Lua).FStringBuffer.Lua;
  TLua(Closure.Header.Lua).unpack_lua_string(Name^, Closure.Header.Name);
  TLua(Closure.Header.Lua).Error('Invalid arguments count of %s %s %s (%d), reqired: %d',
    [
      INSTANCE_KINDS[Closure.Header.InstanceKind],
      CLOSURE_KINDS[Closure.Header.Kind],
      Name^,
      ClosureArgsCount,
      ArgsCount
    ]);

  Result := 0;
end;

procedure TLua.push_closure(const Name: __luaname; const Kind: Byte{TLuaClosureKind}; const InstanceKind: Byte{TLuaInstanceKind};
  const Instance: Pointer; const Address: Pointer; const InvokableMode: Boolean; const InvokableArgsCount: __luapointer);
var
  Closure: PLuaClosure;
begin
  Closure := lua_newuserdata(Handle, SizeOf(TLuaClosure));
  Closure.Header.Lua := Pointer(Self);
  Closure.Header.Marker := LUA_CLOSURE_MARKER;
  Closure.Header.Kind := TLuaClosureKind(Kind);
  Closure.Header.InstanceKind := TLuaInstanceKind(InstanceKind);
  Closure.Header.InvokableMode := InvokableMode;
  Closure.Header.Name := Name;

  Closure.Address := Address;
  Closure.Invokable := InvokableArgsCount;

  Closure.Instance := nil;
  case TLuaInstanceKind(InstanceKind) of
    {.$ifdef AUTOREFCOUNT}
    ikObject:
    begin
      PLuaObjectClosure(Closure).Instance := TObject(Instance);
      // ToDo
    end;
    {.$endif}
    {.$ifdef WEAKREF}
    ikWeakObject:
    begin
      PLuaWeakClosure(Closure).Instance := TObject(Instance);
      // ToDo
    end;
    {.$endif}
    ikInterface:
    begin
      PLuaInterfaceClosure(Closure).Instance := IInterface(Instance);
      // ToDo
    end;
  else
    Closure.Instance := Instance;
  end;

  lua_pushcclosure(Handle, LuaClosureWrapper, 1);
end;

procedure TLua.InternalRegisterCallback(const Name: __luaname; const Callback: Pointer; const P1, P2: __luapointer);
begin
  lua_pushlstring(Handle, Pointer(Name), LStrLen(Name));
  push_newfunction(Callback, P1, P2, False);
  lua_rawset(Handle, -3);
end;

procedure TLua.InternalUnregisterCallback(const Name: __luaname);
begin
  lua_pushlstring(Handle, Pointer(Name), LStrLen(Name));

  {$ifdef LUA_NATIVEFUNC}
  lua_pushvalue(Handle, -1);
  lua_rawget(Handle, -3);
  TLuaCFunctionHeap(FCFunctionHeap).Free(Pointer(lua_tocfunction(Handle, -1)));
  stack_pop;
  {$endif}

  lua_pushnil(Handle);
  lua_rawset(Handle, -3);
end;

function TLua.InternalRegisterMetaTable(const MetaType: PLuaMetaType): Integer;
const
  LUA_GLOBALSINDEX = -10002;
  LUA_RIDX_GLOBALS = 2;
begin
  Result := global_alloc_ref;

  lua_createtable(Handle, 0, 0);
  if (MetaType <> nil) then
  begin
    lua_pushlightuserdata(Handle, MetaType);
    lua_rawseti(Handle, -2, 0);
  end;
  global_fill_value(Result);

  if (not Assigned(MetaType)) then
  begin
    if (LUA_VERSION_52) then
    begin
      lua_rawgeti(Handle, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
      global_push_value(Result);
      lua_setmetatable(Handle, -2);
      stack_pop;
    end else
    begin
      global_push_value(Result);
      lua_setmetatable(Handle, LUA_GLOBALSINDEX);
    end;
  end else
  begin
    MetaType.F.Ref := Result;

    global_push_value(Result);
    lua_pushvalue(Handle, 1);
    lua_setmetatable(Handle, -2);
    stack_pop;
  end;
end;

function TLua.InternalTableToMetaType(const StackIndex: Integer): PLuaMetaType;
var
  Ptr: Pointer;
begin
  Result := nil;
  if (lua_type(Handle, StackIndex) = LUA_TTABLE) then
  begin
    lua_rawgeti(Handle, StackIndex, 0);
    Ptr := nil;
    if (lua_type(Handle, -1) = LUA_TLIGHTUSERDATA) then Ptr := lua_touserdata(Handle, -1);
    stack_pop;
    if (Ptr <> nil) then
    try
      if (PLuaMetaType(Ptr).F.Marker <> LUA_METATYPE_MARKER) then
        Result := Ptr;
    except
    end;
  end;
end;

function TLua.InternalAddGlobal(const AKind: Byte{TLuaGlobalKind}; const Name: __luaname): Pointer{PLuaGlobalEntity};
const
  KIND_NAMES: array[TLuaGlobalKind] of string = ('type', 'variable', 'method', 'enum', '');
var
  Kind: TLuaGlobalKind absolute AKind;
  Item: PLuaDictionaryItem;
  Value: __luapointer;
  Entity: PLuaGlobalEntity;
begin
  Item := TLuaDictionary(FGlobalEntities).InternalFind(Name, True);
  Value := Item.Value;
  if (Value = LUA_POINTER_INVALID) then
  begin
    Value := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaGlobalEntity));
    Item.Value := Value;
    Entity := TLuaMemoryHeap(FMemoryHeap).Unpack(Value);

    if (Kind in NATIVE_GLOBAL_KINDS) then
    begin
      Entity.Ptr := LUA_POINTER_INVALID;
    end else
    begin
      Entity.Ref := global_alloc_ref;
    end;
  end else
  begin
    Entity := TLuaMemoryHeap(FMemoryHeap).Unpack(Value);
    if (Entity.Kind = Kind) then
    begin
      Result := Entity;
      Exit;
    end;

    // solve name space conflict
    if (Entity.Kind = gkScriptVariable) then
    begin
      if (Kind in GLOBAL_INDEX_KINDS) then
      begin
        // clear variable, but stay Ref
        lua_pushnil(Handle);
        global_fill_value(Entity.Ref);
      end else
      begin
        // dispose variable and Ref, pointer will be filled later
        global_free_ref(Entity.Ref);
        Entity.Ptr := LUA_POINTER_INVALID;
      end;
    end else
    begin
      unpack_lua_string(FStringBuffer.Lua, Name);
      raise ELua.CreateFmt('Global %s "%s" is already registered',
        [KIND_NAMES[Entity.Kind], FStringBuffer.Lua]) at FReturnAddress;
    end;
  end;

  Entity.Kind := Kind;
  Entity.ConstMode := (Kind in CONST_GLOBAL_KINDS);
  Result := Entity;
end;

function TLua.InternalAddMetaType(const Kind: TLuaMetaKind; const Name: LuaString;
  const TypeInfo: Pointer; const InstanceSize: Integer; const AdditionalSize: NativeInt): PLuaMetaType;
const
  SIZES: array[TLuaMetaKind] of Integer = (SizeOf(TLuaClassInfo),
    SizeOf(TLuaRecordInfo), SizeOf(TLuaArrayInfo), SizeOf(TLuaSetInfo));
var
  LuaName: __luaname;
  Size: NativeInt;
  Ptr: __luapointer;
  Entity: PLuaGlobalEntity;
begin
  // global entity
  LuaName := TLuaNames(FNames).Add(Name);
  Entity := InternalAddGlobal(Ord(gkMetaType), LuaName);

  // allocation
  Size := SIZES[Kind] + AdditionalSize;
  Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(Size);
  Entity.Ptr := Ptr;
  Result := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);

  // base properties
  Result.F.Marker := LUA_METATYPE_MARKER;
  PInteger(@Result.F.Kind)^ := Ord(Kind);
  {$ifdef LARGEINT}
  Result.F.Ptr := Ptr;
  {$endif}
  Result.F.TypeInfo := TypeInfo;
  Result.FLua := Self;
  Result.FName := Name;

  // metatable (Ref)
  {Result.F.Ref := } InternalRegisterMetaTable(Result);

  // metatypes dictionary
  TLuaDictionary(FMetaTypes).Add(LuaName, Ptr);
  if (Assigned(TypeInfo)) then
    TLuaDictionary(FMetaTypes).Add(TypeInfo, Ptr);

  // clear instance (specific fields)
  System.FillChar(Pointer(NativeInt(Result) + SizeOf(TLuaMetaType))^,
    Size - SizeOf(TLuaMetaType), #0);
end;
             (*
function  TLua.internal_add_class_info(const is_global_space: boolean = false): integer;
var
  ClassInfo: ^TLuaClassInfo;
begin
  if (is_global_space) then
  begin
    Result := -1;
    ClassInfo := @GlobalNative;
  end else
  begin
    Result := Length(ClassesInfo);
    SetLength(ClassesInfo, Result+1);
    ClassInfo := @ClassesInfo[Result];
  end;

  // заполнение
  ZeroMemory(ClassInfo, sizeof(TLuaClassInfo));
  ClassInfo._ClassIndex := Result;
  ClassInfo._DefaultProperty := -1;
  ClassInfo.ParentIndex := -1;
end;          *)

function TLua.InternalNearestClass(const AClass: TClass): PLuaMetaType;
var
  ClassValue: TClass;
  Item: PLuaDictionaryItem;
begin
  ClassValue := AClass;
  if (AClass <> nil) then
  repeat
    Item := TLuaDictionary(FMetaTypes).InternalFind(Pointer(ClassValue), False);
    if (Assigned(Item)) then
    begin
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Item.Value);
      Exit;
    end else
    begin
      ClassValue := PPointer(NativeInt(ClassValue) + vmtParent)^;
      if (ClassValue = nil) then Break;
      {$ifNdef FPC}
      ClassValue := PPointer(ClassValue)^;
      {$endif}
    end;
  until (False);

  Result := nil;
end;

// add class, if not already exists
// UsePublished means to register all published properties
// in registrator-Class case:
// addition sub-registered class using published registrator-Class methods, fields and properties
function TLua.InternalAddClass(const AClass: TClass; const UsePublished: Boolean): PLuaClassInfo;
var
  i: Integer;
  ClassName: LuaString;
  LuaClassName: __luaname;
  ClassRegistrator, ClassValue, ClassChild: TClass;
  ClassPtr: __luapointer;
  ClassInfo, Parent: PLuaClassInfo;
  Item: PLuaDictionaryItem;

  procedure AddPublishedMethods(const AClass: TClass);
  type
    TMethodRec = packed record
      Size: Word;
      Addr: Pointer;
    case Integer of
      0: (Name: ShortString);
      1: (NameLength: Byte);
    end;
  var
    i, Count: Integer;
    MethodCount: PWord;
    MethodRec: ^TMethodRec;
    Buffer: ShortString;
  begin
    // several registrators case
    if (ClassRegistrator <> nil) and (AClass.ClassParent <> ClassValue) then
    begin
      AddPublishedMethods(AClass.ClassParent);
    end;

    // method records
    MethodCount := PWord(PPointer(NativeInt(AClass) + vmtMethodTable)^);
    if (MethodCount = nil) then Exit;
    MethodRec := Pointer(NativeInt(MethodCount) + SizeOf(Word));

    // add "lua..." methods
    for i := 1 to MethodCount^ do
    begin
      Count := MethodRec.NameLength;
      if (Count > 3) and (Byte(MethodRec.Name[1]) = Byte('l')) and
        (Byte(MethodRec.Name[2]) = Byte('u')) and (Byte(MethodRec.Name[3]) = Byte('a')) then
      begin
        Dec(Count, 3);
        PByte(@Buffer)^ := Count;
        System.Move(MethodRec.Name[4], Buffer[1], Count);
        unpack_lua_string(FStringBuffer.Lua, Buffer);

        //InternalAddMethod(ClassInfo, FStringBuffer.Lua, -1, Ord(pkClosure), MethodRec.Addr);
      end;

      Inc(NativeInt(MethodRec), MethodRec.Size);
    end;
  end;

  procedure AddPublishedProperties(const AClass: TClass);
  var
    TypeInfo: PTypeInfo;
    PropList: {$ifdef UNITSCOPENAMES}System.{$endif}TypInfo.PPropList;
    PropInfo: {$ifdef UNITSCOPENAMES}System.{$endif}TypInfo.PPropInfo;
    PropCount, i: Integer;
    PropName: LuaString;
    Getter, Setter, Flags: NativeUInt;
  begin
    TypeInfo := AClass.ClassInfo;
    if (TypeInfo = nil) then Exit;

    PropCount := GetPropList(TypeInfo, PropList);
    if (PropCount <= 0) then Exit;

    try
      for i := 0 to PropCount-1 do
      begin
        PropInfo := PropList[i];
        unpack_lua_string(PropName, PShortString(@PropInfo.Name)^);

        Flags := PROP_CONSTREF_MODE;
        Getter := NativeUInt(PropInfo.GetProc);
        Setter := NativeUInt(PropInfo.SetProc);
        if (Getter <> 0) then
        begin
          if (Getter >= PROPSLOT_FIELD) then
          begin
            Flags := Flags or NativeUInt(pmField);
            Getter := Getter and PROPSLOT_CLEAR;
          end else
          if (Getter >= PROPSLOT_VIRTUAL) then
          begin
            Flags := Flags or NativeUInt(pmVirtual);
            Getter := Getter and PROPSLOT_CLEAR;
          end else
          begin
            Flags := Flags or NativeUInt(pmStatic);
          end;
        end;
        if (Setter <> 0) then
        begin
          if (Setter >= PROPSLOT_FIELD) then
          begin
            Flags := Flags or (NativeUInt(pmField) shl PROP_SLOTSETTER_SHIFT);
            Setter := Setter and PROPSLOT_CLEAR;
          end else
          if (Setter >= PROPSLOT_VIRTUAL) then
          begin
            Flags := Flags or (NativeUInt(pmVirtual) shl PROP_SLOTSETTER_SHIFT);
            Setter := Setter and PROPSLOT_CLEAR;
          end else
          begin
            Flags := Flags or (NativeUInt(pmStatic) shl PROP_SLOTSETTER_SHIFT);
          end;
        end;
        if (PropInfo.Index <> Low(Integer)) then
        begin
          if (PropInfo.GetProc <> nil) then Inc(Flags, 1);
          if (PropInfo.SetProc <> nil) then Inc(Flags, 1 shl PROP_SLOTSETTER_SHIFT);
        end;

        InternalAddProperty(ClassInfo, PropName, PropInfo.PropType{$ifNdef FPC}^{$endif},
          False, True, Getter, Setter, Flags, PropInfo.Index);
      end;
    finally
      FreeMem(PropList);
    end;
  end;

  procedure AddPublishedFields(const AClass: TClass);
  type
    PClassFieldInfo = ^TClassFieldInfo;
    TClassFieldInfo = packed record
      Offset: Cardinal;
      TypeIndex: Word;
      Name: ShortString;
    end;
    TClassesTable = packed record
      Count: Word;
      Values: array[0..0] of ^TClass;
    end;
    PClassFieldTable = ^TClassFieldTable;
    TClassFieldTable = packed record
      Count: Word;
      Classes: ^TClassesTable;
      Fields: array[0..0] of TClassFieldInfo;
    end;
  var
    i: Integer;
    Table: PClassFieldTable;
    Field: PClassFieldInfo;
    FieldName: LuaString;
    FieldClass: TClass;
  begin
    if (AClass = nil) then Exit;
    AddPublishedFields(AClass.ClassParent);

    Table := PPointer(NativeInt(AClass) + vmtFieldTable)^;
    if (Table = nil) then Exit;

    // classes
    if (Table.Classes <> nil) then
    for i := 0 to Table.Classes.Count - 1 do
    begin
      FieldClass := Table.Classes.Values[i]^;
      if (TLuaDictionary(FMetaTypes).Find(Pointer(FieldClass)) = nil) then
        InternalAddClass(FieldClass, True);
    end;

    // classes fields
    Field := @Table.Fields[0];
    for i := 0 to Table.Count - 1 do
    begin
      unpack_lua_string(FieldName, Field.Name);
      InternalAddProperty(ClassInfo, FieldName, TypeInfo(TObject), False, True,
        Field.Offset, Field.Offset, Ord(pmField) + Ord(pmField) shl PROP_SLOTSETTER_SHIFT,
        Low(Integer));

      Inc(PByte(Field), (SizeOf(Cardinal) + SizeOf(Word) + SizeOf(Byte)) + PByte(@Field.Name)^);
    end;
  end;

begin
  if (NativeUInt(AClass) <= $FFFF) then
    raise ELua.Create('AClass not defined') at FReturnAddress;

  // find exists
  ClassPtr := TLuaDictionary(FMetaTypes).FindValue(Pointer(AClass));
  if (ClassPtr <> LUA_POINTER_INVALID) and (not UsePublished) then
  begin
    Result := TLuaMemoryHeap(FMemoryHeap).Unpack(ClassPtr);
    if (Result.F.Kind <> mtClass) then
    begin
      ClassName := Result.Name;
      raise ELua.CreateFmt('Invalid AClass parameter (%s)', [ClassName]) at FReturnAddress;
    end;
    Exit;
  end;

  // registrator, class name
  ClassValue := AClass;
  ClassRegistrator := nil;
  repeat
    ClassName := LuaString(ClassValue.ClassName);
    if (Length(ClassName) > 3) and (ClassName[1] = 'l') and
      (ClassName[2] = 'u') and (ClassName[3] = 'a') then
    begin
      ClassRegistrator := AClass;
      ClassChild := ClassValue;
      ClassValue := ClassValue.ClassParent;
      if (ClassValue = nil) then
        raise ELua.Create('ClassRegistrator is defined, but really Class not found') at FReturnAddress;

      if (ClassValue.InstanceSize <> ClassChild.InstanceSize) then
        raise ELua.CreateFmt('Class registrator "%s" can''t have own fields', [ClassChild.ClassName]) at FReturnAddress;

      ClassPtr := TLuaDictionary(FMetaTypes).FindValue(Pointer(ClassValue));
    end else
    begin
      Break;
    end;
  until (False);

  // existing or new class
  if (ClassPtr <> LUA_POINTER_INVALID) then
  begin
    ClassInfo := TLuaMemoryHeap(FMemoryHeap).Unpack(ClassPtr);
  end else
  begin
    // check existing name (type)
    LuaClassName := TLuaNames(FNames).Add(ClassName);
    if (TLuaDictionary(FMetaTypes).Find(LuaClassName) <> nil) then
      raise ETypeRegistered(ClassName) at FReturnAddress;

    // globals, metatypes dictionary, metatable
    ClassInfo := Pointer(InternalAddMetaType(mtClass, ClassName, Pointer(ClassValue), SizeOf(Pointer)));

    // register parents
    if (ClassValue.ClassParent = nil) then
    begin
      ClassInfo.Parent := LUA_POINTER_INVALID;
      ClassInfo.DefaultProperty := LUA_POINTER_INVALID;
     end else
    begin
      Parent := InternalAddClass(AClass.ClassParent, UsePublished);
      TLuaDictionary(ClassInfo.FNameSpace).Assign(TLuaDictionary(Parent.FNameSpace));
      Item := Pointer(TLuaDictionary(ClassInfo.FNameSpace).FItems);
      for i := 1 to TLuaDictionary(ClassInfo.FNameSpace).Count do
      begin
        Item.Value := Item.Value or NAMESPACE_FLAG_INHERITED;
        Inc(Item);
      end;

      ClassInfo.Parent := Parent.Ptr;
      ClassInfo.DefaultProperty := Parent.DefaultProperty;
      ClassInfo.CFunctionCreate := Parent.CFunctionCreate;
      ClassInfo.CFunctionFree := Parent.CFunctionFree;
      ClassInfo.AssignCallback := Parent.AssignCallback;
      ClassInfo.CreateCallback := Parent.CreateCallback;
      ClassInfo.CreateCallbackArgsCount := Parent.CreateCallbackArgsCount;
    end;
  end;

  // published
  if (ClassRegistrator <> nil) then
  begin
    AddPublishedMethods(ClassRegistrator);
    AddPublishedProperties(ClassRegistrator);
    AddPublishedFields(ClassRegistrator);
  end else
  if (UsePublished) then
  begin
    AddPublishedMethods(ClassValue);
    AddPublishedProperties(ClassValue);
    AddPublishedFields(ClassValue);
  end;

  // result
  Result := ClassInfo;
end;

// TypeInfo can be the following:
//  - TypeInfo(record)
//  - TypeInfo(Dynamic array of record type)
//  - Pointer(SizeOf(record))
function TLua.InternalAddRecord(const Name: LuaString; const TypeInfo: Pointer;
  const UseRttiContext: Boolean): PLuaRecordInfo;
var
  RecordTypeInfo: PTypeInfo;
  RecordSize: Integer;
  TypeData: PTypeData;
  RecordPtr: __luapointer;
  LuaName: __luaname;
begin
  if (not IsValidIdent(Name)) then
    raise ELua.CreateFmt('Non-supported record type name ("%s")', [Name]) at FReturnAddress;

  if (TypeInfo = nil) then
    raise ELua.CreateFmt('TypeInfo of record "%s" not defined', [Name]) at FReturnAddress;

  RecordTypeInfo := nil;
  if (NativeUInt(TypeInfo) <= $FFFF) then
  begin
    RecordSize := NativeInt(TypeInfo);
  end else
  begin
    TypeData := GetTypeData(TypeInfo);

    // record or dynamic array
    if (PTypeInfo(TypeInfo).Kind in RECORD_TYPES) then
    begin
      RecordSize := TypeData.elSize;
      RecordTypeInfo := TypeInfo;
    end else
    if (PTypeInfo(TypeInfo).Kind = tkDynArray) then
    begin
      RecordSize := TypeData.elSize;

      if (TypeData.elType <> nil) then
      begin
        RecordTypeInfo := TypeData.elType^;
        if (RecordTypeInfo <> nil) and (not (RecordTypeInfo.Kind in RECORD_TYPES)) then
        begin
          unpack_lua_string(FStringBuffer.Lua, PShortString(@RecordTypeInfo.Name)^);
          GetTypeKindName(FStringBuffer.Default, RecordTypeInfo.Kind);
          raise ELua.CreateFmt('Sub dynamic type "%s" is not record type (%s)',
            [FStringBuffer.Lua, FStringBuffer.Default]) at FReturnAddress;
        end;
      end;
    end else
    begin
      GetTypeKindName(FStringBuffer.Default, PTypeInfo(TypeInfo).Kind);
      raise ELua.CreateFmt('Type "%s" is not record or subdynamic type (%s)',
        [Name, FStringBuffer.Default]) at FReturnAddress;
    end;
  end;

  // find existing, name item
  if (Assigned(RecordTypeInfo)) then
  begin
    unpack_lua_string(FStringBuffer.Lua, PShortString(@RecordTypeInfo.Name)^);

    if (FStringBuffer.Lua <> Name) then
      raise ELua.CreateFmt('Mismatch of names: TypeInfo "%s" and "%s" as parameter "Name"',
        [FStringBuffer.Lua, Name]) at FReturnAddress;

    RecordPtr := TLuaDictionary(FMetaTypes).FindValue(RecordTypeInfo);
  end else
  begin
    RecordPtr := LUA_POINTER_INVALID;
  end;
  LuaName := TLuaNames(FNames).Add(Name);
  if (RecordPtr = LUA_POINTER_INVALID) then RecordPtr := TLuaDictionary(FMetaTypes).FindValue(LuaName);

  if (RecordPtr <> LUA_POINTER_INVALID) then
  begin
    Result := TLuaMemoryHeap(FMemoryHeap).Unpack(RecordPtr);

    if (Result.Kind <> mtRecord) then
      raise ETypeRegistered(Name) at FReturnAddress;

    if (Result.Size <> RecordSize) then
      raise ELua.CreateFmt('Size of %s (%d) differs from the previous value (%d)', [Name, RecordSize, Result.Size])
        at FReturnAddress;

    if (Assigned(RecordTypeInfo)) then
    begin
      if (Result.F.TypeInfo = nil) then
      begin
        Result.F.TypeInfo := RecordTypeInfo;
        TLuaDictionary(FMetaTypes).Add(RecordTypeInfo, RecordPtr);
      end else
      if (Result.F.TypeInfo <> RecordTypeInfo) then
      begin
        raise ELua.CreateFmt('TypeInfo of "%s" differs from the previous value', [Name]) at FReturnAddress;
      end;
    end;
  end else
  begin
    Result := Pointer(InternalAddMetaType(mtRecord, Name, RecordTypeInfo, RecordSize));
  end;

  if (UseRttiContext) then
  begin
    // ToDo
  end;
end;

function TLua.InternalAddArray(const Identifier, ItemTypeInfo: Pointer;
  const ABounds: array of Integer): PLuaArrayInfo;
const
  STATIC_DYNAMIC: array[boolean] of string = ('static', 'dynamic');
var
  ArrayTypeInfo, BufTypeInfo: PTypeInfo;
  TypeData: PTypeData;
  ArrayPtr: __luapointer;
  LuaName: __luaname;
  IsDynamic: Boolean;
  ItemSize, Dimention, i, Count: Integer;
  elType: PPTypeInfo;
  AdvancedSize: NativeInt;
begin
  // identifier: name or typeinfo
  begin
    if (NativeUInt(Identifier) <= $FFFF) then
      raise ELua.Create('Array identifier not defined') at FReturnAddress;
    try
      if (TTypeKind(Identifier^) in [tkArray, tkDynArray]) then
      begin
        ArrayTypeInfo := PTypeInfo(Identifier);
        unpack_lua_string(FStringBuffer.Lua, PShortString(@ArrayTypeInfo.Name)^);
      end else
      begin
        FStringBuffer.Lua := PLuaChar(Identifier);
        ArrayTypeInfo := nil;
      end;
    except
      raise ELua.Create('Array identifier is invalid') at FReturnAddress;
    end;
    if (not IsValidIdent(FStringBuffer.Lua)) then
      raise ELua.CreateFmt('Non-supported array type name ("%s")', [FStringBuffer.Lua]) at FReturnAddress;
  end;

  // static/dynamic
  IsDynamic := Assigned(ArrayTypeInfo) and (ArrayTypeInfo.Kind = tkDynArray);
  if (IsDynamic <> (Length(ABounds) = 0)) then
  begin
    if (IsDynamic) then
    begin
      raise ELua.CreateFmt('Dynamic array "%s" has no bounds', [FStringBuffer.Lua]) at FReturnAddress;
    end else
    begin
      raise ELua.CreateFmt('Array information of "%s" not defined', [FStringBuffer.Lua]) at FReturnAddress;
    end;
  end;

  // item: typeinfo, size
  // ToDo
  begin
    ItemSize := 100500;
    (*if (itemtypeinfo = nil) then
    ELua.Assert('"%s" registering... The typeinfo of %s array item not defined', [Name, STATIC_DYNAMIC[IsDynamic]], CodeAddr);
    PropertyInfo.Base := GetLuaPropertyBase(Self, '', Name, ptypeinfo(itemtypeinfo), CodeAddr);
    itemtypeinfo := PropertyInfo.Base.Information;
    FItemSize := GetLuaItemSize(PropertyInfo.Base);
    if (FItemSize = 0) then ELua.Assert('"%s" registering... The size of %s array item not defined', [Name, STATIC_DYNAMIC[IsDynamic]], CodeAddr);*)
  end;

  // Dimention, Bounds
  if (IsDynamic) then
  begin
    BufTypeInfo := ArrayTypeInfo;
    Dimention := 0;
    while (BufTypeInfo <> nil) do
    begin
      Inc(Dimention);
      TypeData := GetTypeData(BufTypeInfo);
      elType := TypeData.elType;

      if (elType = nil) then
      begin
        if (ItemSize = TypeData.elSize) then Break;
      end else
      begin
        BufTypeInfo := elType^;
        if (BufTypeInfo = ItemTypeInfo) then Break;

        if (BufTypeInfo.Kind = tkDynArray) then
        begin
          //ToDo if (PLuaArrayInfo(ItemTypeInfo).FTypeInfo = BufTypeInfo) then Break;
          Continue;
        end else
        if (BufTypeInfo.Kind = tkArray) then
        begin
          //ToDo if (PLuaArrayInfo(ItemTypeInfo).FTypeInfo = BufTypeInfo) or
          //ToDo    (PLuaArrayInfo(ItemTypeInfo).FSize = GetTypeData(BufTypeInfo).elSize) then Break;
        end else
        if (BufTypeInfo.Kind in RECORD_TYPES) then
        begin
          //ToDo if (PLuaRecordInfo(ItemTypeInfo).FTypeInfo = BufTypeInfo) then Break;
        end;
      end;

      raise ELua.CreateFmt('Incorrect ItemTypeInfo of dynamic array "%s"',
        [FStringBuffer.Lua]) at FReturnAddress;
    end;
  end else
  begin
    Dimention := Length(ABounds);
    if (Dimention and 1 = 1) then
      raise ELua.CreateFmt('"%s" registering... Bounds size should be even. %d is an incorrect size',
      [FStringBuffer.Lua, Dimention]) at FReturnAddress;
    Dimention := Dimention shr 1;

    for i := 0 to Dimention-1 do
    if (ABounds[i * 2] > ABounds[i * 2 + 1]) then
      raise ELua.CreateFmt('"%s" registering... Incorrect bounds: "%d..%d"',
      [FStringBuffer.Lua, ABounds[i * 2], ABounds[i * 2 + 1]]) at FReturnAddress;
  end;

  // find
  ArrayPtr := LUA_POINTER_INVALID;
  if (Assigned(ArrayTypeInfo)) then ArrayPtr := TLuaDictionary(FMetaTypes).FindValue(ArrayTypeInfo);
  if (ArrayPtr = LUA_POINTER_INVALID) then
  begin
    LuaName := TLuaNames(FNames).Add(FStringBuffer.Lua);
    ArrayPtr := TLuaDictionary(FMetaTypes).FindValue(LuaName);
    if (ArrayPtr = LUA_POINTER_INVALID) then
    begin
      AdvancedSize := Length(ABounds) * SizeOf(Integer);
      Inc(AdvancedSize, (Dimention - 1) * SizeOf(NativeInt));
      Result := Pointer(InternalAddMetaType(mtArray, FStringBuffer.Lua, ArrayTypeInfo, 0{fill later}, AdvancedSize));

      Result.FIsDynamic := IsDynamic;
      Result.FDimention := Dimention;
      Result.FItemSize := ItemSize;

      if (IsDynamic) then
      begin
        BufTypeInfo := ArrayTypeInfo;
        PTypeInfo(Result.FMultiplies[0]) := BufTypeInfo;
        for i := 1 to Dimention - 1 do
        begin
          BufTypeInfo := GetTypeData(BufTypeInfo).elType^;
          PTypeInfo(Result.FMultiplies[i]) := BufTypeInfo;
        end;

        Result.F.Size := SizeOf(Pointer);
        Result.FFinalTypeInfo := ArrayTypeInfo;
        Result.FFinalItemsCount := 1;
      end else
      begin
        Result.FMultiplies[Dimention - 1] := ItemSize;
        for i := Dimention - 2 downto 0 do
        begin
          Count := (ABounds[(i + 1) * 2 + 1] - ABounds[(i + 1) * 2]) + 1;
          Result.FMultiplies[i] := Result.FMultiplies[i + 1] * Count;
        end;

        Result.FBounds := Pointer(@Result.FMultiplies[Dimention]);
        System.Move(ABounds[0], Result.FBounds^, Length(ABounds) * SizeOf(Integer));

        Result.F.Size := Result.FMultiplies[0];
        Result.FFinalTypeInfo := ItemTypeInfo{ToDo ???};
        Result.FFinalItemsCount := Result.F.Size div ItemSize;
      end;
    end else
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(ArrayPtr);
      if (Result.Kind <> mtArray) {ToDo or (Result.Low <> Low) or (Result.High <> High)} then
        raise ETypeRegistered(FStringBuffer.Lua) at FReturnAddress;

      if (Assigned(ArrayTypeInfo)) then
        TLuaDictionary(FMetaTypes).Add(ArrayTypeInfo, ArrayPtr);
    end;
  end else
  begin
    Result := TLuaMemoryHeap(FMemoryHeap).Unpack(ArrayPtr);
  end;
end;

function TLua.InternalAddSet(const TypeInfo: Pointer): PLuaSetInfo;
const
  MASK_3 = $FF shl 3;
var
  Ptr: __luapointer;
  ItemTypeInfo: PTypeInfo;
  TypeData: PTypeData;
  Low, High: Integer;
  LuaName: __luaname;
begin
  if (NativeUInt(TypeInfo) <= $FFFF) then
    raise ELua.Create('TypeInfo of set not defined') at FReturnAddress;

  if (PTypeInfo(TypeInfo).Kind <> tkSet) then
  begin
    GetTypeKindName(FStringBuffer.Default, PTypeInfo(TypeInfo).Kind);
    raise ELua.CreateFmt('TypeInfo of set is not correct: TypeKind = %s',
      [FStringBuffer.Default]) at FReturnAddress;
  end;

  Ptr := TLuaDictionary(FMetaTypes).FindValue(TypeInfo);
  if (Ptr = LUA_POINTER_INVALID) then
  begin
    ItemTypeInfo := GetTypeData(TypeInfo).CompType{$ifNdef FPC}^{$endif};
    TypeData := GetTypeData(ItemTypeInfo);
    Low := TypeData.MinValue;
    High := TypeData.MaxValue;

    unpack_lua_string(FStringBuffer.Lua, PShortString(@PTypeInfo(TypeInfo).Name)^);
    LuaName := TLuaNames(FNames).Add(FStringBuffer.Lua);
    Ptr := TLuaDictionary(FMetaTypes).FindValue(LuaName);
    if (Ptr = LUA_POINTER_INVALID) then
    begin
      Result := Pointer(InternalAddMetaType(mtSet, FStringBuffer.Lua, TypeInfo, 0{fill later}));

      Result.FItemTypeInfo := ItemTypeInfo;
      Result.FLow := Low;
      Result.FHigh := High;
      with Result^ do
      begin
      {$ifdef FPC}
        if (FHigh > 31) then F.Size := 32 else F.Size := 4;
        FRealSize := F.Size;
        FCorrection := 0;
        FAndMasks := $0000FFFF;
      {$else}
        F.Size := (((FHigh + 7 + 1) and MASK_3) - (FLow and MASK_3))shr 3;
        FRealSize := F.Size;
        if (F.Size = 3) then F.Size := 4;
        FCorrection := (FLow and MASK_3);
        FAndMasks := Integer(Byte($FF shr (7 - (FHigh and 7)))) +
                     Integer(Byte($FF shl (FLow - FCorrection))) shl 8;
      {$endif}
      end;

      if (ItemTypeInfo.Kind = tkEnumeration) and (not IsBooleanTypeInfo(ItemTypeInfo)) then
      begin
        //ToDo Self.RegEnum(SetInfo.FTypeInfo);
      end;
    end else
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
      if (Result.Kind <> mtSet) or (Result.Low <> Low) or (Result.High <> High) then
        raise ETypeRegistered(FStringBuffer.Lua) at FReturnAddress;

      TLuaDictionary(FMetaTypes).Add(TypeInfo, Ptr);
    end;
  end else
  begin
    Result := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
  end;
end;

function TLua.InternalBuildInvokable(const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind): __luapointer;
var
  Builder: ^TLuaInvokableBuilder;
begin
  Result := LUA_POINTER_INVALID;
  Builder := @TLuaInvokableBuilder(FInvokableBuilder);

  if (Assigned(TypeInfo)) then
  case TypeInfo.Kind of
    tkProcedure: Result := Builder.BuildProcedure(TypeInfo, MethodKind);
       tkMethod: Result := Builder.BuildMethod(TypeInfo, MethodKind);
    tkInterface:
    begin
      if (IsReferenceTypeInfo(TypeInfo)) then
        Result := Builder.BuildReference(TypeInfo, MethodKind);
    end;
  end;
end;

function TLua.InternalBuildInvokable(const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv): __luapointer;
var
  Builder: ^TLuaInvokableBuilder;
begin
  Builder := @TLuaInvokableBuilder(FInvokableBuilder);
  Result := Builder.BuildCustom(Params, ResultType, MethodKind, CallConv);
end;

function TLua.InternalAddMethod(const MetaType: PLuaMetaType; const Name: LuaString;
  Address: Pointer; const MethodKind: TLuaMethodKind;
  const Invokable: __luapointer; const ArgsCount{Universal}: Integer): __luapointer;
label
  new_or_replace, done;
var
  VmtOffset: NativeInt;
  ProcName: __luaname;
  TypeName: ^LuaString;
  Value: TLuaMethod;
  Item: PLuaDictionaryItem;
  Entity: PLuaGlobalEntity;
  LastPtr, Ptr: __luapointer;
  Proc: ^TLuaMethod;
begin
  if (NativeUInt(Address) <= $FFFF) then
    raise ELua.Create('Method address not defined') at FReturnAddress;

  if (Invokable <> LUA_POINTER_INVALID) and
    ((ArgsCount < -1) or (ArgsCount > 20)) then
    raise ELua.CreateFmt('Non-available ArgsCount value (%d)', [ArgsCount]) at FReturnAddress;

  // property name
  if (not IsValidIdent(Name)) then
    raise ELua.CreateFmt('Non-supported method name ("%s")', [Name]) at FReturnAddress;
  ProcName := TLuaNames(FNames).Add(Name);

  if Assigned(MetaType) and (MetaType.F.Kind = mtClass) then
  begin
    VmtOffset := PLuaClassInfo(MetaType).VmtOffset(Address);
    if (VmtOffset >= 0) then
    begin
      Address := Pointer(NativeInt(PROPSLOT_VIRTUAL) or VmtOffset);
    end;
  end;

  // type name
  if (MetaType = nil) then
  begin
    TypeName := @GLOBAL_NAME_SPACE;
  end else
  begin
    TypeName := @MetaType.FName;
  end;

(*
  // конструктор
  IsConstructor := (AClass <> nil) and (SameStrings(LUA_CONSTRUCTOR, ProcName));
  if (IsConstructor) then
  begin
    if (with_class) then
    ELua.Assert('Contructor can''t be a class method', CodeAddr);

    if IsClass then Index := InternalAddClass(AClass, False, CodeAddr)
    else Index := internal_class_index(AClass);

    FillConstructor(Index, Address, ArgsCount);
    Result := -1;
    exit;
  end;
*)

  // parameters
  Value.Address := Address;
  Value.MethodKind := MethodKind;
  Value.InvokableMode := (Invokable <> LUA_POINTER_INVALID);
  Value.Invokable := Invokable;
  if (not Value.InvokableMode) then
    Value.ArgsCount := ArgsCount;

  // find existing/add item
  Entity := nil;
  Item := nil;
  if (MetaType = nil) then
  begin
    Entity := InternalAddGlobal(Ord(gkProc), ProcName);
    LastPtr := Entity.Ptr;
  end else
  begin
    Item := TLuaDictionary(PLuaRecordInfo(MetaType).FNameSpace).InternalFind(ProcName, True);
    LastPtr := Item.Value;
  end;

  // fill/update routine
  //  not found: fill and add childs (replace invalid)
  //  found inherited: replace to new, replace last value to new
  if (LastPtr = LUA_POINTER_INVALID) then
  begin
  new_or_replace:
    Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaMethod));
    if (MetaType = nil) then
    begin
      Entity.Ptr := Ptr;
    end else
    begin
      Item.Value := Ptr or NAMESPACE_FLAG_PROC;
    end;
    Proc := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
    Proc^ := Value;

    if (Assigned(MetaType) and (MetaType.F.Kind = mtClass)) then
    begin
      InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(MetaType.F.AClass)),
        ProcName, LastPtr, Ptr or NAMESPACE_FLAG_INHERITED, False);
    end
  end else
  if (LastPtr and NAMESPACE_FLAG_PROC = 0) then
  begin
    raise ELua.CreateFmt('Property "%s" is already contained in %s', [Name, TypeName^]) at FReturnAddress;
  end else
  begin
    if (LastPtr and NAMESPACE_FLAG_INHERITED = 0) then
    begin
      // own method
      Ptr := LastPtr;
      Proc := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr and NAMESPACE_FLAGS_CLEAR);
      Proc^ := Value;
    end else
    begin
      // inherited method: done if not changed
      Proc := TLuaMemoryHeap(FMemoryHeap).Unpack(LastPtr and NAMESPACE_FLAGS_CLEAR);
      if (Value.Address = Proc.Address) and (Value.MethodKind = Proc.MethodKind) and
        (Value.Invokable{ArgsCount} = Proc.Invokable{ArgsCount}) then
      begin
        Ptr := LastPtr;
        goto done;
      end;

      // replace to new allocated method
      goto new_or_replace;
    end;
  end;

done:
  Result := Ptr and NAMESPACE_FLAGS_CLEAR;
end;

         (*
function TLua.InternalAddMethod(const IsClass: boolean; AClass: pointer; const ProcName: string; ArgsCount: integer; const with_class: boolean; Address, CodeAddr: pointer): integer;
var
  Index: integer;
  IsConstructor: boolean;
  ClassInfo: ^TLuaClassInfo;
  ProcInfo: ^TLuaClosureInfo;

  // рекурсивное заполнение данных по конструктору потомков
  procedure FillConstructor(const ClassIndex: integer; const Address: pointer; const ArgsCount: integer);
  var
    LastAddress: pointer;
    i, LastArgsCount: integer;
  begin
    LastAddress := ClassesInfo[ClassIndex].constructor_address;
    LastArgsCount := ClassesInfo[ClassIndex].constructor_args_count;
    if (LastAddress = Address) and (LastArgsCount = ArgsCount) then exit;

    for i := 0 to Length(ClassesInfo)-1 do
    with ClassesInfo[i] do
    if (ParentIndex = ClassIndex) then
    begin
      if (constructor_address = LastAddress) and (constructor_args_count = LastArgsCount) then
      FillConstructor(i, Address, ArgsCount);
    end;

    ClassesInfo[ClassIndex].constructor_address := Address;
    ClassesInfo[ClassIndex].constructor_args_count := ArgsCount;

    FInitialized := false;
  end;


  // рекурсивное заполнение указателя на метод Assign()
  procedure FillAssignProc(const ClassIndex: integer; const Address: pointer);
  var
    i: integer;
    LastAddress: pointer;
  begin
    LastAddress := ClassesInfo[ClassIndex].assign_address;
    if (LastAddress = Address) then exit;

    for i := 0 to Length(ClassesInfo)-1 do
    with ClassesInfo[i] do
    if (ParentIndex = ClassIndex) then
    begin
      if (assign_address = LastAddress) then FillAssignProc(i, Address);
    end;                                                                

    ClassesInfo[ClassIndex].assign_address := Address;
    FInitialized := false;
  end;


begin
  // проверки
  if (Address = nil) then
  ELua.Assert('ProcAddress = NIL', CodeAddr);

  if (ArgsCount < -1) or (ArgsCount > 20) then
  ELua.Assert('Non-available ArgsCount value (%d)', [ArgsCount], CodeAddr);

  if (not IsValidIdent(ProcName)) then
  ELua.Assert('Non-supported ProcName ("%s")', [ProcName], CodeAddr);

  // корректировка Address
  if (IsClass) and (AClass <> nil) then
  begin
    Index := IntPos(integer(Address), PInteger(AClass), VMTMethodsCount(AClass));
    if (Index >= 0) then integer(Address) := integer($FE000000) or Index*4;
  end;

  // конструктор
  IsConstructor := (AClass <> nil) and (SameStrings(LUA_CONSTRUCTOR, ProcName));
  if (IsConstructor) then
  begin
    if (with_class) then
    ELua.Assert('Contructor can''t be a class method', CodeAddr);

    if IsClass then Index := InternalAddClass(AClass, False, CodeAddr)
    else Index := internal_class_index(AClass);

    FillConstructor(Index, Address, ArgsCount);
    Result := -1;
    exit;
  end;

  // метод Assign()
  if (AClass <> nil) and (SameStrings(LUA_ASSIGN, ProcName)) then
  begin
    if (with_class) then
    ELua.Assert('Assign() method can''t be a class method', CodeAddr);

    if IsClass then Index := InternalAddClass(AClass, False, CodeAddr)
    else Index := internal_class_index(AClass);

    if (ArgsCount <> -1) and (ArgsCount <> 1) then
    ELua.Assert('Assign() method should have just 1 argument', CodeAddr);

    FillAssignProc(Index, Address);
    Result := -1;
    exit;                          
  end;


  // зарегистрировать класс, глобальную переменную (если нужно) и получить указатель на ProcInfo
  if (AClass = nil) then
  begin
    Result := internal_register_global(ProcName, gkProc, CodeAddr).Index;
    ProcInfo := @GlobalNative.Procs[Result];
  end else
  begin
    if IsClass then Index := InternalAddClass(AClass, False, CodeAddr)
    else Index := internal_class_index(AClass);

    ClassInfo := @ClassesInfo[Index];
    Result := ClassInfo.InternalAddName(ProcName, true, FInitialized, CodeAddr);
    ProcInfo := @ClassInfo.Procs[Result];
  end;

  // заполнение
  ProcInfo.ArgsCount := ArgsCount;
  ProcInfo.with_class := with_class;
  ProcInfo.Address := Address;
end;     *)

// Getter/Setter - static pointers, nil, or field/virtual offset
(*
  PROP_SLOT_MASK = 7;
  PROP_SLOTSETTER_SHIFT = 3;
  PROP_CLASS_MODE = 1 shl 6;
  PROP_CONSTREF_MODE = 1 shl 7;

  LuaPropertyMode = (pmNone, pmField, pmStatic, pmStaticIndex, pmStaticParams,
    pmVirtual, pmVirtualIndex, pmVirtualParams);


  TLuaProperty = packed record
    TypeInfo: Pointer;
    Getter: NativeUInt;
    Setter: NativeUInt;
    case Integer of
      0: (Index: Integer;
          Flags: Byte;
          {
            GetterMode: TLuaPropertyMode:3;
            SetterMode: TLuaPropertyMode:3;
            ClassMode: Boolean:1;
            ConstMode: Boolean:1;
          }
          Kind: TLuaPropertyKind;
          case Integer of
            0: (OrdType: TOrdType);
            1: (FloatType: TFloatType);
            2: (BoolType: TLuaPropBoolType);
            3: (StringType: TLuaPropStringType; MaxShortLength: Byte);
         );
      1: (ParamsPtr: __luapointer);
  end;
*)

// replacing childs properties {+ defaults}
// in default case: change self
procedure TLua.InternalReplaceChildNameSpace(const ParentMetaItem: Pointer{PLuaDictionaryItem};
  const Name: __luaname; const LastValue{+inherited}, Value{+inherited}: __luapointer; const IsDefaultProp: Boolean);
var
  CurrentMetaItem, TopMetaItem: PLuaDictionaryItem;
  ParentPtr, LastDefaultPtr: __luapointer;
  MetaType: PLuaClassInfo;
  ChildNameItem: PLuaDictionaryItem;
begin
  MetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(PLuaDictionaryItem(ParentMetaItem).Value);
  LastDefaultPtr := MetaType.DefaultProperty;
  if (IsDefaultProp) then
  begin
    MetaType.DefaultProperty := Value and NAMESPACE_FLAGS_CLEAR;
  end;

  CurrentMetaItem := ParentMetaItem;
  ParentPtr := CurrentMetaItem.Value;
  TopMetaItem := @TLuaDictionary(FMetaTypes).FItems[TLuaDictionary(FMetaTypes).Count];
  repeat
    Inc(CurrentMetaItem);
    if (CurrentMetaItem = TopMetaItem) then Break;
    if (CurrentMetaItem.Value = ParentPtr) then Continue;

    MetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(CurrentMetaItem.Value);
    if (MetaType.Kind = mtClass) and (MetaType.Parent = ParentPtr) then
    begin
      ChildNameItem := TLuaDictionary(MetaType.FNameSpace).InternalFind(Name, True);
      if (ChildNameItem.Value = LastValue{invalid(new) or last}) then
      begin
        ChildNameItem.Value := Value;
        InternalReplaceChildNameSpace(CurrentMetaItem, Name, LastValue, Value, IsDefaultProp);
      end else
      if (IsDefaultProp) and (MetaType.DefaultProperty = LastDefaultPtr) then
      begin
        // replace defaults
        InternalReplaceChildNameSpace(CurrentMetaItem, Name, LastValue, Value, IsDefaultProp);;
      end;
    end;
  until (False);
end;

function TLua.InternalAddProperty(const MetaType: PLuaMetaType; const Name: LuaString;
  const TypeInfo: Pointer; const IsDefault, IsAutoRegister: Boolean; const Getter, Setter, Flags: NativeUInt;
  const IndexParams: Integer): __luapointer;
label
  new_or_replace, done;
var
  PropMode: Integer;
  PropName: __luaname;
  TypeName: ^LuaString;
  Value: TLuaProperty;
  PropMeta: PLuaMetaType;
  Item: PLuaDictionaryItem;
  Entity: PLuaGlobalEntity;
  LastPtr, Ptr: __luapointer;
  ReplaceDefault: Boolean;
  Prop: ^TLuaProperty;
begin
  // property name
  PropMode := Ord(MetaType = nil) * 2 +
    Ord(Assigned(MetaType) and (MetaType.F.Kind = mtRecord) and (Getter = Setter) and
      (Getter and (1 shl (PROP_SLOTSETTER_SHIFT * 2) - 1) = Ord(pmField) + Ord(pmField) shl PROP_SLOTSETTER_SHIFT));
  if (not IsValidIdent(Name)) then
    raise ELua.CreateFmt('Non-supported %s name ("%s")', [PROPERTY_MODES[PropMode], Name]) at FReturnAddress;
  PropName := TLuaNames(FNames).Add(Name);

  // type name
  if (MetaType = nil) then
  begin
    TypeName := @GLOBAL_NAME_SPACE;
  end else
  begin
    TypeName := @MetaType.FName;
  end;

  // parameters
  if (NativeUInt(TypeInfo) <= $FFFF) then
    raise ELua.CreateFmt('TypeInfo of %s.%s not defined', [TypeName^, Name]) at FReturnAddress;
  PropMeta := Pointer(TLuaDictionary(FMetaTypes).InternalFind(TypeInfo, False));
  if (Assigned(PropMeta)) then PropMeta := TLuaMemoryHeap(FMemoryHeap).Unpack(PLuaDictionaryItem(PropMeta).Value);
  if (Assigned(PropMeta)) then
  begin
    Value.MetaType := PropMeta;
  end else
  begin
    if (not Value.InternalSetTypeInfo(TypeInfo, Self, IsAutoRegister)) then
      raise ELua.CreateFmt('Unknown type "%s" (%s) for %s.%s porperty',
        [FStringBuffer.Lua, FStringBuffer.Default, TypeName^, Name]) at FReturnAddress;
  end;
  Value.Getter := Getter;
  Value.Setter := Setter;
  Value.Index := IndexParams;
  Value.Flags := Flags;

  // find existing/add item
  Entity := nil;
  Item := nil;
  if (MetaType = nil) then
  begin
    Entity := InternalAddGlobal(Ord(gkVariable), PropName);
    Entity.ConstMode := (Flags and PROP_CONSTREF_MODE <> 0);
    LastPtr := Entity.Ptr;
  end else
  begin
    Item := TLuaDictionary(PLuaRecordInfo(MetaType).FNameSpace).InternalFind(PropName, True);
    LastPtr := Item.Value;
  end;

  // fill/update routine
  //  not found: fill and add childs (replace invalid)
  //  not found and defaut: fill+default and add childs (replace invalid defaults)
  //  found inherited: replace to new, replace last value to new
  //  found inherited and default: replace to new, replace last value to new, replace last default to new
  //  found and set default: (replace last value to new), replace last default to new
  //  found and not set default: fill
  if (LastPtr = LUA_POINTER_INVALID) then
  begin
  new_or_replace:
    Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaProperty));
    if (MetaType = nil) then
    begin
      Entity.Ptr := Ptr;
    end else
    begin
      Item.Value := Ptr {or 0};
    end;
    Prop := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
    Prop^ := Value;

    if (Assigned(MetaType) and (MetaType.F.Kind = mtClass)) then
    begin
      ReplaceDefault := (IsDefault) or (PLuaClassInfo(MetaType).DefaultProperty = LastPtr and NAMESPACE_FLAGS_CLEAR);
      InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(MetaType.F.AClass)),
        PropName, LastPtr, Ptr or NAMESPACE_FLAG_INHERITED, ReplaceDefault);
    end;
  end else
  if (LastPtr and NAMESPACE_FLAG_PROC <> 0) then
  begin
    raise ELua.CreateFmt('Method "%s" is already contained in %s', [Name, TypeName^]) at FReturnAddress;
  end else
  begin
    if (LastPtr and NAMESPACE_FLAG_INHERITED = 0) then
    begin
      // own property
      Ptr := LastPtr;
      Prop := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr {and NAMESPACE_FLAGS_CLEAR});
      Prop^ := Value;
      if (IsDefault) and (PLuaClassInfo(MetaType).DefaultProperty <> Ptr) then
      begin
        // set default
        InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(MetaType.F.AClass)),
            PropName, Ptr or NAMESPACE_FLAG_INHERITED, Ptr or NAMESPACE_FLAG_INHERITED, True);
      end;
    end else
    begin
      // inherited property: done if not changed
      Prop := TLuaMemoryHeap(FMemoryHeap).Unpack(LastPtr and NAMESPACE_FLAGS_CLEAR);
      if (Value.TypeInfo = Prop.TypeInfo) and (Value.Getter = Prop.Getter) and
        (Value.Setter = Prop.Setter) and (Value.Index = Prop.Index) and (Value.FlagsEx = Prop.FlagsEx) then
      begin
        if (not IsDefault) or (PLuaClassInfo(MetaType).DefaultProperty = LastPtr and NAMESPACE_FLAGS_CLEAR) then
        begin
          Ptr := LastPtr;
          goto done;
        end;
      end;

      // replace to new allocated property
      goto new_or_replace;
    end;
  end;

done:
  Result := Ptr {and NAMESPACE_FLAGS_CLEAR};
end;

           (*
// IsConst имеет значение только для глобальных переменных (AClass = GLOBAL_NAME_SPACE)
// tpinfo может быть как обычным typeinfo, так и PLuaRecordInfo
// проперти работают только для классов, структур (поля) и глобальные перменные
function  TLua.InternalAddProperty(const IsClass: boolean; AClass: pointer; const PropertyName: string; tpinfo: ptypeinfo; const IsConst, IsDefault: boolean; const PGet, PSet, Parameters, CodeAddr: pointer): integer;
var
  IsGlobal: boolean;
  Index: integer;
  ClassInfo: ^TLuaClassInfo;
  GlobalVariable: PLuaGlobalVariable;
  PropBase: TLuaPropertyInfoBase;
  PropInfo: ^TLuaPropertyInfo;

  // рекурсивное заполнение данных по дефолтному свойству у потомков
  procedure FillDefaultProperty(const ClassIndex: integer; const Value: integer);
  var
    i: integer;
    LastValue: integer;
  begin
    LastValue := ClassesInfo[ClassIndex]._DefaultProperty;
    if (LastValue = Value) then exit;

    for i := 0 to Length(ClassesInfo)-1 do
    with ClassesInfo[i] do
    if (ParentIndex = ClassIndex) then
    begin
      if (_DefaultProperty = LastValue) then FillDefaultProperty(i, Value);
    end;

    ClassesInfo[ClassIndex]._DefaultProperty := Value;
    FInitialized := false;
  end;
begin
  // определиться ClassInfo, найти или зарегистрировать при необходимости
  IsGlobal := (AClass = GLOBAL_NAME_SPACE);
  if (IsGlobal) then ClassInfo := @GlobalNative
  else
  begin
    if (IsClass) then Index := InternalAddClass(AClass, False, CodeAddr)
    else Index := internal_class_index(AClass);

    ClassInfo := @ClassesInfo[Index];
  end;

  // проверки
  if (not IsValidIdent(PropertyName)) then
  ELua.Assert('Non-supported %s name "%s"', [ClassInfo.PropertyIdentifier, PropertyName], CodeAddr);

  if (tpinfo = nil) then
  ELua.Assert('TypeInfo of %s "%s" not defined', [ClassInfo.PropertyIdentifier, PropertyName], CodeAddr);

  if (IsDefault) and (Parameters = nil) then
  ELua.Assert('Simple property ("%s") can''t be default property', [PropertyName], CodeAddr);

  case integer(Parameters) of
    0, integer(INDEXED_PROPERTY), integer(NAMED_PROPERTY): ;
    else
      if (PLuaRecordInfo(Parameters).FieldsCount = 0) then
      ELua.Assert('Property information "%s" has no fields', [PLuaRecordInfo(Parameters).Name], CodeAddr);
  end;  

  // определение базовых параметров
  PropBase := GetLuaPropertyBase(Self, '', PropertyName, tpinfo, CodeAddr);

  // найти/добавить/зарегистрировать свойство
  if (not IsGlobal) then
  begin
    Result := ClassInfo.InternalAddName(PropertyName, false, FInitialized, CodeAddr);

    if (IsDefault) then
    begin
      //ClassInfo._DefaultProperty := (not Result);
      FillDefaultProperty(ClassInfo._ClassIndex, integer(SmallPoint(ClassInfo._ClassIndex, not Result)));
    end;  
  end else
  begin
    GlobalVariable := internal_register_global(PropertyName, gkVariable, CodeAddr);
    GlobalVariable.IsConst := IsConst;
    Result := GlobalVariable.Index;
  end;                            
  PropInfo := @ClassInfo.Properties[not Result];

  // заполнить информацию
  PropInfo.Fill(ClassInfo^, PropBase, PGet, PSet, Parameters);
  if (IsConst) then PropInfo.write_mode := MODE_NONE_USE;
end;
       *)

const
  PTR_FALSE = pointer(ord(false));
  PTR_TRUE = pointer(ord(true));
  OPERATOR_NEG = 0;
  OPERATOR_ADD = 1;
  OPERATOR_SUB = 2;
  OPERATOR_MUL = 3;
  OPERATOR_DIV = 4;
  OPERATOR_MOD = 5;
  OPERATOR_POW = 6;
  OPERATOR_EQUAL = 7;
  OPERATOR_LESS = 8;
  OPERATOR_LESS_EQUAL = 9;
  OPERATOR_CONCAT = 10; {бонусный. только для динамических массивов}

         (*
// проинициализировать пространства имён и создать калбеки
procedure TLua.INITIALIZE_NAME_SPACE();
var
  i: integer;

  // добавить глобальное имя в свой список
  procedure AddToNameSpace(var ClassInfo: TLuaClassInfo; const Name: string;
                          const IsProc: boolean; const ClassIndex, Ind: integer);
  var
    NewValue, P: integer;
    ProcInfo, PropertyInfo: pointer;
  begin
    // NewValue := (ord(IsProc) shl 31) or (ClassIndex shl 16) or (Ind);
    NewValue := ClassIndex; // word part
    with TSmallPoint(NewValue) do
    begin y := Ind; if (not IsProc) then y := not y; end;
    P := ClassInfo.NameSpacePlace(Self, pchar(Name), Length(Name), ProcInfo, PropertyInfo);

    if (ProcInfo = nil) and (PropertyInfo = nil) then
    begin
      // добавить
      with TLuaHashIndex(DynArrayInsert(ClassInfo.NameSpace, typeinfo(TLuaHashIndexDynArray), P)^) do
      begin
        Hash := StringHash(Name);
        Index := NewValue;
      end;
    end else
    begin
      // изменить
      ClassInfo.NameSpace[P].Index := NewValue;
    end;
  end;

  // калбек для метатаблицы в стеке
  procedure add_metatable_callback(const proc_name: pchar; const CFunctionProc: pointer); overload;
  begin
    lua_push_pchar(Handle, proc_name);

    if (CFunctionProc = nil) then lua_pushnil(Handle)
    else lua_pushcclosure(Handle, CFunctionProc, 0);

    lua_rawset(Handle, -3);
  end;
  // зарегистрировать и добавить калбек
  procedure add_metatable_callback(const proc_name: pchar; const CallbackProc: pointer; const P1: pointer; const P2: pointer); overload;
  begin
    if (CallbackProc = nil) then add_metatable_callback(proc_name, nil)
    else add_metatable_callback(proc_name, pointer(AddLuaCallbackProc(Self, P1, P2, CallbackProc)));
  end;

  // сгенерировать и заполнить все калбеки
  procedure CreateCallbacks(var ClassInfo: TLuaClassInfo);
  var
    i: integer;
    Operators: TLuaOperators;


    // калбек для метатаблицы в стеке
    procedure register_callback(const proc_name: pchar; const CallbackProc: pointer; const Value: pointer=nil);
    var
      P1: pointer;
    begin
      P1 := @ClassInfo;
      if (@ClassInfo = @GlobalNative) then P1 := PTR_FALSE;

      add_metatable_callback(proc_name, CallbackProc, P1, Value);
    end;

    // зарегистрировать оператор
    // учитывая фильтр Operators
    procedure register_operator(const Value: integer);
    const
      OPERATORS_NAME: array[OPERATOR_NEG..OPERATOR_CONCAT]
      of pansichar = ('__unm','__add','__sub','__mul','__div','__mod','__pow','__eq','__lt','__le','__concat');
    var
      CallbackProc: pointer;
    begin
      CallbackProc := @TLua.__operator;

      if (Value in [OPERATOR_NEG..OPERATOR_POW]) then
      begin
        if (not (TLuaOperator(Value) in Operators)) then CallbackProc := nil;
      end else
      if (Value = OPERATOR_CONCAT) then with ClassInfo do
      begin
         if (_ClassKind <> ckArray) or (PLuaArrayInfo(_Class).Dimention <> 1)
         or (not PLuaArrayInfo(_Class).IsDynamic) then CallbackProc := nil;
      end else
      begin
        if (not (loCompare in Operators)) then CallbackProc := nil;
        if (Value = OPERATOR_LESS) and (ClassInfo._ClassKind = ckSet) then CallbackProc := nil;
      end;

      add_metatable_callback(OPERATORS_NAME[Value], CallbackProc, @ClassInfo, pointer(Value));
    end;

  begin
    // запушить метатаблицу
    global_push_value(ClassInfo.Ref);

    // заполнение метатаблицы базовыми функциями
    if (@ClassInfo = @GlobalNative) then
    begin
      // глобальное пространство
      register_callback('__index', @TLua.__global_index);
      register_callback('__newindex', @TLua.__global_newindex);
    end else
    if (ClassInfo._ClassKind = ckArray) then
    begin
      // массив
      register_callback('__index', @TLua.__array_index, PTR_FALSE);
      register_callback('__newindex', @TLua.__array_newindex, PTR_FALSE);
      register_callback('__len', @TLua.__len);
      register_callback('__call', @TLua.__call);
      register_callback('__gc', @TLua.__destructor, PTR_FALSE);

      // оператор конкатенации
      register_operator(OPERATOR_CONCAT);
    end else
    begin
      // класс, структура или множество
      register_callback('__index', @TLua.__index_prop_push);
      register_callback('__newindex', @TLua.__newindex_prop_set);
      register_callback('__len', @TLua.__len);
      register_callback('__call', @TLua.__call);
      register_callback('__gc', @TLua.__destructor, PTR_FALSE);


      // персональные функции-конструкторы/деструкторы
      if (ClassInfo._ClassKind = ckClass) then ClassInfo.__Create := pointer(AddLuaCallbackProc(Self, @ClassInfo, PTR_TRUE, @TLua.__constructor));
      if (ClassInfo._ClassKind = ckClass) then ClassInfo.__Free := pointer(AddLuaCallbackProc(Self, @ClassInfo, PTR_TRUE, @TLua.__destructor));

      // операторы для структур и множеств
      case ClassInfo._ClassKind of
        ckRecord: begin
                    Operators := PLuaRecordInfo(ClassInfo._Class).FOperators;
                    if (@PLuaRecordInfo(ClassInfo._Class).FOperatorCallback = nil) then Operators := [];
                  end;

           ckSet: Operators := [loNeg, loAdd, loSub, loMul, loCompare];
      else
         Operators := [];
      end;
      if (ClassInfo._ClassKind <> ckClass) then
      for i := OPERATOR_NEG to OPERATOR_LESS_EQUAL do register_operator(i);
    end;

    // глобальный калбек __tostring
    if (@ClassInfo <> @GlobalNative) then
    begin
      lua_push_pchar(Handle, '__tostring');
      lua_pushcclosure(Handle, cfunction_tostring, 0);
      lua_rawset(Handle, -3);
    end;

    // очистить стек
    lua_settop(Handle, 0);

    // создать калбеки для функций
    for i := 0 to Length(ClassInfo.Procs)-1 do
    ClassInfo.Procs[i].lua_CFunction := pointer(AddLuaCallbackProc(Self, @ClassInfo, @ClassInfo.Procs[i], @TLua.ProcCallback));
  end;

  // создать глобальное пространство имён для класса,
  // включая пространство имён предка
  procedure FillNameSpace(var AlreadyInitialized: TBooleanDynArray; var ClassInfo: TLuaClassInfo; const ClassIndex: integer); overload;
  var
    i, ParentIndex: integer;
  begin
    if (AlreadyInitialized[ClassIndex]) then exit;
    ClassInfo._ClassSimple := false;

    // зарегистрировать предка, взять его таблицу глобальных имён
    ParentIndex := ClassInfo.ParentIndex;
    if (ParentIndex >= 0) then
    begin
      FillNameSpace(AlreadyInitialized, ClassesInfo[ParentIndex], ParentIndex);
      DynArrayCopy(ClassInfo.NameSpace, ClassesInfo[ParentIndex].NameSpace, typeinfo(TLuaHashIndexDynArray));
    end else
    begin
      ClassInfo.NameSpace := nil;
    end;

    // дополнить информацию о методах
    for i := 0 to Length(ClassInfo.Procs)-1 do
    AddToNameSpace(ClassInfo, ClassInfo.Procs[i].ProcName, true, ClassIndex, i);

    // добавить информацию о каждом из свойств
    for i := 0 to Length(ClassInfo.Properties)-1 do
    AddToNameSpace(ClassInfo, ClassInfo.Properties[i].PropertyName, false, ClassIndex, i);


    // определить, является ли класс "простым"
    // это значит что у него нет функций, количество свойств <= 10 и все содержатся в его Properties
    ClassInfo._ClassSimple := (ClassInfo.Procs = nil) and (Length(ClassInfo.Properties) <= 10);
    if (ClassInfo._ClassSimple) then
    for i := 0 to Length(ClassInfo.NameSpace)-1 do
    if (ClassInfo.NameSpace[i].Index and $FFFF <> ClassIndex) then
    begin
      ClassInfo._ClassSimple := false;
      break;
    end;

    // пометить, что зарегистрировано
    AlreadyInitialized[ClassIndex] := true;
  end;

  // создать список всевозможножных методов и свойств
  // включай методы и свойства предков
  // !!! для GlobalNative этого делать не нужно. Глобальные переменные всегда актуальны
  procedure FillNameSpace(); overload;
  var
    i, Count: integer;
    AlreadyInitialized: TBooleanDynArray;
  begin
    // инициализация массива
    Count := Length(ClassesInfo);
    SetLength(AlreadyInitialized, Count);
    if (Count <> 0) then ZeroMemory(pointer(AlreadyInitialized), Count);

    // инициализация классов
    for i := 0 to Count-1 do
    FillNameSpace(AlreadyInitialized, ClassesInfo[i], i);
  end;

begin
  if (FInitialized) then exit;
  DeleteCFunctionDumps(Self);

  // создать список всевозможножных методов и свойств
  // включай методы и свойства предков
  // !!! для GlobalNative этого делать не нужно. Глобальные переменные всегда актуальны
  FillNameSpace();


  // создать универсальные калбеки
  begin
    cfunction_tostring := pointer(AddLuaCallbackProc(Self, nil, nil, @TLua.__tostring));
    cfunction_inherits_from := pointer(AddLuaCallbackProc(Self, nil, nil, @TLua.__inherits_from));
    cfunction_assign := pointer(AddLuaCallbackProc(Self, nil, nil, @TLua.__assign));
    cfunction_dynarray_resize := pointer(AddLuaCallbackProc(Self, nil, nil, @TLua.__array_dynamic_resize));
    cfunction_array_include := pointer(AddLuaCallbackProc(Self, pointer(1), nil, @TLua.__array_include));
    cfunction_set_include  := pointer(AddLuaCallbackProc(Self, PTR_FALSE, pointer(0), @TLua.__set_method));
    cfunction_set_exclude  := pointer(AddLuaCallbackProc(Self, PTR_FALSE, pointer(1), @TLua.__set_method));
    cfunction_set_contains := pointer(AddLuaCallbackProc(Self, PTR_FALSE, pointer(2), @TLua.__set_method));
  end;

  // сгенерировать и заполнить специфичные калбеки
  begin
    CreateCallbacks(GlobalNative);

    for i := 0 to Length(ClassesInfo)-1 do
    CreateCallbacks(ClassesInfo[i]);

    // сложные свойства
    global_push_value(mt_properties);
    add_metatable_callback('__index', @TLua.__array_index, nil, PTR_TRUE);
    add_metatable_callback('__newindex', @TLua.__array_newindex, nil, PTR_TRUE);
    add_metatable_callback('__gc', @TLua.__destructor, nil, nil);
    add_metatable_callback('__tostring', cfunction_tostring);
    lua_settop(Handle, 0);
  end;

  // отсортировать таблицу CFunctionDumps
  {$ifdef NO_CRYSTAL}
    QuickSort4(pointer(CFunctionDumps), 0, Length(CFunctionDumps)-1);
  {$else}
    SortArray4(CFunctionDumps);
  {$endif}

  // флаг "всё проинициализированно"
  FInitialized := true;  
end;
        *)
          (*
// преобразование userdata или метатаблицы в строку
function __arrayindex_description(const Prefix, Value: string; const Index, Dimention: integer): string; forward;
function TLua.__tostring(): integer;
var
  userdata: PLuaUserData;

  // описание difficult свойства - это отдельная тема 
  procedure FillPropertyDescription();
  begin
    with userdata^, FBufferArg do
    str_data := __arrayindex_description('NON FINSHED DIFFICULT PROPERTY: ' + PropertyInfo.PropertyName,
               'xxx', integer(array_params) and $F - 1, array_params shr 4);
  end;

  // то же самое для сложных многомерных массивов
  procedure FillArrayDescription();
  begin
    with userdata^, FBufferArg do
    str_data := __arrayindex_description(ArrayInfo.Name, 'xxx', integer(array_params) and $F - 1, array_params shr 4);
  end;


begin
  Result := 1;

  if (lua_type(Handle, 1) = LUA_TTABLE) then
  begin
    lua_push_pascalstring(Handle, ClassesInfo[LuaTableToClass(Handle, 1)]._ClassName);
  end else
  begin
    userdata := lua_touserdata(Handle, 1);

    if (userdata = nil) or (userdata.instance = nil) then lua_push_pascalstring(Handle, 'Incorrect userdata')
    else
    with userdata^ do
    if (kind = ukProperty) then
    begin
      FillPropertyDescription();
      lua_push_pascalstring(Handle, FBufferArg.str_data);
    end else
    if (kind = ukArray) and (array_params and $F <> 0) then
    begin
      FillArrayDescription();
      lua_push_pascalstring(Handle, FBufferArg.str_data);
    end else
    begin
      // TObject, Record, Set
      stack_luaarg(FBufferArg, 1, true); 

      with FBufferArg do
      begin
        if (LuaType <> ltEmpty) and (LuaType <> ltString) then
        begin
          if (str_data <> '') then str_data := '';
          TForceString(@TLuaArg.ForceString)(FBufferArg, str_data);
        end;

        if (LuaType = ltEmpty) or (str_data = '') then
          lua_push_pascalstring(Handle, 'nil')
        else
          lua_push_pascalstring(Handle, str_data);
      end;
    end;
  end;    
end;      *)
            (*

// является ли объект наследником (Класса, PLuaRecordInfo, PLuaArrayInfo, PLuaSetInfo)
function TLua.__inherits_from(): integer;
label
  Exit;
var
  Ret: boolean;
  userdata: PLuaUserData;
  ClassIndex1, ClassIndex2: integer;
  ClassInfo1, ClassInfo2: ^TLuaClassInfo;
begin
  Ret := false;
  Result := 1;

  if (lua_gettop(Handle) <> 2) then
  ScriptAssert('Wrong arguments count(%d) in InheritsForm() method', [lua_gettop(Handle)]);

  if (lua_type(Handle, 2) <> LUA_TTABLE) then goto Exit; 
  ClassIndex2 := LuaTableToClass(Handle, 2);
  if (ClassIndex2 < 0) then goto Exit;

  case (lua_type(Handle, 1)) of
    LUA_TUSERDATA: begin
                     userdata := lua_touserdata(Handle, 1);
                     if (userdata = nil) or (userdata.instance = nil) then goto Exit;

                     case userdata.kind of
                       ukInstance: ClassIndex1 := userdata.ClassIndex;
                            ukSet: ClassIndex1 := userdata.SetInfo.FClassIndex;
                          ukArray: if (userdata.array_params and $F <> 0) then goto Exit
                                   else ClassIndex1 := userdata.ArrayInfo.FClassIndex;
                     else
                       goto Exit;
                     end;
                   end;
       LUA_TTABLE: begin
                     ClassIndex1 := LuaTableToClass(Handle, 1);
                     if (ClassIndex1 < 0) then goto Exit;
                   end;
  else
    goto Exit;
  end;

  // результат
  Ret := (ClassIndex1 = ClassIndex2);
  if (Ret) then goto Exit;

  // для классов
  ClassInfo1 := @ClassesInfo[ClassIndex1];
  ClassInfo2 := @ClassesInfo[ClassIndex2];
  Ret := (ClassInfo1._ClassKind = ckClass) and (ClassInfo1._ClassKind = ckClass)
     and (TClass(ClassInfo1._Class).InheritsFrom(TClass(ClassInfo2._Class)));

Exit:  
  lua_pushboolean(Handle, Ret);
end;
      *)
        (*
// одной переменной присваивается значение другой переменной
// что-то типа копирования
//
// ещё assign() работает в режиме инициализации по таблице
function TLua.__assign(): integer;
var
  Dest, Src: PLuaUserData;
  src_luatype: integer;
  assign_addr: pointer;

  // показать ошибку, что невозможно выполнить assign
  procedure ThrowWrongParameters(const Description: string='Wrong types.');
  var
    X1, X2: TLuaArg;
  begin
    stack_luaarg(X1, 1, true);
    stack_luaarg(X2, 2, true);

    ScriptAssert('Can''t realize %s.Assign(%s). %s', [X1.ForceString, X2.ForceString, Description]);
  end;

begin
  Result := 0;

  if (lua_gettop(Handle) <> 2) then
  ScriptAssert('Not found instance or wrong arguments count in an Assign() method', []);

  // протестировать второй аргумент
  // возможно это таблица (режим инициализации полей)
  // а может быть и какой-то класс (а этом случае ошибка)
  src_luatype := lua_type(Handle, 2);
  if (src_luatype = LUA_TTABLE) then
  begin
    if (LuaTableToClass(Handle, 2) >= 0) then ThrowWrongParameters();
  end;

  // проверка типов
  if (lua_type(Handle, 1) <> LUA_TUSERDATA) or (not (src_luatype in [LUA_TUSERDATA, LUA_TTABLE]))
  then ThrowWrongParameters();

  // проверка Dest (Self)
  Dest := lua_touserdata(Handle, 1);
  if (Dest.instance = nil) then ThrowWrongParameters('Instance is already destroyed.');

  // если режим инициализации
  if (src_luatype = LUA_TTABLE) then
  begin
    // инициализация по таблице работает только для классов и структур (ukInstance)
    if (Dest.kind <> ukInstance) then ThrowWrongParameters();

    // вызов
    Self.__initialize_by_table(Dest, 2);

    exit;
  end;

  // проверка Dest и Src
  Src := lua_touserdata(Handle, 2);
  if (Src.instance = nil) then ThrowWrongParameters('Argument is already destroyed.');
  if (Dest.kind <> Src.kind) or (Dest.kind = ukProperty) then ThrowWrongParameters();

  
  if (Dest.kind = ukSet) then
  begin
    if (Dest.SetInfo <> Src.SetInfo) then ThrowWrongParameters();
    Move(Src.instance^, Dest.instance^, Dest.SetInfo.FSize);
  end else
  if (Dest.kind = ukArray) then
  begin
    if (Dest.ArrayInfo <> Src.ArrayInfo) then ThrowWrongParameters();

    with Dest.ArrayInfo^ do
    if (FTypeInfo = nil) then Move(Src.instance^, Dest.instance^, FSize)
    else CopyArray(Dest.instance, Src.instance, FTypeInfo, FItemsCount);
  end else
  with ClassesInfo[Dest.ClassIndex] do
  begin
    if (_ClassKind <> ClassesInfo[Src.ClassIndex]._ClassKind) then ThrowWrongParameters();

    // если зарегистрирована специальная функция Assign(const Arg: TLuaArg)
    if (assign_address <> nil) then
    begin
      FArgsCount := 1;
      SetLength(FArgs, 1);
      stack_luaarg(FArgs[0], 2, false);
      assign_addr := assign_address;
      if (dword(assign_addr) >= $FE000000) then assign_addr := ppointer(dword(Dest.instance^) + dword(assign_addr) and $00FFFFFF)^;

      TLuaClassProc16(assign_addr)(Dest.instance^, FArgs, TLuaArg(nil^));
      FArgsCount := 0;
      exit;
    end;

    { -- стандартное присвоение -- }

    if (_ClassKind = ckRecord) then
    begin
      // копирование структуры
      if (Dest.ClassIndex <> Src.ClassIndex) then ThrowWrongParameters();

      with PLuaRecordInfo(_Class)^ do
      if (FTypeInfo <> nil) then CopyRecord(Dest.instance, Src.instance, FTypeInfo)
      else Move(Src.instance^, Dest.instance^, FSize);      
    end else
    if (TObject(Dest.instance) is TPersistent) and (TObject(Src.instance) is TPersistent) then
    begin
      TPersistent(Dest.instance).Assign(TPersistent(Src.instance));
    end else
    begin
      // копирование TObject
      CopyObject(TObject(Dest.instance), TObject(Src.instance));
    end;
  end;
end;      *)
            (*
// эта процедура отвечает за инициализацю
// свойств (полей) класса или структуры по таблице.
// функция рекурсивная (т.е. можно инициализировать вложенные структуры и экземпляры класса)
//
// функция напрямую не вызывается из Lua, но имеет схожую стилистику
// только чтобы попадать в общий вид подобных функций.
// с другой стороны функция 100% вызывается не на нативной стороне, поэтому в случае ошибки - ScriptAssert()
//
// stack_index здесь всегда в прямой адресации!
function TLua.__initialize_by_table(const userdata: PLuaUserData; const stack_index: integer): integer;
const
  FIELD_PROPERTY: array[boolean] of string = ('Field', 'Property');

var
  S: pansichar;
  SLength: integer;
  ClassInfo: PLuaClassInfo;
  is_class: boolean;
  ProcInfo: ^TLuaClosureInfo;
  PropertyInfo: ^TLuaPropertyInfo;
  prop_struct: TLuaPropertyStruct;
  recursive_userdata: TLuaUserData;

  // в случае ошибки
  procedure ThrowKeyValue(const Description: string);
  var
    key_arg, value_arg: TLuaArg;
  begin
    stack_luaarg(key_arg, -2, true);
    if (key_arg.LuaType <> ltString) then key_arg.AsString := '"' + key_arg.ForceString + '"';
    stack_luaarg(value_arg, -1, true);

    ScriptAssert('Can''t change %s.%s to "%s". %s.', [ClassInfo._ClassName,
                  key_arg.ForceString, value_arg.ForceString, Description]);
  end;

  // когда в ошибке нужно указать, что это поле или свойство
  procedure ThrowFieldProp(const Desc: string); overload;
  begin
    ThrowKeyValue(FIELD_PROPERTY[is_class] + ' ' + Desc);
  end;

begin
  Result := 0; // результат в реальности не имеет значения

  // ClassInfo, prop_struct
  ClassInfo := @ClassesInfo[userdata.ClassIndex];
  is_class := (ClassInfo._ClassKind = ckClass);
  prop_struct.Instance := userdata.instance;
  prop_struct.Index := nil;
  prop_struct.ReturnAddr := nil;
  prop_struct.StackIndex := lua_gettop(Handle)+2;

  // неизменяемая структура
  if (userdata.is_const) then
  ScriptAssert('"%s" instance is constant', [ClassInfo._ClassName]);

  // цикл по всем элементам
  lua_pushnil(Handle);
  while (lua_next(Handle, stack_index) <> 0) do
  begin
    // Key
    if (lua_type(Handle, -2) <> LUA_TSTRING) then ThrowKeyValue('Incorrect key');

    // получаем строковое представление ключа
    // проводим проверки и выдаём ошибку если нужно.
    // в будущем нужно будет отследить так же "стандартные свойства" (todo)
    begin
      S := lua_tolstring(Handle, -2, @SLength);
      ProcInfo := nil;
      PropertyInfo := nil;
      ClassInfo.NameSpacePlace(Self, S, SLength, pointer(ProcInfo), pointer(PropertyInfo));

      if (ProcInfo <> nil) then
      ThrowKeyValue('Method can''t be changed');

      if (PropertyInfo = nil) then
      ThrowFieldProp('not found');

      if (PropertyInfo.write_mode = MODE_NONE_USE) then
      ThrowFieldProp('is readonly');

      if (PropertyInfo.Parameters <> nil) then
      ThrowFieldProp('is difficult to initialize');
    end;

    // если поле под ключом является структурой или экземпляром класса
    // а в Value таблица, то нужно вызвать рекурсию
    // (+ провести ряд проверок)
    if (lua_type(Handle, -1) = LUA_TTABLE) and (LuaTableToClass(Handle,-1)<0) then
    begin
      // проверка на присваемый тип
      if (not(PropertyInfo.Base.Kind in [pkObject, pkRecord])) then ThrowKeyValue('Incompatible types');

      // если поле/свойство нечитабельное
      if (PropertyInfo.read_mode = MODE_NONE_USE) then ThrowFieldProp('is writeonly');

      // если свойство по геттеру
      if (PropertyInfo.read_mode < 0) then ThrowFieldProp('is difficult to initialize, because it has a getter function');

      // подготовка данных для рекурсивного вызова
      pinteger(@recursive_userdata.kind)^ := 0; // сразу все поля
      if (PropertyInfo.Base.Kind = pkObject) then
      begin
        // инициализируем экземпляр класса
        recursive_userdata.instance := ppointer(integer(userdata.instance)+PropertyInfo.read_mode)^;
        if (recursive_userdata.instance = nil) then ThrowFieldProp('is nil');
        recursive_userdata.ClassIndex := internal_class_index(TClass(recursive_userdata.instance^));
      end else
      begin
        // структура (по ссылке)
        recursive_userdata.instance := pointer(integer(userdata.instance)+PropertyInfo.read_mode);
        recursive_userdata.ClassIndex := PLuaRecordInfo(PropertyInfo.Base.Information).FClassIndex;
      end;

      // сам вызов
      __initialize_by_table(@recursive_userdata, lua_gettop(Handle));
    end else
    begin
      // обычное происвоение
      prop_struct.PropertyInfo := PropertyInfo;
      __newindex_prop_set(ClassInfo^, @prop_struct);
    end;

    // next iteration
    lua_settop(Handle, -1-1);
  end;
end;    *)
          (*

// вызов метода + корректное восстановление стека
procedure TMethod_Call(const ASelf, P1, P2: integer; const Code: pointer);
asm
  push esi
  mov esi, esp
    CALL [EBP+8]
  mov esp, esi
  pop esi
end;
           *)(*
// простой вызов события с 2мя возможными простыми типами
function TLua.__tmethod_call(const Method: TMethod): integer;
var
  userdata: PLuaUserData;
  Offset, ArgsCount, i: integer;
  P_DATA: array[0..1] of integer;
  S_DATA: array[0..1] of string;
  P: pinteger;
  S: pstring;
  Number: double;
  IntValue: integer absolute Number;  

  // показать, что параметр не подходит для вызова метода
  procedure ThrowWrongParameter(const i: integer; const ParamType: pchar);
  begin
    ScriptAssert('%d parameter of TMethod() call has unsupported type: "%s"' , [i+1, ParamType]);
  end;

  // если переменная маленькая, то откорректировать её
  procedure CorrectPointer(var Value: integer; const Size: integer);
  begin
    case (Size) of
      1: Value := pbyte(Value)^;
      2: Value := pword(Value)^;
      3: Value := integer(pword(Value)^) or (ord(pchar(Value)[2]) shl 16);
      4: Value := pinteger(Value)^;
    end;
  end;
  
begin
  Result := 0;

  // проверка на возможность вызова
  if (Method.Code = nil) then
  ScriptAssert('Method is not assigned (Code = %p, Data = %p)', [Method.Code, Method.Data]);  

  // определиться с Offset
  // Stack[1] 100% = Method. А вот со вторым параметром могут быть проблемы
  LuaTypeName(lua_type(Handle, 2));
  userdata := lua_touserdata(Handle, 2);
  Offset := ord((userdata <> nil) or (userdata.kind <> ukInstance));

  // количество аргументов
  ArgsCount := lua_gettop(Handle)-1-Offset;
  if (ArgsCount < 0) or (ArgsCount > 2) then
  ScriptAssert('Wrong TMethod() arguments count = %d. Max arguments count = 2', [ArgsCount]);

  // сбор параметров
  P_DATA[0] := 0;
  P_DATA[1] := 0;
  P := @P_DATA[0];
  S := @S_DATA[0];
  for i := 1 to ArgsCount do
  begin
    case lua_type(Handle, 1+Offset+i) of
    LUA_TNIL           : ;  
    LUA_TBOOLEAN      : if (lua_toboolean(Handle, 1+Offset+i)) then P^ := 1;
    LUA_TNUMBER       : begin
                          if (NumberToInteger(Number, Handle, 1+Offset+i)) then
                          P^ := IntValue
                          else
                          P^ := Trunc(Number);
                        end;
    LUA_TSTRING       : begin
                          lua_to_pascalstring(S^, Handle, 1+Offset+i);
                          P^ := integer(S^);
                        end;
    LUA_TLIGHTUSERDATA: ppointer(P)^ := lua_touserdata(Handle, 1+Offset+i);
    LUA_TFUNCTION     : begin
                          ppointer(P)^ := CFunctionPtr(lua_tocfunction(Handle, 1+Offset+i));
                          if (pdword(P)^ >= $FE000000) then pdword(P)^ := pdword(P)^ and $00FFFFFF;
                        end;
    LUA_TUSERDATA     : begin
                          // объект класса или структура или массив или ...
                          userdata := lua_touserdata(Handle, 1+Offset+i);
                          if (userdata <> nil) then
                          with userdata^ do
                          begin
                            P^ := integer(instance);
                            case kind of
                               ukInstance: with ClassesInfo[ClassIndex] do
                                             if (_ClassKind <> ckClass) then
                                             with PLuaRecordInfo(_Class)^ do
                                             if (FSize <= 4) then CorrectPointer(P^, FSize);
                                  ukArray: with ArrayInfo^ do
                                             if (FSize <= 4) then CorrectPointer(P^, FSize);
                                    ukSet: with SetInfo^ do
                                             if (FSize <= 4) then CorrectPointer(P^, FSize);
                            else
                              { ukProperty }
                              ThrowWrongParameter(i, 'DifficultProperty');
                            end;
                          end;
                        end;
    LUA_TTABLE        : begin
                          // TClass, Info или Таблица
                          IntValue := LuaTableToClass(Handle, 1+Offset+i);

                          if (IntValue >= 0) then
                          begin
                            pointer(P^) := ClassesInfo[IntValue]._Class;
                          end else
                          begin
                            ThrowWrongParameter(i, 'LuaTable');
                          end;
                        end;
    else
      // неподдерживаемый тип
      ThrowWrongParameter(i, LuaTypeName(lua_type(Handle, 1+Offset+i)));
    end;


    inc(P);
    inc(S);
  end;

  // вызов
  TMethod_Call(integer(Method.Data), P_DATA[0], P_DATA[1], Method.Code);
end;
       *)


             (*
// взять 2 первых параметра в калбеке
// если первый аргумент не понятен - Result = false
function __read_lua_arguments(const Handle: pointer; var userdata: PLuaUserData;
                                  var S: pchar; var luatype, SLength, stdindex: integer): boolean;
type
  T12bytes = array[0..2] of integer;
var
  Data: ^T12bytes;// absolute S;
const
  _Type = $65707954;
  _Name = $656D614E;
  _Pare = $65726150;
  _nt   = $0000746E;
  _Inhe = $65686E49;
  _rits = $73746972;
  _From = $6D6F7246;
  _Assi = $69737341;
  _gn   = $00006E67;
  _IsRe = $65527349;
  _f    = $00000066;
  _IsCo = $6F437349;
  _nst  = $0074736E;
  _IsCl = $6C437349;
  _ass  = $00737361;
  _cord = $64726F63;
  _IsAr = $72417349;
  _ray  = $00796172;
  _IsSe = $65537349;
  _t    = $00000074;
  _IsEm = $6D457349;
  _pty  = $00797470;
  _Crea = $61657243;
  _te   = $00006574;
  _Free = $65657246;
  _Low  = $00776F4C;
  _High = $68676948;
  _Leng = $676E654C;
  _th   = $00006874;
  _Resi = $69736552;
  _ze   = $0000657A;
  _Incl = $6C636E49;
  _ude  = $00656475;
  _Excl = $6C637845;
  _Cont = $746E6F43;
  _ains = $736E6961;
  _Valu = $756C6156;
  _e    = $00000065;

begin
  case lua_type(Handle, 1) of
    LUA_TTABLE: begin
                  userdata := nil;
                  Result := true;
                end;
 LUA_TUSERDATA: begin
                  userdata := lua_touserdata(Handle, 1);
                  Result := (userdata <> nil);
                end;
  else
    Result := false;
    exit;
  end;
  if (not Result) then exit;


  SLength := 0;
  luatype := lua_type(Handle, 2);
  if (luatype = LUA_TSTRING) then S := lua_tolstring(Handle, 2, @SLength);
  if (SLength = 0) then
  begin
    S := nil;
    stdindex := -1;
    exit;
  end;

  //  ----------- STD_INDEX часть -------------
  Data := pointer(S);
  case (SLength) of
    3: begin
         if (Data[0] = _Low) then stdindex := STD_LOW
         else stdindex := 0;
       end;
    4: begin
         if (Data[0] = _Type) then stdindex := STD_TYPE
         else
         if (Data[0] = _Free) then stdindex := STD_FREE
         else
         if (Data[0] = _High) then stdindex := STD_HIGH
         else
         stdindex := 0;
       end;
    5: begin
         if (Data[0] = _IsRe) and (word(Data[1]) = _f) then stdindex := STD_IS_REF
         else
         if (Data[0] = _IsSe) and (word(Data[1]) = _t) then stdindex := STD_IS_SET
         else
         if (Data[0] = _Valu) and (word(Data[1]) = _e) then stdindex := STD_VALUE
         else
         stdindex := 0;
       end;
    6: begin
         if (Data[0] = _Crea) and (word(Data[1]) = _te) then stdindex := STD_CREATE
         else
         if (Data[0] = _Leng) and (word(Data[1]) = _th) then stdindex := STD_LENGTH
         else
         if (Data[0] = _Resi) and (word(Data[1]) = _ze) then stdindex := STD_RESIZE
         else
         if (Data[0] = _Assi) and (word(Data[1]) = _gn) then stdindex := STD_ASSIGN
         else
         stdindex := 0;
       end;
    7: begin
         if (Data[0] = _IsCo) and (Data[1] = _nst) then stdindex := STD_IS_CONST
         else
         if (Data[0] = _IsCl) and (Data[1] = _ass) then stdindex := STD_IS_CLASS
         else
         if (Data[0] = _IsEm) and (Data[1] = _pty) then stdindex := STD_IS_EMPTY
         else
         if (Data[0] = _IsAr) and (Data[1] = _ray) then stdindex := STD_IS_ARRAY
         else
         if (Data[0] = _Incl) and (Data[1] = _ude) then stdindex := STD_INCLUDE
         else
         if (Data[0] = _Excl) and (Data[1] = _ude) then stdindex := STD_EXCLUDE
         else
         stdindex := 0;
       end;
    8: begin
         if (Data[0] = _Type) and (Data[1] = _Name) then stdindex := STD_TYPE_NAME
         else
         if (Data[0] = _IsRe) and (Data[1] = _cord) then stdindex := STD_IS_RECORD
         else
         if (Data[0] = _Cont) and (Data[1] = _ains) then stdindex := STD_CONTAINS
         else
         stdindex := 0;
       end;
   10: begin
         if (Data[0] = _Type) and (Data[1] = _Pare) and (word(Data[2]) = _nt) then
           stdindex := STD_TYPE_PARENT
         else
           stdindex := 0;
       end;
   12: begin
         if (Data[0] = _Inhe) and (Data[1] = _rits) and (Data[2] = _From) then
           stdindex := STD_INHERITS_FROM
         else
           stdindex := 0;
       end;
  else
    stdindex := 0;
  end;
end;      *)

           (*
// проверка на допустимость вызова дефолтного свойства
function __can_jump_default_property(const Lua: TLua; const luatype: integer; const PropInfo: TLuaPropertyInfo): boolean;
begin
  Result := (luatype <> LUA_TSTRING);

  // проверка, если аргумент - строка
  if (not Result) then
  with PropInfo, PLuaRecordInfo(Parameters)^ do
  if (Parameters <> INDEXED_PROPERTY) then                                       {todo потом посмотреть более детально}
  Result := (Parameters = NAMED_PROPERTY) or (Lua.ClassesInfo[FClassIndex].Properties[0].Base.Kind = pkString);
end;            *)
                  (*
// функция взятия стандартного свойства или метода.
// функция вызывается редко, поэтому вынесена в отдельный код
// true если "попал"
function __push_std_prop(const Lua: TLua; const _ClassInfo: TLuaClassInfo; const stdindex: integer; const userdata: PLuaUserData; const S: pansichar): boolean;

  // невозможно вызвать метод, потому что инстенст - константа
  procedure ThrowConst();
  begin
    Lua.ScriptAssert('%s() method can''t be called, because %s instance is const', [S, _ClassInfo._ClassName]);
  end;
begin
  Result := false;

    with Lua do
    case (stdindex) of
           STD_TYPE: begin
                       global_push_value(_ClassInfo.Ref);
                       Result := true;
                     end;
      STD_TYPE_NAME: begin
                       lua_push_pascalstring(Handle, _ClassInfo._ClassName);
                       Result := true;
                     end;
    STD_TYPE_PARENT: begin
                       if (_ClassInfo.ParentIndex < 0) then lua_pushnil(Handle)
                       else global_push_value(ClassesInfo[_ClassInfo.ParentIndex].Ref);

                       Result := true;
                     end;
  STD_INHERITS_FROM: begin
                       lua_pushcclosure(Handle, lua_CFunction(cfunction_inherits_from), 0);
                       Result := true;
                     end;
         STD_ASSIGN: begin
                       if (userdata <> nil) then
                       begin
                         if (_ClassInfo._ClassKind <> ckClass) and (userdata.is_const) then ThrowConst();
                         lua_pushcclosure(Handle, lua_CFunction(cfunction_assign), 0);
                         Result := true;
                       end;
                     end;
         STD_IS_REF: begin
                       lua_pushboolean(Handle, (userdata <> nil) and (not userdata.gc_destroy));
                       Result := true;
                     end;
       STD_IS_CONST: begin
                       lua_pushboolean(Handle, (userdata <> nil) and (userdata.is_const));
                       Result := true;
                     end;
       STD_IS_CLASS: begin
                       lua_pushboolean(Handle, _ClassInfo._ClassKind = ckClass);
                       Result := true;
                     end;
      STD_IS_RECORD: begin
                       lua_pushboolean(Handle, _ClassInfo._ClassKind = ckRecord);
                       Result := true;
                     end;
         STD_IS_SET: begin
                       lua_pushboolean(Handle, _ClassInfo._ClassKind = ckSet);
                       Result := true;
                     end;
       STD_IS_EMPTY: begin
                       if (userdata = nil) or (userdata.instance = nil) then lua_pushboolean(Handle, true)
                       else
                       case _ClassInfo._ClassKind of
                         ckClass: lua_pushboolean(Handle, false{todo ?});
                        ckRecord: lua_pushboolean(Handle, IsMemoryZeroed(userdata.instance, PLuaRecordInfo(_ClassInfo._Class).FSize));
                           ckSet: lua_pushboolean(Handle, IsMemoryZeroed(userdata.instance, PLuaSetInfo(_ClassInfo._Class).FSize));
                       end;

                       Result := true;
                     end;
         STD_CREATE: begin
                       if (userdata = nil) and (_ClassInfo._ClassKind = ckClass) then
                       begin
                         lua_pushcclosure(Handle, lua_CFunction(_ClassInfo.__Create), 0);
                         Result := true;
                       end;
                     end;
           STD_FREE: begin
                       if (userdata <> nil) and (_ClassInfo._ClassKind = ckClass) then
                       begin
                         lua_pushcclosure(Handle, lua_CFunction(_ClassInfo.__Free), 0);
                         Result := true;
                       end;
                     end;
            STD_LOW: begin
                       if (_ClassInfo._ClassKind = ckSet) then
                       begin
                         lua_pushinteger(Handle, PLuaSetInfo(_ClassInfo._Class).FLow);
                         Result := true;
                       end;
                     end;
           STD_HIGH: begin
                       if (_ClassInfo._ClassKind = ckSet) then
                       begin
                         lua_pushinteger(Handle, PLuaSetInfo(_ClassInfo._Class).FHigh);
                         Result := true;
                       end;
                     end;
        STD_INCLUDE: begin
                       if (_ClassInfo._ClassKind = ckSet) and (userdata <> nil) then
                       begin
                         if (userdata.is_const) then ThrowConst();
                         lua_pushcclosure(Handle, lua_CFunction(cfunction_set_include), 0);
                         Result := true;
                       end;
                     end;
        STD_EXCLUDE: begin
                       if (_ClassInfo._ClassKind = ckSet) and (userdata <> nil) then
                       begin
                         if (userdata.is_const) then ThrowConst();
                         lua_pushcclosure(Handle, lua_CFunction(cfunction_set_exclude), 0);
                         Result := true;
                       end;
                     end;
       STD_CONTAINS: begin
                       if (_ClassInfo._ClassKind = ckSet) and (userdata <> nil) then
                       begin
                         lua_pushcclosure(Handle, lua_CFunction(cfunction_set_contains), 0);
                         Result := true;
                       end;
                     end;
          STD_VALUE: begin
                       if (_ClassInfo._ClassIndex = TLUA_REFERENCE_CLASS_INDEX) and (userdata <> nil) and (userdata.instance <> nil) then
                       begin
                         lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaReference(userdata.instance).Index);
                         Result := true;
                       end;
                     end;
    end;
end;
       *)

             (*
// размер TypeInfo
function TLua.__len(const ClassInfo: TLuaClassInfo): integer;
begin
  Result := 1;

  case (ClassInfo._ClassKind) of
    ckClass: lua_pushinteger(Handle, TClass(ClassInfo._Class).InstanceSize);
   ckRecord: lua_pushinteger(Handle, PLuaRecordInfo(ClassInfo._Class).FSize);
    ckArray: lua_pushinteger(Handle, PLuaArrayInfo(ClassInfo._Class).FSize);
      ckSet: lua_pushinteger(Handle, PLuaSetInfo(ClassInfo._Class).FSize);
  else
    lua_pushinteger(Handle, 0);
  end;
end;
           *)
                         (*
// операторы над структурами и множествами
// neg, add, sub, mul, div, mod, pow, equal, less, less_equal
// операторы над множествами уже встроены: neg, +, -, *, <=, ==

// операторы над структурами опциональны. но есть ограничения
// при сравнении Result - это результат сравнения,
// при операторе neg - X1 и X2 равны - это исходная структура
// +,- и сравнение - делается c двумя операндами структурами
// для *,/,% и ^ - второй операнд - double
function TLua.__operator(const ClassInfo: TLuaClassInfo; const Kind: integer): integer;
var
  INDEXES: array[boolean] of integer;

  X1_type, X2_type: integer;
  Dest, X1, X2: PLuaUserData;
  DestCompare: integer;
  sizeofSet: integer; // размер и признак операнда

  X2_Value: pointer;
  Number: double;
  IntValue: integer;

  // вызывается, когда указаны плохие операнды
  procedure ThrowWrongOperands();
  const
    operators: array[OPERATOR_NEG..OPERATOR_POW] of string = ('neg', '+', '-', '*', '/', '%', '^');
  var
    X1, X2: TLuaArg;
    _kind, part2: string;
  begin
    if (Kind = OPERATOR_CONCAT) then _kind := '..'
    else
    if (Kind > OPERATOR_POW) then _kind := 'compare'
    else
    _kind := operators[Kind];


    if (Kind <> OPERATOR_NEG) then
    begin
      stack_luaarg(X1, 1, true);
      stack_luaarg(X2, 2, true);
      part2 := Format('s "%s" and "%s"', [X1.ForceString, X2.ForceString]);
    end else
    begin
      stack_luaarg(X1, 1, true);
      part2 := Format(' "%s"', [X1.ForceString]);
    end;

    ScriptAssert('Fail operation "%s" with operand%s', [_kind, part2]);
  end;

begin
  Result := 1;

  // особый случай - конкатенация, с целью получения одномерного динамического массива
  if (Kind = OPERATOR_CONCAT) then
  begin
    push_userdata(ClassInfo, true, nil);
    __array_include(2{concat_mode});
    exit;
  end;

  // sizeofSet
  if (ClassInfo._ClassKind = ckRecord) then sizeofSet := 0
  else sizeofSet := PLuaSetInfo(ClassInfo._Class).FSize;

  // унарный оператор отрицания - тоже особый случай
  if (Kind = OPERATOR_NEG) then
  begin
    X1 := lua_touserdata(Handle, 1);
    if (X1 = nil) then ThrowWrongOperands();
    Dest := push_userdata(ClassInfo, true, nil);

    if (sizeofSet = 0) then
    begin
      // инвертировать структуру
      PLuaRecordInfo(ClassInfo._Class).FOperatorCallback(Dest.instance^, X1.instance^, X1.instance^, TLuaOperator(Kind))
    end else
    begin
      // инвертировать множество
      with PLuaSetInfo(ClassInfo._Class)^ do
      SetInvert(Dest.instance, X1.instance, FAndMasks, FRealSize);
    end;

    exit;
  end;

  // оператор - либо арифметическое (+,-,*,/) либо сравнение (<= и т.д.)
  // первый операнд - 100% TUserData и скорее всего совпадает с ClassInfo
  X1_type := lua_type(Handle, 1);
  X2_type := lua_type(Handle, 2);
  if (X1_type = LUA_TUSERDATA) then
  begin
    INDEXES[false] := 1;
    INDEXES[true] := 2;
  end else
  begin
    // в этих случаях свап невозможен
    if (sizeofSet = 0) and (Kind in [OPERATOR_DIV..OPERATOR_POW]) then ThrowWrongOperands();

    // swap
    INDEXES[false] := 2;
    INDEXES[true] := 1;
    X2_type := X1_type; // имеет значения тип второго операнда
  end;
  X1 := lua_touserdata(Handle, INDEXES[false]);
  if (X1 = nil) or ((X1.kind=ukSet)<>(ClassInfo._ClassKind=ckSet)) then ThrowWrongOperands();

  // проверка второго операнда
  if (X2_type = LUA_TNUMBER) then
  begin
    Number := lua_tonumber(Handle, INDEXES[true]);

    if (sizeofSet = 0) then
    begin
      if (not (Kind in [OPERATOR_MUL..OPERATOR_POW])) then ThrowWrongOperands();
    end else
    begin
      if (not NumberToInteger(Number, IntValue)) then ThrowWrongOperands();

      with PLuaSetInfo(ClassInfo._Class)^ do
      if (IntValue < Low) or (IntValue > High) then ThrowWrongOperands();
    end;
  end else
  begin
    if (X2_type <> LUA_TUSERDATA) then ThrowWrongOperands();
    if (sizeofSet = 0) and (Kind in [OPERATOR_MUL..OPERATOR_POW]) then ThrowWrongOperands();
  end;


  if (X2_type = LUA_TNUMBER) then
  begin
    if (sizeofSet = 0) then X2_Value := @Number
    else integer(X2_Value) := IntValue
  end else
  begin
    X2 := lua_touserdata(Handle, INDEXES[true]);
    if (X2 = nil) or (X2.ClassIndex <> X1.ClassIndex{или SetInfo}) then ThrowWrongOperands();
    X2_Value := X2.instance;
  end;


  // расчитать и вернуть результат
  if (Kind in [OPERATOR_EQUAL..OPERATOR_LESS_EQUAL]) then
  begin
    // сравнения
    if (sizeofSet = 0) then PLuaRecordInfo(ClassInfo._Class).FOperatorCallback(DestCompare, X1.instance^, X2_Value^, loCompare)
    else
    if (X2_type = LUA_TNUMBER) then DestCompare := SetsCompare(X1.instance, integer(X2_Value), sizeofSet, Kind=OPERATOR_LESS_EQUAL)
    else
    DestCompare := SetsCompare(X1.instance, X2_Value, sizeofSet, Kind=OPERATOR_LESS_EQUAL);

    // коррекция DestCompare если был свап
    if (INDEXES[false] <> 1) then DestCompare := -DestCompare;
    
    // результат
    case (Kind) of
            OPERATOR_EQUAL: lua_pushboolean(Handle, (DestCompare = 0));
             OPERATOR_LESS: lua_pushboolean(Handle, (DestCompare < 0));
       OPERATOR_LESS_EQUAL: lua_pushboolean(Handle, (DestCompare <= 0));
    end;
  end else
  begin
    // конкретная операция
    Dest := push_userdata(ClassInfo, true, nil);

    if (sizeofSet = 0) then PLuaRecordInfo(ClassInfo._Class).FOperatorCallback(Dest.instance^, X1.instance^, X2_Value^, TLuaOperator(Kind))
    else
    if (X2_type = LUA_TNUMBER) then
    case (Kind) of
      OPERATOR_ADD: SetsUnion(Dest.instance, X1.instance, integer(X2_Value), sizeofSet);
      OPERATOR_SUB: SetsDifference(Dest.instance, X1.instance, integer(X2_Value), sizeofSet, (INDEXES[false] <> 1));
      OPERATOR_MUL: SetsIntersection(Dest.instance, X1.instance, integer(X2_Value), sizeofSet);
    end
    else
    case (Kind) of
      OPERATOR_ADD: SetsUnion(Dest.instance, X1.instance, X2_Value, sizeofSet);
      OPERATOR_SUB: SetsDifference(Dest.instance, X1.instance, X2_Value, sizeofSet);
      OPERATOR_MUL: SetsIntersection(Dest.instance, X1.instance, X2_Value, sizeofSet);
    end;
  end;
end;
           *)

             (*
// по умолчанию сюда приходит калбек конструктора Create() для классов.
// но сюда так же перенаправляются конструкторы "на стеке" из __call(create = false)
// не только из классов, но так же из структур, массивов, множеств
//
// поэтому с ОЧЕНЬ большой вероятностью, первый аргумент является таблицей-"классом"
// для чистоты используемости как раз проверяем первый аргумент (если что - ошибка)
function TLua.__constructor(const ClassInfo: TLuaClassInfo; const __create: boolean): integer;
const
  OFFSET = 1; // чтобы не учитывать первый параметр, который таблица-"класс"
var
  i: integer;
  userdata: PLuaUserData;
  constructor_address: pointer;
  ArgsCount: integer;
  initialize_mode: boolean;
begin
  Result := 1;

  // проверка на корректность использования
  // чаще всего это может произойти (даже не знаю почему)
  if (lua_type(Handle,1)<>LUA_TTABLE) or (ClassInfo._ClassIndex<>LuaTableToClass(Handle,1)) then
  ScriptAssert('Incorrect usage of %s constructor.', [ClassInfo._ClassName]);

  // запушить userdata, выполнить базовую инициализацию
  // либо в случае класса - новый Instance, либо в случае другого типа - указатель на данные внутри себя
  userdata := push_userdata(ClassInfo, not __create, nil);
  if (ClassInfo._ClassKind = ckClass) then userdata.instance := pointer(TClass(ClassInfo._Class).NewInstance);


  // определить количество аргументов,
  // является ли последний параметр "инициализирующей таблицей"
  initialize_mode := false;
  ArgsCount := lua_gettop(Handle)-OFFSET-{userdata}1;
  if (ArgsCount > 0) then
  begin
     if (lua_type(Handle,-2)=LUA_TTABLE) and (LuaTableToClass(Handle,-2)<0) then
     begin
       initialize_mode := true;
       dec(ArgsCount);
     end;
  end;

  // проверка на возможность инициализации по таблице
  // в будущем возможно будет инициализация по всем типам
  // todo ?
  if (initialize_mode) and (userdata.kind <> ukInstance) then
  ScriptAssert('%s can not be initialized by a table.', [ClassInfo._ClassName]);


  // если не ukInstance
  case (userdata.kind) of
      ukArray:  begin
                  __array_include(0{constructor_mode});
                  exit;
                end;
        ukSet:  begin
                  __set_method(true, 0{include_mode});
                  exit;
                end;
  else
    if (userdata.kind <> ukInstance) then {такого быть не должно} exit;

    // TLuaReference - особый случай, для него используется свой конструктор (из стека)
    if (ClassInfo._ClassIndex = TLUA_REFERENCE_CLASS_INDEX) then
    begin
      if (ArgsCount < 0) or (ArgsCount > 1) then ScriptAssert('Wrong arguments count(%d) of TLuaReference() constructor', [ArgsCount]);

      // проинициализировать ссылку
      if (ArgsCount = 0) then lua_pushnil(Handle) else lua_pushvalue(Handle, -2);
      TLuaReference(userdata.instance).Initialize(Self);

      exit;
    end;
  end;


  // может быть определён конструктор
  // если определён - вызываем (альтернативный конструктор)
  if (ClassInfo.constructor_address <> nil) then
  begin
    // заполнить массив аргументов
    FArgsCount := ArgsCount;
    SetLength(FArgs, ArgsCount);
    for i := 0 to ArgsCount-1 do stack_luaarg(FArgs[i], i+OFFSET+1, true);

    // проверка на количество аргументов
    if (ClassInfo.constructor_args_count >= 0) and (FArgsCount <> ClassInfo.constructor_args_count) then
    ScriptAssert('Constructor of %s should have %d arguments', [ClassInfo._ClassName]);

    // вызов
    begin
      constructor_address := ClassInfo.constructor_address;
      if (dword(constructor_address) >= $FE000000) then constructor_address := ppointer(dword(userdata.instance^) + dword(constructor_address) and $00FFFFFF)^;

      TLuaClassProc16(constructor_address)(userdata.instance^, FArgs, TLuaArg(nil^));
    end;

    // обнулить количество параметров, не трогая при этом FArgs
    FArgsCount := 0;

    // особая логика для TMethod
    if (ClassInfo._ClassIndex = TMETHOD_CLASS_INDEX) and (pint64(userdata.instance)^ = 0) then
    begin
      lua_remove(Handle, -1);
      lua_pushnil(Handle);
    end;
  end;

  // если последний аргумент действительно была инициализирующая таблица
  // то вызываем соответствующий (заполняющий) обработчик
  if (initialize_mode) then
  __initialize_by_table(userdata, lua_gettop(Handle)-1 {аналог -2 в прямой адресации})
end;            *)
                  (*
function TLua.__destructor(const ClassInfo: TLuaClassInfo; const __free: boolean): integer;
var
  luatype: integer;
  userdata: PLuaUserData;
begin
  Result := 0;

  luatype := lua_type(Handle, 1);
  if (luatype <> LUA_TUSERDATA) then
  ScriptAssert('Wrong destruction type "%s"', [LuaTypeName(luatype)]);

  userdata := lua_touserdata(Handle, 1);

  // не надо удалять объекты при __gc, если не стоит соответствующего флага
  if (not __free) and (not userdata .gc_destroy) then exit;

  // если уже удалён
  if (userdata.instance = nil) then
  begin
    if (__free) then
    ScriptAssert('Instance of %s type is already destroyed', [ClassInfo._ClassName]);

    exit;
  end;

  // очистить данные для массивов и сложных свойств
  if (@ClassInfo = nil) then
  with userdata^ do
  begin
    if (kind = ukProperty) then
    with PropertyInfo^ do
    begin
      if (Parameters = NAMED_PROPERTY) then
        pstring(integer(userdata)+sizeof(TLuaUserData))^ := ''
      else
        Finalize(pointer(integer(userdata)+sizeof(TLuaUserData)), PLuaRecordInfo(Parameters).FTypeInfo);
    end;

    instance := nil;
    exit;
  end;

  // вызвать дестуктор Object-а или очистить структуру
  case (ClassInfo._ClassKind) of
     ckClass: TObject(userdata.instance).Free;
    ckRecord: begin
                // удаление Record внутри UserData
                with PLuaRecordInfo(ClassInfo._Class)^ do
                if (FTypeInfo <> nil) then Finalize(userdata.instance, FTypeInfo);
              end;
     ckArray: begin
                // удаление массива
                with PLuaArrayInfo(ClassInfo._Class)^ do
                if (FTypeInfo <> nil) then Finalize(userdata.instance, FTypeInfo, FItemsCount);
              end;
  end;            

  // занулить в любом случае
  userdata.instance := nil;
end;
                 *)
                   (*
// первый параметр стека всегда есть!
// калбек вызывается тогда, когда у Типа (класс, структура, массив, множество)
// или его экземпляра вызывается метод
// например так: TButton(Form1), TPoint(12, 13), Form1. OnClick(), ArrayInstance(/*ошибка*/)
//
// калбек так же вызывается при инициализации таблицей
// например так:
// Button1 {Caption="Text", Color=clBtnFace}
//
// соответственно придёт и такой калбек:
// Button1( {Caption="Text", Color=clBtnFace} )
//
// все ошибочные случаи так же нужно отследить
function TLua.__call(const ClassInfo: TLuaClassInfo): integer;
var
  userdata: PLuaUserData;
begin
  Result := 0;

  // если первый параметр именно класс - то переадресуем в конструктор
  if (lua_type(Handle,1)=LUA_TTABLE) and (ClassInfo._ClassIndex=LuaTableToClass(Handle,1)) then
  begin
    Result := __constructor(ClassInfo, false);
    exit;
  end;

  // остался случай когда происходит "вызов" экземпляра
  // либо событие, либо "инициализация по таблице", либо ошибка


  // "event"
  if (ClassInfo._ClassIndex = TMETHOD_CLASS_INDEX) then
  begin
    userdata := lua_touserdata(Handle, 1);
    if (userdata = nil) or (userdata.ClassIndex <> TMETHOD_CLASS_INDEX) then  ScriptAssert('Wrong TMethod usage', []);

    Result := __tmethod_call(TMethod(userdata.instance^));
    exit;
  end;

  // если происходит инициализация по таблице
  if (lua_gettop(Handle)=2) and (lua_type(Handle,2)=LUA_TTABLE) and (LuaTableToClass(Handle,2)<0) then
  begin
    userdata := lua_touserdata(Handle, 1);
    if (userdata = nil) {но вообще быть не должно} then ScriptAssert('Unsupported operation.', []);

    // проверка на возможность инициализации по таблице
    // в будущем возможно будет инициализация по всем типам
    // todo ?
    if (userdata.kind <> ukInstance) then
    ScriptAssert('%s can not be initialized by a table.', [ClassInfo._ClassName]);

    // вызов
    __initialize_by_table(userdata, 2);
  end else
  begin
    // 100% ошибочная ситуация вызова конструктора из экземпляра
    // но я перенаправляю вызов в конструктор (там нужное сообщение покажется)
    Result := __constructor(ClassInfo, false);
  end;
end;
             *)

function TLua.push_stdandard(const MetaType: PLuaClassInfo; const StdIndex: Integer;
  const UserData: Pointer): Boolean;
begin
  Result := False{ToDo};
end;

function TLua.set_stdandard(const MetaType: PLuaClassInfo; const StdIndex: Integer; const UserData: Pointer): Boolean;
begin
  Result := False{ToDo};
end;

function TLua.call_prop_getter(const Instance: Pointer; const AProp: Pointer): Pointer;
type
  TProcStd = function(const P1, P2, P3: NativeInt): NativeInt;
  TProcInt64 = function(const P1, P2, P3: NativeInt): Int64;
  TProcSingle = function(const P1, P2, P3: NativeInt): Single;
  TProcDouble = function(const P1, P2, P3: NativeInt): Double;
  TProcComp = function(const P1, P2, P3: NativeInt): Comp;
  TProcCurrency = function(const P1, P2, P3: NativeInt): Currency;
var
  Prop: ^TLuaProperty;
  Proc: Pointer;
  Mode: Cardinal;
  P1, P2, P3: NativeInt;
begin
  // result arguments (P3)
  Prop := AProp;
  P3 := 0;
  case Prop.Kind of
    pkBoolean,
    pkInteger,
    pkInt64,
    pkFloat,
    pkPointer,
    pkClass: ;
    pkObject:
    begin
      // ???
    end;
    pkString:
    begin
      case Prop.StringType of
        {$ifNdef NEXTGEN}
        stShortString:
        begin
          P3 := NativeInt(@FStringBuffer.Short);
        end;
        stAnsiString:
        begin
          P3 := NativeInt(@FStringBuffer.Ansi);
        end;
        stWideString:
        begin
          P3 := NativeInt(@FStringBuffer.Wide);
        end;
        {$endif}
        stUnicodeString:
        begin
          P3 := NativeInt(@FStringBuffer.Unicode);
        end;
      end;
    end;
    pkVariant:
    begin
      P3 := NativeInt(@FTempBuffer.V);
    end;
    pkInterface:
    begin
      P3 := NativeInt(@FTempBuffer.Intf);
    end;
  else
    // pkRecord, pkArray, pkSet, pkUniversal
    P3 := NativeInt(ResultBuffer.AllocMetaType(Prop.MetaType));
  end;

  // instance, proc, index, unused arguments
  P1 := NativeInt(Instance);
  Proc := Pointer(Prop.Getter);
  Mode := Prop.FlagsEx and PROP_SLOT_MASK;
  if (Mode >= Cardinal(pmVirtual)) then
  begin
    Inc(NativeUInt(Proc), PNativeUInt(P1)^);
    Dec(Mode, 3);
  end;
  case Mode of
    Ord(pmStaticIndex):
    begin
      P2 := Cardinal(Prop.Index);
    end;
    Ord(pmStaticParams):
    begin
      P2 := NativeInt({$ifdef LARGEINT}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Prop.ParamsPtr));
    end;
  else
    // pmStatic
    P2 := P3;
  end;
  if (Prop.FlagsEx and PROP_CLASS_MODE <> 0) then
  begin
    P1 := P2;
    P2 := P3;
  end;

  // call
  if (P3 = 0) then
  begin
    case Prop.Kind of
      // pkObject{???}
      pkInt64:
      begin
        Result := @FTempBuffer.I64;
        PInt64(Result)^ := TProcInt64(Proc)(P1, P2, P3);
      end;
      pkFloat:
      begin
        Result := @FTempBuffer.D;
        case Prop.FloatType of
          ftSingle: PDouble(Result)^ := TProcSingle(Proc)(P1, P2, P3);
            ftComp: PDouble(Result)^ := TProcComp(Proc)(P1, P2, P3);
            ftCurr: PDouble(Result)^ := TProcCurrency(Proc)(P1, P2, P3);
        else
          // ftDouble, ftExtended
          PDouble(Result)^ := TProcDouble(Proc)(P1, P2, P3);
        end;
      end;
    else
      Result := @FTempBuffer.N;
      PNativeInt(Result)^ := TProcStd(Proc)(P1, P2, P3);
    end;
  end else
  begin
    Result := Pointer(P3);
    TProcStd(Proc)(P1, P2, P3);
  end;
end;

procedure TLua.call_prop_setter(const Instance: Pointer; const AProp: Pointer; const Value: NativeInt);
label
  specific_call;
type
  TProcStd = procedure(const P1, P2, P3: NativeInt);
  TProcInt641 = procedure(const Value: Int64);
  TProcSingle1 = procedure(const Value: Single);
  TProcDouble1 = procedure(const Value: Double);
  TProcExtended1 = procedure(const Value: Extended);
  TProcComp1 = procedure(const Value: Comp);
  TProcCurrency1 = procedure(const Value: Currency);
  TProcInt642 = procedure(const P1: NativeInt; const Value: Int64);
  TProcSingle2 = procedure(const P1: NativeInt; const Value: Single);
  TProcDouble2 = procedure(const P1: NativeInt; const Value: Double);
  TProcExtended2 = procedure(const P1: NativeInt; const Value: Extended);
  TProcComp2 = procedure(const P1: NativeInt; const Value: Comp);
  TProcCurrency2 = procedure(const P1: NativeInt; const Value: Currency);
  TProcInt643 = procedure(const P1, P2: NativeInt; const Value: Int64);
  TProcSingle3 = procedure(const P1, P2: NativeInt; const Value: Single);
  TProcDouble3 = procedure(const P1, P2: NativeInt; const Value: Double);
  TProcExtended3 = procedure(const P1, P2: NativeInt; const Value: Extended);
  TProcComp3 = procedure(const P1, P2: NativeInt; const Value: Comp);
  TProcCurrency3 = procedure(const P1, P2: NativeInt; const Value: Currency);
var
  Prop: ^TLuaProperty;
  Proc: Pointer;
  Mode, ArgsCount: Cardinal;
  P1, P2, P3: NativeInt;
begin
  // basic
  Prop := AProp;
  P3 := Value;

  // instance, proc, index, unused arguments
  P1 := NativeInt(Instance);
  Proc := Pointer(Prop.Setter);
  Mode := (Prop.FlagsEx shr PROP_SLOTSETTER_SHIFT) and PROP_SLOT_MASK;
  if (Mode >= Cardinal(pmVirtual)) then
  begin
    Inc(NativeUInt(Proc), PNativeUInt(P1)^);
    Dec(Mode, 3);
  end;
  ArgsCount := 3;
  case Mode of
    Ord(pmStaticIndex):
    begin
      P2 := Cardinal(Prop.Index);
    end;
    Ord(pmStaticParams):
    begin
      P2 := NativeInt({$ifdef LARGEINT}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Prop.ParamsPtr));
    end;
  else
    // pmStatic
    P2 := P3;
    Dec(ArgsCount);
  end;
  if (Prop.FlagsEx and PROP_CLASS_MODE <> 0) then
  begin
    P1 := P2;
    P2 := P3;
    Dec(ArgsCount);
  end;

  // call specific(Int64, Float) or standard
  case Prop.Kind of
    pkInt64:
    begin
      Mode := Succ(Ord(High(TFloatType)));
      goto specific_call;
    end;
    pkFloat:
    begin
      Mode := Ord(Prop.FloatType);
    specific_call:
      case (Mode * 3 + (ArgsCount - 1)) of
        // ftSingle
        0: TProcSingle1(Proc)(PSingle(P3)^);
        1: TProcSingle2(Proc)(P1, PSingle(P3)^);
        2: TProcSingle3(Proc)(P1, P2, PSingle(P3)^);
        // ftDouble
        3: TProcDouble1(Proc)(PDouble(P3)^);
        4: TProcDouble2(Proc)(P1, PDouble(P3)^);
        5: TProcDouble3(Proc)(P1, P2, PDouble(P3)^);
        // ftExtended
        6: TProcExtended1(Proc)(PExtended(P3)^);
        7: TProcExtended2(Proc)(P1, PExtended(P3)^);
        8: TProcExtended3(Proc)(P1, P2, PExtended(P3)^);
        // ftComp
        9: TProcComp1(Proc)(PComp(P3)^);
       10: TProcComp2(Proc)(P1, PComp(P3)^);
       11: TProcComp3(Proc)(P1, P2, PComp(P3)^);
        // tCurr
       12: TProcCurrency1(Proc)(PCurrency(P3)^);
       13: TProcCurrency2(Proc)(P1, PCurrency(P3)^);
       14: TProcCurrency3(Proc)(P1, P2, PCurrency(P3)^);
        // pkInt64
       15: TProcInt641(Proc)(PInt64(P3)^);
       16: TProcInt642(Proc)(P1, PInt64(P3)^);
       17: TProcInt643(Proc)(P1, P2, PInt64(P3)^);
      end;
    end;
  else
    // call standard
    TProcStd(Proc)(P1, P2, P3);
  end;
end;

function TLua.__index(const AMetaType: __luapointer): Integer;
label
  invalid_instance, property_push, field_push, done;
var
  MetaType: ^TLuaClassInfo;
  UserData: PLuaUserData;
  LuaType: Integer;
  LuaName: __luaname;
  Item: PLuaDictionaryItem;
  Ptr: __luapointer;
  Prop: ^TLuaProperty;
  Proc: ^TLuaClosure;
  Flags: Integer;
  Instance, Value: Pointer;
begin
  // instance/class
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  UserData := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: ;
 LUA_TUSERDATA: begin
                  UserData := lua_touserdata(Handle, 1);
                  if (not Assigned(UserData)) then goto invalid_instance;
                  if (not Assigned(UserData.Instance)) then
                  begin
                    Self.InternalErrorFmt('%s instance is already destroyed', [MetaType.FName]);
                  end;
                end;
  else
  invalid_instance:
    Self.InternalError('Invalid self argument');
  end;

  // argument
  LuaName := nil;
  LuaType := lua_type(Handle, 2);
  if (LuaType = LUA_TSTRING) then LuaName := lua_tolstring(Handle, 2, nil);

  // try to find
  if (Assigned(LuaName)) and (MetaType.F.Kind <= mtRecord) then
  begin
    Item := TLuaDictionary(MetaType.FNameSpace).InternalFind(LuaName, False);
    if (Assigned(Item)) then
    begin
      Ptr := Item.Value;
      if (Ptr and NAMESPACE_FLAG_PROC = 0) then
      begin
        Ptr := Ptr and NAMESPACE_FLAGS_CLEAR;
        Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
        goto property_push;
      end else
      begin
        Ptr := Ptr and NAMESPACE_FLAGS_CLEAR;
        Proc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
        //push_function(Proc.Value);
        goto done;
      end;
    end;
  end;

  // try to find standard
  if (Assigned(LuaName)) then
  begin
    Item := TLuaDictionary(FStandardNames).InternalFind(LuaName, False);
    if (Assigned(Item)) then
    begin
      if (push_stdandard(MetaType, Item.Value, UserData)) then goto done;
    end;
  end;

  // try look default property
  if (MetaType.F.Kind = mtClass) and (MetaType.DefaultProperty <> LUA_POINTER_INVALID) then
  begin
    Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(MetaType.DefaultProperty);
    // ToDo
    goto property_push;
  end;

  // not found
  stack_force_unicode_string(FStringBuffer.Unicode, 2);
  Self.InternalErrorFmt('"%s" not found in %s %s', [FStringBuffer.Unicode, MetaType.FName,
    INSTANCE_MODES[Byte(UserData = nil) + (Byte(UserData = nil) and Byte(MetaType.F.Kind = mtClass))]]);
  goto done;

property_push:
  Flags := Prop.Flags;
  if (Assigned(UserData)) >= (Flags and PROP_CLASS_MODE = 0) then
  begin
    Instance := nil;
    if (Flags and PROP_CLASS_MODE = 0) then Instance := UserData.Instance;
    Flags := Flags and PROP_SLOT_MASK;
    if (Flags = Ord(pmField)) then
    begin
      Value := Pointer(Prop.Getter);
      Inc(NativeUInt(Value), NativeUInt(Instance));
    field_push:
      Prop.GetValue(Value^, Self,
        Ord(Prop.FlagsEx and PROP_SLOT_MASK <> Cardinal(pmField)) +
        2 * Ord(Prop.FlagsEx and PROP_CONSTREF_MODE <> 0)
      );
    end else
    if (Flags <> Ord(pmNone)) then
    begin
      Value := call_prop_getter(Instance, Prop);
      if (Prop.Kind <> pkFloat) then
      begin
        goto field_push;
      end else
      begin
        lua_pushnumber(Handle, PDouble(Value)^);
      end;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      Self.InternalErrorFmt('%s.%s property is a writeonly property', [MetaType.FName, FStringBuffer.Lua]);
    end;
  end else
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    Self.InternalErrorFmt('%s.%s property is not a class property', [MetaType.FName, FStringBuffer.Lua]);
  end;

done:
  Result := 1;
end;

function TLua.__newindex(const AMetaType: __luapointer): Integer;
label
  invalid_instance, property_set, failure_set, done;
const
  PROP_FIELD = Ord(pmField) shl PROP_SLOTSETTER_SHIFT;
var
  MetaType: ^TLuaClassInfo;
  UserData: PLuaUserData;
  LuaType: Integer;
  LuaName: __luaname;
  Item: PLuaDictionaryItem;
  Ptr: __luapointer;
  Prop: ^TLuaProperty;
  Flags: Integer;
  Setter, Value: NativeInt;

  function CallVariantSetter(const Self: TLua; const Instance: Pointer; const Prop: PLuaProperty): Boolean; far;
  begin
    Result := Prop.SetValue(Self.FTempBuffer.V, Self, -1);
    if (Result) then
    try
      Self.call_prop_setter(Instance, Prop, NativeInt(@Self.FTempBuffer.V));
    finally
      VarClear(Self.FTempBuffer.V);
    end;
  end;

  function CallIntfSetter(const Self: TLua; const Instance: Pointer; const Prop: PLuaProperty): Boolean; far;
  begin
    Result := Prop.SetValue(Self.FTempBuffer.Intf, Self, -1);
    if (Result) then
    try
      Self.call_prop_setter(Instance, Prop, NativeInt(Self.FTempBuffer.Intf));
    finally
      Self.FTempBuffer.Intf := nil;
    end;
  end;

  procedure CallEmptyMetaTypeSetter(const Self: TLua; MetaType: PLuaMetaType; const Instance: Pointer;
    const AProp: Pointer); far;
  var
    Size: Integer;
    Buffer: array[0..511] of Byte;
    Ptr: Pointer;
  begin
    Size := (MetaType.F.Size + SizeOf(Pointer)) and -SizeOf(Pointer);

    if (Size <= SizeOf(Buffer)) then
    begin
      if (Size > SizeOf(Pointer)) then
      begin
        FillChar(Buffer, Size, #0);
        Self.call_prop_setter(Instance, AProp, NativeInt(@Buffer));
      end else
      begin
        Self.call_prop_setter(Instance, AProp, 0);
      end;
    end else
    begin
      GetMem(Ptr, Size);
      try
        FillChar(Ptr^, Size, #0);
        Self.call_prop_setter(Instance, AProp, NativeInt(Ptr));
      finally
        FreeMem(Ptr);
      end;
    end;
  end;

  function AssignMetaType(const Self: TLua; const Instance: Pointer; const Prop: PLuaProperty): Boolean; far;
  label
    failure, done;
  var
    UserData: PLuaUserData;
    MetaType: PLuaMetaType;
    Buffer: array[0..SizeOf(Pointer) - 1] of Byte;
  begin
    MetaType := Prop.MetaType;
    case lua_type(Self.Handle, -1) of
      LUA_TNIL:
      begin
        CallEmptyMetaTypeSetter(Self, MetaType, Instance, Prop);
      end;
      LUA_TUSERDATA:
      begin
        UserData := lua_touserdata(Self.Handle, -1);
        if (UserData = nil) or (UserData.Instance = nil) or
          (UserData.MetaType <> {$ifdef SMALLINT}__luapointer(MetaType){$else}MetaType.Ptr{$endif}) then
          goto failure;

        if (MetaType.F.Size <= SizeOf(Pointer)) then
        begin
          PNativeInt(@Buffer)^ := 0;
          System.Move(UserData.Instance^, Buffer, MetaType.F.Size);
          Self.call_prop_setter(Instance, Prop, NativeInt(Buffer));
        end else
        begin
          Self.call_prop_setter(Instance, Prop, NativeInt(UserData.Instance));
        end;
      end;
    else
    failure:
      Result := False;
      Exit;
    end;

  done:
    Result := True;
  end;

begin
  // instance/class
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  UserData := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: ;
 LUA_TUSERDATA: begin
                  UserData := lua_touserdata(Handle, 1);
                  if (not Assigned(UserData)) then goto invalid_instance;
                  if (not Assigned(UserData.Instance)) then
                  begin
                    Self.InternalErrorFmt('%s instance is already destroyed', [MetaType.FName]);
                  end;
                end;
  else
  invalid_instance:
    Self.InternalError('Invalid self argument');
  end;

  // argument
  LuaName := nil;
  LuaType := lua_type(Handle, 2);
  if (LuaType = LUA_TSTRING) then LuaName := lua_tolstring(Handle, 2, nil);

  // try to find
  if (Assigned(LuaName)) and (MetaType.F.Kind <= mtRecord) then
  begin
    Item := TLuaDictionary(MetaType.FNameSpace).InternalFind(LuaName, False);
    if (Assigned(Item)) then
    begin
      Ptr := Item.Value;
      if (Ptr and NAMESPACE_FLAG_PROC = 0) then
      begin
        Ptr := Ptr and NAMESPACE_FLAGS_CLEAR;
        Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
        goto property_set;
      end else
      begin
        stack_lua_string(FStringBuffer.Lua, 2);
        Self.InternalErrorFmt('Method %s.%s can not be changed', [MetaType.FName, FStringBuffer.Lua]);
        goto done;
      end;
    end;
  end;

  // try to find standard
  if (Assigned(LuaName)) then
  begin
    Item := TLuaDictionary(FStandardNames).InternalFind(LuaName, False);
    if (Assigned(Item)) then
    begin
      if (set_stdandard(MetaType, Item.Value, UserData)) then goto done;
    end;
  end;

  // try look default property
  if (MetaType.F.Kind = mtClass) and (MetaType.DefaultProperty <> LUA_POINTER_INVALID) then
  begin
    Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(MetaType.DefaultProperty);
    // ToDo
    goto property_set;
  end;

  // not found
  stack_force_unicode_string(FStringBuffer.Unicode, 2);
  Self.InternalErrorFmt('"%s" not found in %s %s', [FStringBuffer.Unicode, MetaType.FName,
    INSTANCE_MODES[Byte(UserData = nil) + (Byte(UserData = nil) and Byte(MetaType.F.Kind = mtClass))]]);
  goto done;

property_set:
  Flags := Prop.Flags;
  if (Assigned(UserData)) >= (Flags and PROP_CLASS_MODE = 0) then
  begin
    Setter := 0;
    if (Flags and PROP_CLASS_MODE = 0) then Setter := NativeInt(UserData.Instance);
    Flags := Flags and (PROP_SLOT_MASK shl PROP_SLOTSETTER_SHIFT);
    if (Flags <> 0) then
    begin
      if (Flags = PROP_FIELD) then
      begin
        if (not Prop.SetValue(Pointer(NativeUInt(Setter) + Prop.Setter)^, Self, -1)) then
          goto failure_set;
      end else
      begin
        case Prop.Kind of
          pkBoolean,
          pkInteger,
          pkPointer,
          pkClass:
          begin
            if (not Prop.SetValue(Pointer(@FTempBuffer.N)^, Self, -1)) then goto failure_set;
            Value := FTempBuffer.N;
          end;
          pkInt64,
          pkFloat:
          begin
            if (not Prop.SetValue(Pointer(@FTempBuffer.E)^, Self, -1)) then goto failure_set;
            Value := NativeInt(@FTempBuffer.E);
          end;
          pkString:
          begin
            case Prop.StringType of
              {$ifNdef NEXTGEN}
              stShortString:
              begin
                stack_short_string(FStringBuffer.Short, -1, Prop.MaxShortLength);
                Value := NativeInt(@FStringBuffer.Short);
              end;
              stAnsiString:
              begin
                stack_ansi_string(FStringBuffer.Ansi, -1,
                  {$ifdef INTERNALCODEPAGE}GetTypeData(Prop.TypeInfo).CodePage{$else}0{$endif});
                Value := NativeInt(@FStringBuffer.Ansi);
              end;
              stWideString:
              begin
                stack_wide_string(FStringBuffer.Wide, -1);
                Value := NativeInt(@FStringBuffer.Wide);
              end;
              {$endif}
              stUnicodeString:
              begin
                stack_unicode_string(FStringBuffer.Unicode, -1);
                Value := NativeInt(@FStringBuffer.Unicode);
              end;
            else
              if (not Prop.SetValue(Pointer(@FTempBuffer.N)^, Self, -1)) then goto failure_set;
              Value := FTempBuffer.N;
            end;
          end;
          pkVariant:
          begin
            if (not CallVariantSetter(Self, Pointer(Setter), Prop)) then goto failure_set;
            goto done;
          end;
          pkInterface:
          begin
            if (not CallIntfSetter(Self, Pointer(Setter), Prop)) then goto failure_set;
            goto done;
          end;
          pkObject:
          begin
            {ToDo}
            if (not Prop.SetValue(Pointer(@FTempBuffer.N)^, Self, -1)) then
              goto failure_set;
            Value := FTempBuffer.N;
          end;
        else
          // pkRecord, pkArray, pkSet, pkUniversal
          if (not AssignMetaType(Self, Pointer(Setter), Prop)) then goto failure_set;
          goto done;
        end;

        call_prop_setter(Pointer(Setter), Prop, Value);
      end;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
      Self.InternalErrorFmt('%s.%s property is a readonly property', [MetaType.FName, FStringBuffer.Lua]);
    end;
  end else
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    InternalErrorFmt('%s.%s property is not a class property', [MetaType.FName, FStringBuffer.Lua]);
    goto done;
  failure_set:
    stack_lua_string(FStringBuffer.Lua, 2);
    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    Error('Can not change %s.%s %s to %s ("%s")', [MetaType.FName,
      FStringBuffer.Lua, PROPERTY_MODES[Ord(MetaType.F.Kind = mtRecord)],
      FStringBuffer.Default, FStringBuffer.Unicode]);
  end;

done:
  Result := 0;
end;

function TLua.__global_index(const ModifyLow, ModifyHigh: Cardinal): Integer;
label
  unsupported;
var
  NativeModify: ^TLuaGlobalModify;
  Entity: PLuaGlobalEntity;
  LuaType: Integer;
  LuaName: __luaname;
  EntityItem: ^TLuaDictionaryItem;
  Name: PLuaString;
  MetaType: ^TLuaMetaType;
  Prop: ^TLuaProperty;
  Proc: ^TLuaMethod;
begin
  NativeUInt(NativeModify) := {$ifdef LARGEINT}(NativeUInt(ModifyHigh) shl 32) +{$endif} ModifyLow;

  // entity
  if (Assigned(NativeModify)) then
  begin
    LuaName := nil;
    Entity := GetGlobalEntity(NativeModify.Name^, False);
    if (Assigned(Entity)) then
    begin
      LuaName := TLuaNames(FNames).FindValue(NativeModify.Name^);
    end;
  end else
  begin
    LuaType := lua_type(Handle, 2);
    if (LuaType <> LUA_TSTRING) then
    begin
      GetLuaTypeName(FStringBuffer.Default, LuaType);
      Error('Global key should be a string. Type %s is not available as a global key', [FStringBuffer.Default]);
    end;
    LuaName := lua_tolstring(Handle, 2, nil);
    EntityItem := TLuaDictionary(FGlobalEntities).InternalFind(LuaName, False);

    Entity := nil;
    if (Assigned(EntityItem)) then
    begin
      Entity := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(EntityItem.Value);
    end;
  end;
  if (not Assigned(Entity)) then
  begin
    if (Assigned(NativeModify)) then
    begin
      Name := NativeModify.Name;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      Name := @FStringBuffer.Lua;
    end;

    InternalErrorFmt('Global variable "%s" not found', [Name^]);
  end;

  // push entity value
  case Entity.Kind of
    gkMetaType:
    begin
      MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
      global_push_value(MetaType.Ref);
    end;
    gkVariable:
    begin
      Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
      Prop.GetValue(Pointer(Prop.Getter)^, Self,
        Ord(Prop.FlagsEx and PROP_SLOT_MASK <> Cardinal(pmField)) +
        2 * Ord(Prop.FlagsEx and PROP_CONSTREF_MODE <> 0)
      );
    end;
    gkProc:
    begin
      Proc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
      push_closure(LuaName, Ord(ckStatic), Ord(ikNone), nil, Proc.Address, Proc.InvokableMode, Proc.Invokable);
    end;
  else
    // gkConst, gkScriptVariable
    global_push_value(Entity.Ref);
  end;

  if (Assigned(NativeModify)) then
  begin
    if (NativeModify.VariantMode) then
    begin
      if (not stack_variant(NativeModify.V^, -1, False)) then goto unsupported;
    end else
    begin
      if (not stack_luaarg(NativeModify.Arg^, -1, False)) then
      begin
      unsupported:
        stack_pop;
        raise ELua.CreateFmt('Can''t get global variable "%s" of type "%s"', [NativeModify.Name^,
          FStringBuffer.Default]) at FReturnAddress;
      end;
    end;
    stack_pop;
  end;

  Result := 1;
end;

function TLua.__global_newindex(const ModifyLow, ModifyHigh: Cardinal): Integer;
label
  script_variable, unsupported;
var
  NativeModify: ^TLuaGlobalModify;
  Entity: PLuaGlobalEntity;
  LuaType: Integer;
  LuaName: __luaname;
  EntityItem: ^TLuaDictionaryItem;
  Ptr: __luapointer;
  Name: PLuaString;
  Prop: ^TLuaProperty;
  Done: Boolean;
begin
  NativeUInt(NativeModify) := {$ifdef LARGEINT}(NativeUInt(ModifyHigh) shl 32) +{$endif} ModifyLow;

  // entity
  if (Assigned(NativeModify)) then
  begin
    Entity := GetGlobalEntity(NativeModify.Name^, True);
  end else
  begin
    LuaType := lua_type(Handle, 2);
    if (LuaType <> LUA_TSTRING) then
    begin
      GetLuaTypeName(FStringBuffer.Default, LuaType);
      Error('Global key should be a string. Type %s is not available as a global key', [FStringBuffer.Default]);
    end;
    LuaName := lua_tolstring(Handle, 2, nil);
    EntityItem := TLuaDictionary(FGlobalEntities).InternalFind(LuaName, True);

    Ptr := EntityItem.Value;
    if (Ptr = LUA_POINTER_INVALID) then
    begin
      lua_pushvalue(Handle, 2);
      global_fill_value(global_alloc_ref);
      Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaGlobalEntity));
      EntityItem.Value := Ptr;
      Entity := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
      PInteger(Entity)^ := 0;
      PLuaGlobalEntity(Entity).Ptr := LUA_POINTER_INVALID;
    end else
    begin
      Entity := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
    end;
  end;

  if (Entity.Ptr = LUA_POINTER_INVALID) then
  begin
    // new item
    Entity.Kind := gkScriptVariable;
    Entity.Ref := global_alloc_ref;
    goto script_variable;
  end else
  if (Entity.ConstMode) then
  begin
    if (Assigned(NativeModify)) then
    begin
      Name := NativeModify.Name;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      Name := @FStringBuffer.Lua;
    end;
    InternalErrorFmt('Global const "%s" can not be changed', [Name^]);
  end else
  if (Entity.Kind = gkScriptVariable) then
  begin
  script_variable:
    if (NativeModify = nil) then
    begin
      lua_pushvalue(Handle, 3);
    end else
    if (NativeModify.VariantMode) then
    begin
      if (not push_variant(NativeModify.V^)) then goto unsupported;
    end else
    begin
      if (not push_luaarg(NativeModify.Arg^)) then
      begin
      unsupported:
        raise ELua.CreateFmt('Can''t set global variable "%s" of type "%s"', [NativeModify.Name^,
          FStringBuffer.Default]) at FReturnAddress;
      end;
    end;
    global_fill_value(Entity.Ref);
  end else
  begin
    // gkVariable
    // optional push native value
    if (Assigned(NativeModify)) then
    begin
      if (NativeModify.VariantMode) then
      begin
        if (not push_variant(NativeModify.V^)) then goto unsupported;
      end else
      begin
        if (not push_luaarg(NativeModify.Arg^)) then goto unsupported;
      end;
    end;

    // set property value, optional pop native value
    Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
    Done := Prop.SetValue(Pointer(Prop.Setter)^, Self, -1);
    if (Assigned(NativeModify)) then stack_pop;

    // check error
    if (not Done) then
    begin
      if (Assigned(NativeModify)) then
      begin
        Name := NativeModify.Name;
      end else
      begin
        stack_lua_string(FStringBuffer.Lua, 2);
        Name := @FStringBuffer.Lua;
      end;
      InternalErrorFmt('Can not change global variable "%s" to %s ("%s")',
        [Name^, FStringBuffer.Default, FStringBuffer.Unicode]);
    end;
  end;

  Result := 0;
end;
           (*
// описание вида Prefix[...][...][Value][...][...][...]
// или Prefix[...][...].Value
function __arrayindex_description(const Prefix, Value: string; const Index, Dimention: integer): string;
var
  i: integer;
begin
  Result := Prefix;

  if (Dimention > 0) then
  begin
    // array style
    for i := 0 to Dimention-1 do
    if (i = Index) then Result := Result + '[' + Value + ']'
    else Result := Result + '[...]';
  end else
  begin
    // property style
    for i := 0 to Index-1 do
    Result := Result + '[...]';

    Result := Result + '.' + Value;
  end;  
end;     *)
           (*
// прочитать значение из lua-стека по индексу 2 и занести в очередной параметр (для сложного свойства)
// если массив - то проверить индекс !
// возвращает true если нужно брать конкретные данные
// index = (property: список параметров свойства), (array: указатель на конечные данные)
function __read_array_parameter(const Lua: TLua; const userdata: PLuaUserData; var index: pointer): boolean;
var
  Value: double;
  param_number, low, high: integer;
  array_index: integer absolute index;

  // Exception неверного индекса в массиве
  procedure ThrowBounds();
  var
    array_instance: pointer;
    S: string;
  begin
    with userdata^ do
    begin
      array_instance := userdata.instance;
      if (userdata.ArrayInfo.IsDynamic) then array_instance := ppointer(array_instance)^;

      S := __arrayindex_description(ArrayInfo.Name, IntToStr(array_index), param_number, ArrayInfo.Dimention)
    end;
    
    Lua.ScriptAssert('Wrong bounds in array %s. Array pointer = %p. Available bounds = %d..%d', [S, array_instance, low, high]);
  end;

  // присвоить (индекс в стеке - 2)
  procedure FillPropValue(const PropertyInfo: PLuaPropertyInfo);
  var
    prop_struct: TLuaPropertyStruct;
  begin
    prop_struct.PropertyInfo := PropertyInfo;
    prop_struct.Instance := index;
    prop_struct.Index := nil;
    prop_struct.StackIndex := 2;
    prop_struct.ReturnAddr := nil; // работа массивами и сложными свойствами - всегда из скрипта

    Lua.__newindex_prop_set(Lua.GlobalNative, @prop_struct);
  end;  

begin
  with userdata^ do
  if (kind = ukArray) then
  begin
    // для массивов
    param_number := integer(array_params) and $F;

    with ArrayInfo^ do
    begin
      // найти границы
      if (IsDynamic) then
      begin
        if (pinteger(instance)^ = 0) then
        begin
          low := 0;
          high := 0;
          ThrowBounds();
        end else
        begin
          low := 0;
          high := DynArrayLength(instance^)-1;
        end;
      end else
      begin
        low := FBoundsData[param_number*2];
        high := FBoundsData[param_number*2+1];
      end;

      // выход за границы
      if (array_index < low) or (array_index > high) then
      ThrowBounds();

      // расчёт конечного указателя
      if (array_index = low) then
      begin
        // вариант, когда смещение не осуществляется
        if (not IsDynamic) then index := instance
        else index := ppointer(instance)^;
      end else
      if (not IsDynamic) then
      begin
        // смещение по статическому массиву
        index := pointer(integer(instance)+(array_index-low)*FMultiplies[param_number]);
      end else
      begin
        // смещение по динамическому массиву
        if (param_number = Dimention-1) then index := pointer(pinteger(instance)^+array_index*FItemSize)
        else index := pointer(pdword(instance)^+dword(array_index)*4)
      end;              
    end;

    __read_array_parameter := (byte((param_number+1)and$F) = (array_params shr 4));
  end else
  begin
    // свойства
    index := pointer(integer(userdata)+sizeof(TLuaUserData));

    with PropertyInfo^ do
    case integer(Parameters) of
      integer(INDEXED_PROPERTY): begin
                                   Value := lua_tonumber(Lua.Handle, 2);
                                   NumberToInteger(Value, pinteger(index)^);
                                   index := ppointer(index)^;
                                 end;
        integer(NAMED_PROPERTY): begin
                                   lua_to_pascalstring(pstring(index)^, Lua.Handle, 2);
                                   index := ppointer(index)^;
                                 end;
    else
      with Lua, ClassesInfo[PLuaRecordInfo(Parameters).FClassIndex] do
      begin
        //stack_luaarg(FBufferArg, 2, false);
        //Properties[array_params and $F]._Set(Lua, index, FBufferArg);
        FillPropValue(@Properties[array_params and $F]);

        if (PLuaRecordInfo(Parameters).Size <= 4) then  // не раньше ли нужно ? todo
        index := ppointer(index)^;
      end;
    end;

    inc(array_params);
    __read_array_parameter := (array_params and $F = (array_params shr 4));
  end;
end;       *)
             (*
// вызвать ошибку, связанную с некорректным индексом массива или свойства
procedure __throw_array_index(const Lua: TLua; const ClassInfo: TLuaClassInfo; const userdata: PLuaUserData; const is_getter: boolean);
const
  CHANGE_GET: array[boolean] of string = ('change', 'get');
  ITEM_PROPERTY: array[boolean] of string = ('item', 'property');
var
  IsClass: boolean;
  IsProperty: boolean;
  Index, Dimention: integer;
  Description: string;

  instance: pointer;
  Error: string;
begin
  IsClass := (userdata = nil);
  IsProperty := (IsClass) or (lua_type(Lua.Handle, 2) = LUA_TSTRING);

  if (IsClass) then Index := 0
  else Index := (userdata.array_params and $F);

  if (IsProperty) then Dimention := 0
  else Dimention := userdata.ArrayInfo.Dimention;

  Description := __arrayindex_description(ClassInfo._ClassName, Lua.StackArgument(2), Index, Dimention);


  // описание ошибки
  Error := Format('Can''t %s value of %s %s', [CHANGE_GET[is_getter], Description, ITEM_PROPERTY[IsProperty]]);

  // instance
  if (not IsClass) then
  begin
    instance := userdata.instance;
    if (userdata.ArrayInfo.IsDynamic) then instance := ppointer(instance)^;

    Error := Error + Format('. Array pointer = %p', [instance]);
  end;  

  // ошибка
  Lua.ScriptAssert(Error, []);
end;
        *)
            (*
// операции по промежуточным user-data массивам
function TLua.__array_index(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
const
  FAIL_USAGE = pchar(1);
var
  userdata, dest: PLuaUserData;
  S: pchar;
  luatype, SLength, stdindex: integer;

  Number: double; 
  index: pointer absolute Number;
  array_index: integer absolute index;

  // невозможно вызвать метод, потому что инстенст - константа
  procedure ThrowConst();
  begin
    ScriptAssert('%s() method can''t be called, because %s instance is const', [S, ClassInfo._ClassName]);
  end;

  // push значение свойства
  procedure PushPropertyValue(const PropertyInfo: PLuaPropertyInfo; const Instance: pointer; const IsConst: boolean; const Index: pointer);
  var
    prop_struct: TLuaPropertyStruct;
  begin
    prop_struct.PropertyInfo := PropertyInfo;
    prop_struct.Instance := Instance;
    prop_struct.IsConst := IsConst;
    prop_struct.Index := Index;
    prop_struct.ReturnAddr := nil; // работа массивами и сложными свойствами - всегда из скрипта
    
    Self.__index_prop_push(ClassInfo, @prop_struct);
  end;


begin
  Result := 1;

  // прочитать userdata и информацию по первому аргументу
  if (not __read_lua_arguments(Handle, userdata, S, luatype, SLength, stdindex)) then
  begin
    lua_pushnil(Handle);
    exit;
  end;


  { -- обработка сложного свойства -- }
  if (is_property) then
  begin
    // прочитать следующий параметр
    // если произошло заполнение параметров, то вернуть значение по свойству
    if (__read_array_parameter(Self, userdata, index)) then
    begin
      PushPropertyValue(userdata.PropertyInfo, userdata.instance, false, index);
    end else
    begin
      // вернуть Userdata в качестве результата
      lua_insert(Handle, 1); // swap. param -> userdata
      lua_pushnil(Handle);
      lua_insert(Handle, 1); // nil, param, userdata (result)
    end;

    // выход
    exit;
  end;

  { -- дальше идёт блок только для массивов -- }


  // проследить ошибочное использование массива
  // допускаются либо свойства (которые тоже кстати могут не приводить к результату)
  // либо индексы (для "классов" и удалённых массивов - тоже ошибка)
  if (stdindex < 0) then
  begin
    if (userdata = nil) then S := FAIL_USAGE
    else
    if (luatype = LUA_TNUMBER) then
    begin
      if (not NumberToInteger(Number, Handle, 2)) then S := FAIL_USAGE;
    end else
    S := FAIL_USAGE;
  end;

  // стандартное свойство или число строкой
  if (stdindex > 0) then
  begin
    // для стандартных имён
    case (stdindex) of
         // стандартные
           STD_TYPE: begin
                       if (userdata = nil) or (userdata.array_params and $F = 0) then
                         global_push_value(ClassInfo.Ref)
                       else
                         lua_pushnil(Handle);
                         
                       exit;
                     end;
      STD_TYPE_NAME: begin
                       if (userdata = nil) or (userdata.array_params and $F = 0) then
                         lua_push_pascalstring(Handle, ClassInfo._ClassName)
                       else
                         lua_push_pascalstring(Handle, 'UNKNOWN_ARRAY');

                       exit;
                     end;
    STD_TYPE_PARENT: begin
                       lua_pushnil(Handle);
                       exit;
                     end;
  STD_INHERITS_FROM: begin
                       lua_pushcclosure(Handle, lua_CFunction(cfunction_inherits_from), 0);
                       exit;
                     end;                     
         STD_ASSIGN: begin
                       if (userdata <> nil) and (userdata.array_params and $F = 0) then
                       begin
                         if (userdata.is_const) then ThrowConst();
                         lua_pushcclosure(Handle, lua_CFunction(cfunction_assign), 0);
                         exit;
                       end;
                     end;
         STD_IS_REF: begin
                       lua_pushboolean(Handle, (userdata <> nil) and (not userdata.gc_destroy));
                       exit;
                     end;
       STD_IS_CONST: begin
                       lua_pushboolean(Handle, (userdata <> nil) and (userdata.is_const));
                       exit;
                     end;                                       
       STD_IS_CLASS: begin
                       lua_pushboolean(Handle, false);
                       exit;
                     end;
      STD_IS_RECORD: begin
                       lua_pushboolean(Handle, false);
                       exit;
                     end;
       STD_IS_ARRAY: begin
                       lua_pushboolean(Handle, true);
                       exit;
                     end;
         STD_IS_SET: begin
                       lua_pushboolean(Handle, false);
                       exit;
                     end;
       STD_IS_EMPTY: begin
                       if (userdata = nil) then lua_pushboolean(Handle, false)
                       else
                       if (userdata.ArrayInfo.IsDynamic) then lua_pushboolean(Handle, integer(userdata.instance^) = 0)
                       else
                       lua_pushboolean(Handle, IsMemoryZeroed(userdata.instance, PLuaArrayInfo(ClassInfo._Class).FSize));

                       exit;
                     end;   
        // по массивам
            STD_LOW: begin
                       with PLuaArrayInfo(ClassInfo._Class)^ do
                       if (IsDynamic) then lua_pushinteger(Handle, 0)
                       else
                       if (userdata = nil) then lua_pushinteger(Handle, FBoundsData[0])
                       else
                       lua_pushinteger(Handle, FBoundsData[(userdata.array_params and $F) * 2]);

                       exit;
                     end;
           STD_HIGH: begin
                       with PLuaArrayInfo(ClassInfo._Class)^ do
                       if (IsDynamic) then
                       begin
                         if (userdata = nil) then lua_pushinteger(Handle, -1)
                         else lua_pushinteger(Handle, DynArrayLength(userdata.instance^)-1);
                       end else
                       begin
                         if (userdata = nil) then lua_pushinteger(Handle, FBoundsData[1])
                         else
                         lua_pushinteger(Handle, FBoundsData[(userdata.array_params and $F) * 2 + 1]);
                       end;

                       exit;
                     end;
         STD_LENGTH: begin
                       with PLuaArrayInfo(ClassInfo._Class)^ do
                       if (IsDynamic) then
                       begin
                         if (userdata = nil) then lua_pushinteger(Handle, 0)
                         else lua_pushinteger(Handle, DynArrayLength(userdata.instance^));
                       end else
                       begin
                         if (userdata = nil) then array_index := 0
                         else array_index := (userdata.array_params and $F) * 2;

                         lua_pushinteger(Handle, FBoundsData[array_index+1]-FBoundsData[array_index]+1);
                       end;

                       exit;
                     end;
         STD_RESIZE: if (userdata <> nil)and (PLuaArrayInfo(ClassInfo._Class)^.IsDynamic) then
                     begin
                       if (userdata.is_const) then ThrowConst();
                       lua_pushcclosure(Handle, lua_CFunction(cfunction_dynarray_resize), 0);
                       exit;
                     end;
        STD_INCLUDE: if (userdata <> nil) and (userdata.array_params and $F + 1 = userdata.array_params shr 4) then
                     begin
                       if (userdata.is_const) then ThrowConst();
                       lua_pushcclosure(Handle, lua_CFunction(cfunction_array_include), 0);
                       exit;
                     end;        
    end;
  end;

  
  // если некорректное использование массива - то вызвать ошибку
  if (S <> nil) then
  begin
    __throw_array_index(Self, ClassInfo, userdata, true);
  end;
                                

  // необходимо взять и запушить значение или промежуточное значение
  // необходимый индекс = array_index
  // очень важно! на входе index - это array index, а на выходе это непосредственный указатель !

  if (__read_array_parameter(Self, userdata, index)) then
  begin
    PushPropertyValue(@TLuaPropertyInfo(userdata.ArrayInfo.ItemInfo), index, userdata.is_const, nil);
  end else
  begin
    // вернуть Userdata в качестве результата
    dest := PLuaUserData(lua_newuserdata(Handle, sizeof(TLuaUserData)));
    dest^ := userdata^;

    with dest^ do
    begin
      instance := index;
      inc(array_params);
      gc_destroy := false;
    end;

    // метатаблица
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassInfo.Ref); // global_push_value(Ref);
    lua_setmetatable(Handle, -2);
  end;
  
end;     *)
           (*
// изменить userdata-массив
function TLua.__array_newindex(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
const
  FAIL_USAGE = pchar(1);
var
  userdata: PLuaUserData;
  S: pchar;
  luatype, SLength, stdindex: integer;

  Number: double;
  index: pointer absolute Number;
  array_index: integer absolute index;

  // недостаточно параметров чтобы что-то изменить
  procedure ThrowBounds();
  var
    params: integer;
    Kind, Prefix, Value: string;
  begin
    if (is_property) then
    begin
      Kind := 'property';
      Prefix := userdata.PropertyInfo.PropertyName;
    end else
    begin
      Kind := 'array';
      Prefix := userdata.ArrayInfo.Name;
    end;
    Value := StackArgument(2);
    params := userdata.array_params;

    ScriptAssert('Can not change %s %s: not anought parameters',
                [Kind, __arrayindex_description(Prefix, Value, (params-ord(is_property))and $F, params shr 4)]);
  end;


  // вызывается если производится попытка изменения константного массива
  procedure ThrowConstant();
  var
    params: integer;
    Err: string;
  begin
    params := userdata.array_params;
    Err := __arrayindex_description(ClassInfo._ClassName, StackArgument(2), params and $F, params shr 4);

    if (stdindex = STD_LENGTH) then Err := 'Can not change Length of constant array '+Err
    else Err := 'Can not change constant array %s'+Err;

    ScriptAssert(Err, []);
  end;


  // присваивается новое значение длинны динамического массива
  // и значение некорректно - (-1) или вообще не integer
  procedure ThrowLengthValue();
  var
    Err: string;
  begin
    Err := __arrayindex_description(ClassInfo._ClassName, 'Length', userdata.array_params and $F, 0);

    ScriptAssert('Can''t change %s property, because "%s" is not correct length value', [Err, StackArgument(3)]);
  end;


  // взять значение из третьего аргумента и присвоить
  procedure FillPropValue(const PropertyInfo: PLuaPropertyInfo; const Instance: pointer);
  var
    prop_struct: TLuaPropertyStruct;
  begin
    prop_struct.PropertyInfo := PropertyInfo;
    prop_struct.Instance := Instance;
    prop_struct.StackIndex := 3;
    prop_struct.Index := nil;
    prop_struct.ReturnAddr := nil; // работа массивами и сложными свойствами - всегда из скрипта

    Self.__newindex_prop_set(ClassInfo, @prop_struct);
  end;


begin
  Result := 0;

  // прочитать userdata и информацию по первому аргументу
  if (not __read_lua_arguments(Handle, userdata, S, luatype, SLength, stdindex)) then
  begin
    exit;
  end;

  // прочитать параметр 3
  // если произошло заполнение, то присвоить, иначе ошибка
  if (is_property) then
  begin
    if (__read_array_parameter(Self, userdata, index)) then
    begin
      //stack_luaarg(FBufferArg, 3, false);
      //userdata.PropertyInfo._Set(Self, userdata.instance, FBufferArg, index);
      FillPropValue(userdata.PropertyInfo, userdata.instance);
    end else
    begin
      ThrowBounds();
    end;

    exit;
  end;

  { -- дальше идёт блок только для массивов -- }

  // проследить ошибочное использование массива
  // допускаются либо свойства (которые тоже кстати могут не приводить к результату)
  // либо индексы (для "классов" и удалённых массивов - тоже ошибка)
  if (stdindex < 0) then
  begin
    if (userdata = nil) then S := FAIL_USAGE
    else
    if (luatype = LUA_TNUMBER) then
    begin
      if (not NumberToInteger(Number, Handle, 2)) then S := FAIL_USAGE;
    end else
    S := FAIL_USAGE;
  end;

  // изменять можно только длинну и у динамических массивов
  if (stdindex = STD_LENGTH) and (userdata <> nil) and (PLuaArrayInfo(ClassInfo._Class).IsDynamic) then
  begin
    // если динамический массив константный, то у него нельзя менять размер
    if (userdata.is_const) then ThrowConstant();

    // проверка значения Length
    if (lua_type(Handle, 3) <> LUA_TNUMBER) or (not NumberToInteger(Number, Handle, 3))
    or (array_index < 0) then ThrowLengthValue();

    // изменить Length динамического массива
    with userdata^ do
    DynArraySetLength(pointer(instance^), ptypeinfo(ArrayInfo.FMultiplies[array_params and $F]), 1, @array_index);

    exit;
  end;

  // если некорректное использование массива - то вызвать ошибку
  if (S <> nil) then
  begin
    __throw_array_index(Self, ClassInfo, userdata, false);
  end;

  // неизменяемый массив
  if (UserData.is_const) then ThrowConstant();
  

  // присвоение или ошибка - нарушение Bounds
  if (__read_array_parameter(Self, userdata, index)) then
  begin
    //stack_luaarg(FBufferArg, 3, false);
    //TLuaPropertyInfo(userdata.ArrayInfo.ItemInfo)._Set(Self, index, FBufferArg);
    FillPropValue(@TLuaPropertyInfo(userdata.ArrayInfo.ItemInfo), index);
  end else
  begin
    ThrowBounds();
  end;
end;


// получить базовые понятия о стеке (количество аргументов, смещение)
// и userdata - объект над которым производятся действия
// в основном это нужно для функций инициализаторов/конструкторов
// если возвращается false, то userdata не найден (только в случае прямого вызова, не конструктор)
function __inspect_proc_stack(const is_construct: boolean; const Handle: pointer;
           var userdata: PLuaUserData; var ArgsCount, Offset: integer): boolean;
begin
  Result := false;

  if (is_construct) then
  begin
    Offset := ord(lua_type(Handle, 1) = LUA_TTABLE);
    userdata := PLuaUserData(lua_touserdata(Handle, -1));
    ArgsCount := lua_gettop(Handle)-{userdata}1-Offset;
  end else
  begin
    userdata := PLuaUserData(lua_touserdata(Handle,  1));
    Offset := ord(lua_type(Handle, 1) = LUA_TUSERDATA);
    if (Offset = 0) or (userdata = nil) or (userdata.instance = nil) then exit;
    ArgsCount := lua_gettop(Handle)-{userdata}1;
  end;

  if (ArgsCount < 0) then ArgsCount := 0;
  Result := true;  
end;                              


// изменить размер динамического массива (userdata - первый параметр)
// 2 режима: мягкий (is_construct) и жёсткий (.Resize(...))
function TLua.__array_dynamic_resize(): integer;
var
  userdata: PLuaUserData;
  tpinfo: ptypeinfo;
  ArgsCount, Offset, Dimention, i: integer;
  Arguments: TIntegerDynArray;


  // если i-й аргумент не подходит для того чтобы
  // использовать в изменения размерности
  procedure ThrowArgument(const N: integer);
  begin
    ScriptAssert('Wrong argument №%d of Resize() method = "%s"', [N, FBufferArg.ForceString]);
  end;
begin
  Result := 0;

  if (not __inspect_proc_stack(false, Handle, userdata, ArgsCount, Offset)) then
  ScriptAssert('The first operand of Resize() method should be a dynamic array', []);

  // подсчитать реальную размерность динамического массива
  tpinfo := userdata.ArrayInfo.FTypeInfo;
  Dimention := 1;
  while (true) do
  begin
    tpinfo := pointer(GetTypeData(tpinfo).elType);
    if (tpinfo <> nil) then tpinfo := ppointer(tpinfo)^;
    if (tpinfo = nil) or (tpinfo.Kind <> tkDynArray) then break;
    inc(Dimention);
  end;

  // проверка ArgsCount
  Dimention := Dimention - userdata.array_params and $F;
  if (ArgsCount = 0) then ScriptAssert('Resize() method has no arguments', []);
  if (ArgsCount > Dimention) then ScriptAssert('Resize(%d arguments) method can''t be called, because maximum array dimention is %d', [ArgsCount, Dimention]);

  // заполнение массива
  SetLength(Arguments, ArgsCount);
  ZeroMemory(pointer(Arguments), ArgsCount*sizeof(integer));
  for i := 1 to ArgsCount do
  begin
    stack_luaarg(FBufferArg, i+Offset, true); 

    with FBufferArg do
    if (LuaType = ltInteger) then
    begin
      if (Data[0] < 0) then ThrowArgument(i);
      if (Data[0] = 0) then break;
      Arguments[i-1] := Data[0];
    end else
    begin
      ThrowArgument(i);
    end;
  end;


  // изменение размера массива
  with userdata^ do
  DynArraySetLength(ppointer(instance)^, ptypeinfo(ArrayInfo.FMultiplies[array_params and $F]), ArgsCount, pointer(Arguments));
end;     *)
           (*

// функция имеет 3 назначения.
// И всегда работает на заполнение одномерной (конечной) части массива
// в статических массивах происходит наполнение "с нуля"
// в динамических - добавляется в конец
// операнды - (конечные) элементы массива или (конечные) массивы, чьи элементы совпадают
function TLua.__array_include(const mode: integer{constructor, include, concat}): integer;
const
  DESCRIPTION: array[0..2] of string = (LUA_CONSTRUCTOR, 'Include() method', 'operator ".."');

var
  userdata: PLuaUserData;
  Instance: pointer;
  PropertyInfo: PLuaPropertyInfo;
  DynamicInfo: ptypeinfo;
  ArgsCount, Offset, i: integer;

  UseOneArgument: boolean;
  Len{текущее наполнение}, CurLen{необходимое наполнение}, MaxLen{максимальная длинна (для статических)}: integer;  
  FLow: integer; // Low (для коррекции индексов в татических массивах)
  item_class_index: integer; // очень быстрая проверка если добвляется сложный элемент (userdata)

  ItemKind: TLuaPropertyKind;
  ItemInfomation: pointer; // typeinfo или TLuaDifficultType.Info - сверки совместимости массивов
  ItemTypeInfo: ptypeinfo; // nil (для Move) или другое значение (для CopyArray)
  ItemsCount: integer; // множитель для копирования/финализации
  ItemSize: integer; // размер элемента (для копирования)

  CopyCount: integer;
  DestInstance, SrcInstance: pointer;

  // расшифровка метода
  function MethodName: pchar;
  begin
    FBufferArg.str_data := DESCRIPTION[mode];
    if (userdata <> nil) then FBufferArg.str_data := userdata.ArrayInfo.Name + '.' + FBufferArg.str_data;
    Result := pchar(FBufferArg.str_data);
  end;

  // если i-й аргумент не подходит для того чтобы добавить его или его элементы
  procedure ThrowArgument(const N: integer);
  const
    NULL_S: array[boolean] of string = ('', 's');
  var
    Count: integer;
    Description, Bounds: string;
  begin
    stack_luaarg(FBufferArg, N+Offset, true);
    Description := FBufferArg.ForceString;

    if (MaxLen <> 0) and (CurLen > MaxLen) then
    begin
      Count := (CurLen-Len);
      Bounds := Format('. Can''t overflow an array by %d item%s, because max index is %d', [Count, NULL_S[Count>1], (MaxLen-1)+FLow]);
    end;

    ScriptAssert('Wrong argument №%d of %s = "%s"%s', [N, MethodName, Description, Bounds]);
  end;

  // взять значение из стека и занести в Instance
  procedure FillPropValue(const StackIndex: integer; const Instance: pointer);
  var
    prop_struct: TLuaPropertyStruct;
  begin
    prop_struct.PropertyInfo := PropertyInfo;
    prop_struct.Instance := Instance;
    prop_struct.StackIndex := StackIndex;
    prop_struct.Index := nil;
    prop_struct.ReturnAddr := nil; // работа массивами и сложными свойствами - всегда из скрипта

    Self.__newindex_prop_set(Self.ClassesInfo[userdata.ArrayInfo^.FClassIndex], @prop_struct);
  end;

begin
  Result := 0;

  if (not __inspect_proc_stack({is_construct}mode<>1, Handle, userdata, ArgsCount, Offset)) then
  ScriptAssert('The first operand of %s should be an array', [MethodName]);

  if (ArgsCount = 0) then
  begin
    if (mode = 1{Include()}) then ScriptAssert('%s has no arguments', [MethodName]);
    exit;
  end;

  // сбор информации
  Instance := userdata.instance;
  Len := userdata.array_params and $F;
  CurLen := 0;
  with userdata.ArrayInfo^ do
  begin
    if (mode = 0) and (Dimention > 1) then ScriptAssert('%s should have no arguments, because array dimention = %d', [MethodName, Dimention]);

    PropertyInfo := @TLuaPropertyInfo(ItemInfo);
    ItemKind := PropertyInfo.Base.Kind;
    ItemInfomation := PropertyInfo.Base.Information;
    ItemTypeInfo := GetLuaDifficultTypeInfo(PropertyInfo.Base);
    ItemSize := FItemSize;
    {дополнительный множитель} if (ItemKind = pkArray) then ItemsCount := PLuaArrayInfo(ItemInfomation).FItemsCount else ItemsCount := 1;

    // для быстрого определения в userdata-случаях
    case (ItemKind) of
      pkObject: item_class_index := 0; 
      pkRecord: item_class_index := PLuaRecordInfo(ItemInfomation).FClassIndex;
       pkArray: item_class_index := integer(ItemInfomation);
         pkSet: item_class_index := integer(ItemInfomation);
    else
      item_class_index := integer($FFFFFFFF);
    end;

    // расчитать текущее наполнение и максимальную длинну
    if (IsDynamic) then
    begin
      FLow := 0;
      MaxLen := 0;
      DynamicInfo := ptypeinfo(FMultiplies[Len]);
      if (pinteger(Instance)^ = 0) then Len := 0 else Len := DynArrayLength(ppointer(Instance)^);
    end else
    begin
      inc(Len, Len);
      FLow := FBoundsData[Len];
      MaxLen := FBoundsData[Len+1]-FLow+1;
      DynamicInfo := nil;
      Len := 0;
    end;
  end;


  // чтение параметров
  SrcInstance := nil;
  CopyCount := 0;
  for i := 1 to ArgsCount do
  begin
    UseOneArgument := (lua_type(Handle, i+Offset) <> LUA_TUSERDATA);

    if (not UseOneArgument) then
    begin
      // ситуация, когда аргумент - userdata
      // с другой стороны userdata может быть элементом такого массива
      // поэтому проводятся проверки. если элемент подходит, то выставляется флаг UseOneArgumnt
      // если userdata конечный (одномерный массив) и он подходит - то будет копирование массива
      // в противном случае ThrowArgument(i).

      userdata := lua_touserdata(Handle, i+Offset);
      if (userdata = nil) or (userdata.instance = nil) then continue;


      with userdata^ do
      if (ClassIndex = item_class_index) then UseOneArgument := true
      else
      if (kind = ukArray) and (array_params and $F + 1 = array_params shr 4) then
      begin
        // userdata.instance - конечный(одноэлементный) массив

        with TLuaPropertyInfo(ArrayInfo.ItemInfo) do     
        if (ItemKind = Base.Kind) and (ItemInfomation = Base.Information) then
        begin
          // всё подходит
          // необходимо определиться с указателем на данные и количеством копируемых элементов

          if (ArrayInfo.IsDynamic) then
          begin
            SrcInstance := ppointer(userdata.instance)^;
            if (SrcInstance = nil) then continue; // empty dyn array
            CopyCount := DynArrayLength(userdata.instance^);
          end else
          begin
            SrcInstance := userdata.instance;
            CopyCount := ((array_params and $F) shl 1);

            with ArrayInfo^ do
            CopyCount := FBoundsData[CopyCount+1]-FBoundsData[CopyCount]+1;
          end; 
        end else
        ThrowArgument(i);
        
      end else
      if (kind = ukInstance) and (item_class_index = 0{TObject}) and (ClassesInfo[ClassIndex]._ClassKind = ckClass) then
      begin
        UseOneArgument := true;
      end else
      ThrowArgument(i);
    end;


    // Копирование. 1го элемента или (подходящего) массива
    if (UseOneArgument) then
    begin
      // добавить один аргумент - тот который в стеке
      //stack_luaarg(FBufferArg, i+Offset, false);

      if (MaxLen = 0) then
      begin
        // динамический
        inc(Len);
        DynArraySetLength(pointer(Instance^), DynamicInfo, 1, @Len);
        //PropertyInfo._Set(Self, pointer(pinteger(Instance)^ + (Len-1)*ItemSize), FBufferArg);
        FillPropValue(i+Offset, pointer(pinteger(Instance)^ + (Len-1)*ItemSize));
      end else
      begin
        // статический
        CurLen := Len+1;
        if (CurLen > MaxLen) then ThrowArgument(i);
        //PropertyInfo._Set(Self, pointer(integer(Instance) + Len*ItemSize), FBufferArg);
        FillPropValue(i+Offset, pointer(integer(Instance) + Len*ItemSize));
        Len := CurLen;
      end;
    end else
    begin
      // добавить массив элементов
      CurLen := Len+CopyCount;

      if (MaxLen = 0) then
      begin
        // динамический
        DynArraySetLength(pointer(Instance^), DynamicInfo, 1, @CurLen);
        DestInstance := pointer(pinteger(Instance)^ + Len*ItemSize);
      end else
      begin
        // статический
        if (CurLen > MaxLen) then ThrowArgument(i);
        DestInstance := pointer(integer(Instance) + Len*ItemSize);
      end;

      // копировать память или делать умное копирование (через RTTI)
      if (ItemTypeInfo = nil) then
      begin
        System.Move(SrcInstance^, DestInstance^, CopyCount*ItemSize);
      end else
      begin
        CopyArray(DestInstance, SrcInstance, ItemTypeInfo, CopyCount*ItemsCount);
      end;

      Len := CurLen;
    end;
  end;      
end;
         *)
           (*
// враппер для определения входит ли операнд в множество Dest
function  _SetLe_Wrap(const Dest, X1, X2: pointer; const Size: integer): boolean;
begin _SetLe_Wrap := _SetLe(X2, Dest, Size); end;
                 *)
                   (*
// включить или исключить числа/множества из множества (userdata) в стеке
// метод так же вызывается в конструкторе множеств
function TLua.__set_method(const is_construct: boolean; const method: integer{0..2}): integer;
const
  METHOD_NAME: array[0..2] of string = ('Include() method', 'Exclude() method', 'Contains() method');
var
  Ret: byte;
  userdata: PLuaUserData;
  ArgsCount, Offset, i: integer;

  FDest: pointer;
  FSetInfo: PLuaSetInfo;
  FSize, FLow, FHigh, FCorrection: integer;
  FBitProc: function(const _Set: pointer; const Bit: integer): byte;
  FSetProc: function(const Dest, X1, X2: pointer; const Size: integer): byte;

  // расшифровка метода
  function MethodName: pchar;
  begin
    with FBufferArg do
    begin
      if (is_construct) then str_data := LUA_CONSTRUCTOR else str_data := METHOD_NAME[method];
      if (userdata <> nil) then str_data := userdata.ArrayInfo.Name + '.' + str_data;
      Result := pchar(str_data);
    end;
  end;

  // если i-й аргумент не подходит
  procedure ThrowArgument(const N: integer);
  begin
    ScriptAssert('Wrong argument №%d of %s = "%s"', [N, MethodName, FBufferArg.ForceString]);
  end;
begin
  Result := ord(method=2); // если метод "Contains", то надо возвращать boolean

  if (not __inspect_proc_stack(is_construct, Handle, userdata, ArgsCount, Offset)) then
  ScriptAssert('The first operand of %s() method should be a set', [MethodName]);

  // если нет аргументов
  if (ArgsCount = 0) then
  begin
    if (not is_construct) then ScriptAssert('%s() method has no arguments', [MethodName]);
    exit;
  end;  

  // параметры
  FDest := userdata.instance;
  FSetInfo := userdata.SetInfo;
  FSize := FSetInfo.FSize;
  FLow := FSetInfo.FLow;
  FHigh := FSetInfo.FHigh;
  FCorrection := FSetInfo.FCorrection;

  // метод
  case method of
    0: begin
         FBitProc := pointer(@IncludeSetBit);
         FSetProc := pointer(@SetsUnion);
       end;

    1: begin
         FBitProc := pointer(@ExcludeSetBit);
         FSetProc := pointer(@SetsDifference);
       end;
  else
    //2:
    FBitProc := pointer(@SetBitContains);
    FSetProc := pointer(@_SetLe_Wrap);
  end;



  // заполнение
  Ret := 0;
  for i := 1 to ArgsCount do
  begin
    stack_luaarg(FBufferArg, i+Offset, true); 

    with FBufferArg do
    if (LuaType = ltInteger) then
    begin
      if (Data[0] < FLow) or (Data[0] > FHigh) then ThrowArgument(i);
      Ret := FBitProc(FDest, Data[0]-FCorrection);
    end else
    if (LuaType = ltSet) then
    begin
      if (pointer(Data[0]) = nil) or (pointer(Data[1]) <> FSetInfo) then ThrowArgument(i);
      Ret := FSetProc(FDest, FDest, pointer(Data[0]), FSize);
    end else
    begin
      ThrowArgument(i);
    end;

    // результат ("Contains")
    if (method = 2) and (Ret = 0) then break;
  end;


  // результат для "Contains"
  if (method = 2) then
  begin
    lua_pushboolean(Handle, boolean(Ret));
  end;
end;      *)

function TLua.__print: Integer;
var
  Count, i: Integer;
{$ifNdef UNICODE}
  Output: THandle;
  Written: Cardinal;
const
  CRLF_CHAR: WideChar = #10;
{$endif}
begin
  Count := lua_gettop(Handle);

  if (Count = 0) then
  begin
    Writeln;
  end else
  for i := 1 to Count do
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, i, False);

    if (i <> 1) then Write(#9);

    {$ifdef UNICODE}
      if (i = Count) then
      begin
        Writeln(FStringBuffer.Unicode);
      end else
      begin
        Write(FStringBuffer.Unicode);
      end;
    {$else .MSWINDOWS}
      Output := GetStdHandle(STD_OUTPUT_HANDLE);
      WriteConsoleW(Output, Pointer(FStringBuffer.Unicode), Length(FStringBuffer.Unicode), Written, nil);
      if (i = Count) then WriteConsoleW(Output, @CRLF_CHAR, 1, Written, nil);
    {$endif}
  end;

  Result := 0;
end;

function TLua.__printf: Integer;
const
  SLN_CONST: Cardinal = Ord('S') + (Ord('l') shl 8) + (Ord('n') shl 16);
var
  Count, ErrCode, P: Integer;
  Err: ^string;
begin
  Count := lua_gettop(Handle);

  global_push_value(FFormatWrapper);
  lua_insert(Handle, 1);
  ErrCode := lua_pcall(Self.Handle, Count, LUA_MULTRET, 0);
  if (ErrCode = 0) then ErrCode := lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0);
  if (ErrCode <> 0) then
  begin
    Err := @FStringBuffer.Default;
    {$ifdef UNICODE}
      stack_unicode_string(Err^, -1);
    {$else}
      stack_ansi_string(Err^, -1, 0);
    {$endif}
    stack_pop;
    P := Pos(']', Err^);
    Delete(Err^, 1, P);

    repeat
      P := Pos('format', Err^);
      if (P = 0) then Break;

      Err^[P + 0] := 'p';
      Err^[P + 1] := 'r';
      Err^[P + 2] := 'i';
      Err^[P + 3] := 'n';
      Err^[P + 4] := 't';
      Err^[P + 5] := 'f';
    until (False);

    Self.Error(Err^);
  end;

  Result := __print;
end;

function TLua.ProcCallback(const AClassInfo: __luapointer; const AProcInfo: __luapointer): Integer;
begin
  Result := 0;
end;

function TLua.ClosureCallback(const AClosure: Pointer{PLuaClosure}; const ArgsCount: Integer): Integer;
begin
  Result := 0;
end;

function TLua.InvokableCallback(const AClosure: Pointer{PLuaClosure}; const AInvokableData: Pointer): Integer;
label
  fill_instance, invalid_argument;
var
  i: Integer;
  Closure: PLuaClosure;
  Invokable: PLuaInvokable;
  InvokableData: NativeInt;
  Param: PLuaInvokableParam;
  LuaType: Integer;
  Ptr: Pointer;
begin
  Closure := PLuaClosure(AClosure);
  Invokable := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Closure.Invokable);
  InvokableData := NativeInt(AInvokableData);

  case Closure.Header.Kind of
    ckStatic: ;
    ckConstructor:
    begin
      PBoolean(InvokableData + Invokable.ConstructorFlag)^ := (Closure.Header.InstanceKind <> ikClass);
      goto fill_instance;
    end;
  else
  fill_instance:
    PPointer(InvokableData + Invokable.Instance)^ := Closure.Instance;
  end;

  Param := @Invokable.Params[0];
  for i := 1 to Invokable.ParamCount do
  begin
    LuaType := lua_type(Handle, i);
       {.$message 'здесь'}
    {if () then
    begin

    end else }
    begin
      Ptr := Pointer(InvokableData + Param.DataValue);
      if (not Param.SetValue(Ptr^, Self, i, LuaType)) then
      begin
      invalid_argument:
        unpack_lua_string(FStringBuffer.Lua, Closure.Header.Name);
        unpack_lua_string(FStringBuffer.LuaReserved, Param.Name^);
        Self.Error('Invalid %s.%s argument value: "%s" (%s)',
          [FStringBuffer.Lua, FStringBuffer.LuaReserved, FStringBuffer.Unicode, FStringBuffer.Default]);
      end;
    end;

    Inc(Param);
  end;

  // function/procedure invoke
  if (Invokable.ResultKind <> rkNone) then
  begin
    // ToDo optional prepare result user data
    if (Invokable.ResultKind = rkUserData) then
    begin
      case Invokable.Result.F.Kind of
        pkInterface:
        begin

        end;
        pkRecord:
        begin

        end;
        pkArray:
        begin

        end;
        pkSet:
        begin

        end;
      end;
    end;

    // invoke
    Invokable.Invoke(Closure.Address, Pointer(InvokableData));

    // optional get value
    if (Invokable.ResultKind <> rkUserData) then
      Invokable.Result.GetValue(Pointer(InvokableData + Invokable.Result.DataValue)^, Self, 0);
  end else
  begin
    // procedure
    Invokable.Invoke(Closure.Address, Pointer(InvokableData));
  end;

  // done
  Result := Byte(Invokable.ResultKind <> rkNone);
end;

            (*

function TLua.ProcCallback(const ClassInfo: TLuaClassInfo; const ProcInfo: TLuaClosureInfo): integer;
var
  userdata: PLuaUserData;
  luatype, i, class_index: integer;
  arg_offset: integer;
  proc_address: pointer;
  proc_class: TClass;
begin
  FBufferArg.Empty := true;
  FArgsCount := lua_gettop(Handle);

  // получить Object
  userdata := nil;
  class_index := -1;
  arg_offset := 1;
  if (ClassInfo._Class <> GLOBAL_NAME_SPACE) then
  begin
    // pointer(userdata^) или в случае класса - nil
    luatype := lua_type(Handle, 1);
    case lua_type(Handle, 1) of
      LUA_TTABLE: begin
                    userdata := nil;
                    class_index := LuaTableToClass(Handle, 1);

                    if (class_index < 0) then ELua.Assert('Operand of "%s" method is not a class', [ProcInfo.ProcName]);
                  end;
   LUA_TUSERDATA: begin
                    userdata := lua_touserdata(Handle, 1);
                    if (userdata = nil) then
                    begin
                      Result := 0; // но по идее такого не должно быть
                      exit;
                    end;
                  end;
    else
      ScriptAssert('Can''t call proc "%s.%s" because the instance is unknown (%s)', [ClassInfo._ClassName, ProcInfo.ProcName, LuaTypeName(luatype)]);
    end;

    // классовость функции
    if (userdata = nil) and (not ProcInfo.with_class) then
    ScriptAssert('%s.%s is not class method', [ClassInfo._ClassName, ProcInfo.ProcName]);

    // если инстенс уже удалён
    if (userdata <> nil) and (userdata.instance = nil) then
    ScriptAssert('%s instance is already destroyed. Proc "%s"', [ClassInfo._ClassName, ProcInfo.ProcName]);

    // сместить индекс стека, параметров функции меньше
    inc(arg_offset); 
    dec(FArgsCount);
  end;

  // заполнить массив аргументов
  SetLength(FArgs, ArgsCount);
  for i := 0 to ArgsCount-1 do stack_luaarg(FArgs[i], i+arg_offset, true);

  // проверка на количество аргументов
  if (ProcInfo.ArgsCount >= 0) and (FArgsCount <> ProcInfo.ArgsCount) then
  begin
    if (ClassInfo._Class = GLOBAL_NAME_SPACE) or (ClassInfo._ClassKind <> ckClass) then
      CheckArgsCount(ProcInfo.ArgsCount, ProcInfo.ProcName)
    else
      CheckArgsCount(ProcInfo.ArgsCount, ProcInfo.ProcName, TClass(ClassInfo._Class));
  end;

  // вызвать
  proc_address := ProcInfo.Address;
  begin
    if (ClassInfo._Class = GLOBAL_NAME_SPACE) then
    begin
      TLuaClosure2(ProcInfo.Address)(FArgs, FBufferArg) // глобальная
    end else
    begin
      if (userdata = nil) then proc_class := TClass(ClassesInfo[class_index]._Class) else proc_class := TClass(userdata.instance^);
      if (dword(proc_address) >= $FE000000) then proc_address := ppointer(dword(proc_class) + dword(proc_address) and $00FFFFFF)^;

      if (userdata = nil) then
        TLuaClassProc23(proc_address)(proc_class, FArgs, FBufferArg) // классовая
      else
      if (ProcInfo.with_class) then
        TLuaClassProc23(proc_address)(proc_class, FArgs, FBufferArg) // классовая, но через объект
      else
        TLuaClassProc9(proc_address)(TObject(userdata.instance), FArgs, FBufferArg) // обычная с объектом класса
    end;
  end;

  // результат
  FArgsCount := 0; // FArgs не обнуляется, чтобы избежать дополнительных реаллоков и финализаторов
  if (not push_luaarg(FBufferArg)) then ELua.Assert('Can''t return value type "%s"', [FBufferArg.str_data], proc_address);
  Result := 1; // т.е. результат всегда есть, даже если nil
end;    *)

                                          (*
// проверить количество аргуметов в стеке на
// соответствие ожидаемому количеству аргументов
function __TLuaCheckArgsCount__1(const Self: TLua; const ArgsCount: TIntegerDynArray;
         const ProcName: string; const AClass: TClass; const ReturnAddr: pointer): integer;
begin
  if (Length(ArgsCount) = 0) then
  ELua.Assert('ArgsCount set is not difined', [], ReturnAddr);

  Result := Self.InternalCheckArgsCount(pinteger(ArgsCount), Length(ArgsCount), ProcName, AClass);
end;

function TLua.CheckArgsCount(const ArgsCount: TIntegerDynArray; const ProcName: string=''; const AClass: TClass=nil): integer;
asm
  pop ebp
  push [esp]
  jmp __TLuaCheckArgsCount__1
end;            *)
                     (*
function __TLuaCheckArgsCount_2(const Self: TLua; const ArgsCount: array of integer;
         const ProcName: string; const AClass: TClass; const ReturnAddr: pointer): integer;
begin
  if (Length(ArgsCount) = 0) then
  ELua.Assert('ArgsCount set is not difined', [], ReturnAddr);

  Result := Self.InternalCheckArgsCount(@ArgsCount[0], Length(ArgsCount), ProcName, AClass);
end;

function TLua.CheckArgsCount(const ArgsCount: array of integer; const ProcName: string=''; const AClass: TClass=nil): integer;
asm
  pop ebp
  push [esp]
  jmp __TLuaCheckArgsCount_2
end;              *)
                    (*
procedure TLua.CheckArgsCount(const ArgsCount: integer; const ProcName: string=''; const AClass: TClass=nil);
begin
  InternalCheckArgsCount(@ArgsCount, 1, ProcName, AClass);
end;      *)

procedure TLua.RegClass(const AClass: TClass; const UsePublished: Boolean);
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.InternalAddClass(AClass, UsePublished);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp TLua.InternalAddClass
end;
{$endif}

procedure __TLuaRegClasses(const Self: TLua; const AClasses: array of TClass; const UsePublished: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(AClasses) do
  Self.InternalAddClass(AClasses[i], UsePublished);
end;

procedure TLua.RegClasses(const AClasses: array of TClass; const UsePublished: Boolean);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegClasses(Self, AClasses, UsePublished);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegClasses
end;
{$endif}

function TLua.RegRecord(const Name: LuaString; const TypeInfo: PTypeInfo; const UseRttiContext: Boolean): PLuaRecordInfo;
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Result := InternalAddRecord(Name, TypeInfo, UseRttiContext);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp TLua.InternalAddRecord
end;
{$endif}

function TLua.RegArray(const Identifier: Pointer; const ItemTypeInfo: PTypeInfo; const ABounds: array of Integer): PLuaArrayInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  Result := InternalAddArray(Identifier, ItemTypeInfo, ABounds);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp TLua.InternalAddArray
end;
{$endif}

function TLua.RegSet(const TypeInfo: PTypeInfo): PLuaSetInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  Result := InternalAddSet(TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp TLua.InternalAddSet
end;
{$endif}

procedure __TLuaRegUniversalProc(const Lua: TLua; const ProcName: LuaString;
  const Proc: TLuaProcCallback; const ArgsCount: Integer);
begin
  Lua.InternalAddMethod(nil, ProcName, @Proc, mkStatic, LUA_POINTER_INVALID, ArgsCount);
end;

procedure TLua.RegProc(const ProcName: LuaString; const Callback: TLuaProcCallback; const ArgsCount: Integer);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegUniversalProc(Self, ProcName, Callback, ArgsCount);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegUniversalProc
end;
{$endif}

procedure __TLuaRegRTIIProc(const Lua: TLua; const ProcName: LuaString;
  const Address: Pointer; const TypeInfo: PTypeInfo);
begin
  Lua.InternalAddMethod(nil, ProcName, Address, mkStatic,
    Lua.InternalBuildInvokable(TypeInfo, mkStatic));
end;

procedure TLua.RegProc(const ProcName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegRTIIProc(Self, ProcName, Address, TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegRTIIProc
end;
{$endif}

procedure __TLuaRegCustomProc(const Lua: TLua; const ProcName: LuaString; const Address: Pointer;
  const Params: array of TLuaProcParam; const ResultType: PTypeInfo; const CallConv: TCallConv);
begin
  Lua.InternalAddMethod(nil, ProcName, Address, mkStatic,
    Lua.InternalBuildInvokable(Params, ResultType, mkStatic, CallConv));
end;

procedure TLua.RegProc(const ProcName: LuaString; const Address: Pointer;
  const Params: array of TLuaProcParam; const ResultType: PTypeInfo; const CallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegCustomProc(Self, ProcName, Address, Params, ResultType, CallConv);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegCustomProc
end;
{$endif}

procedure __TLuaRegUniversalClassMethod(const Lua: TLua; const AClass: TClass;
  const MethodName: LuaString; const Method: TLuaMethodCallback;
  const ArgsCount: Integer; const MethodKind: TLuaMethodKind);
begin
  Lua.InternalAddMethod(Lua.InternalAddClass(AClass, False), MethodName, @Method,
    MethodKind, LUA_POINTER_INVALID, ArgsCount);
end;

procedure TLua.RegMethod(const AClass: TClass; const MethodName: LuaString; const Callback: TLuaMethodCallback;
  const ArgsCount: Integer; const MethodKind: TLuaMethodKind);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegUniversalClassMethod(Self, AClass, MethodName, Callback, ArgsCount, MethodKind);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegUniversalClassMethod
end;
{$endif}

procedure __TLuaRegRTIIClassMethod(const Lua: TLua; const AClass: TClass;
  const MethodName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo;
  const MethodKind: TLuaMethodKind);
begin
  Lua.InternalAddMethod(Lua.InternalAddClass(AClass, False), MethodName, Address, MethodKind,
    Lua.InternalBuildInvokable(TypeInfo, MethodKind));
end;

procedure TLua.RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer;
  const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegRTIIClassMethod(Self, AClass, MethodName, Address, TypeInfo, MethodKind);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegRTIIClassMethod
end;
{$endif}

procedure __TLuaRegCustomClassMethod(const Lua: TLua; const AClass: TClass; const MethodName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv);
begin
  Lua.InternalAddMethod(Lua.InternalAddClass(AClass, False), MethodName, Address, MethodKind,
    Lua.InternalBuildInvokable(Params, ResultType, mkStatic, CallConv));
end;

procedure TLua.RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer;
  const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegCustomClassMethod(Self, AClass, MethodName, Address, Params, ResultType, MethodKind, CallConv);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegCustomClassMethod
end;
{$endif}

procedure __TLuaRegProperty(const Self: TLua; const AClass: TClass; const Name: LuaString;
  const TypeInfo: PTypeInfo; const AGetter, ASetter: Pointer; const GetterPtr, SetterPtr: TLuaPropPtr;
  const Params: PLuaRecordInfo; const Default, ConstRef: Boolean; const Index: Integer);
type
  TInstanceClassProp = (icNone, icInstance, icClass);
label
  getter_field, getter_proc, getter_field_invalid, setter_field, setter_proc, setter_field_invalid;
var
  ClassInfo: PLuaClassInfo;
  ClassName: LuaString;
  Getter, Setter, Flags: NativeUInt;
  GetterProp, SetterProp: TInstanceClassProp;
  IndexParams: Integer;
  VmtOffset: NativeInt;

  procedure RaiseError(const FmtStr: string; const Args: array of const; const IsSetter: Boolean);
  const
    KINDS: array[Boolean] of string = ('getter', 'setter');
  var
    S: string;
  begin
    S := Format(FmtStr, Args);
    S := Format(S, [KINDS[IsSetter]]);
    raise ELua.Create(S) at Self.FReturnAddress;
  end;
begin
  ClassInfo := Self.InternalAddClass(AClass, False);
  ClassName := ClassInfo.Name;

  // flags, property mode
  Getter := NativeUInt(AGetter);
  Setter := NativeUInt(ASetter);
  GetterProp := icNone;
  SetterProp := icNone;
  Flags := 0;
  if (ConstRef) then Flags := PROP_CONSTREF_MODE;
  if (AGetter <> nil) then
  case GetterPtr of
    ppAuto:
    begin
      if (Getter < NativeUInt(AClass.InstanceSize)) then goto getter_field;
      goto getter_proc;
    end;
    ppField:
    begin
    getter_field:
      GetterProp := icInstance;
      if (Getter < SizeOf(Pointer)) or (Getter >= NativeUInt(AClass.InstanceSize)) then goto getter_field_invalid;
      Flags := Flags or NativeUInt(pmField);
    end;
    ppProc:
    begin
    getter_proc:
      GetterProp := icInstance;
      if (Getter <= $FFFF) then goto getter_field_invalid;
      Flags := Flags or NativeUInt(pmStatic);
      VmtOffset := ClassInfo.VmtOffset(Pointer(Getter));
      if (VmtOffset >= 0) then
      begin
        Getter := VmtOffset;
        Inc(Flags, 3);
      end;
    end;
    ppClassField:
    begin
      GetterProp := icClass;
      if (Getter <= $FFFF) then goto getter_field_invalid;
      Flags := Flags or NativeUInt(pmField);
    end;
    ppClassProc:
    begin
      GetterProp := icClass;
      if (Getter <= $FFFF) then
      begin
      getter_field_invalid:
        RaiseError('Invalid %s field %%s offset (%d)', [ClassName, Getter], False);
      end;
      Flags := Flags or NativeUInt(pmStatic);
    end;
  else
    RaiseError('Invalid %s property %%s kind (%d)', [ClassName, Ord(GetterPtr)], False);
  end;
  if (ASetter <> nil) then
  case SetterPtr of
    ppAuto:
    begin
      if (Setter < NativeUInt(AClass.InstanceSize)) then goto setter_field;
      goto setter_proc;
    end;
    ppField:
    begin
    setter_field:
      SetterProp := icInstance;
      if (Setter < SizeOf(Pointer)) or (Setter >= NativeUInt(AClass.InstanceSize)) then goto setter_field_invalid;
      Flags := Flags or (NativeUInt(pmField) shl PROP_SLOTSETTER_SHIFT);
    end;
    ppProc:
    begin
    setter_proc:
      SetterProp := icInstance;
      if (Setter <= $FFFF) then goto setter_field_invalid;
      Flags := Flags or (NativeUInt(pmStatic) shl PROP_SLOTSETTER_SHIFT);
      VmtOffset := ClassInfo.VmtOffset(Pointer(Setter));
      if (VmtOffset >= 0) then
      begin
        Setter := VmtOffset;
        Inc(Flags, 3 shl PROP_SLOTSETTER_SHIFT);
      end;
    end;
    ppClassField:
    begin
      SetterProp := icClass;
      if (Setter <= $FFFF) then goto setter_field_invalid;
      Flags := Flags or (NativeUInt(pmField) shl PROP_SLOTSETTER_SHIFT);
    end;
    ppClassProc:
    begin
      SetterProp := icClass;
      if (Setter <= $FFFF) then
      begin
      setter_field_invalid:
        RaiseError('Invalid %s field %%s offset (%d)', [ClassName, Setter], True);
      end;
      Flags := Flags or (NativeUInt(pmStatic) shl PROP_SLOTSETTER_SHIFT);
    end;
  else
    RaiseError('Invalid %s property %%s kind (%d)', [ClassName, Ord(SetterPtr)], True);
  end;

  // class/instance
  if ((GetterProp = icNone) and (SetterProp = icNone)) or
    ((GetterProp in [icNone, icClass]) <> (SetterProp in [icNone, icClass])) then
  begin
    raise ELua.Create('Can not detect class/instance property kind') at Self.FReturnAddress;
  end;
  if (Ord(GetterProp) or Ord(SetterProp) = Ord(icClass)) then Flags := Flags or PROP_CLASS_MODE;

  // index, parameters
  IndexParams := Index;
  if (Index <> Low(Integer)) <> (Assigned(Params)) then
  begin
    raise ELua.Create('Can not detect index/parametrized property kind') at Self.FReturnAddress;
  end;
  if (Assigned(Params)) then
  begin
    if (Params = INDEXED_PROPERTY) or (Params = NAMED_PROPERTY ) then
    begin
      IndexParams := NativeInt(Params);
    end else
    begin
      Params.CheckInstance(mtRecord, Self.FReturnAddress);
      IndexParams := Params.Ptr;
    end;

    if (Default) then
    begin
      raise ELua.CreateFmt('Simple property ("%s") can not be a default property', [Name]) at Self.FReturnAddress;
    end;
  end;
  if (IndexParams <> Low(Integer)) then
  begin
    if (GetterProp <> icNone) then
    begin
      Inc(Flags, 1 + Ord(Assigned(Params)));
    end;
    if (SetterProp <> icNone) then
    begin
      Inc(Flags, (1 + Ord(Assigned(Params))) shl PROP_SLOTSETTER_SHIFT);
    end;
  end;

  // register class property
  Self.InternalAddProperty(ClassInfo, Name, TypeInfo, Default, False,
    Getter, Setter, Flags, IndexParams);
end;

procedure TLua.RegProperty(const AClass: TClass; const Name: LuaString; const TypeInfo: PTypeInfo;
  const Getter, Setter: Pointer; const GetterPtr, SetterPtr: TLuaPropPtr;
  const Params: PLuaRecordInfo; const Default, ConstRef: Boolean;
  const Index: Integer);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegProperty(Self, AClass, Name, TypeInfo, Getter, Setter, GetterPtr, SetterPtr,
    Params, Default, ConstRef, Index);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegProperty
end;
{$endif}

procedure __TLuaRegVariable(const Self: TLua; const Name: LuaString; const Instance;
  const TypeInfo: PTypeInfo; const IsConst: Boolean);
var
  Ptr, Flags: NativeUInt;
begin
  if (not IsValidIdent(Name)) then
    raise ELua.CreateFmt('Invalid variable name "%s"', [Name]) at Self.FReturnAddress;

  Ptr := NativeUInt(@Instance);
  if (Ptr <= $FFFF) then
    raise ELua.CreateFmt('Pointer to variable "%s" not defined', [Name]) at Self.FReturnAddress;

  Flags := PROP_CLASS_MODE or Ord(pmField) or (Ord(pmField) shl PROP_SLOTSETTER_SHIFT);
  if (IsConst) then Flags := Flags or PROP_CONSTREF_MODE;

  Self.InternalAddProperty(nil, Name, TypeInfo, False, False, Ptr, Ptr, Flags,
    Low(Integer));
end;

procedure TLua.RegVariable(const Name: LuaString; const Instance; const TypeInfo: PTypeInfo;
  const IsConst: Boolean);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegVariable(Self, Name, Instance, TypeInfo, IsConst);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegVariable
end;
{$endif}

procedure __TLuaRegConstVariant(const Self: TLua; const Name: LuaString;
  const Value: Variant);
var
  Ref: integer;
  LuaName: __luaname;
begin
  if (not IsValidIdent(Name)) then
    raise ELua.CreateFmt('Invalid constant name "%s"', [Name]) at Self.FReturnAddress;

  LuaName := TLuaNames(Self.FNames).Add(Name);
  Ref := PLuaGlobalEntity(Self.InternalAddGlobal(Ord(gkConst), LuaName)).Ref;
  if (not Self.push_variant(Value)) then
  begin
    Self.stack_pop;
    raise ELua.CreateFmt('Not supported variant value (%s)', [Self.FStringBuffer.Default]) at Self.FReturnAddress;
  end else
  begin
    Self.global_fill_value(Ref);
  end;
end;

procedure TLua.RegConst(const Name: LuaString; const Value: Variant);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegConstVariant(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegConstVariant
end;
{$endif}

procedure __TLuaRegConstLuaArg(const Self: TLua; const Name: LuaString;
  const Value: TLuaArg);
var
  Ref: integer;
  LuaName: __luaname;
begin
  if (not IsValidIdent(Name)) then
    raise ELua.CreateFmt('Invalid constant name "%s"', [Name]) at Self.FReturnAddress;

  LuaName := TLuaNames(Self.FNames).Add(Name);
  Ref := PLuaGlobalEntity(Self.InternalAddGlobal(Ord(gkConst), LuaName)).Ref;
  if (not Self.push_luaarg(Value)) then
  begin
    Self.stack_pop;
    raise ELua.CreateFmt('Not supported argument value (%s)', [Self.FStringBuffer.Default]) at Self.FReturnAddress;
  end else
  begin
    Self.global_fill_value(Ref);
  end;
end;

procedure TLua.RegConst(const Name: LuaString; const Value: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegConstLuaArg(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegConstLuaArg
end;
{$endif}

procedure __TLuaRegEnum(const Self: TLua; const TypeInfo: PTypeInfo);
var
  i, VMin, VMax, Ref: Integer;
  S: ^ShortString;
  LuaName: __luaname;
begin
  if (NativeUInt(TypeInfo) <= $FFFF) then
    raise ELua.Create('EnumTypeInfo not defined') at Self.FReturnAddress;

  if (TypeInfo.Kind <> tkEnumeration) or (IsBooleanTypeInfo(TypeInfo)) then
  begin
    Self.unpack_lua_string(Self.FStringBuffer.Lua, PShortString(@TypeInfo.Name)^);
    GetTypeKindName(Self.FStringBuffer.Default, TypeInfo.Kind);
    raise ELua.CreateFmt('Type "%s" (kind: %s) is not enumeration',
      [Self.FStringBuffer.Lua, Self.FStringBuffer.Default]) at Self.FReturnAddress;
  end;

  // check fake (enumeration) storage
  if (Assigned(TLuaDictionary(Self.FGlobalEntities).InternalFind(TypeInfo, False))) then
    Exit;

  // each enumeration value
  with GetTypeData(TypeInfo)^ do
  begin
    VMin := MinValue;
    VMax := MaxValue;
    S := Pointer(@GetTypeData(BaseType^)^.NameList);

    for i := VMin to VMax do
    begin
      LuaName := TLuaNames(Self.FNames).Add(S^);
      Ref := PLuaGlobalEntity(Self.InternalAddGlobal(Ord(gkConst), LuaName)).Ref;
      lua_pushinteger(Self.Handle, i);
      Self.global_fill_value(Ref);

      Inc(NativeInt(S), PByte(S)^ + 1);
    end;
  end;

  // fake (enumeration) storage
  TLuaDictionary(Self.FGlobalEntities).Add(TypeInfo, LUA_POINTER_INVALID);
end;

procedure TLua.RegEnum(const TypeInfo: PTypeInfo);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  __TLuaRegEnum(Self, TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  {$endif}
  jmp __TLuaRegEnum
end;
{$endif}

function TLua.GetUnit(const Index: Integer): TLuaUnit;
begin
  if (Cardinal(Index) >= Cardinal(FUnitCount)) then
    raise ELua.CreateFmt('Invalid unit index %d, unit count: %d', [Index, FUnitCount]);

  Result := FUnits[Index];
end;

function TLua.GetUnitByName(const Name: LuaString): TLuaUnit;
var
  i, Count: Integer;
begin
  if (Pointer(Name) <> nil) then
  begin
    Count := PInteger(NativeInt(Name) - SizeOf(Integer))^ {$ifdef LUA_LENGTH_SHIFT}shr 1{$endif};

    for i := 0 to FUnitCount - 1 do
    begin
      Result := FUnits[i];
      if (Result.FNameLength = Count) and
        (CompareMem(Pointer(Result.FName), Pointer(Name), Count * SizeOf(LuaChar))) then Exit;
    end;
  end;

  Result := nil;
end;


initialization
  InitUnicodeLookups;
  InitCharacterTable;
  {$ifdef LUA_INITIALIZE}Lua := CreateLua;{$endif}

finalization
  {$ifdef LUA_INITIALIZE}FreeAndNil(Lua);{$endif}
  FreeLuaLibrary;


end.
