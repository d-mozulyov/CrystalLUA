unit CrystalLUA;

{******************************************************************************}
{ Copyright (c) Dmitry Mozulyov                                                }
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
    {$WARN SYMBOL_DEPRECATED OFF}
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
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif},{$endif}
       {$ifdef CPUARM64}System.Rtti,{$endif}
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
  {$ifNdef EXTENDEDRTTI}
    TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall);
  {$endif}

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
  __TLuaStringDictionary = array[1..28{$ifdef LARGEINT}* 2{$endif}] of Byte;
  __TLuaInvokableBuilder = array[1..28{$ifdef LARGEINT}* 2{$endif}] of Byte;

type
  TLua = class;
  PLuaMetaType = ^TLuaMetaType;
  PLuaArg = ^TLuaArg;
  PLuaTable = ^TLuaTable;

  // incorrect script use exception
  ELuaScript = class(ELua);

  // script stack overflow
  ELuaStackOverflow = class(ELuaScript);

  // Lua types, that is used between script and native side
  TLuaArgType = (
    // simple types
    ltEmpty, ltBoolean, ltInteger, ltDouble, ltPointer, ltString,
    // difficult types
    ltClass, ltObject, ltRecord, ltArray, ltSet, ltTable
    {ToDo: ltInterface, ltMethod, ltReference }
    );
  PLuaArgType = ^TLuaArgType;

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
    FInterface: IInterface;
    {$ifdef AUTOREFCOUNT}
    FObject: TObject;
    {$endif}
    FString: LuaString;
    F: packed record
      LuaType: TLuaArgType;
      AdvancedData: array[0..2] of Byte; { used in TLuaRecord/TLuaArray/TLuaTable etc }
    case Integer of
      0: (VDouble: Double);
      1: (VBoolean: Boolean);
      2: (VPointer: Pointer);
      3: (VInteger: Integer);
      4: (VNativeInt: NativeInt);
      5: (VData: Pointer; VInfo: Pointer; VCode: Pointer);
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
  TLuaArgDynArray = array of TLuaArg;
  PLuaArgDynArray = ^TLuaArgDynArray;
  TLuaArgList = array[0..High(Integer) div SizeOf(TLuaArg) - 1] of TLuaArg;
  PLuaArgList = ^TLuaArgList;
  TLuaArgs = packed record
    Items: PLuaArgList;
    Count: Integer;
  end;
  PLuaArgs = ^TLuaArgs;


{ TLuaArguments object
  Temporary argument and memory manager for script and universal functions }

  TLuaArguments = object
  protected
    F: packed record
    case Integer of
      0: (
           ParamArgs: TLuaArgs;
           ResultArgs: TLuaArgs;
           ParamCapacity: Integer;
           ResultCapacity: Integer;
         );
      1: (
           Args: array[0..1] of TLuaArgs;
           Capacities: array[0..1] of Integer;
         )
    end;
    FHeap: __TLuaMemoryHeap;
    FHeapCurrent: PByte;
    FHeapOverflow: PByte;
    FHeapEmpty: NativeInt;
    FAllocatedMetaTypes: Pointer;

    procedure GrowSetArgsCount(const AValue: Integer; const AIndex: NativeInt);
    procedure SetArgsCount(const AValue: Integer; const AIndex: NativeInt);
    procedure SetParamCount(const AValue: Integer); {$ifdef INLINESUPPORT}inline;{$endif}
    procedure SetResultCount(const AValue: Integer); {$ifdef INLINESUPPORT}inline;{$endif}
    function GrowAdd: PLuaArg;
    function GrowAlloc(const ASize: NativeUInt): Pointer;
    function AllocMetaType(const AMetaType: PLuaMetaType): Pointer;
    procedure Clear;
    procedure Finalize;
  public
    function AllocRecord(const AInfo: PLuaRecordInfo): Pointer;
    function AllocArray(const AInfo: PLuaArrayInfo): Pointer;
    function AllocSet(const AInfo: PLuaSetInfo): Pointer;

    function Add: PLuaArg;
    function AddRecord(const AInfo: PLuaRecordInfo): Pointer;
    function AddArray(const AInfo: PLuaArrayInfo): Pointer;
    function AddSet(const AInfo: PLuaSetInfo): Pointer;

    property ParamArgs: TLuaArgs read F.ParamArgs;
    property Params: PLuaArgList read F.ParamArgs.Items;
    property ParamCount: Integer read F.ParamArgs.Count;
    property ResultArgs: TLuaArgs read F.ResultArgs;
    property Results: PLuaArgList read F.ResultArgs.Items;
    property ResultCount: Integer read F.ResultArgs.Count write SetResultCount;
  end;
  PLuaArguments = ^TLuaArguments;

  TLuaScriptFrame = packed record
    Arguments: TLuaArguments;
    Buffer: TBytes;
    BufferCapacity: NativeUInt;
  end;
  PLuaScriptFrame = ^TLuaScriptFrame;

  TLuaReference = (lrDefault, lrWeak, lrUnsafe);
  PLuaReference = ^TLuaReference;

  TLuaPropAccessMode = (amNone, amInstField, amInstProc, amStaticField, amStaticProc);
  PLuaPropAccessMode = ^TLuaPropAccessMode;

  TLuaMethodKind = (mkStatic, mkInstance, mkClass, mkConstructor, mkDestructor, {ToDo}mkOperator);
  PLuaMethodKind = ^TLuaMethodKind;

  TLuaProcCallback = procedure(const AArgs: PLuaArgList; const AArguments: PLuaArguments);
  TLuaMethodCallback = procedure(const AInstance: Pointer; const AArgs: PLuaArgList; const AArguments: PLuaArguments);

  TLuaPropAccess = packed record
    Mode: TLuaPropAccessMode;
    Reference: TLuaReference;
    case Integer of
      0: (Offset: NativeUInt);
      1: (Address: Pointer);
  end;
  PLuaPropAccess = ^TLuaPropAccess;

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
  TLuaVariable = class
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
//  TLuaVariableDynArray = array of TLuaVariable;
//  {$hints on}

  TLuaMetaKind = (mtClass, mtInterface, mtRecord, mtArray, mtSet);
  PLuaMetaKind = ^TLuaMetaKind;

  TLuaMetaType = object
  protected
    F: record
      Marker: Integer;
      Kind: TLuaMetaKind;
      Weak: Boolean;
      Managed: Boolean;
      HFA: Boolean;
      Size: Integer;

      {$ifdef LARGEINT}
      Ptr: __luapointer;
      {$endif}
      Ref: Integer;
    case Integer of
      0: (TypeInfo: PTypeInfo);
      1: (ClassType: TClass);
    end;
    FLua: TLua;
    FName: LuaString;
    FNameSpace: __TLuaDictionary;

    // ToDo
    {.$ifdef SMALLINT}
    function GetPtr: __luapointer; {$ifdef INLINESUPPORT}inline;{$endif}
    {.$endif}
    procedure CheckInstance(const AKind: TLuaMetaKind; const ReturnAddress: Pointer);
    procedure FillManagedValue;
    procedure FillHFAValue;
    function GetHFAElementType: TFloatType;
    function GetHFAElementCount: Integer;
    function GetReturnReferenced: Boolean;
  protected
    property Kind: TLuaMetaKind read F.Kind;
    property Ptr: __luapointer read GetPtr;//{$ifdef SMALLINT}GetPtr{$else .LARGEINT}F.Ptr{$endif};
    property Ref: Integer read F.Ref;
    property Weak: Boolean read F.Weak;
    property Managed: Boolean read F.Managed;
    property HFA: Boolean read F.HFA;
    property HFAElementType: TFloatType read GetHFAElementType;
    property HFAElementCount: Integer read GetHFAElementCount;
    property ReturnReferenced: Boolean read GetReturnReferenced;

    function ParamReferenced(const ACallConv: TCallConv = ccReg; const AIsConst: Boolean = True): Boolean;
    procedure Dispose(const Value: Pointer{/TObject/IInterface});
    procedure Clear(var Instance; const FillZero: Boolean);
    procedure Assign(var Instance; const Value: Pointer{/TObject/IInterface});
  public
    property Lua: TLua read FLua;
    property Name: LuaString read FName;
    property Size: Integer read F.Size;
  end;

  // operators
  TLuaOperator = (loNeg, loAdd, loSub, loMul, loDiv, loMod, loPow, loCompare);
  PLuaOperator = ^TLuaOperator;
  TLuaOperators = set of TLuaOperator;
  PLuaOperators = ^TLuaOperators;
  TLuaOperatorCallback = procedure(var AResult, ALeft, ARight; const Kind: TLuaOperator);

  // all information (such as name, field, methods)
  // you should use it to operate records between native and script
  TLuaRecordInfo = object(TLuaMetaType)
  protected
    FOperators: TLuaOperators;
    FOperatorCallback: TLuaOperatorCallback;

    procedure SetOperators(const Value: TLuaOperators);
    procedure SetOperatorCallback(const Value: TLuaOperatorCallback);
  public
    procedure RegMethod(const MethodName: LuaString; const Callback: TLuaMethodCallback; const MinArgsCount: Word = 0; const MaxArgsCount: Word = High(Word); const IsStatic: Boolean = False); overload;
    procedure RegMethod(const MethodName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo; const IsStatic: Boolean = False); overload;
    procedure RegMethod(const MethodName: LuaString; const Address: Pointer;
      const Params: array of TLuaProcParam;
      const ResultType: PTypeInfo = nil; const IsResultUnsafe: Boolean = False;
      const IsStatic: Boolean = False; const CallConv: TCallConv = ccReg); overload;
    procedure RegField(const AName: LuaString; const AOffset: NativeUInt; const ATypeInfo: Pointer; const AReference: TLuaReference = lrDefault); overload;
    procedure RegField(const AName: LuaString; const AField: Pointer; const ATypeInfo: Pointer; const ARecord: Pointer = nil; const AReference: TLuaReference = lrDefault); overload;
    procedure RegClassField(const AName: LuaString; const AAddress: Pointer; const ATypeInfo: Pointer; const AReference: TLuaReference = lrDefault);
    procedure RegProperty(const AName: LuaString; const AGetter, ASetter: TLuaPropAccess;
      const ATypeInfo: PTypeInfo; const AConstRefHint: Boolean = True; const AIndex: Integer = Low(Integer));
    procedure RegComplexProperty(const AName: LuaString;
      const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam;
      const ATypeInfo: PTypeInfo; const ADefault: Boolean = False;
      const AConstRefHint: Boolean = True; const AIndex: Integer = Low(Integer));

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

  // (internal) information needed to use interfaces between native and script side
  PLuaInterfaceInfo = ^TLuaInterfaceInfo;
  TLuaInterfaceInfo = object(TLuaMetaType)
    Parent: __luapointer;
    Count: Integer;
    // ToDo?

    function InheritsFrom(const Value: PLuaInterfaceInfo): Boolean;
  end;

  // (internal) information needed to use classes between native and script side
  PLuaClassInfo = ^TLuaClassInfo;
  TLuaClassInfo = object(TLuaMetaType)
  protected
    function VmtOffset(const Proc: Pointer; const StandardProcs: Boolean): NativeInt;
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
    property ClassType: TClass read F.ClassType;
  end;

  TLuaScriptBOM = (sbNone, sbUTF8, sbUTF16, sbUTF16BE, cbUTF32, cbUTF32BE);
  PLuaScriptBOM = ^TLuaScriptBOM;
  TLuaBufferUnique = function(var Data: __luabuffer): NativeInt;
  TLuaOnPreprocessScript = procedure(var Data, Source: __luabuffer; var Offset: Integer; const UnitName: LuaString; const Unique: TLuaBufferUnique) of object;

  TLuaUnitLine = record
    Chars: __luadata;
    Count: Integer;
  end;
  PLuaUnitLine = ^TLuaUnitLine;

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


{ TLua class
  Script and registered types/functions manager }

  TLuaOnRegisterError = function(const Path, Error: LuaString; const CallDepth: Integer; const Critical: Boolean): Boolean of object;

  TLua = class(TInterfacedObject)
  protected
    // unicode routine
    FCodePage: Word;
    FUnicodeTable: array[Byte] of Word;
    FUTF8Table: array[Byte] of Cardinal;

    procedure SetCodePage(Value: Word);
    function AnsiFromUnicode(ATarget: PAnsiChar; ACodePage: Word; ASource: PWideChar; ALength: Integer): Integer;
    procedure UnicodeFromAnsi(ATarget: PWideChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer);
    { class function Utf8FromUnicode(ATarget: PAnsiChar; ASource: PWideChar; ALength: Integer): Integer; static; }
    { class function UnicodeFromUtf8(ATarget: PWideChar; ASource: PAnsiChar; ALength: Integer): Integer; static; }
    function Utf8FromAnsi(ATarget: PAnsiChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer): Integer;
    function AnsiFromUtf8(ATarget: PAnsiChar; ACodePage: Word; ASource: PAnsiChar; ALength: Integer): Integer;
    function AnsiFromAnsi(ATarget: PAnsiChar; ATargetCodePage: Word; ASource: PAnsiChar; ASourceCodePage: Word; ALength: Integer): Integer;
  protected
    // stack helpers
    FHandle: Pointer;
    FInternalBuffer: __TLuaBuffer;
    FInvokableBuilder: __TLuaInvokableBuilder;
    FStringBuffer: record
      {$ifNdef NEXTGEN}
      Short: ShortString;
      Ansi: AnsiString;
      Wide: WideString;
      {$endif}
      Unicode: UnicodeString;
      UnicodeReserved: UnicodeString;
      Lua: LuaString;
      LuaReserved: LuaString;
      Default: string;
    end;
    FTempBuffer: packed record
      V: Variant;
      Intf: IInterface;
      O: TObject;
      M: TLuaOnPreprocessScript{TMethod};
      MUnsafe: TMethod;
      A: TLuaArg;
    case Integer of
      0: (B: Boolean);
      1: (D: Double);
      2: (E: Extended);
      3: (I: Integer);
      4: (C: Cardinal);
      5: (I64: Int64);
      6: (N: NativeInt);
    end;
    FParamBuffer: packed record
      UserData: Pointer{PLuaUserData};
      Prop: Pointer{PLuaProperty};
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

    function  push_metatype_instance(const AMetaType: PLuaMetaType; const ASource; const AOwner, ATemporary: Boolean): Pointer{PLuaUserData};
    function  push_complex_property(const PropertyInfo; const Instance: Pointer): Pointer{PLuaUserData};
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
    FNames: __TLuaStringDictionary {TLuaNames: <LuaString,__luaname>};
    FMetaTypes: __TLuaDictionary {Pointer, PLuaMetaType};
    FClosureTypes: __TLuaDictionary {PTypeInfo, PLuaClosureType};
    FGlobalEntities: __TLuaDictionary {__luaname, PLuaGlobalEntity};
    FGlobalMetaTable: Integer;
    FFormatWrapper: Integer;
    FObjectMetaType: PLuaClassInfo;

    // standard name spaces
    FStdObjectNameSpace: __TLuaDictionary;
    FStdInterfaceNameSpace: __TLuaDictionary;
    FStdRecordNameSpace: __TLuaDictionary;
    FStdArrayNameSpace: __TLuaDictionary;
    FStdSetNameSpace: __TLuaDictionary;

    // special meta tables
    FClosureMetaTable: Integer;

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
    FRegisteredTypeNames: __TLuaStringDictionary {TLuaRegisteredTypeNames: <LuaString,PLuaRegisteredTypeName>};
   (*
    mt_properties: integer;
  *)

  (*  function AllocMethod: __luapointer;
    procedure FreeMethod(const Value: __luapointer);
    function AllocProperty: __luapointer;
    procedure FreeProperty(const Value: __luapointer);  *)

    function alloc_luafunction(const Callback: Pointer; const P1, P2: __luapointer): __luafunction; overload;
    function alloc_luafunction(const AMethod: Pointer{PLuaMethod}): __luafunction; overload;
    procedure alloc_push_luafunction(const Callback: Pointer; const P1, P2: __luapointer);
    procedure push_luafunction(const Func: __luafunction);
    // procedure release_function(const Func: __luafunction);
    function function_topointer(const StackIndex: Integer): Pointer;
    function alloc_push_closure(const AReferenceOwner: Boolean): Pointer{PLuaClosureMethod};

    procedure InternalRegisterCallback(const Name: __luaname; const Callback: Pointer; const P1: __luapointer = -1; const P2: __luapointer = -1);
    //procedure InternalUnregisterCallback(const Name: __luaname);
    procedure InternalRegTypeName(const TypeName: LuaString; const TypeInfo: PTypeInfo; const PointerDepth: Integer);
    procedure InternalRegStandardTypeNames;
    function InternalNewMetaTable: Integer;
    function InternalRegisterMetaTable(const MetaType: PLuaMetaType = nil): Integer;
    function InternalTableToMetaType(const StackIndex: Integer): PLuaMetaType;
    function InternalAddGlobal(const AKind: Byte{TLuaGlobalKind}; const Name: __luaname): Pointer{PLuaGlobalEntity};
    function InternalAddMetaType(const Kind: TLuaMetaKind; const Name: LuaString; const TypeInfo: Pointer;
      const InstanceSize: Integer; const AdditionalSize: NativeInt = 0): PLuaMetaType;
    procedure InternalReplaceChildNameSpace(const ParentMetaItem: Pointer{PLuaDictionaryItem};
      const Name: __luaname; const LastValue, Value: __luapointer; const IsDefaultProp: Boolean);
    function  InternalAddMethod(const MetaType: PLuaMetaType; const Name: LuaString;
      Address: Pointer; const MethodKind: TLuaMethodKind; const Invokable: __luapointer; const Critical: Boolean;
      {Universal} const MinArgsCount: Word = 0; const MaxArgsCount: Word = High(Word)): __luapointer;
    function InternalAddProperty(const AName: LuaString; const AOptions: Pointer{PLuaPropertyOptions};
      const AAutoRegister, ACritical: Boolean): __luapointer;
    function InternalAddField(const AMetaType: PLuaMetaType; const AName: LuaString;
      const AOffset: NativeUInt; const ATypeInfo: PTypeInfo; const AReference: TLuaReference;
      const AClassField, AAutoRegister, ACritical: Boolean): __luapointer;
    function  InternalAddClass(const AClass: TClass; const UseRtti: Boolean; const Critical: Boolean): PLuaClassInfo;
    function  InternalGetClassInfo(const AClass: TClass): PLuaClassInfo;
    function  InternalAddRecord(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaRecordInfo;
    function  InternalAddRecordEx(const Name: LuaString; const TypeInfo: Pointer; const UseRtti: Boolean; const Critical: Boolean): PLuaRecordInfo;
    function  InternalAddInterface(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaInterfaceInfo;
    function  InternalAddArray(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaArrayInfo;
    function  InternalAddArrayEx(const Identifier, ItemTypeInfo: Pointer; const ABounds: array of Integer; const Critical: Boolean): PLuaArrayInfo;
    function  InternalAddSet(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaSetInfo;
    procedure InternalAddEnum(const TypeInfo: PTypeInfo; const Critical: Boolean);
    function  InternalInvokableBuilderEnter: __TLuaInvokableBuilder;
    procedure InternalInvokableBuilderLeave(const AValue: __TLuaInvokableBuilder);
    function  InternalAutoRegister(const TypeInfo: PTypeInfo; const UseRtti: Boolean = True; const Critical: Boolean = False): Pointer{PLuaMetaType/PLuaClosureType};
    function  InternalBuildInvokable(const MetaType: PLuaMetaType; const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind; const Critical: Boolean): __luapointer; overload;
    function  InternalBuildInvokable(const MetaType: PLuaMetaType; const Name: LuaString; const Params: array of TLuaProcParam; const ResultType: PTypeInfo; const IsResultUnsafe: Boolean; const MethodKind: TLuaMethodKind; const CallConv: TCallConv; const Critical: Boolean): __luapointer; overload;
  private
    // script callbacks helpers
    function __print: Integer;
    function __printf: Integer;
    function __GetUserData(const AMetaType: __luapointer; const CheckAlreadyDestroyed: Boolean = True): Pointer;
    function __GetSelfInstance(const AMethodName: __luaname): Pointer;
    function __GetSelfClass(const AMethodName: __luaname; const AAllowInstance: Boolean): TClass;
    function __InitTableValues(const AUserData: Pointer{PLuaUserData}; const StackIndex: Integer): Integer;
    function __ExecuteSetMethod(const AUserData: Pointer{PLuaUserData}; const FirstArgIndex, ArgsCount, Method: Integer): Integer;

    // specific script callbacks
    function __len(const AMetaType: __luapointer): Integer;
    function __tostring(const AMetaType: __luapointer): Integer;
    function __gc(const AMetaType: __luapointer): Integer;
    function __call(const AMetaType: __luapointer; const ParamMode: Boolean): Integer;
    function __operator(const AMetaType: __luapointer; const Kind: Cardinal{Byte}): Integer;
    function __closuregc: Integer;

    // index/newindex helpers
    function push_standard(const MetaType: PLuaMetaType; const StdIndex: Cardinal): Boolean;
    function set_standard(const MetaType: PLuaMetaType; const StdIndex: Cardinal): Boolean;
    procedure push_prop_tempgetter(const AInstance: Pointer; const AProp: Pointer);
    function call_prop_getter(const AInstance: Pointer; const AProp: Pointer): Pointer;
    procedure call_prop_setter(const AInstance: Pointer; const AProp: Pointer; const AValue: NativeInt);

    // index/newindex
    function __index(const AMetaType: __luapointer): Integer;
    function __newindex(const AMetaType: __luapointer; const AParamMode: Boolean): Integer;
    function __global_index(const ModifyLow, ModifyHigh: Cardinal): Integer;
    function __global_newindex(const ModifyLow, ModifyHigh: Cardinal): Integer;

    // internal closures
  protected
    function __InheritsFrom(const AMetaType: PLuaMetaType; const ArgsCount: Integer): Integer;
    function __Assign(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __Free(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __ClassCreate(const AMetaType: PLuaMetaType; const ArgsCount: Integer): Integer;
    function __IntfAddRef(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __IntfRelease(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __DynArrayInclude(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __DynArrayResize(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __SetInclude(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __SetExclude(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
    function __SetContains(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
  private
    // method callbacks
    function UniversalMethodCallback(const AMethod: Pointer{PLuaMethod}; const ArgsCount: Integer): Integer;
    function InvokableMethodCallback(const AMethod: Pointer{PLuaMethod}; const AInvokableData: Pointer): Integer;
    function __MethodCallback(const AMethodLow, AMethodHigh: Cardinal): Integer;
  private
    // errors
    FReturnAddress: Pointer;
    FFrames: Pointer{PLuaNativeFrame};
    FPrintRegisterError: Boolean;
    FOnRegisterError: TLuaOnRegisterError;

    procedure NativeFrameEnter(var Frame{: TLuaNativeFrame}; const Name: PShortString; const Critical: Boolean);
    //procedure NativeFrameEnterParentCritical(var Frame{: TLuaNativeFrame}; const Name: PShortString);
    procedure NativeFrameLeave;
    procedure NativeFrameRename(const Name: PShortString); overload;
    procedure NativeFrameRename(var NameBuffer: ShortString; const Name: LuaString); overload;
    procedure InternalError(const Text: LuaString);
    procedure InternalErrorFmt(const FmtStr: LuaString; const Args: array of const);
    procedure RegisterError(const Text: LuaString);
    procedure RegisterErrorFmt(const FmtStr: LuaString; const Args: array of const);
    procedure RegisterErrorTypeExists(const Name: LuaString);
  private
    // scripts and units routine
    FOnPreprocessScript: TLuaOnPreprocessScript;
    FStackFrames: array[1 - 1..16] of TLuaScriptFrame;
    FStackFrameIndex: NativeUInt;
    FArguments: PLuaArguments{PLuaScriptFrame};

         (*
 //   FReferences: TLuaVariableDynArray; // список ссылок (LUA_REGISTRYINDEX)  *)
    FUnitCount: Integer;
    FUnits: array of TLuaUnit;
    procedure CheckScriptError(const ErrCode: Integer; AUnit: TLuaUnit = nil);
    function EnterScript(const AReturnAddress: Pointer): Pointer;
    procedure LeaveScriptStack(const AReturnAddress: Pointer);
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
    function CreateReference(const global_name: string=''): TLuaVariable;
    class function GetProcAddress(const ProcName: pchar; const throw_exception: boolean = false): pointer; // низкий уровень. адрес функции lua.dll *)

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

    // methods
    function VariableExists(const Name: LuaString): Boolean; overload;
    function ProcExists(const Name: LuaString): Boolean; overload;
    function VariableExists(const Table, Name: LuaString): Boolean; overload;
    function ProcExists(const Table, Name: LuaString): Boolean; overload;
    function Call(const ProcName: LuaString): TLuaArgs; overload;
    function Call(const ProcName: LuaString; const Args: array of TLuaArg): TLuaArgs; overload;
    function Call(const ProcName: LuaString; const Args: array of const): TLuaArgs; overload;
    function Call(const Table, ProcName: LuaString): TLuaArgs; overload;
    function Call(const Table, ProcName: LuaString; const Args: array of TLuaArg): TLuaArgs; overload;
    function Call(const Table, ProcName: LuaString; const Args: array of const): TLuaArgs; overload;

    // registrations
    procedure RegTypeName(const TypeName: LuaString; const TypeInfo: PTypeInfo; const PointerDepth: Integer = 0);
    procedure RegClass(const AClass: TClass; const UseRtti: Boolean = True);
    procedure RegClasses(const AClasses: array of TClass; const UseRtti: Boolean = True);
    function  RegRecord(const TypeInfo: PTypeInfo): PLuaRecordInfo; overload;
    function  RegRecord(const Name: LuaString; const TypeInfo: PTypeInfo; const UseRtti: Boolean = True): PLuaRecordInfo; overload;
    function  RegArray(const TypeInfo: PTypeInfo): PLuaArrayInfo; overload;
    function  RegArray(const Identifier: Pointer; const ItemTypeInfo: PTypeInfo; const ABounds: array of Integer): PLuaArrayInfo; overload;
    function  RegSet(const TypeInfo: PTypeInfo): PLuaSetInfo;
    procedure RegProc(const ProcName: LuaString; const Callback: TLuaProcCallback; const MinArgsCount: Word = 0; const MaxArgsCount: Word = High(Word)); overload;
    procedure RegProc(const ProcName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo); overload;
    procedure RegProc(const ProcName: LuaString; const Address: Pointer; const Params: array of TLuaProcParam;
      const ResultType: PTypeInfo = nil; const IsResultUnsafe: Boolean = False;
      const CallConv: TCallConv = ccReg); overload;
    procedure RegMethod(const AClass: TClass; const MethodName: LuaString; const Callback: TLuaMethodCallback; const MinArgsCount: Word = 0; const MaxArgsCount: Word = High(Word); const MethodKind: TLuaMethodKind = mkInstance); overload;
    procedure RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind = mkInstance); overload;
    procedure RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer;
      const Params: array of TLuaProcParam; const ResultType: PTypeInfo = nil; const IsResultUnsafe: Boolean = False;
      const MethodKind: TLuaMethodKind = mkInstance; const CallConv: TCallConv = ccReg); overload;
    procedure RegProperty(const AClass: TClass; const AName: LuaString;
      const AGetter, ASetter: TLuaPropAccess; const ATypeInfo: PTypeInfo;
      const AConstRefHint: Boolean = True; const AIndex: Integer = Low(Integer));
    procedure RegComplexProperty(const AClass: TClass; const AName: LuaString;
      const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam; const ATypeInfo: PTypeInfo;
      const ADefault: Boolean = False; const AConstRefHint: Boolean = True; const AIndex: Integer = Low(Integer));
    procedure RegVariable(const Name: LuaString; const Instance; const AClass: TClass;
      const IsConst: Boolean = False; const Reference: TLuaReference = lrDefault); overload;
    procedure RegVariable(const Name: LuaString; const Instance; const TypeInfo: PTypeInfo;
      const IsConst: Boolean = False; const Reference: TLuaReference = lrDefault); overload;
    procedure RegConst(const Name: LuaString; const Value: Variant); overload;
    procedure RegConst(const Name: LuaString; const Value: TLuaArg); overload;
    procedure RegEnum(const TypeInfo: PTypeInfo);

    // advanced properties
    property Arguments: PLuaArguments read FArguments;
    property Variable[const Name: LuaString]: Variant read GetVariable write SetVariable;
    property VariableEx[const Name: LuaString]: TLuaArg read GetVariableEx write SetVariableEx;
    property TableVariable[const Table, Name: LuaString]: Variant read GetTableVariable write SetTableVariable;
    property TableVariableEx[const Table, Name: LuaString]: TLuaArg read GetTableVariableEx write SetTableVariableEx;
    property RecordInfo[const Name: LuaString]: PLuaRecordInfo read GetRecordInfo;
    property ArrayInfo[const Name: LuaString]: PLuaArrayInfo read GetArrayInfo;
    property SetInfo[const Name: LuaString]: PLuaSetInfo read GetSetInfo;

    // basic properties
    property Handle: Pointer read FHandle;
    property PrintRegisterError: Boolean read FPrintRegisterError write FPrintRegisterError;
    property OnRegisterError: TLuaOnRegisterError read FOnRegisterError write FOnRegisterError;

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
  TypeInfoLuaArg  = PTypeInfo(NativeUInt($7DDD0000));
  TypeInfoPWideChar = PTypeInfo(NativeUInt($7CCC0000));
  TypeInfoPAnsiChar = PTypeInfo(NativeUInt($7BBB0000));
  TypeInfoPUTF8Char = PTypeInfo(NativeUInt($7BBB0000) + 65001);

  // special complex properties parameters
  INDEXED_PROPERTY = PLuaRecordInfo(NativeUInt($7EEEEEEE));
  NAMED_PROPERTY   = PLuaRecordInfo(NativeUInt($7AAAAAAA));

  // all possible operators
  ALL_OPERATORS: TLuaOperators = [Low(TLuaOperator)..High(TLuaOperator)];


// helper functions
function CreateLua: TLua;
function LuaPropNone: TLuaPropAccess;
function LuaPropField(const AOffset: NativeUInt; const AReference: TLuaReference = lrDefault): TLuaPropAccess; overload;
function LuaPropField(const AAddress: Pointer; const AReference: TLuaReference = lrDefault): TLuaPropAccess; overload;
function LuaPropField(const AInstance, AAddress: Pointer; const AReference: TLuaReference = lrDefault): TLuaPropAccess; overload;
function LuaPropProc(const AAddress: Pointer; const AReference: TLuaReference = lrDefault): TLuaPropAccess;
function LuaPropClassField(const AAddress: Pointer; const AReference: TLuaReference = lrDefault): TLuaPropAccess;
function LuaPropClassProc(const AAddress: Pointer; const AReference: TLuaReference = lrDefault): TLuaPropAccess;
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
function LuaArgDynArray(const Count: Integer): TLuaArgDynArray;
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

function EInvalidScriptFrameIndex(const Index: NativeUInt): ELuaStackOverflow;
begin
  Result := ELuaStackOverflow.CreateFmt('Invalid script stack frame index: %d', [Index]);
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

function LuaPropNone: TLuaPropAccess;
begin
  Result.Mode := amNone;
  Result.Reference := lrDefault;
  Result.Offset := 0;
end;

function LuaPropField(const AOffset: NativeUInt; const AReference: TLuaReference): TLuaPropAccess;
begin
  Result.Mode := amInstField;
  Result.Reference := AReference;
  Result.Offset := AOffset;
end;

function LuaPropField(const AAddress: Pointer; const AReference: TLuaReference): TLuaPropAccess;
begin
  Result.Mode := amInstField;
  Result.Reference := AReference;
  Result.Offset := NativeUInt(AAddress);
end;

function LuaPropField(const AInstance, AAddress: Pointer; const AReference: TLuaReference): TLuaPropAccess;
begin
  Result.Mode := amInstField;
  Result.Reference := AReference;
  Result.Offset := NativeUInt(AAddress) - NativeUInt(AInstance);
end;

function LuaPropProc(const AAddress: Pointer; const AReference: TLuaReference): TLuaPropAccess;
begin
  Result.Mode := amInstProc;
  Result.Reference := AReference;
  Result.Address := AAddress;
end;

function LuaPropClassField(const AAddress: Pointer; const AReference: TLuaReference): TLuaPropAccess;
begin
  Result.Mode := amStaticField;
  Result.Reference := AReference;
  Result.Address := AAddress;
end;

function LuaPropClassProc(const AAddress: Pointer; const AReference: TLuaReference): TLuaPropAccess;
begin
  Result.Mode := amStaticProc;
  Result.Reference := AReference;
  Result.Address := AAddress;
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

  {$if not Defined(FPC) and (CompilerVersion < 20)}
    hfFieldSize = 0;
  {$ifend}

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
  PAnonymousFieldInfo = ^TAnonymousFieldInfo;
  TAnonymousFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
    {$ifdef LARGEINT}
    _Padding: Integer;
    {$endif}
  end;

  PAnonymousFieldTable = ^TAnonymousFieldTable;
  TAnonymousFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TAnonymousFieldInfo;
  end;

  PClassMethodEntry = ^TClassMethodEntry;
  TClassMethodEntry = packed record
    Size: Word;
    Address: Pointer;
  case Integer of
    0: (Name: ShortString);
    1: (NameLength: Byte; NameChars: array[1..9] of Byte);
  end;

{$ifdef EXTENDEDRTTI}
  TSpecialMethod = (smConstructor, smDestructor, smOperatorOverload);
  TDispatchKind = (dkStatic, dkVtable, dkDynamic, dkMessage, dkInterface);
  TMemberVisibility = (mvPrivate, mvProtected, mvPublic, mvPublished);

  PExtendedClassMethodEntry = ^TExtendedClassMethodEntry;
  TExtendedClassMethodEntry = packed record
    Entry: PClassMethodEntry;
    Flags: Word;
    VirtualIndex: Smallint;

    function IsAbstract: Boolean;
    function IsSpecial: Boolean;
    function IsClassMethod: Boolean;
    function IsHasSelf: Boolean;
    function DispatchKind: TDispatchKind;
    function MemberVisibility: TMemberVisibility;
    function SpecialMethod: TSpecialMethod;
    function DefaultMethodKind: TLuaMethodKind;
  end;

function TExtendedClassMethodEntry.IsAbstract: Boolean;
begin
  Result := (Flags and (1 shl 7) <> 0);
end;

function TExtendedClassMethodEntry.IsSpecial: Boolean;
begin
  Result := (Flags and 4 <> 0);
end;

function TExtendedClassMethodEntry.IsClassMethod: Boolean;
begin
  if (IsSpecial) then
  begin
    Result := (SpecialMethod = smConstructor);
  end else
  begin
    Result := (Flags and (1 shl 0) <> 0);
  end;
end;

function TExtendedClassMethodEntry.IsHasSelf: Boolean;
begin
  if (IsSpecial) then
  begin
    Result := (SpecialMethod in [smConstructor, smDestructor]);
  end else
  begin
    Result := (Flags and (1 shl 1) <> 0);
  end;
end;

function TExtendedClassMethodEntry.DispatchKind: TDispatchKind;
begin
  Result := TDispatchKind((Flags shr 3) and 3);
end;

function TExtendedClassMethodEntry.MemberVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility((Flags shr 5) and 3);
end;

function TExtendedClassMethodEntry.SpecialMethod: TSpecialMethod;
begin
  Result := TSpecialMethod((Flags shr 0) and 3);
end;

function TExtendedClassMethodEntry.DefaultMethodKind: TLuaMethodKind;
begin
  if (Self.IsSpecial) then
  begin
    case Self.SpecialMethod of
     smConstructor: Result := mkConstructor;
      smDestructor: Result := mkDestructor;
    else
      // smOperatorOverload
      Result := mkOperator;
    end;
  end else
  if (Self.IsClassMethod) then
  begin
    Result := mkClass;
    if (not Self.IsHasSelf) then
      Result := mkStatic;
  end else
  begin
    Result := mkInstance;
  end;
end;
{$else .OLD_RTTI}
type
  PIntfMethodTable = ^TIntfMethodTable;
  TIntfMethodTable = packed record
    Count: Word; // methods in this interface
    RttiCount: Word; // =Count, or $FFFF if no further data
   {Entry: array[1..Count] of TIntfMethodEntry;
    AttrData: TAttrData;}
  end;

  PIntfMethodEntry = ^TIntfMethodEntry;
  TIntfMethodEntry = packed record
    Name: ShortString;
  { Kind: Byte; // 0=proc or 1=func
    CC: TCallConv;
    ParamCount: Byte;
    Params: array[1..ParamCount] of TIntfMethodParam;
    ResultTypeName: string; // only if func
    ResultType: PPTypeInfo; // only if Len(Name) > 0
    AttrData: TAttrData;}
  end;
{$endif}

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

function SkipAttributes(const AAttrData: Pointer): Pointer; overload;
var
  P: PByte;
begin
  P := AAttrData;

  {$ifdef EXTENDEDRTTI}
  Inc(P, PAttrData(P).Len);
  {$endif}

  Result := P;
end;

function SkipAttributes(const AAttrData: Pointer; out AReference: TLuaReference): Pointer; overload;
var
  P: PByte;
  {$ifdef WEAKREF}
  LAttrType: PTypeInfo;
  LClass: TClass;
  {$endif}
begin
  P := AAttrData;
  Result := P;
  {$ifdef EXTENDEDRTTI}
  Inc(NativeInt(Result), PAttrData(P).Len);
  {$endif}
  AReference := lrDefault;

  {$ifdef WEAKREF}
  Inc(P, SizeOf(Word));
  while (P <> Result) do
  begin
    LAttrType := GetTypeInfo(PPointer(P)^);
    if (Assigned(LAttrType)) then
    begin
      LClass := GetTypeData(LAttrType).ClassType;
      if (LClass.InheritsFrom(UnsafeAttribute)) then
      begin
        AReference := lrUnsafe;
        Break;
      end else
      if (LClass.InheritsFrom(WeakAttribute)) then
      begin
        AReference := lrWeak;
      end;
    end;

    Inc(P, SizeOf(Pointer));
    Inc(P, SizeOf(Pointer));
    Inc(P, PWord(P)^ + SizeOf(Word));
  end;
  {$endif}
end;

function IsBooleanType(const Value: PTypeInfo): Boolean;
var
  Base: PTypeInfo;
  pBase: ^PTypeInfo;
  TypeData: PTypeData;
  S: PByte;
begin
  Result := Assigned(Value) and (Value.Kind = tkEnumeration);
  if (not Result) then
  begin
    {$ifdef FPC}Result := Assigned(Value) and (Value.Kind = tkBool);{$endif}
    Exit;
  end;

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

function IsReferenceMethodType(const Value: PTypeInfo): Boolean;
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
    $ffff: Result := (PByte(@MethodEntry.Name)^ = 2){no RTTI reference};
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

function IsWeakTypeInfo(const Value: PTypeInfo): Boolean;
{$ifdef WEAKREF}
var
  i: Cardinal;
  WeakMode: Boolean;
  FieldTable: PAnonymousFieldTable;
begin
  Result := False;

  if Assigned(Value) then
  case Value.Kind of
    {$ifdef WEAKINSTREF}
    tkMethod:
    begin
      Result := True;
    end;
    {$endif}
    tkArray{static array}:
    begin
      FieldTable := PAnonymousFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if (FieldTable.Fields[0].TypeInfo <> nil) then
        Result := IsWeakTypeInfo(FieldTable.Fields[0].TypeInfo^);
    end;
    tkRecord{$ifdef FPC}, tkObject{$endif}:
    begin
      FieldTable := PAnonymousFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if FieldTable.Count > 0 then
      begin
        WeakMode := False;
        for i := 0 to FieldTable.Count - 1 do
        begin
          if FieldTable.Fields[i].TypeInfo = nil then
          begin
            WeakMode := True;
            Continue;
          end;
          if (not WeakMode) then
          begin
            if (IsWeakTypeInfo(FieldTable.Fields[i].TypeInfo^)) then
            begin
              Result := True;
              Exit;
            end;
          end else
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;
{$else}
begin
  Result := False;
end;
{$endif}

function IsManagedTypeInfo(const Value: PTypeInfo): Boolean;
var
  i: Cardinal;
  {$ifdef WEAKREF}
  WeakMode: Boolean;
  {$endif}
  FieldTable: PAnonymousFieldTable;
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
      FieldTable := PAnonymousFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if (FieldTable.Fields[0].TypeInfo <> nil) then
        Result := IsManagedTypeInfo(FieldTable.Fields[0].TypeInfo^);
    end;
    tkRecord{$ifdef FPC}, tkObject{$endif}:
    begin
      FieldTable := PAnonymousFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
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

{$ifdef EXTENDEDRTTI}
function GetPointerTypeDepth(const AValue: PTypeInfo; var AResult: PTypeInfo): Integer;
var
  LValue: PTypeInfo;
  LTypeData: PTypeData;
begin
  Result := 0;
  LValue := AValue;
  repeat
    if (not Assigned(LValue)) or (LValue.Kind <> tkPointer) then
      Break;

    Inc(Result);
    LTypeData := GetTypeData(LValue);
    LValue := GetTypeInfo(LTypeData.RefType);
  until (False);

  if (not Assigned(LValue)) then
  begin
    Dec(Result);
    AResult := System.TypeInfo(Pointer);
  end else
  begin
    AResult := LValue;
  end;
end;
{$endif}


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

  {todo Monitor}

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
      tkArray, tkRecord, tkObject: ItemSize := PAnonymousFieldTable(NativeUInt(TypeInfo) + PByte(@TypeInfo.Name)^).Size;
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
    if (Rec.RefCount >= 0) then
    begin
      AtomicIncrement(Rec.RefCount);
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

function IsMemoryFilledZero(Source: PByte; Size: NativeUInt): Boolean;
label
  zero, non_zero;
begin
  if (Size <> 0) then
  begin
    if (Size >= SizeOf(NativeUInt)) then
    repeat
      if (PNativeUInt(Source)^ <> 0) then goto non_zero;
      Dec(Size, SizeOf(NativeUInt));
      Inc(Source, SizeOf(NativeUInt));
    until (Size < SizeOf(NativeUInt));

    {$ifdef LARGEINT}
    if (Size >= SizeOf(Cardinal)) then
    begin
      if (PCardinal(Source)^ <> 0) then goto non_zero;
      Dec(Size, SizeOf(Cardinal));
      Inc(Source, SizeOf(Cardinal));
    end;
    {$endif}

    if (Size >= SizeOf(Word)) then
    begin
      if (PWord(Source)^ <> 0) then goto non_zero;
      Dec(Size, SizeOf(Word));
      Inc(Source, SizeOf(Word));
    end;

    if (Size <> 0) then
    begin
      if (PByte(Source)^ <> 0) then goto non_zero;
    end;

    goto zero;
  end else
  begin
  zero:
    Result := True;
    Exit;
  non_zero:
    Result := False;
  end;
end;

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
  LUA_FUNC_INVALID = __luafunction({$ifdef LUA_NATIVEFUNC}nil{$else}LUA_POINTER_INVALID{$endif});

type
  PLuaNativeFrame = ^TLuaNativeFrame;
  TLuaNativeFrame = record
    Parent: PLuaNativeFrame;
    Name: PShortString;
    Critical: Boolean;
  end;

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

  PLuaCustomStringDictionaryItem = ^TLuaCustomStringDictionaryItem;
  TLuaCustomStringDictionaryItem = record
    HashCode: Integer;
    Value: Pointer;
    Next: Integer;
  end;

  PLuaCustomStringDictionary = ^TLuaCustomStringDictionary;
  TLuaCustomStringDictionaryOnProcessKey = procedure(const Self: PLuaCustomStringDictionary;
    const Chars: PByte; const Length: Integer);

  TLuaCustomStringDictionary = object{TDictionary<LuaString,Pointer>}
  protected
    FLua: TLua;
    FItems: array of TLuaCustomStringDictionaryItem;
    FHashes: array of Integer;
    FHashesMask: NativeInt;
    FCapacity: NativeInt;
    FCount: NativeInt;
    FOnProcessKey: TLuaCustomStringDictionaryOnProcessKey;

    procedure Grow;
    procedure InitBuffer(const Key: LuaString); overload;
    procedure InitBuffer(const RttiName: ShortString); overload;
    function GetHashCode(const Chars: PByte; const Length: Integer): Integer;
    function InternalAdd(const HashCode: Integer; const Value: Pointer): PLuaCustomStringDictionaryItem;
  public
    procedure Clear;
    procedure TrimExcess;

    property Lua: TLua read FLua write FLua;
    property Capacity: NativeInt read FCapacity;
    property Count: NativeInt read FCount;
  end;

  TLuaNames = object(TLuaCustomStringDictionary){TDictionary<LuaString,__luaname>}
  private
    procedure InternalProcessKey(const Chars: PByte; const Length: Integer);
    function InternalFind(const ModeCreate: Boolean): PLuaCustomStringDictionaryItem;
  public
    procedure Init(const ALua: TLua);
    function Add(const Key: LuaString): __luaname; overload;
    function Add(const RttiName: ShortString): __luaname; overload;
    function FindValue(const Key: LuaString): __luaname; overload;
    function FindValue(const RttiName: ShortString): __luaname; overload;
  end;

  PLuaRegisteredTypeName = ^TLuaRegisteredTypeName;
  TLuaRegisteredTypeName = record
    TypeInfo: PTypeInfo;
    PointerDepth: Integer;
    Chars: array[0..0] of Byte;
  end;

  TLuaRegisteredTypeNames = object(TLuaCustomStringDictionary){TDictionary<LuaString,PLuaRegisteredTypeName>}
  private
    procedure InternalProcessKey(const Chars: PByte; const Length: Integer);
    function InternalFind(const Offset: Integer; const ModeCreate: Boolean): PLuaCustomStringDictionaryItem;
  public
    procedure Init(const ALua: TLua);
    procedure Add(const Name: LuaString; const TypeInfo: PTypeInfo; const PointerDepth: Integer);
    function Find(const RttiName: ShortString): PLuaRegisteredTypeName;
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

procedure TLuaCustomStringDictionary.Clear;
begin
  FItems := nil;
  FHashes := nil;
  FHashesMask := 0;
  FCapacity := 0;
  FCount := 0;
end;

procedure TLuaCustomStringDictionary.TrimExcess;
var
  NewItems: array of TLuaCustomStringDictionaryItem;
begin
  SetLength(NewItems, FCount);
  System.Move(Pointer(FItems)^, Pointer(NewItems)^, FCount * SizeOf(TLuaCustomStringDictionaryItem));
  System.FillChar(Pointer(FItems)^, FCount * SizeOf(TLuaCustomStringDictionaryItem), #0);
  SwapPtr(Pointer(FItems), Pointer(NewItems));
  FCapacity := FCount;
end;

procedure TLuaCustomStringDictionary.Grow;
var
  i: NativeInt;
  Item: PLuaCustomStringDictionaryItem;
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

function Utf8FromUnicode(ATarget: PAnsiChar; ASource: PWideChar; ALength: Integer): Integer; forward;

procedure TLuaCustomStringDictionary.InitBuffer(const Key: LuaString);
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

  if (Assigned(FOnProcessKey)) then
    FOnProcessKey(@Self, Pointer(Buffer.FBytes), Buffer.Size);
end;

procedure TLuaCustomStringDictionary.InitBuffer(const RttiName: ShortString);
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
      Size := Length + 3;
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

function TLuaCustomStringDictionary.GetHashCode(const Chars: PByte; const Length: Integer): Integer;
var
  S: PByte;
  X, i: Integer;
begin
  S := Chars;
  X := 63689;
  Result := Length;
  for i := 1 to Length do
  begin
    Result := Result * X + Integer(S^);
    Inc(S);
    X := X * 378551;
  end;

  X := Length;
  if (Length > 255) then X := 255;
  Result := Result and $00ffffff;
  X := X shl 24;
  Inc(Result, X);
end;

function TLuaCustomStringDictionary.InternalAdd(const HashCode: Integer;
  const Value: Pointer): PLuaCustomStringDictionaryItem;
label
  start;
var
  Index: NativeInt;
  Parent: PInteger;
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
    Result.Value := Value;
    Result.Next := Parent^;
    Parent^ := Index;
  end else
  begin
    Grow;
    goto start;
  end;
end;

procedure TLuaNames.Init(const ALua: TLua);
begin
  Clear;
  FLua := ALua;
  Pointer(@FOnProcessKey) := Pointer(@TLuaNames.InternalProcessKey);
end;

procedure TLuaNames.InternalProcessKey(const Chars: PByte; const Length: Integer);
{$if CompilerVersion >= 20}
var
  i: Integer;
  S: PByte;
begin
  S := Chars;
  for i := 1 to Length do
  begin
    case S^ of
      Ord('<'), Ord('>') {$ifdef UNITSCOPENAMES}, Ord('.'){$endif}:
      begin
        S^ := Ord('_');
      end;
    end;
    Inc(S);
  end;
end;
{$else}
begin
end;
{$ifend}

function TLuaNames.InternalFind(const ModeCreate: Boolean): PLuaCustomStringDictionaryItem;
var
  Chars: PByte;
  HashCode: Integer;
  HashesMask: NativeInt;
  Index: NativeInt;
begin
  // chars, hash code
  Chars := Pointer(TLuaBuffer(FLua.FInternalBuffer).FBytes);
  HashCode := GetHashCode(Chars, TLuaBuffer(FLua.FInternalBuffer).Size);

  // find
  HashesMask := FHashesMask;
  if (HashesMask <> 0) then
  begin
    Index := FHashes[NativeInt(HashCode) and HashesMask];
    if (Index >= 0) then
    repeat
      Result := @FItems[Index];
      if (Result.HashCode = HashCode) and (CompareMem(Result.Value, Chars, HashCode shr 24)) then Exit;
      Index := Result.Next;
    until (Index < 0);
  end;

  // not found
  if (ModeCreate) then
  begin
    lua_pushlstring(FLua.Handle, Pointer(Chars), HashCode shr 24);
    Chars := Pointer(lua_tolstring(FLua.Handle, -1, nil));
    FLua.global_fill_value(FLua.global_alloc_ref);
    Result := InternalAdd(HashCode, Chars);
  end else
  begin
    Result := nil;
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
    Result := PLuaCustomStringDictionaryItem(Result).Value;
end;

function TLuaNames.FindValue(const RttiName: ShortString): __luaname;
begin
  InitBuffer(RttiName);
  Result := Pointer(InternalFind(False));
  if (Assigned(Result)) then
    Result := PLuaCustomStringDictionaryItem(Result).Value;
end;

procedure TLuaRegisteredTypeNames.Init(const ALua: TLua);
begin
  Clear;
  FLua := ALua;
  Pointer(@FOnProcessKey) := @TLuaRegisteredTypeNames.InternalProcessKey;
end;

procedure TLuaRegisteredTypeNames.InternalProcessKey(const Chars: PByte; const Length: Integer);
var
  i: Integer;
  S: PByte;
begin
  S := Chars;
  for i := 1 to Length do
  begin
    case S^ of
      Ord('A')..Ord('Z'): S^ := S^ or $20;
    end;
    Inc(S);
  end;
end;

function TLuaRegisteredTypeNames.InternalFind(const Offset: Integer;
  const ModeCreate: Boolean): PLuaCustomStringDictionaryItem;
var
  Chars: PByte;
  HashCode: Integer;
  HashesMask: NativeInt;
  Index: NativeInt;
  Value: PLuaRegisteredTypeName;
begin
  // chars, hash code
  Chars := Pointer(@TLuaBuffer(FLua.FInternalBuffer).FBytes[Offset]);
  HashCode := GetHashCode(Chars, TLuaBuffer(FLua.FInternalBuffer).Size - Offset);

  // find
  HashesMask := FHashesMask;
  if (HashesMask <> 0) then
  begin
    Index := FHashes[NativeInt(HashCode) and HashesMask];
    if (Index >= 0) then
    repeat
      Result := @FItems[Index];
      if (Result.HashCode = HashCode) and
        (CompareMem(@PLuaRegisteredTypeName(Result.Value).Chars, Chars, HashCode shr 24)) then Exit;
      Index := Result.Next;
    until (Index < 0);
  end;

  // not found
  if (ModeCreate) then
  begin
    Value := TLuaMemoryHeap(FLua.FMemoryHeap).Unpack(
      TLuaMemoryHeap(FLua.FMemoryHeap).Alloc(SizeOf(TLuaRegisteredTypeName) -
        SizeOf(Byte) + (HashCode shr 24)));

    Value.TypeInfo := nil;
    Value.PointerDepth := 0;
    System.Move(Chars^, Value.Chars, HashCode shr 24);
    Result := InternalAdd(HashCode, Value);
  end else
  begin
    Result := nil;
  end;
end;

procedure TLuaRegisteredTypeNames.Add(const Name: LuaString; const TypeInfo: PTypeInfo;
  const PointerDepth: Integer);
var
  Value: PLuaRegisteredTypeName;
begin
  InitBuffer(Name);
  Value := InternalFind(0, True).Value;
  Value.TypeInfo := TypeInfo;
  Value.PointerDepth := PointerDepth;
end;

function TLuaRegisteredTypeNames.Find(const RttiName: ShortString): PLuaRegisteredTypeName;
var
  Chars: PByte;
  Offset, Length: Integer;
  AdditionalPointerDepth: Boolean;
  Item: PLuaCustomStringDictionaryItem;
  Value: PLuaRegisteredTypeName;
begin
  InitBuffer(RttiName);
  Chars := Pointer(TLuaBuffer(FLua.FInternalBuffer).FBytes);
  Length := TLuaBuffer(FLua.FInternalBuffer).Size;

  Item := nil;
  Offset := 0;
  AdditionalPointerDepth := False;
  while (Offset <> Length) do
  begin
    Item := InternalFind(Offset, False);
    if (Assigned(Item)) then
      Break{found};

    if (Chars^ <> Ord('p')) then
      Break{not found};

    Chars^ := Ord('t');
    Item := InternalFind(Offset, False);
    if (Assigned(Item)) then
    begin
      AdditionalPointerDepth := True;
      Break{found: additional mode};
    end;

    Chars^ := Ord('i');
    Item := InternalFind(Offset, False);
    if (Assigned(Item)) then
    begin
      AdditionalPointerDepth := True;
      Break{found: additional mode};
    end;

    Inc(Chars);
    Inc(Offset);
  end;

  if (not Assigned(Item)) then
  begin
    Result := nil;
    Exit;
  end;

  Result := Item.Value;
  if (AdditionalPointerDepth) then
  begin
    Chars^ := Ord('p');
    Value := InternalFind(Offset, True).Value;
    Value.TypeInfo := Result.TypeInfo;
    Value.PointerDepth := Result.PointerDepth + 1;
    Result := Value;
  end;

  while (Offset <> 0) do
  begin
    Dec(Chars);

    Chars^ := Ord('p');
    Value := InternalFind(Offset, True).Value;
    Value.TypeInfo := Result.TypeInfo;
    Value.PointerDepth := Result.PointerDepth + 1;
    Result := Value;

    Dec(Offset);
  end;
end;


{ TLuaArguments }

procedure TLuaArguments.GrowSetArgsCount(const AValue: Integer; const AIndex: NativeInt);
var
  LArgs: PLuaArgs;
  LCapacity: PInteger;
  LNewCapacity: Integer;
begin
  LArgs := @F.Args[AIndex];
  LCapacity := @F.Capacities[AIndex];

  LNewCapacity := (AValue + 15) and -16;
  ReallocMem(LArgs.Items, LNewCapacity * SizeOf(TLuaArg));
  FillChar(LArgs.Items[LCapacity^], (LNewCapacity - LCapacity^) * SizeOf(TLuaArg), #0);
  LCapacity^ := LNewCapacity;

  LArgs.Count := AValue;
end;

procedure TLuaArguments.SetArgsCount(const AValue: Integer; const AIndex: NativeInt);
var
  LArgs: PLuaArgs;
  LCapacity: PInteger;
  LCount: Integer;
  LArg, LTopArg: PLuaArg;
begin
  LArgs := @F.Args[AIndex];
  LCapacity := @F.Capacities[AIndex];

  LCount := LArgs.Count;
  if (AValue >= LCount) then
  begin
    if (AValue <= LCapacity^) then
    begin
      LArgs.Count := AValue;
    end else
    begin
      GrowSetArgsCount(AValue, AIndex);
    end;
  end else
  begin
    LTopArg := @LArgs.Items[LCount];
    if (AValue <= 0) then
    begin
      LArgs.Count := 0;
      LArg := Pointer(LArgs.Items);
    end else
    begin
      LArgs.Count := AValue;
      LArg := @LArgs.Items[AValue];
    end;

    repeat
      if (LArg.F.LuaType >= ltString) then
      begin
        if (Pointer(LArg.FInterface) <> nil) then
        begin
          LArg.FInterface := nil;
        end;
        {$ifdef AUTOREFCOUNT}
        if (Pointer(LArg.FObject) <> nil) then
        begin
          LArg.FObject := nil;
        end;
        {$endif}
        if (Pointer(LArg.FString) <> nil) then
        begin
          LArg.FString := '';
        end;
      end;
      PInteger(@LArg.F.LuaType)^ := 0;

      Inc(LArg);
    until (LArg = LTopArg);
  end;
end;

procedure TLuaArguments.SetParamCount(const AValue: Integer);
{$ifdef INLINESUPPORT}
begin
  SetArgsCount(AValue, 0);
end;
{$else .CPUX86}
asm
  xor ecx, ecx
  jmp TLuaArguments.SetArgsCount
end;
{$endif}

{$ifdef INLINESUPPORT}
procedure TLuaArguments.SetResultCount(const AValue: Integer);
begin
  SetArgsCount(AValue, 1);
end;
{$else .CPUX86}
asm
  mov ecx, 1
  jmp TLuaArguments.SetArgsCount
end;
{$endif}

function TLuaArguments.GrowAdd: PLuaArg;
begin
  ResultCount := ResultCount + 1;
  Result := @F.ResultArgs.Items[ResultCount - 1];
end;

type
  PAllocatedMetaType = ^TAllocatedMetaType;
  TAllocatedMetaType = record
    MetaType: PLuaMetaType;
    Next: PAllocatedMetaType;
  end;

function TLuaArguments.GrowAlloc(const ASize: NativeUInt): Pointer;
var
  LHeap: ^TLuaMemoryHeap;
  LFirst: Boolean;
  LPtr: NativeUInt;
begin
  LHeap := Pointer(@FHeap);
  LFirst := (LHeap.FBuffers = nil);
  Result := LHeap.Unpack(LHeap.GrowAlloc(ASize));

  LPtr := NativeUInt(Result) + ASize;
  NativeUInt(FHeapCurrent) := LPtr;
  NativeUInt(FHeapOverflow) := LPtr + NativeUInt(LHeap.FMargin);

  if (LFirst) then
  begin
    FHeapEmpty := LHeap.FCurrent - NativeInt(ASize);
  end;
end;

function TLuaArguments.AllocMetaType(const AMetaType: PLuaMetaType): Pointer;
var
  LSize: NativeUInt;
  LPtr: NativeUInt;
  LAllocatedMetaType: PAllocatedMetaType;
begin
  // size
  LSize := NativeUInt((AMetaType.Size + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer));

  // allocate
  Inc(LSize, SizeOf(TAllocatedMetaType));
  LPtr := NativeUInt(FHeapCurrent);
  Inc(LPtr, LSize);
  if (LPtr <= NativeUInt(FHeapOverflow)) then
  begin
    NativeUInt(FHeapOverflow) := LPtr;
    Dec(LPtr, LSize);
    LAllocatedMetaType := Pointer(LPtr);
  end else
  begin
    LAllocatedMetaType := GrowAlloc(LSize);
  end;

  // include
  LAllocatedMetaType.MetaType := AMetaType;
  LAllocatedMetaType.Next := FAllocatedMetaTypes;
  FAllocatedMetaTypes := LAllocatedMetaType;

  // result
  Dec(LSize, SizeOf(TAllocatedMetaType));
  Inc(LAllocatedMetaType);
  if (LSize = SizeOf(Pointer)) then
  begin
    PNativeUInt(LAllocatedMetaType)^ := 0;
  end else
  begin
    FillChar(LAllocatedMetaType^, LSize, #0);
  end;
  Result := LAllocatedMetaType;
end;

procedure TLuaArguments.Clear;
var
  LAllocatedMetaType: PAllocatedMetaType;
  LTypeInfo: PTypeInfo;
  LCount: Integer;
  LMetaType: PLuaMetaType;
  LHeap: ^TLuaMemoryHeap;
  LDynArrayRec: PDynArrayRec;
begin
  // arguments
  if (F.ParamArgs.Count <> 0) then
  begin
    SetParamCount(0);
  end;
  if (F.ResultArgs.Count <> 0) then
  begin
    SetResultCount(0);
  end;

  // allocated meta types
  repeat
    LAllocatedMetaType := FAllocatedMetaTypes;
    if (not Assigned(LAllocatedMetaType)) then Break;
    FAllocatedMetaTypes := LAllocatedMetaType.Next;

    LTypeInfo := nil;
    LCount := 1;
    LMetaType := LAllocatedMetaType.MetaType;
    case LMetaType.F.Kind of
      mtRecord:
      begin
        LTypeInfo := PLuaRecordInfo(LMetaType).F.TypeInfo;
      end;
      mtArray:
      begin
        LTypeInfo := PLuaArrayInfo(LMetaType).FFinalTypeInfo;
        LCount := PLuaArrayInfo(LMetaType).FFinalItemsCount;
      end;
    end;

    if (LTypeInfo <> nil) then
    begin
      Inc(LAllocatedMetaType);
      FinalizeArray(LAllocatedMetaType, LTypeInfo, LCount);
    end;
  until (False);

  // memory heap
  LHeap := Pointer(@FHeap);
  if (LHeap.FCurrent <> FHeapEmpty) then
  begin
    LDynArrayRec := Pointer(NativeUInt(LHeap.FBuffers) - SizeOf(TDynArrayRec));
    if (LDynArrayRec.Length <> 1) then SetLength(LHeap.FBuffers, 1);
    LDynArrayRec := Pointer(NativeUInt(LHeap.FBuffers[0]) - SizeOf(TDynArrayRec));
    LHeap.FCurrent := FHeapEmpty;
    LHeap.FMargin := LDynArrayRec.Length;
    FHeapCurrent := Pointer(LHeap.FBuffers[0]);
    NativeUInt(FHeapOverflow) := NativeUInt(FHeapCurrent) + NativeUInt(LHeap.FMargin);
  end;
end;

procedure TLuaArguments.Finalize;
begin
  Clear;
  TLuaMemoryHeap(FHeap).Clear;
  if (Assigned(F.ParamArgs.Items)) then
  begin
    FreeMem(F.ParamArgs.Items);
  end;
  if (Assigned(F.ResultArgs.Items)) then
  begin
    FreeMem(F.ResultArgs.Items);
  end
end;

{$ifdef RETURNADDRESS}
function TLuaArguments.AllocRecord(const AInfo: PLuaRecordInfo): Pointer;
{$else}
function TLuaArgumentsAllocRecord(const Self: TLuaArg; const AInfo: PLuaRecordInfo; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  AInfo.CheckInstance(mtRecord, ReturnAddress);
  Result := Self.AllocMetaType(AInfo);
end;

{$ifNdef RETURNADDRESS}
function TLuaArguments.AllocRecord: Boolean;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgumentsAllocRecord
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArguments.AllocArray(const AInfo: PLuaArrayInfo): Pointer;
{$else}
function TLuaArgumentsAllocArray(const Self: TLuaArg; const AInfo: PLuaArrayInfo; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  AInfo.CheckInstance(mtArray, ReturnAddress);
  Result := Self.AllocMetaType(AInfo);
end;

{$ifNdef RETURNADDRESS}
function TLuaArguments.AllocArray: Boolean;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgumentsAllocArray
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArguments.AllocSet(const AInfo: PLuaSetInfo): Pointer;
{$else}
function TLuaArgumentsAllocSet(const Self: TLuaArg; const AInfo: PLuaSetInfo; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  AInfo.CheckInstance(mtSet, ReturnAddress);
  Result := Self.AllocMetaType(AInfo);
end;

{$ifNdef RETURNADDRESS}
function TLuaArguments.AllocSet: Boolean;
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} .NOFRAME
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgumentsAllocSet
end;
{$endif}

function TLuaArguments.Add: PLuaArg;
var
  LCount: Integer;
begin
  LCount := F.ResultArgs.Count;
  if (LCount = F.ResultCapacity) then
  begin
    Result := GrowAdd;
  end else
  begin
    Inc(LCount);
    F.ResultArgs.Count := LCount;
    Dec(LCount);
    Result := @F.ResultArgs.Items[LCount];
  end;
end;

function TLuaArguments.AddRecord(const AInfo: PLuaRecordInfo): Pointer;
var
  LValue: TLuaRecord;
begin
  LValue.Info := AInfo;
  LValue.Data := AllocRecord(AInfo);
  LValue.IsRef := False;
  LValue.IsConst := False;

  Add.AsRecord := LValue;
  Result := LValue.Data;
end;

function TLuaArguments.AddArray(const AInfo: PLuaArrayInfo): Pointer;
var
  LValue: TLuaArray;
begin
  LValue.Info := AInfo;
  LValue.Data := AllocArray(AInfo);
  LValue.IsRef := False;
  LValue.IsConst := False;

  Add.AsArray := LValue;
  Result := LValue.Data;
end;

function TLuaArguments.AddSet(const AInfo: PLuaSetInfo): Pointer;
var
  LValue: TLuaSet;
begin
  LValue.Info := AInfo;
  LValue.Data := AllocSet(AInfo);
  LValue.IsRef := False;
  LValue.IsConst := False;

  Add.AsSet := LValue;
  Result := LValue.Data;
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
    if (Pointer(FInterface) <> nil) then
      FInterface := nil;
    {$ifdef AUTOREFCOUNT}
    if (Pointer(FObject) <> nil) then
      FObject := nil;
    {$endif}
    if (Pointer(FString) <> nil) then
      FString := '';

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
  Result := Self.FString;
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
  FString := Value;
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
      0: Self.FString := LuaString(DateTimeToStr(Value));
      1: Self.FString := LuaString(DateToStr(Value));
      2: Self.FString := LuaString(TimeToStr(Value));
    else
      Self.FString := '';
    end;
  end;

  {$ifdef UNICODE}
  procedure UnicodeValue(const Self: PLuaArg; const Value: UnicodeString); far;
  begin
    Self.FString := LuaString(Value);
  end;
  {$endif}

  {$ifNdef NEXTGEN}
  procedure AnsiValue(const Self: PLuaArg; const Value: AnsiString); far;
  begin
    Self.FString := LuaString(Value);
  end;

  procedure WideValue(const Self: PLuaArg; const Value: WideString); far;
  begin
    Self.FString := LuaString(Value);
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
       Result := (Pointer(FString) <> nil) and
         (CastStringAsBoolean(PLuaChar(Pointer(FString)),
          PInteger(NativeInt(FString) - SizeOf(Integer))^ {$ifdef LUA_LENGTH_SHIFT}shr 1{$endif}) > 0);
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
     ltString: Result := StringDefault(FString);
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
     ltString: Result := StringDefault(FString);
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
     ltString: Result := FString;
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
    ltString: Result := Pointer(FString);
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

                 if (Pointer(FString) <> nil) then
                 begin
                  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
                    {$ifdef UNICODE}
                      UnicodeString(VarData.VUString) := FString;
                     {$else}
                      WideString(VarData.VOleStr) := FString;
                    {$endif}
                  {$else}
                    AnsiString(VarData.VString) := FString;
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

function LuaArgDynArray(const Count: Integer): TLuaArgDynArray;
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

{ TLuaVariable }

destructor TLuaVariable.Destroy;
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
procedure TLuaVariable.Initialize(const ALua: TLua);
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

procedure TLuaVariable.ThrowLocked(const Operation: string; const CodeAddr: pointer);
begin
  ELua.Assert('Can''t %s, because the reference is locked by table value', [Operation], CodeAddr);
end;

procedure TLuaVariable.ThrowValueType(const CodeAddr: pointer);
begin
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
end;

procedure __TLuaVariableGetValue(const Self: TLuaVariable; var Result: Variant; const ReturnAddr: pointer);
begin
  if (Self.Locked) then Self.ThrowLocked('get value', ReturnAddr);

  with Self.Lua do
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Self.Index);
    stack_variant(Result, -1);
    lua_settop(Handle, -1-1);
  end;
end;

function  TLuaVariable.GetValue(): Variant;
asm
  mov ecx, [esp]
  jmp __TLuaVariableGetValue
end;

procedure __TLuaVariableSetValue(const Self: TLuaVariable; const NewValue: Variant; const ReturnAddr: pointer);
begin
  with Self do
  begin
    if (Locked) then ThrowLocked('change value', ReturnAddr);
    if (not Lua.push_variant(NewValue)) then ThrowValueType(ReturnAddr);
    lua_rawseti(Lua.Handle, LUA_REGISTRYINDEX, Index);
  end;
end;

procedure TLuaVariable.SetValue(const NewValue: Variant);
asm
  mov ecx, [esp]
  jmp __TLuaVariableSetValue
end;

procedure __TLuaVariableGetValueEx(const Self: TLuaVariable; var Result: TLuaArg; const ReturnAddr: pointer);
begin
  if (Self.Locked) then Self.ThrowLocked('get value', ReturnAddr);
  with Self.Lua do
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Self.Index);
    stack_luaarg(Result, -1, false);
    lua_settop(Handle, -1-1);
  end;
end;

function  TLuaVariable.GetValueEx(): TLuaArg;
asm
  mov ecx, [esp]
  jmp __TLuaVariableGetValueEx
end;

procedure __TLuaVariableSetValueEx(const Self: TLuaVariable; const NewValue: TLuaArg; const ReturnAddr: pointer);
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

procedure TLuaVariable.SetValueEx(const NewValue: TLuaArg);
asm
  mov ecx, [esp]
  jmp __TLuaVariableSetValueEx
end;
           *)
             (*
function __TLuaVariableAsTableBegin(const Self: TLuaVariable; var Table: PLuaTable; const ReturnAddr: pointer): boolean;
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

function TLuaVariable.AsTableBegin(var Table: PLuaTable): boolean;
asm
  mov ecx, [esp]
  jmp __TLuaVariableAsTableBegin
end;

function __TLuaVariableAsTableEnd(const Self: TLuaVariable; var Table: PLuaTable; const ReturnAddr: pointer): boolean;
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

function TLuaVariable.AsTableEnd(var Table: PLuaTable): boolean;
asm
  mov ecx, [esp]
  jmp __TLuaVariableAsTableEnd
end;
          *)


{ TLuaMetaType }

type
  TLuaPropertyOptions = packed record
    MetaType: PLuaMetaType;
    TypeInfo: Pointer;
    Getter: TLuaPropAccess;
    IsGetterVirtual: Boolean;
    Setter: TLuaPropAccess;
    IsSetterVirtual: Boolean;
    IsComplex: Boolean;
    IsDefault: Boolean;
    VirtualAddressHint: Boolean;
    ConstRefHint: Boolean;
  case Integer of
    0: (Index: Integer);
    1: (ComplexPtr: __luapointer);
  end;
  PLuaPropertyOptions = ^TLuaPropertyOptions;

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
  PLuaCallOptions = ^TLuaCallOptions;


{.$ifdef SMALLINT}
function TLuaMetaType.GetPtr: __luapointer;
begin
  if (not Assigned(@Self)) then
  begin
    Result := __luapointer(-1){LUA_POINTER_INVALID};
  end else
  begin
    {$ifdef SMALLINT}
    Result := __luapointer(@Self);
    {$else .LARGEINT}
    Result := F.Ptr;
    {$endif}
  end;
end;
{.$endif}

procedure TLuaMetaType.CheckInstance(const AKind: TLuaMetaKind; const ReturnAddress: Pointer);
const
  KINDS: array[TLuaMetaKind] of string = ('class', 'interface', 'record', 'array', 'set');
begin
  if (NativeUInt(@Self) <= $FFFF) or (NativeInt(@Self) and (SizeOf(Pointer) - 1) <> 0) or
    (F.Marker <> LUA_METATYPE_MARKER) or (F.Kind <> AKind) then
    raise ELua.CreateFmt('Invalid %s instance: %p', [KINDS[AKind], @Self]) at ReturnAddress;
end;

procedure TLuaMetaType.FillManagedValue;
var
  Value: Boolean;
begin
  case F.Kind of
    {$ifdef AUTOREFCOUNT}
    mtClass,
    {$endif}
    mtInterface: Value := True;
    mtRecord:
    begin
      Value := (Assigned(F.TypeInfo)) and (IsManagedTypeInfo(F.TypeInfo));
    end;
    mtArray:
    begin
      Value := Assigned(PLuaArrayInfo(@Self).FFinalTypeInfo);
    end;
  else
    Value := False;
  end;

  F.Managed := Value;
end;

procedure TLuaMetaType.FillHFAValue;
{$ifdef CPUARM64}
  function GetRecordHFA(const ATypeInfo: PTypeInfo): Boolean; far;
  var
    Context: TRTTIContext;
  begin
    Result := Context.GetType(ATypeInfo).IsHFA;
  end;
begin
  F.HFA := (F.Kind = mtRecord) and (Assigned(F.TypeInfo)) and GetRecordHFA(F.TypeInfo);
end;
{$else}
begin
  F.HFA := False;
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

function TLuaMetaType.GetReturnReferenced: Boolean;
begin
  case F.Kind of
    {$ifdef AUTOREFCOUNT}
    mtClass,
    {$endif}
    mtInterface:
    begin
      Result := True;
    end;
    mtRecord:
    begin
      {$if Defined(CPUX64)}
        case F.Size of
          1, 2, 4: Result := False;
          8: Result := F.Managed;
        else
          Result := True;
        end;
      {$elseif Defined(CPUX86)}
        case F.Size of
          1, 2: Result := False;
          4: Result := F.Managed;
        else
          Result := True;
        end;
      {$elseif Defined(CPUARM32)}
        case F.Size of
          {$if Defined(ANDROID32)}
          1..4: Result := False;
          {$elseif Defined(IOS32)}
          1: Result := False;
          {$ifend}
        else
          Result := True;
        end;
      {$elseif Defined(CPUARM64)}
        if (F.HFA) then
          Result := False
        else
          Result := (F.Size < 1) or (F.Size > 16);
      {$ifend}
    end;
    mtArray, mtSet:
    begin
      Result := ((F.Kind = mtArray) and (PLuaArrayInfo(@Self).IsDynamic)) or
        (F.Size > 255) or (not (Byte(F.Size) in [
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

function TLuaMetaType.ParamReferenced(const ACallConv: TCallConv; const AIsConst: Boolean): Boolean;
begin
  Result := False;

  case F.Kind of
    mtRecord:
    begin
      {$if Defined(CPUX86)}
        if (ACallConv in [ccCdecl, ccStdCall, ccSafeCall]) and (not AIsConst) then
          Result := False
        else
      {$elseif (not Defined(CPUX64)) or (not Defined(MSWINDOWS))}
        if (ACallConv in [ccReg, ccPascal]) then
          Result := True
        else
      {$ifend}

      {$ifdef CPUARM64}
        if (F.HFA) then
          Result := False
        else
      {$endif}

      case F.Size of
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
        Result := True;
      end;
    end;
    mtArray:
    begin
      if (not PLuaArrayInfo(@Self).IsDynamic) then
      begin
        {$if (not Defined(CPUX86)) and ((not Defined(CPUX64)) or (not Defined(MSWINDOWS)))}
          if (ACallConv in [ccReg, ccPascal]) then
            Result := True
          else
        {$ifend}
        {$ifdef CPUARM64}
          if (F.HFA) then
            Result := False
          else
        {$endif}

        case F.Size of
          {$if Defined(CPUX86) or (Defined(CPUX64) and Defined(MSWINDOWS))}
          1..SizeOf(Pointer)
          {$else}
          1..16
          {$ifend}
          : ;
        else
          Result := True;
        end;
      end;
    end;
    mtSet:
    begin
      Result := (F.Size > SizeOf(Pointer));
    end;
  end;
end;

procedure TLuaMetaType.Dispose(const Value: Pointer{/TObject/IInterface});
begin
  case F.Kind of
    mtClass:
    begin
      TObject(Value).{$IFDEF AUTOREFCOUNT}DisposeOf{$else}Free{$endif};
    end;
    mtInterface:
    begin
      if (Assigned(Value)) then
        IInterface(Value)._Release;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
        FinalizeArray(Value, F.TypeInfo, 1);
    end;
    mtArray:
    begin
      if (Self.Managed) then
        FinalizeArray(Value, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
    end;
    mtSet: ;
  end;
end;

procedure TLuaMetaType.Clear(var Instance; const FillZero: Boolean);
begin
  case F.Kind of
    mtClass:
    begin
      TObject(Instance).CleanupInstance;
      if (FillZero) then
      begin
        FillChar(PPointer(NativeInt(Instance) + SizeOf(Pointer))^,
          TObject(Instance).InstanceSize - (SizeOf(Pointer) + hfFieldSize), #0);
      end;
      Exit;
    end;
    mtInterface:
    begin
      IInterface(Instance) := nil;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
        FinalizeArray(@Instance, F.TypeInfo, 1);
    end;
    mtArray:
    begin
      if (Self.Managed) then
        FinalizeArray(@Instance, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
    end;
    mtSet: ;
  end;

  if (FillZero) then
  begin
    FillChar(Instance, F.Size, #0);
  end;
end;

procedure TLuaMetaType.Assign(var Instance; const Value: Pointer{/TObject/IInterface});
var
  Obj: ^TObject;
begin
  case F.Kind of
    mtClass:
    begin
      Obj := Pointer(@Instance);
      if (not Assigned(Obj^)) then
      begin
        Obj^ := TClass(PPointer(Value)^).NewInstance;
      end;
      CopyObject(Obj^, TObject(Value));
      Exit;
    end;
    mtInterface:
    begin
      IInterface(Instance) := IInterface(Value);
      Exit;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
      begin
        CopyArray(@Instance, Value, F.TypeInfo, 1);
        Exit;
      end;
    end;
    mtArray:
    begin
      if (Self.Managed) then
      begin
        CopyArray(@Instance, Value, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
        Exit;
      end;
    end;
    mtSet: ;
  end;

  System.Move(Value^, Instance, F.Size);
end;


{ TLuaRecordInfo }

const
  RECORD_METHOD_KINDS: array[Boolean{IsStatic}] of TLuaMethodKind = (mkInstance, mkStatic);

procedure __TLuaRecordInfoRegUniversalMethod(const Self: TLuaRecordInfo; const MethodName: LuaString;
  const Method: TLuaMethodCallback; const MinArgsCount, MaxArgsCount: Word; const IsStatic: Boolean);
begin
  Self.FLua.InternalAddMethod(@Self, MethodName, @Method, RECORD_METHOD_KINDS[IsStatic],
    LUA_POINTER_INVALID, True, MinArgsCount, MaxArgsCount);
end;

procedure TLuaRecordInfo.RegMethod(const MethodName: LuaString; const Callback: TLuaMethodCallback;
  const MinArgsCount, MaxArgsCount: Word; const IsStatic: Boolean);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegUniversalMethod(Self, MethodName, Callback, MinArgsCount, MaxArgsCount, IsStatic);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegUniversalMethod
end;
{$endif}

procedure __TLuaRecordInfoRegTypeInfoMethod(const Self: TLuaRecordInfo; const MethodName: LuaString;
  const Address: Pointer; const TypeInfo: PTypeInfo; const IsStatic: Boolean);
var
  Invokable: __luapointer;
begin
  Invokable := Self.FLua.InternalBuildInvokable(@Self, TypeInfo, RECORD_METHOD_KINDS[IsStatic], True);
  if (Invokable <> LUA_POINTER_INVALID) then
  begin
    Self.FLua.InternalAddMethod(@Self, MethodName, Address, RECORD_METHOD_KINDS[IsStatic], Invokable, True);
  end;
end;

procedure TLuaRecordInfo.RegMethod(const MethodName: LuaString; const Address: Pointer;
  const TypeInfo: PTypeInfo; const IsStatic: Boolean);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegTypeInfoMethod(Self, MethodName, Address, TypeInfo, IsStatic);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegUniversalMethod
end;
{$endif}

procedure __TLuaRecordInfoRegCustomMethod(const Self: TLuaRecordInfo; const MethodName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const IsResultUnsafe, IsStatic: Boolean; const CallConv: TCallConv);
var
  Invokable: __luapointer;
begin
  Invokable := Self.FLua.InternalBuildInvokable(@Self, MethodName, Params,
    ResultType, IsResultUnsafe, RECORD_METHOD_KINDS[IsStatic], CallConv, True);
  if (Invokable <> LUA_POINTER_INVALID) then
  begin
    Self.FLua.InternalAddMethod(@Self, MethodName, Address, RECORD_METHOD_KINDS[IsStatic], Invokable, True);
  end;
end;

procedure TLuaRecordInfo.RegMethod(const MethodName: LuaString; const Address: Pointer;
  const Params: array of TLuaProcParam; const ResultType: PTypeInfo;
  const IsResultUnsafe, IsStatic: Boolean; const CallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegCustomMethod(Self, MethodName, Address, Params, ResultType, IsResultUnsafe, IsStatic, CallConv);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegCustomMethod
end;
{$endif}

procedure __TLuaRecordInfoInternalRegField(const ASelf: TLuaRecordInfo;
  const AName: LuaString; const AOffset: NativeUInt;
  const ATypeInfo: Pointer; const AReference: TLuaReference);
begin
  ASelf.CheckInstance(mtRecord, ASelf.FLua.FReturnAddress);
  ASelf.FLua.InternalAddField(@ASelf, AName, AOffset, ATypeInfo, AReference, False, True, True);
end;

procedure TLuaRecordInfo.RegField(const AName: LuaString; const AOffset: NativeUInt;
  const ATypeInfo: Pointer; const AReference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoInternalRegField(Self, AName, AOffset, ATypeInfo, AReference);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoInternalRegField
end;
{$endif}

procedure __TLuaRecordInfoInternalRegFieldEx(const ASelf: TLuaRecordInfo;
  const AName: LuaString; const AField: Pointer;
  const ATypeInfo: Pointer; const ARecord: Pointer; const AReference: TLuaReference);
begin
  __TLuaRecordInfoInternalRegField(ASelf, AName, NativeUInt(AField) - NativeUInt(ARecord), ATypeInfo, AReference);
end;

procedure TLuaRecordInfo.RegField(const AName: LuaString; const AField: Pointer;
  const ATypeInfo: Pointer; const ARecord: Pointer; const AReference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoInternalRegFieldEx(Self, AName, AField, ATypeInfo, ARecord, AReference);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoInternalRegFieldEx
end;
{$endif}

procedure __TLuaRecordInfoInternalRegClassField(const ASelf: TLuaRecordInfo;
  const AName: LuaString; const AAddress: Pointer;
  const ATypeInfo: Pointer; const AReference: TLuaReference);
begin
  ASelf.CheckInstance(mtRecord, ASelf.FLua.FReturnAddress);
  ASelf.FLua.InternalAddField(@ASelf, AName, NativeUInt(AAddress), ATypeInfo, AReference, True, True, True);
end;

procedure TLuaRecordInfo.RegClassField(const AName: LuaString; const AAddress: Pointer;
  const ATypeInfo: Pointer; const AReference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoInternalRegClassField(Self, AName, AAddress, ATypeInfo, AReference);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoInternalRegClassField
end;
{$endif}

procedure __TLuaRecordInfoRegProperty(const ASelf: TLuaRecordInfo;
  const AName: LuaString; const AGetter, ASetter: TLuaPropAccess;
  const ATypeInfo: PTypeInfo; const AConstRefHint: Boolean; const AIndex: Integer);
var
  LOptions: TLuaPropertyOptions;
begin
  ASelf.CheckInstance(mtRecord, ASelf.FLua.FReturnAddress);

  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.MetaType := @ASelf;
  LOptions.TypeInfo := ATypeInfo;
  LOptions.ConstRefHint := AConstRefHint;
  LOptions.Index := AIndex;
  LOptions.Getter := AGetter;
  LOptions.Setter := ASetter;

  ASelf.FLua.InternalAddProperty(AName, @LOptions, True, True);
end;

procedure TLuaRecordInfo.RegProperty(const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const ATypeInfo: PTypeInfo;
  const AConstRefHint: Boolean; const AIndex: Integer);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegProperty(Self, AName, AGetter, ASetter, ATypeInfo, AConstRefHint, AIndex);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegProperty
end;
{$endif}

procedure __TLuaRecordInfoRegComplexProperty(const ASelf: TLuaRecordInfo; const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam;
  const ATypeInfo: PTypeInfo; const ADefault, AConstRefHint: Boolean; const AIndex: Integer);
var
  LOptions: TLuaPropertyOptions;
begin
  ASelf.CheckInstance(mtRecord, ASelf.FLua.FReturnAddress);

  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.MetaType := @ASelf;
  LOptions.TypeInfo := ATypeInfo;
  LOptions.IsComplex := True;
  LOptions.IsDefault := ADefault;
  LOptions.VirtualAddressHint := True;
  LOptions.ConstRefHint := AConstRefHint;
  LOptions.Getter := AGetter;
  LOptions.Setter := ASetter;

  //ToDo AArguments/AIndex
  LOptions.ComplexPtr := LUA_POINTER_INVALID;

  ASelf.FLua.InternalAddProperty(AName, @LOptions, True, True);
end;

procedure TLuaRecordInfo.RegComplexProperty(const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam;
  const ATypeInfo: PTypeInfo; const ADefault, AConstRefHint: Boolean; const AIndex: Integer);
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegComplexProperty(Self, AName, AGetter, ASetter, AArguments,
     ATypeInfo, ADefault, AConstRefHint, AIndex);
end;
{$else}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} .NOFRAME
  mov rax, [RCX].TLuaRecordInfo.FLua
  push [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegComplexProperty
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

{ TLuaInterfaceInfo }

function TLuaInterfaceInfo.InheritsFrom(const Value: PLuaInterfaceInfo): Boolean;
var
  TempPtr, ValuePtr: __luapointer;
begin
  Result := (@Self = Value);
  if (Result) then Exit;

  ValuePtr := Value.Ptr;
  TempPtr := Self.Parent;
  if (TempPtr <> LUA_POINTER_INVALID) then
  repeat
    Result := (TempPtr = ValuePtr);
    if (Result) then Exit;

    TempPtr := PLuaInterfaceInfo({$ifdef LARGEINT}TLuaMemoryHeap(FLua.FMemoryHeap).Unpack{$endif}(TempPtr)).Parent;
  until (TempPtr = LUA_POINTER_INVALID);

  Result := False;
end;


{ TLuaClassInfo }

function TLuaClassInfo.VmtOffset(const Proc: Pointer; const StandardProcs: Boolean): NativeInt;
const
  FIRST_VMT: array[Boolean] of NativeInt = (
    {$ifdef FPC}vmtToString + SizeOf(Pointer){$else .DELPHI}0{$endif},
    {$ifdef FPC}vmtMethodStart{$else .DELPHI}vmtParent + SizeOf(Pointer){$endif});
var
  VMT, Top: NativeUInt;
  Start, Finish, Value: NativeUInt;
begin
  if (Assigned(Proc)) then
  begin
    VMT := NativeUInt(ClassType);
    Start := VMT;
    Finish := VMT;
    Inc(Start, {$ifdef FPC}vmtClassName{$else .DELPHI}(vmtSelfPtr + SizeOf(Pointer)){$endif});
    Inc(Finish, {$ifdef FPC}vmtMsgStrPtr{$else .DELPHI}vmtClassName{$endif});

    Top := High(NativeUInt);
    repeat
      Value := PNativeUInt(Start)^;
      Inc(Start, SizeOf(Pointer));
      if (Value >= VMT) and (Value < Top) then Top := Value;
    until (Start > Finish);

    Top := NativeUInt(NativeInt(Top) and -SizeOf(Pointer));
    Start := VMT;
    Inc(NativeInt(VMT), FIRST_VMT[StandardProcs]);
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

const
  mtComplexProperty = TLuaMetaKind(Succ(High(TLuaMetaKind)));

type
  TLuaUserData = object
  public
    Instance: Pointer;

    Kind: TLuaMetaKind;
    Owner: Boolean;
    Constant: Boolean;
    Counts: Byte; // Count(4bits) and Filled(4bits) for complex properties

    MetaType: __luapointer;
    // complex property?

    procedure GetDescription(var Result: string; const Lua: TLua);
  end;
  PLuaUserData = ^TLuaUserData;


procedure TLuaUserData.GetDescription(var Result: string; const Lua: TLua);
begin
  if (@Self = nil) then
  begin
    Result := 'nil userdata';
  end else
  if (Byte(Kind) > Byte(mtComplexProperty)) then
  begin
    Result := 'unknown user data';
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
      ukProperty: Result := Format('complex property ''%s''', [userdata.PropertyInfo.PropertyName]);
    end; *)
  end;
end;


{ Internal types management, like Rtti.TValue routine }

const
  EXTENDED_GETTER_MASK = 3{TLuaExtendedGetter};
  EXTENDED_SETTER_MASK = 7{TLuaExtendedSetter};

  EXTFLAGS_META_POINTER_MODE = 1 shl 0;
  EXTFLAGS_GETTER_SHIFT = 1;
  EXTFLAGS_GETTER_MASK = EXTENDED_GETTER_MASK shl EXTFLAGS_GETTER_SHIFT;
  EXTFLAGS_SETTER_SHIFT = 5;
  EXTFLAGS_SETTER_MASK = EXTENDED_SETTER_MASK shl EXTFLAGS_SETTER_SHIFT;
  EXTFLAGS_GETTER_CONSTANT_MODE = 1 shl 3;
  EXTFLAGS_GETTER_TEMPORARY_MODE = 1 shl 4;
  EXTFLAGS_GETTER_OWNED_MODE = 2{egOwned} shl EXTFLAGS_GETTER_SHIFT;
  EXTFLAGS_GETTER_HFA_VALUES = 3{gmHFAOwned} shl EXTFLAGS_GETTER_SHIFT;

  OPTIONS_SHIFT = 24;
  OPTIONS_META_POINTER_MODE = EXTFLAGS_META_POINTER_MODE shl OPTIONS_SHIFT;
  OPTIONS_GETTER_SHIFT = EXTFLAGS_GETTER_SHIFT + OPTIONS_SHIFT;
  OPTIONS_GETTER_MASK = EXTFLAGS_GETTER_MASK shl OPTIONS_SHIFT;
  OPTIONS_SETTER_SHIFT = EXTFLAGS_SETTER_SHIFT + OPTIONS_SHIFT;
  OPTIONS_SETTER_MASK = EXTFLAGS_SETTER_MASK shl OPTIONS_SHIFT;
  OPTIONS_GETTER_CONSTANT_MODE = EXTFLAGS_GETTER_CONSTANT_MODE shl OPTIONS_SHIFT;
  OPTIONS_GETTER_TEMPORARY_MODE = EXTFLAGS_GETTER_TEMPORARY_MODE shl OPTIONS_SHIFT;
  OPTIONS_GETTER_OWNED_MODE = EXTFLAGS_GETTER_OWNED_MODE shl OPTIONS_SHIFT;
  OPTIONS_GETTER_HFA_VALUES = EXTFLAGS_GETTER_HFA_VALUES shl OPTIONS_SHIFT;


type

{ TLuaClosureType record
  ToDo
}

  TLuaClosureType = packed record
    Name: PShortString;
    Invokable: __luapointer{PLuaInvokable};
  end;
  PLuaClosureType = ^TLuaClosureType;


{ TLuaMethod object
  ToDo
}

  TLuaMethodMode = (mmUniversal, mmInvokable, {closures:} mmProc, mmMethod, mmReference);

  TLuaMethod = object
  protected
    F: packed record
    case Integer of
      0: (Invokable: __luapointer{PLuaInvokable});
      1: (MinArgsCount, MaxArgsCount: Word{Universal});
    end;
    FName: packed record
    case Integer of
      0: (Item: __luaname);
      1: (Closure: PShortString)
    end;
    function GetIsClosure: Boolean; {$ifdef INLINESUPPORT}inline;{$endif}
  public
    Address: Pointer;
    Kind: TLuaMethodKind;
    Mode: TLuaMethodMode;
    ScriptInstance: Boolean;
    Reserved: array[0..0] of Byte;

    function UnpackName(const ALua: TLua): PLuaString;

    property IsClosure: Boolean read GetIsClosure;
    // names
    property ItemName: __luaname read FName.Item write FName.Item;
    property ClosureName: PShortString read FName.Closure write FName.Closure;
    // PLuaInvokable
    property Invokable: __luapointer read F.Invokable write F.Invokable;
    // Universal
    property MinArgsCount: Word read F.MinArgsCount write F.MinArgsCount;
    property MaxArgsCount: Word read F.MaxArgsCount write F.MaxArgsCount;
  end;
  PLuaMethod = ^TLuaMethod;


function TLuaMethod.GetIsClosure: Boolean;
begin
  Result := (Mode >= mmProc);
end;

function TLuaMethod.UnpackName(const ALua: TLua): PLuaString;
begin
  Result := @ALua.FStringBuffer.Lua;
  if (IsClosure) then
  begin
    ALua.unpack_lua_string(Result^, FName.Closure^);
  end else
  begin
    ALua.unpack_lua_string(Result^, FName.Item);
  end;
end;


type

{ TLuaNamespaceMethod object
  ToDo
}

  TLuaNamespaceMethod = object(TLuaMethod)
    Func: __luafunction;
  end;
  PLuaNamespaceMethod = ^TLuaNamespaceMethod;


{ TLuaClosureMethod object
  ToDo
}

  TLuaClosureMethod = object(TLuaMethod)
    Instance: Pointer;
  end;
  PLuaClosureMethod = ^TLuaClosureMethod;


{ TLuaCustomVariable object
  ToDo
}

  TLuaVariableKind = (vkUnknown, vkBoolean, vkInteger, vkInt64, vkFloat,
    vkPointer, vkString, vkClass,
    // extended types
    vkVariant, {todo: TVarData,} vkLuaArg,
    vkObject, vkInterface, vkRecord, vkArray, vkSet, vkClosure);

  TLuaBoolType = (btBoolean, btByteBool, btWordBool, btLongBool);

  TLuaStringType = (stShortString, stAnsiString, stWideString, stUnicodeString,
    stAnsiChar, stWideChar, stPAnsiChar, stPWideChar);

  TLuaExtendedGetter = (
   {0} egMetaReference,
   {1} egUnsafe,
   {2} egOwned,
   {3} egHFAOwned);

  TLuaExtendedSetter = (
   {0} esMetaReference,
   {1} esUnsafe,
   {2} esManaged,
   {3} esWeakManaged,
   {4} esHFA);

  TLuaDefaultObject = packed record
    Value: TObject;
  end;
  PLuaDefaultObject = ^TLuaDefaultObject;

  TLuaWeakObject = packed record
    {$ifdef WEAKINSTREF}[Weak]{$endif} Value: TObject;
  end;
  PLuaWeakObject = ^TLuaWeakObject;

  TLuaUnsafeObject = packed record
    {$ifdef WEAKINSTREF}[Unsafe]{$endif} Value: TObject;
  end;
  PLuaUnsafeObject = ^TLuaUnsafeObject;

  TLuaDefaultInterface = packed record
    Value: IInterface;
  end;
  PLuaDefaultInterface = ^TLuaDefaultInterface;

  TLuaWeakInterface = packed record
    {$ifdef WEAKINTFREF}[Weak]{$endif} Value: IInterface;
  end;
  PLuaWeakInterface = ^TLuaWeakInterface;

  TLuaUnsafeInterface = packed record
    {$ifdef WEAKINTFREF}[Unsafe]{$endif} Value: IInterface;
  end;
  PLuaUnsafeInterface = ^TLuaUnsafeInterface;

  TLuaCustomVariable = object
  protected
    F: packed record
    case Integer of
      0: (
          TypeInfo: PTypeInfo;
          Flags: Byte;
          Kind: TLuaVariableKind;
          case Integer of
            0: (OrdType: TOrdType);
            1: (FloatType: TFloatType);
            2: (BoolType: TLuaBoolType);
            3: (StringType: TLuaStringType;
                case Integer of
                  0: (CodePageOffset: Byte);
                  1: (MaxShortLength: Byte);
                  2: (_: packed record end);
               );
            4: (VarOle: Boolean);
            5: (
                 ClosureMode: TLuaMethodMode{mmProc, mmMethod, mmReference};
                 {
                   extended types:
                   variants, meta types, closures

                   IsExtendedMetaPointer: Boolean:1;
                   ExtendedGetter: TLuaExtendedGetter:2;
                   IsExtendedGetterConstant: Boolean:1;
                   IsExtendedGetterTemporary: Boolean:1;
                   ExtendedSetter: TLuaExtendedSetter:3;
                 }
                 ExtendedFlags: Byte;
               );
            6: (__: packed record end);
         );
      1: (MetaType: PLuaMetaType; Options: Cardinal);
      2: (ClosureType: PLuaClosureType);
    end;

    function GetSize: Integer;
    function GetExtendedGetter: TLuaExtendedGetter;
    procedure SetExtendedGetter(const AValue: TLuaExtendedGetter);
    function GetExtendedSetter: TLuaExtendedSetter;
    procedure SetExtendedSetter(const AValue: TLuaExtendedSetter);
    function GetIsExtendedMetaPointer: Boolean;
    procedure SetIsExtendedMetaPointer(const AValue: Boolean);
    function GetIsExtendedGetterConstant: Boolean;
    procedure SetIsExtendedGetterConstant(const AValue: Boolean);
    function GetIsExtendedGetterTemporary: Boolean;
    procedure SetIsExtendedGetterTemporary(const AValue: Boolean);
    function GetIsExtendedGetterOwned: Boolean;
    function GetIsExtendedGetterHFA: Boolean;
  protected
    function InternalSetTypeInfo(const ALua: TLua; const AValue: PTypeInfo;
      const APointerDepth: Boolean = False): Integer;
    function InternalSetRttiType(const ALua: TLua; const AValue: PTypeInfo;
      const AValueName: PShortString; const APointerDepth: Boolean = False): Integer;
  public
    // common
    property TypeInfo: PTypeInfo read F.TypeInfo;
    property MetaType: PLuaMetaType read F.MetaType;
    property Flags: Byte read F.Flags write F.Flags;
    property Options: Cardinal read F.Options write F.Options;
    property Size: Integer read GetSize;

    // optional
    property Kind: TLuaVariableKind read F.Kind write F.Kind;
    property OrdType: TOrdType read F.OrdType write F.OrdType;
    property FloatType: TFloatType read F.FloatType write F.FloatType;
    property BoolType: TLuaBoolType read F.BoolType write F.BoolType;
    property StringType: TLuaStringType read F.StringType write F.StringType;
    property MaxShortLength: Byte read F.MaxShortLength write F.MaxShortLength;
    property VarOle: Boolean read F.VarOle write F.VarOle;
    property ClosureMode: TLuaMethodMode read F.ClosureMode write F.ClosureMode;

    // extended
    property ExtendedFlags: Byte read F.ExtendedFlags write F.ExtendedFlags;
    property ExtendedGetter: TLuaExtendedGetter read GetExtendedGetter write SetExtendedGetter;
    property ExtendedSetter: TLuaExtendedSetter read GetExtendedSetter write SetExtendedSetter;
    property IsExtendedMetaPointer: Boolean read GetIsExtendedMetaPointer write SetIsExtendedMetaPointer;
    property IsExtendedGetterConstant: Boolean read GetIsExtendedGetterConstant write SetIsExtendedGetterConstant;
    property IsExtendedGetterTemporary: Boolean read GetIsExtendedGetterTemporary write SetIsExtendedGetterTemporary;
    property IsExtendedGetterOwned: Boolean read GetIsExtendedGetterOwned;
    property IsExtendedGetterHFA: Boolean read GetIsExtendedGetterHFA;

    // script variable conversion
    procedure GetValue(const ASource; const ALua: TLua);
    function SetValue(var ATarget; const ALua: TLua; const AStackIndex: Integer; ALuaType: Integer = -1): Boolean;
  end;
  PLuaCustomVariable = ^TLuaCustomVariable;

function TLuaCustomVariable.GetSize: Integer;
begin
  Result := 0;

  case F.Kind of
    vkBoolean:
    begin
      case F.BoolType of
        btBoolean,
        btByteBool: Result := SizeOf(ByteBool);
        btWordBool: Result := SizeOf(WordBool);
        btLongBool: Result := SizeOf(LongBool);
      end;
    end;
    vkInteger:
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
    vkInt64:
    begin
      Result := SizeOf(Int64);
    end;
    vkFloat:
    begin
      case (F.FloatType) of
        ftSingle: Result := SizeOf(Single);
        ftDouble: Result := SizeOf(Double);
      ftExtended: Result := SizeOf(Extended);
          ftComp: Result := SizeOf(Comp);
          ftCurr: Result := SizeOf(Currency);
      end;
    end;
    vkPointer, vkClass, vkObject, vkInterface:
    begin
      Result := SizeOf(Pointer);
    end;
    vkVariant:
    begin
      Result := SizeOf(Variant);
    end;
    vkString:
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
    vkClosure:
    begin
      case F.ClosureMode of
        mmMethod: Result := SizeOf(TMethod);
      else
        Result := SizeOf(Pointer);
      end;
    end;
    vkRecord, vkArray, vkSet:
    begin
      Result := F.MetaType.Size;
    end;
  end;
end;

function TLuaCustomVariable.GetExtendedGetter: TLuaExtendedGetter;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := TLuaExtendedGetter((F.Options shr OPTIONS_GETTER_SHIFT) and EXTENDED_GETTER_MASK);
  end else
  begin
    Result := egUnsafe;
  end;
end;

procedure TLuaCustomVariable.SetExtendedGetter(const AValue: TLuaExtendedGetter);
begin
  if (F.Kind >= vkVariant) then
  begin
    F.Options := (F.Options and Cardinal(not OPTIONS_GETTER_MASK)) +
      (Cardinal(Byte(AValue)) shl OPTIONS_GETTER_SHIFT);
  end;
end;

function TLuaCustomVariable.GetExtendedSetter: TLuaExtendedSetter;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := TLuaExtendedSetter(F.Options shr OPTIONS_SETTER_SHIFT);
  end else
  begin
    Result := esUnsafe;
  end;
end;

procedure TLuaCustomVariable.SetExtendedSetter(const AValue: TLuaExtendedSetter);
begin
  if (F.Kind >= vkVariant) then
  begin
    F.Options := (F.Options and Cardinal(not OPTIONS_SETTER_MASK)) +
      (Cardinal(Byte(AValue)) shl OPTIONS_SETTER_SHIFT);
  end;
end;

function TLuaCustomVariable.GetIsExtendedMetaPointer: Boolean;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := (F.Options and OPTIONS_META_POINTER_MODE <> 0);
  end else
  begin
    Result := False;
  end;
end;

procedure TLuaCustomVariable.SetIsExtendedMetaPointer(const AValue: Boolean);
begin
  if (F.Kind >= vkVariant) then
  begin
    if (AValue) then
    begin
      F.Options := F.Options or OPTIONS_META_POINTER_MODE;
    end else
    begin
      F.Options := F.Options and Cardinal(not OPTIONS_META_POINTER_MODE);
    end;
  end;
end;

function TLuaCustomVariable.GetIsExtendedGetterConstant: Boolean;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := (F.Options and OPTIONS_GETTER_CONSTANT_MODE <> 0);
  end else
  begin
    Result := False;
  end;
end;

procedure TLuaCustomVariable.SetIsExtendedGetterConstant(const AValue: Boolean);
begin
  if (F.Kind >= vkVariant) then
  begin
    if (AValue) then
    begin
      F.Options := F.Options or OPTIONS_GETTER_CONSTANT_MODE;
    end else
    begin
      F.Options := F.Options and Cardinal(not OPTIONS_GETTER_CONSTANT_MODE);
    end;
  end;
end;

function TLuaCustomVariable.GetIsExtendedGetterTemporary: Boolean;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := (F.Options and OPTIONS_GETTER_TEMPORARY_MODE <> 0);
  end else
  begin
    Result := False;
  end;
end;

procedure TLuaCustomVariable.SetIsExtendedGetterTemporary(const AValue: Boolean);
begin
  if (F.Kind >= vkVariant) then
  begin
    if (AValue) then
    begin
      F.Options := F.Options or OPTIONS_GETTER_TEMPORARY_MODE;
    end else
    begin
      F.Options := F.Options and Cardinal(not OPTIONS_GETTER_TEMPORARY_MODE);
    end;
  end;
end;

function TLuaCustomVariable.GetIsExtendedGetterOwned: Boolean;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := (F.Options and OPTIONS_GETTER_OWNED_MODE <> 0);
  end else
  begin
    Result := False;
  end;
end;

function TLuaCustomVariable.GetIsExtendedGetterHFA: Boolean;
begin
  if (F.Kind >= vkVariant) then
  begin
    Result := (F.Options and OPTIONS_GETTER_MASK >= OPTIONS_GETTER_HFA_VALUES);
  end else
  begin
    Result := False;
  end;
end;

function TLuaCustomVariable.InternalSetTypeInfo(const ALua: TLua; const AValue: PTypeInfo;
  const APointerDepth: Boolean): Integer;
label
  std_typeinfo, meta_typeinfo, done;
const
  META_TYPE_KINDS: array[TLuaMetaKind] of TLuaVariableKind = (
    vkObject{mtClass}, vkInterface{mtInterface}, vkRecord{mtRecord},
    vkArray{mtArray}, vkSet{mtSet});
var
  LValue: PTypeInfo;
begin
  Result := 0;
  F.TypeInfo := AValue;
  F.Options := 0;

  if (AValue = TypeInfoTClass) then
  begin
    F.Kind := vkClass;
    F.MetaType := ALua.InternalAutoRegister(System.TypeInfo(TObject), False);
  end else
  if (AValue = TypeInfoPointer) then
  begin
    F.Kind := vkPointer;
    {$ifdef EXTENDEDRTTI}
    F.TypeInfo := System.TypeInfo(Pointer);
    {$endif}
  end else
  if (AValue = TypeInfoLuaArg) then
  begin
    F.Kind := vkLuaArg;
  end else
  if (AValue = TypeInfoPWideChar) then
  begin
    F.Kind := vkString;
    F.StringType := stPWideChar;
  end else
  if (NativeUInt(AValue) >= NativeUInt(TypeInfoPAnsiChar)) and
    (NativeUInt(AValue) <= NativeUInt(TypeInfoPAnsiChar) or $FFFF) then
  begin
    F.Kind := vkString;
    F.StringType := stPAnsiChar;
  end else
  if (PLuaMetaType(AValue).F.Marker = LUA_METATYPE_MARKER) then
  begin
    F.MetaType := PLuaMetaType(AValue);
    F.Kind := META_TYPE_KINDS[PLuaMetaType(AValue).F.Kind];
    goto meta_typeinfo;
  end else
  begin
    LValue := AValue;
  std_typeinfo:
    if (IsBooleanType(LValue)) then
    begin
      F.Kind := vkBoolean;
      case (GetTypeData(LValue).OrdType) of
        otUByte: F.BoolType := btBoolean;
        otSByte: F.BoolType := btByteBool;
        otSWord, otUWord: F.BoolType := btWordBool;
      else
        // otSLong, otULong:
        F.BoolType := btLongBool;
      end;
    end else
    case LValue.Kind of
      {$ifdef FPC}
      tkBool:
      begin
        F.Kind := vkBoolean;
        F.BoolType := btBoolean;
      end;
      {$endif}
      tkInteger:
      begin
        F.Kind := vkInteger;
        F.OrdType := GetTypeData(LValue).OrdType;
      end;
      tkEnumeration:
      begin
        F.Kind := vkInteger;
        F.OrdType := GetTypeData(LValue).OrdType;
        ALua.InternalAutoRegister(LValue);
      end;
      {$ifdef FPC}
      tkQWord,
      {$endif}
      tkInt64: F.Kind := vkInt64;
      tkFloat:
      begin
        F.Kind := vkFloat;
        F.FloatType := GetTypeData(LValue).FloatType;
      end;
      tkChar:
      begin
        F.Kind := vkString;
        F.StringType := stAnsiChar;
      end;
      tkWChar:
      begin
        F.Kind := vkString;
        F.StringType := stWideChar;
      end;
      tkString:
      begin
        F.Kind := vkString;
        F.StringType := stShortString;
        F.MaxShortLength := GetTypeData(LValue).MaxLength;
      end;
      {$ifdef FPC}tkAString,{$endif}
      tkLString:
      begin
        F.Kind := vkString;
        F.StringType := stAnsiString;
        {$ifdef INTERNALCODEPAGE}
        F.CodePageOffset := NativeUInt(@GetTypeData(LValue).CodePage) - NativeUInt(LValue);
        {$endif}
      end;
      {$ifdef UNICODE}
      tkUString:
      begin
        F.Kind := vkString;
        F.StringType := stUnicodeString;
      end;
      {$endif}
      tkWString:
      begin
        F.Kind := vkString;
        F.StringType := stWideString;
      end;
      tkVariant:
      begin
        F.Kind := vkVariant;
        F.VarOle := (LValue = System.TypeInfo(OleVariant));
        Self.ExtendedGetter := egOwned;
        Self.ExtendedSetter := esManaged;
      end;
      tkClass:
      begin
        F.Kind := vkObject;
        F.MetaType := ALua.InternalAutoRegister(LValue, True{False});
        goto meta_typeinfo;
      end;
      tkInterface:
      begin
        {$ifdef EXTENDEDRTTI}
        if (IsReferenceMethodType(LValue)) then
        begin
          F.Kind := vkClosure;
          F.ClosureMode := mmReference;
          F.ClosureType := ALua.InternalAutoRegister(LValue, True);
          Self.ExtendedGetter := egOwned;
          Self.ExtendedSetter := esManaged;
        end else
        {$endif}
        begin
          F.Kind := vkInterface;
          F.MetaType := ALua.InternalAutoRegister(LValue, True);
          goto meta_typeinfo;
        end;
      end;
      tkMethod:
      begin
        F.Kind := vkClosure;
        F.ClosureMode := mmMethod;
        Self.ExtendedGetter := egUnsafe;
        Self.ExtendedSetter := {$ifdef WEAKINSTREF}esWeakManaged{$else}esUnsafe{$endif};
        F.ClosureType := ALua.InternalAutoRegister(LValue, True);
      end;
      tkRecord{$ifdef FPC}, tkObject{$endif}:
      begin
        F.Kind := vkRecord;
        F.MetaType := ALua.InternalAutoRegister(LValue, False);
        goto meta_typeinfo;
      end;
      tkArray, tkDynArray:
      begin
        F.Kind := vkArray;
        F.MetaType := ALua.InternalAutoRegister(LValue, False);
        goto meta_typeinfo;
      end;
      tkSet:
      begin
        F.Kind := vkSet;
        F.MetaType := ALua.InternalAutoRegister(LValue, False);
      meta_typeinfo:
        if (not Assigned(F.MetaType)) then
        begin
          F.Kind := vkUnknown;
          goto done;
        end;

        Self.ExtendedGetter := egUnsafe;
        Self.ExtendedSetter := esUnsafe;
        case F.Kind of
          {$ifdef AUTOREFCOUNT}
          vkObject,
          {$endif}
          vkInterface:
          begin
            Self.ExtendedGetter := egOwned;
          end;
          vkArray:
          begin
            if (PLuaArrayInfo(F.MetaType).IsDynamic) then
            begin
              Self.ExtendedGetter := egOwned;
            end;
          end;
        end;
        if (F.MetaType.Managed) then
        begin
          if (F.MetaType.Weak) then
          begin
            Self.ExtendedSetter := esWeakManaged;
          end else
          begin
            Self.ExtendedSetter := esManaged;
          end;
        end;
      end;
      {$ifdef EXTENDEDRTTI}
      tkClassRef:
      begin
        F.Kind := vkClass;
        F.MetaType := ALua.InternalAutoRegister(LValue, False);
      end;
      tkPointer:
      begin
        Result := GetPointerTypeDepth(LValue, LValue);
        if (Result <> 0) then goto std_typeinfo;
        F.Kind := vkPointer;
      end;
      tkProcedure:
      begin
        F.Kind := vkClosure;
        F.ClosureMode := mmProc;
        F.ClosureType := ALua.InternalAutoRegister(LValue, True);
        Self.ExtendedGetter := egUnsafe;
        Self.ExtendedSetter := esUnsafe;
      end;
      {$endif}
    end;
  end;

done:
  if (Self.F.Kind = vkUnknown) then
  begin
    if (Assigned(ALua)) then
    begin
      ALua.unpack_lua_string(ALua.FStringBuffer.Lua, PShortString(@PTypeInfo(AValue).Name)^);
      GetTypeKindName(ALua.FStringBuffer.Default, PTypeInfo(AValue).Kind);
    end;

    Result := -1;
  end else
  if (Result <> 0) then
  begin
    if (Self.F.Kind in [vkRecord, vkArray, vkSet]) then
    begin
      F.ExtendedFlags := Cardinal(True){IsExtendedMetaPointer/egMetaReference/esMetaReference};
    end;

    if (not APointerDepth) then
    begin
      if (Result <> 1) then
      begin
        F.TypeInfo := {$ifdef EXTENDEDRTTI}System.TypeInfo(Pointer){$else}TypeInfoPointer{$endif};
        F.Options := 0;
        F.Kind := vkPointer;
      end;

      Result := 0;
    end;
  end;
end;

function TLuaCustomVariable.InternalSetRttiType(const ALua: TLua; const AValue: PTypeInfo;
  const AValueName: PShortString; const APointerDepth: Boolean): Integer;
var
  LRegisteredTypeName: PLuaRegisteredTypeName;
begin
  Result := -1;

  if (Assigned(AValue)) then
  begin
    Result := InternalSetTypeInfo(ALua, AValue, APointerDepth);
  end else
  if (Assigned(AValueName)) then
  begin
    LRegisteredTypeName := TLuaRegisteredTypeNames(ALua.FRegisteredTypeNames).Find(AValueName^);
    if (Assigned(LRegisteredTypeName)) then
    begin
      Result := InternalSetTypeInfo(ALua, LRegisteredTypeName.TypeInfo, APointerDepth);
      if (Result >= 0) then
      begin
        Inc(Result, LRegisteredTypeName.PointerDepth);
      end;
    end;
  end;
end;

procedure TLuaCustomVariable.GetValue(const ASource; const ALua: TLua);
label
  int64_value, nil_value;
var
  LSource: Pointer;
  LOptions: Cardinal;
  LValue: Pointer;
  LUserData: PLuaUserData;
  LClosureMethod: PLuaClosureMethod;
  LStore: record
    ClosureType: PLuaClosureType;
  end;
begin
  LOptions := F.Options;
  LSource := @ASource;
  if (LOptions and OPTIONS_GETTER_MASK = 0) and (Word(LOptions) >= Word(vkVariant) shl 8){ExtendedGetter = egMetaReference} then
  begin
    LSource := PPointer(LSource)^;
  end;

  with ALua do
  case Cardinal(Byte(LOptions shr 8)){F.Kind} of
    Ord(vkBoolean):
    begin
      if (F.BoolType <> btBoolean) then
      begin
        case F.BoolType of
          btByteBool: FTempBuffer.B := ByteBool(LSource^);
          btWordBool: FTempBuffer.B := WordBool(LSource^);
        else
          // btLongBool
          FTempBuffer.B := LongBool(LSource^);
        end;
        LSource := @FTempBuffer.B;
      end;

      lua_pushboolean(Handle, PBoolean(LSource)^);
    end;
    Ord(vkInteger):
    begin
      if (F.OrdType <> otSLong) then
      begin
        case F.OrdType of
          otSByte: FTempBuffer.I := PShortInt(LSource)^;
          otUByte: FTempBuffer.I := PByte(LSource)^;
          otSWord: FTempBuffer.I := PSmallInt(LSource)^;
          otUWord: FTempBuffer.I := PWord(LSource)^;
        else
          // otULong
          {$ifdef LARGEINT}
            lua_pushinteger(Handle, PCardinal(LSource)^);
          {$else .SMALLINT}
            if (PInteger(LSource)^ < 0) then
            begin
              lua_pushnumber(Handle, PCardinal(LSource)^);
              Exit;
            end else
            begin
              FTempBuffer.I := PInteger(LSource)^;
            end;
          {$endif}
        end;
        LSource := @FTempBuffer.I;
      end;

      lua_pushinteger(Handle, PInteger(LSource)^);
    end;
    Ord(vkInt64):
    begin
    int64_value:
      {$ifdef LARGEINT}
        lua_pushinteger(Handle, PInt64(LSource)^);
      {$else .SMALLINT}
        if (-(TPoint(LSource^).X shr 31) = TPoint(LSource^).Y) then
        begin
          lua_pushinteger(Handle, PInteger(LSource)^);
        end else
        begin
          lua_pushnumber(Handle, PInt64(LSource)^);
        end;
      {$endif}
    end;
    Ord(vkFloat):
    begin
      if (F.FloatType <> ftDouble) then
      begin
        case F.FloatType of
           ftSingle: FTempBuffer.D := PSingle(LSource)^;
         ftExtended: FTempBuffer.D := PExtended(LSource)^;
             ftComp: goto int64_value;
        else
          // ftCurr
          FTempBuffer.D := PCurrency(LSource)^;
        end;
        LSource := @FTempBuffer.D;
      end;

      lua_pushnumber(Handle, PDouble(LSource)^);
    end;
    Ord(vkString):
    begin
      case F.StringType of
        {$ifNdef NEXTGEN}
        stShortString:
        begin
          push_short_string(PShortString(LSource)^);
        end;
        stAnsiString:
        begin
          push_ansi_string(PAnsiString(LSource)^);
        end;
        stWideString:
        begin
          push_wide_string(PWideString(LSource)^);
        end;
        {$endif}
        stUnicodeString:
        begin
          push_unicode_string(PUnicodeString(LSource)^);
        end;
        stAnsiChar:
        begin
          push_ansi_chars(LSource, 0, 1);
        end;
        stWideChar:
        begin
          push_wide_chars(LSource, 1);
        end;
        stPAnsiChar:
        begin
          LSource := PPointer(LSource)^;
          push_ansi_chars(LSource, 0, LStrLen(LSource));
        end;
        stPWideChar:
        begin
          LSource := PPointer(LSource)^;
          push_wide_chars(LSource, WStrLen(LSource));
        end;
      else
        ALua.InternalErrorFmt('Unknown string kind: %d', [Ord(F.StringType)]);
      end;
    end;
    Ord(vkVariant):
    begin
      if (not push_variant(PVariant(LSource)^)) then goto nil_value;
    end;
    {todo: TVarData,}
    Ord(vkLuaArg):
    begin
      if (not push_luaarg(PLuaArg(LSource)^)) then goto nil_value;
    end;
    Ord(vkPointer),
    Ord(vkClass),
    Ord(vkObject),
    Ord(vkInterface):
    begin
      LValue := Pointer(LSource^);
      if (not Assigned(LValue)) then goto nil_value;

      case F.Kind of
        vkPointer: lua_pushlightuserdata(Handle, LValue);
        vkClass: lua_rawgeti(Handle, LUA_REGISTRYINDEX, InternalGetClassInfo(TClass(LValue)).Ref);
        vkInterface:
        begin
          push_metatype_instance(F.MetaType, LSource^,
            LOptions and OPTIONS_GETTER_OWNED_MODE <> 0{Self.IsExtendedGetterOwned},
            LOptions and OPTIONS_GETTER_TEMPORARY_MODE <> 0{Self.IsExtendedGetterTemporary});
        end;
      else
        // vkObject
        if (TClass(LValue^) = TLuaVariable) then
        begin
          lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaVariable(LValue).Index);
        end else
        begin
          push_metatype_instance(InternalGetClassInfo(TClass(LValue^)), LSource^,
            LOptions and OPTIONS_GETTER_OWNED_MODE <> 0{Self.IsExtendedGetterOwned},
            LOptions and OPTIONS_GETTER_TEMPORARY_MODE <> 0{Self.IsExtendedGetterTemporary});
        end;
      end;
    end;
    Ord(vkClosure):
    begin
      LStore.ClosureType := F.ClosureType;

      case Cardinal(Byte(LOptions shr 16)){F.ClosureMode} of
        Ord(mmProc):
        begin
          LValue := Pointer(LSource^);
          if (not Assigned(LValue)) then goto nil_value;
          LClosureMethod := alloc_push_closure(False);
          LClosureMethod.Instance := nil;
          LClosureMethod.Address := LValue;
          PCardinal(@LClosureMethod.Kind)^ := Cardinal(mkStatic) + Cardinal(mmProc) shl 8;
        end;
        Ord(mmMethod):
        begin
          LValue := TMethod(LSource^).Code;
          if (not Assigned(LValue)) then goto nil_value;
          LClosureMethod := alloc_push_closure(False);
          LClosureMethod.Instance := TMethod(LSource^).Data;
          LClosureMethod.Address := LValue;
          PCardinal(@LClosureMethod.Kind)^ := Cardinal(mkInstance) + Cardinal(mmMethod) shl 8;
        end;
      else
        // mmReference:
        LValue := Pointer(LSource^);
        if (Assigned(LValue)) then
        begin
          LClosureMethod := alloc_push_closure(Integer(LOptions) and OPTIONS_GETTER_OWNED_MODE <> 0{Self.IsExtendedGetterOwned});
          LClosureMethod.Instance := LValue;
          LClosureMethod.Address := PPointer(PNativeUInt(LValue)^ + 3 * SizeOf(Pointer))^;
          PCardinal(@LClosureMethod.Kind)^ := Cardinal(mkInstance) + Cardinal(mmReference) shl 8;
          if (LOptions and OPTIONS_GETTER_OWNED_MODE <> 0{Self.IsExtendedGetterOwned}) then
          begin
            if (Integer(LOptions) and OPTIONS_GETTER_TEMPORARY_MODE <> 0{Self.IsExtendedGetterTemporary}) then
            begin
              PPointer(LSource)^ := nil;
            end else
            begin
              IInterface(PPointer(LSource)^)._AddRef;
            end;
          end;
        end else
        begin
        nil_value:
          lua_pushnil(Handle);
          Exit;
        end;
      end;

      with LStore.ClosureType^ do
      begin
        LClosureMethod.ClosureName := Name;
        LClosureMethod.Invokable := Invokable;
      end;
    end;
  else
    // vkRecord, vkArray, vkSet
    LUserData := push_metatype_instance(F.MetaType, ASource,
      LOptions and OPTIONS_GETTER_OWNED_MODE <> 0{Self.IsExtendedGetterOwned},
      LOptions and OPTIONS_GETTER_TEMPORARY_MODE <> 0{Self.IsExtendedGetterTemporary});
    LUserData.Constant := (LOptions and OPTIONS_GETTER_CONSTANT_MODE <> 0){Self.IsExtendedGetterConstant};
  end;
end;

function LuaClosureWrapper(L: Plua_State): Integer; cdecl; forward;

function TLuaCustomVariable.SetValue(var ATarget; const ALua: TLua; const AStackIndex: Integer;
  ALuaType: Integer): Boolean;

  function CastAsNumber(const ALua: TLua; const AStackIndex: Integer; const AModeInt64: Boolean): Boolean; far;
  var
    LLuaType: Integer;
    LValue, i: NativeInt;
    LDefault: ^string;
    LSeparator: Char;
  begin
    Result := True;
    ALua.FTempBuffer.I64 := 0;

    LLuaType := lua_type(ALua.Handle, AStackIndex);
    case LLuaType of
      LUA_TNIL: {already 0};
      LUA_TBOOLEAN:
      begin
        if (lua_toboolean(ALua.Handle, AStackIndex)) then
        begin
          ALua.FTempBuffer.B := True;
          if (not AModeInt64) then
            ALua.FTempBuffer.D := 1;
        end;
      end;
      LUA_TLIGHTUSERDATA, LUA_TUSERDATA:
      begin
        Result := AModeInt64;
        if (Result) then
        begin
          LValue := NativeInt(lua_touserdata(ALua.Handle, AStackIndex));
          if (LLuaType = LUA_TUSERDATA) and (LValue <> 0) then
          begin
            LValue := NativeInt(PLuaUserData(LValue).Instance);
          end;
          ALua.FTempBuffer.N := LValue;
        end;
      end;
      LUA_TSTRING:
      begin
        LDefault := @ALua.FStringBuffer.Default;
        {$ifdef UNICODE}
          ALua.stack_unicode_string(LDefault^, AStackIndex);
        {$else .ANSI}
          Lua.stack_ansi_string(LDefault^, AStackIndex, 0);
        {$endif}

        if (AModeInt64) then
        begin
          Result := TryStrToInt64(LDefault^, ALua.FTempBuffer.I64);
        end else
        begin
          {$if Defined(FPC) or (CompilerVersion <= 21)}
            LSeparator := DecimalSeparator;
          {$else}
            LSeparator := FormatSettings.DecimalSeparator;
          {$ifend}

          for i := 1 to Length(LDefault^) do
          case LDefault^[i] of
            '.', ',':
            begin
              if (LDefault^[i] <> LSeparator) then
                LDefault^[i] := LSeparator;

              Break;
            end;
          end;

          Result := TryStrToFloat(LDefault^, ALua.FTempBuffer.D);
        end;
      end;
    else
      Result := False;
    end;
  end;

  procedure CastAsString(const ALua: TLua; const AStackIndex: Integer; const AInstance: Pointer; const ASelf: TLuaCustomVariable); far;
  begin
    with ALua do
    case ASelf.StringType of
      {$ifNdef NEXTGEN}
      stShortString:
      begin
        stack_short_string(PShortString(AInstance)^, AStackIndex, ASelf.MaxShortLength);
      end;
      stAnsiString:
      begin
        stack_ansi_string(PAnsiString(AInstance)^, AStackIndex,
          {$ifdef INTERNALCODEPAGE}GetTypeData(ASelf.TypeInfo).CodePage{$else}0{$endif});
      end;
      stWideString:
      begin
        stack_wide_string(PWideString(AInstance)^, AStackIndex);
      end;
      stAnsiChar:
      begin
        stack_ansi_string(FStringBuffer.Ansi, AStackIndex, 0);
        PByte(AInstance)^ := 0;
        if (Pointer(FStringBuffer.Ansi) <> nil) then PByte(AInstance)^ := PByte(FStringBuffer.Ansi)^;
      end;
      stPAnsiChar:
      begin
        stack_ansi_string(FStringBuffer.Ansi, AStackIndex, Word(NativeUInt(ASelf.F.TypeInfo)));
        PPointer(AInstance)^ := @NULL_CHAR;
        if (Pointer(FStringBuffer.Ansi) <> nil) then PPointer(AInstance)^ := Pointer(FStringBuffer.Ansi);
      end;
      {$endif}
      stUnicodeString:
      begin
        stack_unicode_string(PUnicodeString(AInstance)^, AStackIndex);
      end;
      stWideChar:
      begin
        stack_unicode_string(FStringBuffer.Unicode, AStackIndex);
        PWord(AInstance)^ := 0;
        if (Pointer(FStringBuffer.Unicode) <> nil) then PWord(AInstance)^ := PWord(FStringBuffer.Unicode)^;
      end;
      stPWideChar:
      begin
        stack_unicode_string(FStringBuffer.Unicode, AStackIndex);
        PPointer(AInstance)^ := @NULL_CHAR;
        if (Pointer(FStringBuffer.Unicode) <> nil) then PPointer(AInstance)^ := Pointer(FStringBuffer.Unicode);
      end;
    else
      ALua.InternalErrorFmt('Unknown string kind: %d', [Ord(ASelf.StringType)]);
    end;
  end;

  function CastAsTObject(const ALua: TLua; const AStackIndex: Integer): Pointer; far;
  var
    LUserData: PLuaUserData;
  begin
    LUserData := lua_touserdata(ALua.Handle, AStackIndex);

    if (Assigned(LUserData)) and (LUserData.Kind = mtClass) then
    begin
      Result := LUserData.Instance;
      Exit;
    end;

    Result := nil;
  end;

  function CastAsInterface(const ALua: TLua; const AStackIndex: Integer): Pointer; far;
  var
    LUserData: PLuaUserData;
  begin
    LUserData := lua_touserdata(ALua.Handle, AStackIndex);

    if (Assigned(LUserData)) and (LUserData.Kind = mtInterface) then
    begin
      Result := LUserData.Instance;
      Exit;
    end;

    Result := nil;
  end;

  function AssignMetaType(const ALua: TLua; const AStackIndex: Integer; const AInstance: Pointer;
    const ASelf: TLuaCustomVariable): Boolean; far;
  label
    failure;
  var
    LUserData: PLuaUserData;
    LMetaType: PLuaMetaType;
  begin
    LMetaType := ASelf.MetaType;

    // ToDo ExtendedSetter

    case lua_type(ALua.Handle, AStackIndex) of
      LUA_TNIL:
      begin
        LMetaType.Clear(AInstance^, True);
      end;
      LUA_TUSERDATA:
      begin
        LUserData := lua_touserdata(ALua.Handle, AStackIndex);
        if (LUserData = nil) or (LUserData.Instance = nil) or
          (LUserData.MetaType <> {$ifdef SMALLINT}__luapointer(LMetaType){$else}LMetaType.Ptr{$endif}) then
          goto failure;

        LMetaType.Assign(AInstance^, LUserData.Instance);
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
  LInstance: Pointer;
  LValue: NativeInt;
  LClosure: PLuaClosureMethod;
begin
  LInstance := @ATarget;
  if (ALuaType = -1) then
    ALuaType := lua_type(ALua.Handle, AStackIndex);

  with ALua do
  case F.Kind of
    vkBoolean:
    begin
      LValue := 0;
      case (ALuaType) of
            LUA_TNIL: ;
        LUA_TBOOLEAN: LValue := NativeInt(lua_toboolean(Handle, AStackIndex)) and 1;
         LUA_TSTRING: begin
                        LValue := CastStringAsBoolean(PAnsiChar(lua_tolstring(Handle, AStackIndex, Pointer(@FTempBuffer.N))), FTempBuffer.N);
                        if (LValue < 0) then goto failure;
                      end;
      else
        goto failure;
      end;
      if (F.BoolType <> btBoolean) then LValue := -LValue;

      case F.BoolType of
        btWordBool: PWord(LInstance)^ := LValue;
        btLongBool: PCardinal(LInstance)^ := LValue;
      else
        // btBoolean, btByteBool
        PByte(LInstance)^ := LValue;
      end;

      goto done;
    end;
    vkInteger:
    begin
      if (ALuaType = LUA_TNUMBER) then
      begin
        {$ifNdef LARGEINT}
        if (F.OrdType <> otULong) then
        {$endif}
        begin
          LValue := lua_tointeger(Handle, AStackIndex);

        set_integer:
          case Cardinal(F.OrdType) shr 1 of
            0: PByte(LInstance)^ := LValue;
            1: PWord(LInstance)^ := LValue;
          else
            PCardinal(LInstance)^ := LValue;
          end;

          goto done;
        end;
      end;

      goto unpack_int_int64;
    end;
    vkInt64:
    begin
    unpack_int_int64:
      if (ALuaType = LUA_TNUMBER) then
      begin
        {$ifdef LARGEINT}
          FTempBuffer.I64 := lua_tointeger(Handle, AStackIndex);
        {$else .SMALLINT}
          FTempBuffer.I64 := lua_toint64(Handle, AStackIndex);
        {$endif}

      set_int64:
        if (F.Kind = vkInt64) then
        begin
          PInt64(LInstance)^ := FTempBuffer.I64;
          goto done;
        end else
        begin
          LValue := PCardinal(@FTempBuffer.I64)^;
          goto set_integer;
        end;
      end else
      begin
        if (CastAsNumber(ALua, AStackIndex, True)) then goto set_int64;
        goto failure;
      end;
    end;
    vkFloat:
    begin
      if (F.FloatType = ftComp) then goto unpack_int_int64;
      if (ALuaType = LUA_TNUMBER) then
      begin
        FTempBuffer.D := lua_tonumber(Handle, AStackIndex);
      set_float:
        case F.FloatType of
          ftSingle: PSingle(LInstance)^ := FTempBuffer.D;
          ftDouble: PDouble(LInstance)^ := FTempBuffer.D;
        ftExtended: PExtended(LInstance)^ := FTempBuffer.D;
        else
          // ftCurr
          PCurrency(LInstance)^ := FTempBuffer.D;
        end;

        goto done;
      end else
      begin
        if (CastAsNumber(ALua, AStackIndex, True)) then goto set_float;
        goto failure;
      end;
    end;
    vkString:
    begin
      CastAsString(ALua, AStackIndex, LInstance, Self);
      goto done;
    end;
    vkVariant:
    begin
      if (not stack_variant(PVariant(LInstance)^, AStackIndex, F.VarOle)) then goto failure;
      goto done;
    end;
    vkPointer:
    begin
      case (ALuaType) of
        LUA_TNIL: LValue := 0;
        LUA_TSTRING:
        begin
          {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}
            stack_lua_string(FStringBuffer.Lua, AStackIndex);
            LValue := NativeInt(FStringBuffer.Lua);
          {$else}
            Value := NativeInt(lua_tolstring(Handle, AStackIndex, nil));
          {$ifend}
        end;
        LUA_TLIGHTUSERDATA: LValue := NativeInt(lua_touserdata(Handle, AStackIndex));
        LUA_TUSERDATA:
        begin
          LValue := NativeInt(lua_touserdata(Handle, AStackIndex));
          if (LValue <> 0) then LValue := NativeInt(PLuaUserData(LValue).Instance);
        end;
        LUA_TFUNCTION:
        begin
          LValue := NativeInt(function_topointer(AStackIndex));
        end;
        LUA_TTABLE:
        begin
          LValue := NativeInt(InternalTableToMetaType(AStackIndex));
          if (LValue = 0) then goto failure;
          if (PLuaMetaType(LValue).F.Kind = mtClass) then
            LValue := NativeInt(PLuaMetaType(LValue).F.ClassType);
        end;
      else
        goto failure;
      end;

      goto set_native;
    end;
    vkClass:
    begin
      case (ALuaType) of
        LUA_TNIL: LValue := 0;
        LUA_TTABLE:
        begin
          LValue := NativeInt(InternalTableToMetaType(AStackIndex));
          if (LValue = 0) then goto failure;
          if (PLuaMetaType(LValue).F.Kind <> mtClass) then goto failure;
          LValue := NativeInt(PLuaMetaType(LValue).F.ClassType);
        end;
        LUA_TUSERDATA:
        begin
          LValue := NativeInt(CastAsTObject(ALua, AStackIndex));
          if (LValue = 0) then goto failure;
          LValue := PNativeInt(LValue)^;
        end;
      else
        goto failure;
      end;

      goto set_native;
    end;
    vkObject:
    begin
      case (ALuaType) of
        LUA_TNIL: LValue := 0;
        LUA_TUSERDATA:
        begin
          LValue := NativeInt(CastAsTObject(ALua, AStackIndex));
          if (LValue = 0) then goto failure;
        end;
      else
        goto failure;
      end;

      FTempBuffer.N := LValue;
      if (PNativeInt(LInstance)^ <> LValue) then
      {$ifdef WEAKINSTREF}
      case {TLuaExtendedSetter}(F.Options shr OPTIONS_SETTER_SHIFT) of
        Ord(esWeakManaged):
        begin
          PLuaWeakObject(Instance).Value := TObject(FTempBuffer.N);
        end;
        Ord(esUnsafe):
        begin
          PLuaUnsafeObject(Instance).Value := TObject(FTempBuffer.N);
        end;
      else
      {$endif}
        PLuaDefaultObject(LInstance).Value := TObject(FTempBuffer.N);
      {$ifdef WEAKINSTREF}
      end;
      {$endif}
      goto done;
    end;
    vkInterface:
    begin
      LValue := 0;
      case ALuaType of
        LUA_TNIL: ;
        LUA_TUSERDATA:
        begin
          LValue := NativeInt(CastAsInterface(ALua, AStackIndex));
          if (LValue = 0) then goto failure;
        end;
      else
        goto failure;
      end;

      FTempBuffer.N := LValue;
      if (PNativeInt(LInstance)^ <> LValue) then
      case {TLuaExtendedSetter}(F.Options shr OPTIONS_SETTER_SHIFT) of
        {$ifdef WEAKINTFREF}
        Ord(esWeakManaged):
        begin
          PLuaWeakInterface(LInstance).Value := IInterface(FTempBuffer.N);
        end;
        {$endif}
        Ord(esUnsafe):
        begin
          PPointer(LInstance)^ := Pointer(FTempBuffer.N);
        end;
      else
        PLuaDefaultInterface(LInstance).Value := IInterface(FTempBuffer.N);
      end;
      goto done;
    end;
    vkClosure:
    begin
      case ALuaType of
        LUA_TNIL:
        begin
          FTempBuffer.MUnsafe.Code := nil;
          FTempBuffer.MUnsafe.Data := nil;
        end;
        LUA_TFUNCTION:
        begin
          if (Pointer(lua_tocfunction(Handle, AStackIndex)) <> @LuaClosureWrapper) then
            goto failure;

          lua_getupvalue(Handle, AStackIndex, 1);
          LClosure := lua_touserdata(Handle, -1);
          stack_pop;
          Inc(NativeUInt(LClosure), SizeOf(Pointer));
          if (LClosure.Mode <> F.ClosureMode) then
            goto failure;

          FTempBuffer.MUnsafe.Code := LClosure.Address;
          FTempBuffer.MUnsafe.Data := LClosure.Instance;
        end;
      else
        goto failure;
      end;

      case F.ClosureMode of
        mmProc:
        begin
          PPointer(LInstance)^ := FTempBuffer.MUnsafe.Code;
        end;
        mmMethod:
        begin
          {$ifdef WEAKINTFREF}
          if ({TLuaExtendedSetter}(F.Options shr OPTIONS_SETTER_SHIFT) = Cardinal(esUnsafe)) then
          begin
            PMethod(LInstance)^ := FTempBuffer.MUnsafe;
          end else
          {$endif}
          begin
            TLuaOnPreprocessScript(LInstance^) := TLuaOnPreprocessScript(Pointer(@FTempBuffer.MUnsafe)^);
          end;
        end;
      else
        // mmReference:
        if (PPointer(LInstance)^ <> FTempBuffer.MUnsafe.Data) then
        case {TLuaExtendedSetter}(F.Options shr OPTIONS_SETTER_SHIFT) of
          {$ifdef WEAKINTFREF}
          Ord(esWeakManaged):
          begin
            PLuaWeakInterface(LInstance).Value := IInterface(FTempBuffer.MUnsafe.Data);
          end;
          {$endif}
          Ord(esUnsafe):
          begin
            PPointer(LInstance)^ := Pointer(FTempBuffer.MUnsafe.Data);
          end;
        else
          PLuaDefaultInterface(LInstance).Value := IInterface(FTempBuffer.MUnsafe.Data);
        end;
      end;
      goto done;
    end
  else
    // vkRecord, vkArray, vkSet, vkLuaArg
    if (IsExtendedMetaPointer) then
    begin
      case ALuaType of
        LUA_TNIL:
        begin
          LValue := 0;
          goto set_native;
        end;
        LUA_TLIGHTUSERDATA:
        begin
          LValue := NativeInt(lua_touserdata(Handle, AStackIndex));
          goto set_native;
        end;
        LUA_TUSERDATA:
        begin
          LValue := NativeInt(lua_touserdata(Handle, AStackIndex));
          if (LValue <> 0) then
          begin
            LValue := NativeInt(PLuaUserData(LValue).Instance);
          end;
          goto set_native;
        end;
      end;
    end;

    if (AssignMetaType(ALua, AStackIndex, LInstance, Self)) then
      goto done;
  failure:
    stack_force_unicode_string(FStringBuffer.Unicode, AStackIndex);
    GetLuaTypeName(FStringBuffer.Default, lua_type(Handle, AStackIndex));
    Result := False;
    Exit;
  end;

set_native:
  PNativeInt(LInstance)^ := LValue;

done:
  Result := True;
end;

const
  PARAM_TOTALPOINTERDEPTH_MASK = (1 shl 3 - 1);
  PARAM_MAX_TOTALPOINTERDEPTH = PARAM_TOTALPOINTERDEPTH_MASK - 1;
  PARAM_REFERENCE_MODE = 1 shl 3;
  PARAM_ARRAY_MODE = 1 shl 4;
  PARAM_POINTER_MODE = 1 shl 5;
  PARAM_INSURANCE_MODE = 1 shl 6;
  PARAM_ARMPARTIAL_MODE = 1 shl 7;

type
  TLuaInvokableParam = object(TLuaCustomVariable)
  private
    {
      Flags:
      TotalPointerDepth: Byte:3;
      IsReference: Boolean:1;
      IsArray: Boolean:1;
      IsPointer: Boolean:1;
      IsInsurance: Boolean:1;
      IsARMPartial: Boolean:1;
    }
    function GetIsReference: Boolean;
    procedure SetIsReference(const AValue: Boolean);
    function GetIsArray: Boolean;
    procedure SetIsArray(const AValue: Boolean);
    function GetIsPointer: Boolean;
    procedure SetIsPointer(const AValue: Boolean);
    function GetIsInsurance: Boolean;
    procedure SetIsInsurance(const AValue: Boolean);
    function GetIsARMPartial: Boolean;
    procedure SetIsARMPartial(const AValue: Boolean);
    function GetTotalPointerDepth: Byte;
    procedure SetTotalPointerDepth(const AValue: Byte);
  public
    Name: PShortString;
    Value: Integer;
    DataValue: Integer;
    InsuranceValue: Integer;

    property IsReference: Boolean read GetIsReference write SetIsReference;
    property IsArray: Boolean read GetIsArray write SetIsArray;
    property IsPointer: Boolean read GetIsPointer write SetIsPointer;
    property IsInsurance: Boolean read GetIsInsurance write SetIsInsurance;
    property IsARMPartial: Boolean read GetIsARMPartial write SetIsARMPartial;
    property TotalPointerDepth: Byte read GetTotalPointerDepth write SetTotalPointerDepth;
  end;
  PLuaInvokableParam = ^TLuaInvokableParam;
  TLuaInvokableParams = array[0..0] of TLuaInvokableParam;
  PLuaInvokableParams = ^TLuaInvokableParams;

function TLuaInvokableParam.GetIsReference: Boolean;
begin
  Result := (F.Options and PARAM_REFERENCE_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsReference(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PARAM_REFERENCE_MODE;
  end else
  begin
    F.Options := F.Options and (not PARAM_REFERENCE_MODE);
  end;
end;

function TLuaInvokableParam.GetIsArray: Boolean;
begin
  Result := (F.Options and PARAM_ARRAY_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsArray(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PARAM_ARRAY_MODE;
  end else
  begin
    F.Options := F.Options and (not PARAM_ARRAY_MODE);
  end;
end;

function TLuaInvokableParam.GetIsPointer: Boolean;
begin
  Result := (F.Options and PARAM_POINTER_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsPointer(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PARAM_POINTER_MODE;
  end else
  begin
    F.Options := F.Options and (not PARAM_POINTER_MODE);
  end;
end;

function TLuaInvokableParam.GetIsInsurance: Boolean;
begin
  Result := (F.Options and PARAM_INSURANCE_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsInsurance(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PARAM_INSURANCE_MODE;
  end else
  begin
    F.Options := F.Options and (not PARAM_INSURANCE_MODE);
  end;
end;

function TLuaInvokableParam.GetIsARMPartial: Boolean;
begin
  Result := (F.Options and PARAM_ARMPARTIAL_MODE <> 0);
end;

procedure TLuaInvokableParam.SetIsARMPartial(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PARAM_ARMPARTIAL_MODE;
  end else
  begin
    F.Options := F.Options and (not PARAM_ARMPARTIAL_MODE);
  end;
end;

function TLuaInvokableParam.GetTotalPointerDepth: Byte;
begin
  Result := F.Options and PARAM_TOTALPOINTERDEPTH_MASK;
end;

procedure TLuaInvokableParam.SetTotalPointerDepth(const AValue: Byte);
begin
  if (AValue >= PARAM_MAX_TOTALPOINTERDEPTH) then
  begin
    F.Options := (F.Options and (not PARAM_TOTALPOINTERDEPTH_MASK)) + PARAM_MAX_TOTALPOINTERDEPTH;
  end else
  begin
    F.Options := (F.Options and (not PARAM_TOTALPOINTERDEPTH_MASK)) + AValue;
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
          4: (_: packed record end);
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
          3: (_: packed record end);
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
                2: (_: packed record end);
             );
          3: (__: packed record end);
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
          2: (_: packed record end);
      );
      1:
      (
        RegsGen: array[0..7] of Int64;
        case Integer of
          0: (RegsExt: array[0..7] of m128);
          1: (Regs128x4: m128x4);
          2: (__: packed record end);
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
  TLuaResultMode = (rmNone, rmRegister, rmHFASingle, rmHFADouble, rmBuffer, rmManagedBuffer, rmUserData);
  TLuaStackCleaner = (scCaller, scInvokable, scInvokableEx);

  PLuaInvokable = ^TLuaInvokable;
  TLuaInvokable = object
  public
    MethodKind: TLuaMethodKind;
    CallConv: TCallConv;
    ResultMode: TLuaResultMode;
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
    MinParamCount: Word;
    MaxParamCount: Word;
    {ToDo: default params}
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
  i, TotalPointerDepth, Options: Integer;
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
    Options := Param.F.Options;
    if (Options and (PARAM_INSURANCE_MODE or PARAM_TOTALPOINTERDEPTH_MASK) <> 0) then
    begin
      // insurance
      if (Options and PARAM_INSURANCE_MODE <> 0) then
      begin
        Dec(NativeUInt(Ptr), SizeOf(Pointer));
        PNativeInt(Ptr)^ := 0;
      end;

      // references
      TotalPointerDepth := Options and PARAM_TOTALPOINTERDEPTH_MASK;
      if (TotalPointerDepth <> 0) then
      begin
        if (TotalPointerDepth <> 1) then
        repeat
          Dec(NativeInt(Ptr), SizeOf(Pointer));
          PNativeInt(Ptr)^ := NativeInt(Ptr) + SizeOf(Pointer);
          Dec(TotalPointerDepth);
        until (TotalPointerDepth = 1);

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
  i, Options, VType: Integer;
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
      vkObject:
      begin
        FinalizeArray(Ptr, TypeInfo(TObject), Count);
      end;
      {$endif}
      vkInterface:
      begin
        FinalizeArray(Ptr, TypeInfo(IInterface), Count);
      end;
      vkVariant:
      begin
        FinalizeArray(Ptr, TypeInfo(Variant), Count);
      end;
      vkString:
      begin
        case Param.F.StringType of
            stAnsiString: FinalizeArray(Ptr, TypeInfo(AnsiString), Count);
            stWideString: FinalizeArray(Ptr, TypeInfo(WideString), Count);
         stUnicodeString: FinalizeArray(Ptr, TypeInfo(UnicodeString), Count);
        end;
      end;
      vkClosure:
      begin
        case Param.F.ClosureMode of
          mmReference:
          begin
            FinalizeArray(Ptr, TypeInfo(IInterface), Count);
          end;
          {$ifdef WEAKINSTREF}
          mmMethod:
          begin
            FinalizeArray(Ptr, TypeInfo(TLuaOnPreprocessScript{TMethodProc}), Count);
          end;
          {$endif}
        end;
      end;
      vkRecord, vkArray:
      begin
        MetaType := Param.F.MetaType;

        for i := 1 to Count do
        begin
          MetaType.Clear(Ptr^, False);
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

    Options := Param.F.Options;
    if (Options and PARAM_INSURANCE_MODE = 0) then
    begin
      case ((Options shr 8) and $7f) of
        Ord(vkString):
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
        Ord(vkVariant):
        begin
          VType := PVarData(Ptr).VType;
          if (VType and varDeepData <> 0) then
          case VType of
            varBoolean, varUnknown+1..$15{varUInt64}: ;
          else
            System.VarClear(PVariant(Ptr)^);
          end;
        end;
        Ord(vkClosure):
        begin
          if (Param.F.ClosureMode = mmMethod) then
          begin
            TLuaOnPreprocessScript(Ptr^) := nil;
          end else
          begin
            // mmReference
            if (PNativeInt(Ptr)^ <> 0) then
              PLuaDefaultInterface(Ptr).Value := nil;
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
      if (Options and PARAM_ARRAY_MODE = 0) then
      begin
        case Param.F.Kind of
          vkString:
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
          Options{Count} := PInteger(NativeInt(ParamBlock) + Param.DataValue)^ {$ifNdef FPC} + 1{$endif};
          Dec(Param);
          _FinalizeArray(Param, Ptr, Options{Count});
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

      cmp byte ptr [esi].TLuaInvokable.Result.F.Kind, vkFloat
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
    raise Exception.CreateRes(Pointer(@SParameterCountExceeded)) at ReturnAddress;
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
    if (Self.ResultMode in [rmHFASingle, rmHFADouble]) then
    begin
      if (Self.ResultMode = rmHFASingle) then
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

    function NewInvokable(const MetaType: PLuaMetaType; const MethodKind: TLuaMethodKind;
      const CallConv: TCallConv; const ResultType: PTypeInfo; const ResultTypeName: PShortString;
      const ResultReference: TLuaReference): PLuaInvokable;
    function InternalAddName(const Name: LuaString): NativeInt;
    function InternalAddParam(const Name: PShortString; const TypeInfo: Pointer; const TypeName: PShortString;
      const PointerDepth: Integer; const ParamFlags: TParamFlags): PLuaInvokableParam; overload;
    function InternalAddParam(const Param: TLuaProcParam): PLuaInvokableParam; overload;
    procedure InternalParamsDone(var Invokable: TLuaInvokable);
    function InternalBuildDone: __luapointer;
  public
    procedure Clear;
    function BuildMethod(const MetaType: PLuaMetaType; const AMethod: PTypeInfo; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    {$ifdef EXTENDEDRTTI}
    function BuildProcedureSignature(const MetaType: PLuaMetaType; const ASignature: PProcedureSignature; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildProcedure(const MetaType: PLuaMetaType; const AProcedure: PTypeInfo; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildReferenceMethod(const MetaType: PLuaMetaType; const AReference: PTypeInfo; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    {$endif}
    function BuildIntfMethod(const MetaType: PLuaMetaType; const AMethodEntry: PIntfMethodEntry; MethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildClassMethod(const MetaType: PLuaMetaType; const AMethodEntry: PClassMethodEntry; MethodKind: TLuaMethodKind = TLuaMethodKind($ff); const SkipSelf: Boolean = True): __luapointer;
    function BuildCustom(const MetaType: PLuaMetaType; const Params: array of TLuaProcParam;
      const ResultType: PTypeInfo; const IsResultUnsafe: Boolean;
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

function TLuaInvokableBuilder.NewInvokable(const MetaType: PLuaMetaType;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv;
  const ResultType: PTypeInfo; const ResultTypeName: PShortString;
  const ResultReference: TLuaReference): PLuaInvokable;
{$ifdef WEAKREF}
label
  result_reference;
{$endif}
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

  if (MethodKind = mkConstructor) then
  begin
    if (MetaType.F.Kind = mtClass) then
    begin
      Result.Result.F.Kind := vkObject;
      Result.Result.F.MetaType := FLua.FObjectMetaType;
    end else
    begin
      Result.Result.F.Kind := vkRecord;
      Result.Result.F.MetaType := MetaType;
    end;
  end else
  if (Assigned(ResultType)) or (Assigned(ResultTypeName)) then
  begin
    Result.Result.InternalSetRttiType(FLua, ResultType, ResultTypeName);
    if (Result.Result.Kind = vkUnknown) then
    begin
      FLua.FStringBuffer.Default := Format('Invalid result type "%s" (%s)', [
        FLua.FStringBuffer.Lua, FLua.FStringBuffer.Default]);
      Result := nil;
      Exit;
    end
    {$ifdef CPUX86}
    else
    if (Result.Result.Kind = vkFloat) and (CallConv <> ccSafeCall) and
      (Result.Result.F.FloatType in [ftSingle, ftDouble]) then
    begin
      Result.Result.F.FloatType := ftExtended;
    end
    {$endif}
    ;

    {$ifdef WEAKREF}
    if (not Result.Result.IsExtendedMetaPointer) then
    case Result.Result.Kind of
      vkClosure:
      begin
        case Result.Result.ClosureMode of
          {$ifdef WEAKINSTREF}
          mmMethod: goto result_reference;
          {$endif}
          mmReference: goto result_reference;
        end;
      end;
      {$ifdef WEAKINSTREF}
      vkObject,
      {$endif}
      vkInterface:
      begin
      result_reference:
        if (ResultReference = TLuaReference($ff)) then
        begin
          {$ifdef EXTENDEDRTTI}
          Result.Result.ExtendedGetter := egUnsafe;
          {$endif}
        end else
        if (ResultReference <> lrDefault) then
        begin
          Result.Result.ExtendedGetter := egUnsafe;
        end;
      end;
    end;
    {$endif}
  end;

  {$ifdef CPUX86}
  if CallConv = ccCdecl then
  begin
    Result.StackCleaner := scInvokable;

    {$ifdef MACOS}
    if (Result.Result.F.Kind = vkRecord) then
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
  const TypeName: PShortString; const PointerDepth: Integer; const ParamFlags: TParamFlags): PLuaInvokableParam;
{$ifdef WEAKREF}
label
  managed_reference;
{$endif}
var
  CustomVariable: TLuaCustomVariable;
  TotalPointerDepth: Integer;
  HighArray: PLuaInvokableParam;

  function CallConv: TCallConv;
  begin
    Result := PLuaInvokable(FBuffer.FBytes).CallConv;
  end;

  function IsConst: Boolean;
  begin
    Result := (pfConst in ParamFlags);
  end;
begin
  {$ifdef EXTENDEDRTTI}
  if (pfResult in ParamFlags) then
  begin
    Result := @PLuaInvokable(FBuffer.FBytes).Result;

    {$ifdef WEAKREF}
    if (not Result.IsExtendedMetaPointer) then
    case Result.Kind of
      vkClosure:
      begin
        case Result.ClosureMode of
          {$ifdef WEAKINSTREF}
          mmMethod: goto managed_reference;
          {$endif}
          mmReference: goto managed_reference;
        end;
      end;
      {$ifdef WEAKINSTREF}
      vkObject,
      {$endif}
      vkInterface:
      begin
      managed_reference:
        Result.ExtendedGetter := egOwned;
      end;
    end;
   {$endif}

    Exit;
  end;
  {$endif}

  TotalPointerDepth := CustomVariable.InternalSetRttiType(FLua, TypeInfo, TypeName, True);
  if (TotalPointerDepth < 0) then
  begin
    Result := nil;
    Exit;
  end;
  Inc(TotalPointerDepth, PointerDepth);

  Result := FBuffer.Alloc(SizeOf(TLuaInvokableParam));
  Inc(PLuaInvokable(FBuffer.FBytes).MaxParamCount);

  PLuaCustomVariable(Result)^ := CustomVariable;
  Result.Name := Name;
  Result.Value := VALUE_NOT_DEFINED;
  Result.DataValue := VALUE_NOT_DEFINED;
  Result.InsuranceValue := VALUE_NOT_DEFINED;

  Result.IsArray := (pfArray in ParamFlags);
  Result.IsReference := ([pfVar, pfReference, pfOut] * ParamFlags <> []);
  Result.IsPointer := (Result.F.Kind = vkPointer) or (TotalPointerDepth <> 0) {or (Result.IsExtendedMetaPointer)} or
    ((Result.F.Kind = vkString) and (Result.F.StringType in [stPAnsiChar, stPWideChar]));

  if (not (Result.IsArray or Result.IsReference or Result.IsPointer)) then
  begin
    case Result.F.Kind of
      vkVariant:
      begin
        Result.IsReference :=
        {$if Defined(CPUX86)}
          IsConst or (not (CallConv in [ccCdecl, ccStdCall, ccSafeCall]))
        {$else}
          True
        {$ifend};
      end;
      vkClosure:
      begin
        Result.IsReference :=
        {$if Defined(CPUX86)}
          False
        {$else}
          (Result.F.ClosureMode = mmMethod);
        {$ifend};
      end;
      vkString:
      begin
        Result.IsReference := (Result.F.StringType = stShortString);
      end;
      vkRecord, vkArray, vkSet:
      begin
        Result.IsReference := Result.F.MetaType.ParamReferenced(CallConv, IsConst);
      end;
    end;
  end;

  Result.IsInsurance := Result.IsArray or
   ((Result.F.Kind = vkString) and (Result.F.StringType in [stPAnsiChar, stPWideChar]));

  Result.TotalPointerDepth := TotalPointerDepth + Ord(Result.IsReference);
  if (Result.IsArray) then
  begin
    HighArray := FBuffer.Alloc(SizeOf(TLuaInvokableParam));
    HighArray.InternalSetTypeInfo(FLua, System.TypeInfo(Integer));
    HighArray.Name := Pointer(@PARAMNAME_HIGH_ARRAY);
    HighArray.Value := VALUE_NOT_DEFINED;
    HighArray.DataValue := VALUE_NOT_DEFINED;
    HighArray.InsuranceValue := VALUE_NOT_DEFINED;
  end else
  case Result.F.Kind of
    vkObject, vkInterface:
    begin
      Result.ExtendedSetter := esUnsafe;
    end;
    vkRecord, vkArray, vkSet:
    begin
      if (Result.TotalPointerDepth <> 0) then
        Result.ExtendedSetter := esMetaReference;
    end;
    vkClosure:
    begin
      Result.ExtendedSetter := esUnsafe;
    end;
  end;
end;

function TLuaInvokableBuilder.InternalAddParam(const Param: TLuaProcParam): PLuaInvokableParam;
begin
  Result := InternalAddParam(Pointer(InternalAddName(Param.Name)), Param.TypeInfo, nil,
    Param.PointerDepth, Param.Flags);
end;

procedure TLuaInvokableBuilder.InternalParamsDone(var Invokable: TLuaInvokable);
const
  MASK_BUFFERED = 1 shl 30;
  NATIVE_SHIFT = {$ifdef LARGEINT}3{$else}2{$endif};
type
  TResultArgMode = (ramNone, ramRegister, ramRefFirst, ramRefLast);
  TSelfArgMode = (samNone, samFirst, samSecond, samLast);
var
  i: Integer;
  IsStatic, IsConstructor, IsBackwardArg: Boolean;
  ResultArgMode: TResultArgMode;
  SelfArgMode: TSelfArgMode;
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
    if (Param.TotalPointerDepth <> 0) then
    begin
      Param.DataValue := PutPtr(Param.Value);

      if (Param.TotalPointerDepth > 1) then
      begin
        PutBuffer(Param.DataValue, (Param.TotalPointerDepth - 1) * SizeOf(Pointer));
        Inc(Param.DataValue, (Param.TotalPointerDepth - 1) * SizeOf(Pointer));
      end;

      case Param.F.Kind of
        vkRecord, vkSet, vkArray:
        begin
          if (Param.TotalPointerDepth > 1) then
            Dec(Param.DataValue, SizeOf(Pointer));

          Param.TotalPointerDepth := Param.TotalPointerDepth - 1;
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
    if (Param.Kind = vkFloat) and (Param.FloatType in [ftSingle, ftDouble, ftExtended]) then
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
    if (Param.Kind = vkRecord) and (Param.MetaType.HFA) then
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
    if (Param.Kind in [vkRecord{$ifdef CPUARM32}, vkArray{$endif}]) then
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

    if (ResultArgMode = ramRegister) then
    begin
      Invokable.ResultMode := rmRegister;
      Ptr :=
        {$if Defined(CPUINTEL)}
          @PParamBlock(nil).OutEAX
        {$elseif Defined(CPUARM32)}
          @PParamBlock(nil).OutCR
        {$else .CPUARM64}
          @PParamBlock(nil).OutX
        {$ifend}
        ;

      if (Param.F.Kind = vkFloat) then
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
      if (Param.F.Kind = vkRecord) and (Param.MetaType.HFA) then
      begin
        Param.IsARMPartial := True;
        Ptr := @PParamBlock(nil).OutHFA;

        if (Param.MetaType.HFAElementType = ftSingle) then
        begin
          Invokable.ResultMode := rmHFASingle;
        end else
        begin
          Invokable.ResultMode := rmHFADouble;
        end;
      end;
      {$endif}

      Param.Value := NativeInt(Ptr);
      Param.DataValue := Param.Value;
      Exit;
    end;

    {$ifdef CPUARM64}
    if (ResultMode = rmRefFirst) then
    begin
      Param.Value := NativeInt(@PParamBlock(nil).RegX8);
    end else
    {$endif}
    begin
      PutPtr(Param.Value);
    end;

    if (Param.F.Kind in [vkObject, vkInterface, vkRecord, vkArray, vkSet]) then
    begin
      // user data
      Invokable.ResultMode := rmUserData;
    end else
    begin
      // buffer
      Param.TotalPointerDepth := 1;
      Invokable.ResultMode := rmBuffer;
      PutBuffer(Param.DataValue, Param.Size);

      // is managed
      case Param.F.Kind of
        vkString:
        begin
          if (Param.F.StringType in [stAnsiString, stWideString, stUnicodeString]) then
            Invokable.ResultMode := rmManagedBuffer;
        end;
        vkVariant:
        begin
          Invokable.ResultMode := rmManagedBuffer;
        end;
        vkClosure:
        begin
          if (Param.ExtendedGetter <> egUnsafe) then
            Invokable.ResultMode := rmManagedBuffer;
        end;
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
  IsStatic := (Invokable.MethodKind = mkStatic) or
    ((Invokable.Result.Kind <> vkClass) and (Invokable.MethodKind = mkConstructor));
  IsConstructor := (Invokable.MethodKind = mkConstructor);
  IsBackwardArg := {$ifdef CPUX86}(Invokable.CallConv in [ccCdecl, ccStdCall, ccSafeCall]){$else}True{$endif};

  // result argument mode
  ResultArgMode := ramNone;
  Param := @Invokable.Result;
  if (Param.F.Kind <> vkUnknown) then
  begin
    ResultArgMode := ramRegister;
    if (Param.IsReference) then
    begin
      ResultArgMode := ramRefLast;

      {$if not Defined(CPUX86)}
      if (Invokable.CallConv <> ccSafeCall) then
      begin
        ResultArgMode := ramRefFirst;
      end;
      {$ifend}
    end;
  end;

  // self argument mode
  SelfArgMode := samNone;
  if (not IsStatic) then
  begin
    {$ifdef CPUX86}
    if (Invokable.CallConv = ccPascal) then
    begin
      SelfArgMode := samLast;
    end else
    {$endif}
    begin
      SelfArgMode := samFirst;

      {$if Defined(CPUARM)}
      if (ResultArgMode = ramRefFirst) then
      begin
        SelfArgMode := samSecond;
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
  if (SelfArgMode = samFirst) then PutSelf;
  if (ResultArgMode = ramRefFirst) then PutResult;
  if (SelfArgMode = samSecond) then PutSelf;

  // constructor flag
  if (IsConstructor) and (Invokable.Result.Kind = vkClass) then
  begin
    TempParam.InternalSetTypeInfo(nil, TypeInfo(ShortInt));
    Param := @TempParam;
    PutArg;
    Invokable.ConstructorFlag := TempParam.DataValue;
  end;

  // arguments
  Param := @Invokable.Params[Ord(SelfArgMode = samLast)];
  for i := 0 to Integer(Invokable.MaxParamCount) - 1 do
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
  if (ResultArgMode = ramRefLast) then PutResult;
  if (SelfArgMode = samLast) then PutSelf;
  if (ResultArgMode = ramRegister) then PutResult;

  // total buffer (stack) size
  // calculate real offsets
  Inc(Invokable.TotalDataSize, SizeOf(TLuaInvokable) + Invokable.StackDataSize);
  Param := @Invokable.Params[0];
  for i := 0 to Integer(Invokable.MaxParamCount) - 1 do
  begin
    ApplyOffset;

    if (Param.IsArray) then
    begin
      Inc(Param);
      ApplyOffset;
    end;

    Inc(Param);
  end;
  if (ResultArgMode in [ramRefFirst, ramRefLast]) then
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
    Result := (Param.F.Kind in [vkRecord, vkArray]) and
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
      vkVariant:
      begin
        Result := True;
      end;
      vkString:
      begin
        Result := (Param.F.StringType in [stAnsiString, stWideString, stUnicodeString]);
      end;
      vkClosure:
      begin
        if (Param = @Invokable.Result) then
        begin
          if (Param.ExtendedGetter <> egUnsafe) then
            Result := True;
        end else
        begin
          if (Param.ExtendedSetter <> esUnsafe) then
            Result := True;
        end;
      end;
    end;
  end;

  function IsParamInitial: Boolean;
  begin
    Result := (Param.TotalPointerDepth <> 0) or (Param.IsInsurance) or (IsParamFinal);
  end;
begin
  // result param reference and getter mode
  Invokable := Pointer(FBuffer.FBytes);
  Param := @Invokable.Result;
  if (Param.F.Kind <> vkUnknown) then
  begin
    if (Param.IsExtendedMetaPointer) then
    begin
      Param.IsPointer := True;
    end;

    if (Invokable.CallConv = ccSafeCall) then
    begin
      Param.IsReference := True;
    end else
    case Param.Kind of
      {$ifdef AUTOREFCOUNT}
      vkObject:
      begin
        if (Invokable.MethodKind = mkConstructor) then
        begin
          Param.ExtendedGetter := egUnsafe;
        end else
        begin
          if (Param.ExtendedGetter <> egUnsafe) then
            Param.IsReference := True;
        end;
      end;
      {$endif}
      vkString:
      begin
        if (Param.F.StringType in [stShortString, stAnsiString, stWideString, stUnicodeString]) then
          Param.IsReference := True;
      end;
      vkVariant:
      begin
        Param.IsReference := True;
      end;
      vkRecord, vkArray, vkSet:
      begin
        if (Param.F.MetaType.ReturnReferenced) then
          Param.IsReference := True;
      end;
      vkInterface:
      begin
        if (Param.ExtendedGetter <> egUnsafe) then
          Param.IsReference := True;
      end;
      vkClosure:
      begin
        case Param.F.ClosureMode of
          mmMethod:
          begin
            Param.IsReference := True;
          end;
          mmReference:
          begin
            if (Param.ExtendedGetter <> egUnsafe) then
              Param.IsReference := True;
          end;
        end;
      end;
    end;
  end;

  // buffering
  InternalParamsDone(Invokable^);

  // align advanced buffer
  if (FAdvanced.Size and (SizeOf(Pointer) - 1) <> 0) then
  begin
    FAdvanced.Alloc(SizeOf(Pointer) - (FAdvanced.Size and (SizeOf(Pointer) - 1)));
  end;

  // initial params
  Invokable := Pointer(FBuffer.FBytes);
  Invokable.MinParamCount := Invokable.MaxParamCount;
  Invokable.Initials := Pointer(FBuffer.Size + FAdvanced.Size);
  Param := @Invokable.Params[0];
  for i := 0 to Integer(Invokable.MaxParamCount) - 1 do
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
  if (Invokable.ResultMode in [rmBuffer, rmManagedBuffer]) or (IsParamHFAManaged) then
  begin
    PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(Param) - NativeInt(Invokable);
    Inc(Invokable.InitialCount);
  end;

  // final params
  Param := @Invokable.Params[0];
  Invokable.Finals := Pointer(FBuffer.Size + FAdvanced.Size);
  for i := 0 to Integer(Invokable.MaxParamCount) - 1 do
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
  if (Invokable.ResultMode = rmManagedBuffer) or (IsParamHFAManaged) then
  begin
    PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(Param) - NativeInt(Invokable);
    Inc(Invokable.FinalCount);
  end;

  // concatenate buffers, unpack internal names
  Offset := FBuffer.Size;
  if (FAdvanced.Size <> 0) then
  begin
    System.Move(Pointer(FAdvanced.FBytes)^, FBuffer.Alloc(FAdvanced.Size)^, FAdvanced.Size);
  end;
  Invokable := Pointer(FBuffer.FBytes);
  Param := @Invokable.Params[0];
  Offset := Offset{Last FBuffer.Size} + NativeInt(Invokable);
  for i := 0 to Integer(Invokable.MaxParamCount) - 1 do
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
  for i := 0 to Integer(Invokable.MaxParamCount) - 1 do
  begin
    if (NativeUInt(Param.Name) >= NativeUInt(FBuffer.FBytes)) and
      (NativeUInt(Param.Name) < NativeUInt(FBuffer.FBytes) + NativeUInt(FBuffer.Size)) then
      Inc(NativeInt(Param.Name), Offset);

    if (Param.IsArray) then
      Inc(Param){"High(Array)"};

    Inc(Param);
  end;
end;

function TLuaInvokableBuilder.BuildMethod(const MetaType: PLuaMetaType; const AMethod: PTypeInfo;
  MethodKind: TLuaMethodKind): __luapointer;
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
  if (not Assigned(NewInvokable(MetaType, MethodKind, MethodInfo.CC,
    MethodInfo.ResultTypeInfo, MethodInfo.ResultTypeName, lrDefault{TLuaReference($ff)}))) then
    Exit;

  // parameters
  Ptr := Pointer(@TypeData.ParamList);
  for i := 0 to ParamCount - 1 do
  begin
    ReadParam(i);
    if (InternalAddParam(Param.Name, Param.TypeInfo, Param.TypeName, 0, Param.Flags) = nil) then
      Exit;
  end;

  // done
  Result := InternalBuildDone;
end;

{$ifdef EXTENDEDRTTI}
function TLuaInvokableBuilder.BuildProcedureSignature(const MetaType: PLuaMetaType;
  const ASignature: PProcedureSignature; MethodKind: TLuaMethodKind): __luapointer;
var
  i: Integer;
  CallConv: TCallConv;
  ResultType: PTypeInfo;
  Param: PProcedureParam;
  TypeInfo: PTypeInfo;
begin
  Result := LUA_POINTER_INVALID;
  if (ASignature.Flags = 255) then
  begin
    // error todo
    Exit;
  end;

  // method information
  if (MethodKind = TLuaMethodKind($ff)) then
  begin
    MethodKind := mkStatic;
  end;
  CallConv := ASignature.CC;
  ResultType := GetTypeInfo(ASignature.ResultType);
  if (not Assigned(NewInvokable(MetaType, MethodKind, CallConv, ResultType, nil, lrDefault{TLuaReference($ff)}))) then
    Exit;

  // parameters
  Param := Pointer(NativeUInt(ASignature) + SizeOf(TProcedureSignature));
  for i := 0 to Integer(ASignature.ParamCount) - 1 do
  begin
    TypeInfo := GetTypeInfo(Param.ParamType);
    if (InternalAddParam(Pointer(@Param.Name), TypeInfo, nil, 0, TParamFlags(Param.Flags)) = nil) then
      Exit;

    Param := SkipAttributes(GetTail(Param.Name));
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildProcedure(const MetaType: PLuaMetaType; const AProcedure: PTypeInfo;
  MethodKind: TLuaMethodKind): __luapointer;
begin
  Result := BuildProcedureSignature(MetaType, GetTypeData(AProcedure).ProcSig, MethodKind);
end;

function TLuaInvokableBuilder.BuildReferenceMethod(const MetaType: PLuaMetaType; const AReference: PTypeInfo;
  MethodKind: TLuaMethodKind): __luapointer;
var
  TypeData: PTypeData;
  Table: PIntfMethodTable;
  MethodEntry: PIntfMethodEntry;
begin
  TypeData := GetTypeData(AReference);
  Table := GetTail(TypeData.IntfUnit);
  if (Assigned(Table)) and (Table.Count = 1) and (Table.RttiCount = 1) then
  begin
    MethodEntry := Pointer(NativeUInt(Table) + SizeOf(TIntfMethodTable));
    Result := BuildIntfMethod(MetaType, MethodEntry, MethodKind);
  end else
  begin
    Result := LUA_POINTER_INVALID;
  end;
end;
{$endif}

function TLuaInvokableBuilder.BuildIntfMethod(const MetaType: PLuaMetaType; const AMethodEntry: PIntfMethodEntry;
  MethodKind: TLuaMethodKind): __luapointer;
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

  if (not Assigned(NewInvokable(MetaType, MethodKind, MethodInfo.CC,
    MethodInfo.ResultTypeInfo, MethodInfo.ResultTypeName, lrDefault{TLuaReference($ff)}))) then
    Exit;

  // parameters
  Ptr := GetTail(AMethodEntry.Name);
  Inc(Ptr, SizeOf(Byte) + SizeOf(TCallConv) + SizeOf(Byte));
  ReadParam;
  for i := 1 to ParamCount - 1 do
  begin
    ReadParam;
    if (InternalAddParam(Pointer(@Param.Name), Param.TypeInfo, Param.TypeName, 0, Param.Flags) = nil) then
      Exit;
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildClassMethod(const MetaType: PLuaMetaType;
  const AMethodEntry: PClassMethodEntry; MethodKind: TLuaMethodKind;
  const SkipSelf: Boolean): __luapointer;
type
  PClassMethodParam = ^TClassMethodParam;
  TClassMethodParam = packed record
    Flags: TParamFlags;
    ParamType: PPTypeInfo;
    Location: Word;
    Name: ShortString;
  end;
var
  Ptr: PByte;
  Count, i: Integer;
  CallConv: TCallConv;
  ResultType: PTypeInfo;
  Param: PClassMethodParam;
begin
  Result := LUA_POINTER_INVALID;
  Ptr := GetTail(AMethodEntry.Name);
  if (Ptr = Pointer(NativeUInt(AMethodEntry) + AMethodEntry.Size)) then
  begin
    // todo?
    Exit;
  end;

  // method information
  if (MethodKind = TLuaMethodKind($ff)) then
  begin
    MethodKind := mkInstance;
  end;
  Inc(Ptr); // Version
  CallConv := TCallConv(Ptr^);
  Inc(Ptr);
  ResultType := GetTypeInfo(PPointer(Ptr)^);
  Inc(Ptr, SizeOf(Pointer));
  if (not Assigned(NewInvokable(MetaType, MethodKind, CallConv, ResultType, nil, TLuaReference($ff)))) then
    Exit;

  // parameters
  Inc(Ptr, SizeOf(Word)); // ParOff
  Count := Ptr^;
  Inc(Ptr);
  if (SkipSelf) then
  begin
    Dec(Count);
    Ptr := GetTail(PClassMethodParam(Ptr).Name);
    Ptr := SkipAttributes(Ptr);
  end;
  for i := 1 to Count do
  begin
    Param := Pointer(Ptr);
    if (InternalAddParam(Pointer(@Param.Name), GetTypeInfo(Param.ParamType), nil, 0, Param.Flags) = nil) then
      Exit;

    Ptr := GetTail(Param.Name);
    Ptr := SkipAttributes(Ptr);
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildCustom(const MetaType: PLuaMetaType; const Params: array of TLuaProcParam;
  const ResultType: PTypeInfo; const IsResultUnsafe: Boolean;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv): __luapointer;
var
  i: Integer;
begin
  Result := LUA_POINTER_INVALID;
  if (not Assigned(NewInvokable(MetaType, MethodKind, CallConv, ResultType, nil,
    TLuaReference(Ord(IsResultUnsafe) * 2{lrDefault/lrUnsafe})))) then
    Exit;

  for i := Low(Params) to High(Params) do
  if (not Assigned(InternalAddParam(Params[i]))) then
    Exit;

  Result := InternalBuildDone;
end;


{ Name space management structures  }

const
  {$ifdef SMALLINT}
    PROPSLOT_MASK    = NativeUInt($FF000000);
    PROPSLOT_FIELD   = NativeUInt($FF000000);
    PROPSLOT_VIRTUAL = NativeUInt($FE000000);
  {$else .LARGEINT}
    PROPSLOT_MASK    = NativeUInt($FF00000000000000);
    PROPSLOT_FIELD   = NativeUInt($FF00000000000000);
    PROPSLOT_VIRTUAL = NativeUInt($FE00000000000000);
  {$endif}
  PROPSLOT_CLEAR = (not PROPSLOT_MASK);

  PROP_SLOT_MASK = 7;
  PROP_SLOTGETTER_MASK = PROP_SLOT_MASK;
  PROP_SLOTSETTER_SHIFT = 3;
  PROP_SLOTSETTER_MASK = PROP_SLOT_MASK shl PROP_SLOTSETTER_SHIFT;
  PROP_STATIC_MODE = 1 shl 6;
  PROP_COMPLEX_MODE = 1 shl 7;

  NAMESPACE_STD_MASK = Integer($ffffff00);
  NAMESPACE_STD_PROC = $80;
  NAMESPACE_FLAG_PROC = 1;
  NAMESPACE_FLAG_INHERITED = 2;
  NAMESPACE_FLAGS_CLEAR = -4;

type
  TLuaPropertyMode = (pmNone, pmField, pmStaticProc, pmStaticProcIndex, pmVirtualProc, pmVirtualProcIndex);

  TLuaProperty = object(TLuaCustomVariable)
  private
    {
      Flags:
      GetterMode: TLuaPropertyMode:3;
      SetterMode: TLuaPropertyMode:3;
      IsStatic: Boolean:1;
      IsComplex: Boolean:1;
    }
    FIndex: packed record
    case Integer of
      0: (Value: Integer);
      1: (ComplexPtr: __luapointer);
    end;

    function GetIsField: Boolean;
    function GetIsStatic: Boolean;
    procedure SetIsStatic(const AValue: Boolean);
    function GetIsComplex: Boolean;
    procedure SetIsComplex(const AValue: Boolean);
    function GetGetterMode: TLuaPropertyMode;
    procedure SetGetterMode(const AValue: TLuaPropertyMode);
    function GetSetterMode: TLuaPropertyMode;
    procedure SetSetterMode(const AValue: TLuaPropertyMode);
  public
    Getter: NativeUInt;
    Setter: NativeUInt;

    property IsField: Boolean read GetIsField;
    property IsStatic: Boolean read GetIsStatic write SetIsStatic;
    property IsComplex: Boolean read GetIsComplex write SetIsComplex;
    property GetterMode: TLuaPropertyMode read GetGetterMode write SetGetterMode;
    property SetterMode: TLuaPropertyMode read GetSetterMode write SetSetterMode;

    property Index: Integer read FIndex.Value write FIndex.Value;
    property ComplexPtr: __luapointer read FIndex.ComplexPtr write FIndex.ComplexPtr;
  end;
  PLuaProperty = ^TLuaProperty;


  // global entity
  TLuaGlobalKind = (gkMetaType, gkVariable, gkProc, gkConst{Script}, gkScriptVariable);
  TLuaGlobalEntity = record
    Kind: TLuaGlobalKind;
    Constant: Boolean; // True means can't be changed by native Variables[] property
    case Integer of
      0: (Ptr: __luapointer);
      1: (Ref: Integer); // script entity LUA_GLOBALSINDEX index
  end;
  PLuaGlobalEntity = ^TLuaGlobalEntity;


{ TLuaProperty }

function TLuaProperty.GetIsField: Boolean;
var
  LValue: Cardinal;
begin
  LValue := Self.F.Options and (PROP_SLOT_MASK + (PROP_SLOT_MASK shl PROP_SLOTSETTER_SHIFT));
  Result := (LValue = (Cardinal(pmField) + Cardinal(pmField) shl PROP_SLOTSETTER_SHIFT));
end;

function TLuaProperty.GetIsStatic: Boolean;
begin
  Result := (Self.Options and PROP_STATIC_MODE <> 0);
end;

procedure TLuaProperty.SetIsStatic(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PROP_STATIC_MODE;
  end else
  begin
    F.Options := F.Options and Cardinal(not PROP_STATIC_MODE);
  end;
end;

function TLuaProperty.GetIsComplex: Boolean;
begin
  Result := (Self.Options and PROP_COMPLEX_MODE <> 0);
end;

procedure TLuaProperty.SetIsComplex(const AValue: Boolean);
begin
  if (AValue) then
  begin
    F.Options := F.Options or PROP_COMPLEX_MODE;
  end else
  begin
    F.Options := F.Options and Cardinal(not PROP_COMPLEX_MODE);
  end;
end;

function TLuaProperty.GetGetterMode: TLuaPropertyMode;
begin
  Result := TLuaPropertyMode(Self.F.Options and PROP_SLOT_MASK);
end;

procedure TLuaProperty.SetGetterMode(const AValue: TLuaPropertyMode);
begin
  Self.Options := (Self.Options and (not PROP_SLOT_MASK)) or Cardinal(AValue);
end;

function TLuaProperty.GetSetterMode: TLuaPropertyMode;
begin
  Result := TLuaPropertyMode((Self.F.Options shr PROP_SLOTSETTER_SHIFT) and PROP_SLOT_MASK);
end;

procedure TLuaProperty.SetSetterMode(const AValue: TLuaPropertyMode);
begin
  Self.Options := (Self.Options and (not (PROP_SLOT_MASK shl PROP_SLOTSETTER_SHIFT))) or
    (Cardinal(AValue) shl PROP_SLOTSETTER_SHIFT);
end;

      (*
// имея проперти инфо, необходимо определить - является ли его тип сложным
// необходимо ли его инициализировать/финализировать
function GetLuaDifficultTypeInfo(const Base: TLuaPropertyInfoBase): ptypeinfo;
begin
  Result := nil;

  case Base.Kind of
    vkVariant: Result := typeinfo(Variant);
  vkInterface: Result := typeinfo(IInterface);
     vkRecord: Result := PLuaRecordInfo(Base.Information).FTypeInfo;
      vkArray: Result := PLuaArrayInfo(Base.Information).FTypeInfo;

     vkString: if (Base.StringType in [stAnsiString, stWideString {todo Unicode ?}]) then
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
      vkRecord: begin
                  Data := Lua.FResultBuffer.AllocRecord(PLuaRecordInfo(Info.Base.Information));

                  with PLuaRecordInfo(Info.Base.Information)^ do
                  IsSimple := (FSize <= 4{что если single?}) and (FTypeInfo = nil);
                end;
       vkArray: begin
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
  if (Info.Base.Kind = vkRecord) and (PLuaRecordInfo(Info.Base.Information).FClassIndex = TMETHOD_CLASS_INDEX)
  and (pint64(Instance)^ = 0) then
  begin
    lua_pushnil(Lua.Handle);
    exit;
  end;

  // push-им значение
  Value := PLuaRecordInfo(Info.Base.Information).FClassIndex; // FClassIndex у всех по одному смещению!!!
  Lua.push_metatype_instance(Lua.ClassesInfo[Value], (Info.read_mode < 0), Data).is_const := IsConst;
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
    vkUnknown: typename := 'ERROR';
  vkLuaArg: typename := 'unknown';
    vkBoolean: typename := BOOL_STRS[Base.BoolType];
      vkInt64: typename := 'int64';
      vkFloat: typename := FLOAT_STRS[Base.FloatType];
     vkObject: typename := 'TObject';
     vkString: begin
                 typename := STRING_STRS[Base.StringType];
                 if (Base.StringType = stShortString) then typename := Format(typename, [Base.str_max_len]);
               end;
    vkVariant: typename := 'variant';
  vkInterface: typename := 'IInterface';
    vkPointer: typename := 'pointer';
      vkClass: typename := 'TClass';
      vkArray: typename := PLuaArrayInfo(Base.Information).Name;
        vkSet: typename := PLuaSetInfo(Base.Information).Name;
     vkRecord: with PLuaRecordInfo(Base.Information)^ do
               if (FClassIndex = TMETHOD_CLASS_INDEX) then typename := 'Event' else typename := Name;
  else
     // vkInteger {Enum}
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

  // extended callback parameters
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
  OPERATOR_CONCAT = 10; {for dynamic arrays only}

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

  OPERATOR_NAMES: array[OPERATOR_NEG..OPERATOR_CONCAT] of Pointer = (
    @ID_UNM, @ID_ADD, @ID_SUB, @ID_MUL, @ID_DIV, @ID_MOD, @ID_POW,
    @ID_EQ, @ID_LT, @ID_LE, @ID_CONCAT);

  // each userdata (except complex properties)
  STD_TYPE = 0;
  STD_TYPE_NAME = 1;
  STD_TYPE_PARENT = 2;
  STD_INHERITS_FROM = 3;
  STD_ASSIGN = 4;
  STD_IS_REF = 5;
  STD_IS_CONST = 6;
  STD_IS_CLASS = 7;
  STD_IS_INTERFACE = 8;
  STD_IS_RECORD = 9;
  STD_IS_ARRAY = 10;
  STD_IS_SET = 11;
  STD_IS_EMPTY = 12;
  STD_FREE = 13;
  // STD_EQUALS ?
  // classes
  STD_CREATE = 14;
  // interfaces
  STD_ADDREF = 15;
  STD_RELEASE = 16;
  // arrays
  STD_LENGTH = 17;
  STD_RESIZE = 18;
  // arrays and sets
  STD_LOW = 19;
  STD_HIGH = 20;
  STD_INCLUDE = 21;
  // sets
  STD_EXCLUDE = 22;
  STD_CONTAINS = 23;
  // TLuaVariable
  STD_VALUE = 24;

  ID_TYPE: array[0..4] of Byte = (Ord('T'), Ord('y'), Ord('p'), Ord('e'), 0);
  ID_TYPENAME: array[0..8] of Byte = (Ord('T'), Ord('y'), Ord('p'), Ord('e'), Ord('N'), Ord('a'), Ord('m'), Ord('e'), 0);
  ID_TYPEPARENT: array[0..10] of Byte = (Ord('T'), Ord('y'), Ord('p'), Ord('e'), Ord('P'), Ord('a'), Ord('r'), Ord('e'), Ord('n'), Ord('t'), 0);
  ID_INHERITSFROM: array[0..12] of Byte = (Ord('I'), Ord('n'), Ord('h'), Ord('e'), Ord('r'), Ord('i'), Ord('t'), Ord('s'), Ord('F'), Ord('r'), Ord('o'), Ord('m'), 0);
  ID_ASSIGN: array[0..6] of Byte = (Ord('A'), Ord('s'), Ord('s'), Ord('i'), Ord('g'), Ord('n'), 0);
  ID_ISREF: array[0..5] of Byte = (Ord('I'), Ord('s'), Ord('R'), Ord('e'), Ord('f'), 0);
  ID_ISCONST: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('C'), Ord('o'), Ord('n'), Ord('s'), Ord('t'), 0);
  ID_ISCLASS: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('C'), Ord('l'), Ord('a'), Ord('s'), Ord('s'), 0);
  ID_ISINTERFACE: array[0..11] of Byte = (Ord('I'), Ord('s'), Ord('I'), Ord('n'), Ord('t'), Ord('e'), Ord('r'), Ord('f'), Ord('a'), Ord('c'), Ord('e'), 0);
  ID_ISRECORD: array[0..8] of Byte = (Ord('I'), Ord('s'), Ord('R'), Ord('e'), Ord('c'), Ord('o'), Ord('r'), Ord('d'), 0);
  ID_ISARRAY: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'), 0);
  ID_ISSET: array[0..5] of Byte = (Ord('I'), Ord('s'), Ord('S'), Ord('e'), Ord('t'), 0);
  ID_ISEMPTY: array[0..7] of Byte = (Ord('I'), Ord('s'), Ord('E'), Ord('m'), Ord('p'), Ord('t'), Ord('y'), 0);
  ID_FREE: array[0..4] of Byte = (Ord('F'), Ord('r'), Ord('e'), Ord('e'), 0);
  ID_ADDREF: array[0..7] of Byte = (Ord('_'), Ord('A'), Ord('d'), Ord('d'), Ord('R'), Ord('e'), Ord('f'), 0);
  ID_RELEASE: array[0..8] of Byte = (Ord('_'), Ord('R'), Ord('e'), Ord('l'), Ord('e'), Ord('a'), Ord('s'), Ord('e'), 0);
  ID_CREATE: array[0..6] of Byte = (Ord('C'), Ord('r'), Ord('e'), Ord('a'), Ord('t'), Ord('e'), 0);
  ID_LENGTH: array[0..6] of Byte = (Ord('L'), Ord('e'), Ord('n'), Ord('g'), Ord('t'), Ord('h'), 0);
  ID_RESIZE: array[0..6] of Byte = (Ord('R'), Ord('e'), Ord('s'), Ord('i'), Ord('z'), Ord('e'), 0);
  ID_LOW: array[0..3] of Byte = (Ord('L'), Ord('o'), Ord('w'), 0);
  ID_HIGH: array[0..4] of Byte = (Ord('H'), Ord('i'), Ord('g'), Ord('h'), 0);
  ID_INCLUDE: array[0..7] of Byte = (Ord('I'), Ord('n'), Ord('c'), Ord('l'), Ord('u'), Ord('d'), Ord('e'), 0);
  ID_EXCLUDE: array[0..7] of Byte = (Ord('E'), Ord('x'), Ord('c'), Ord('l'), Ord('u'), Ord('d'), Ord('e'), 0);
  ID_CONTAINS: array[0..8] of Byte = (Ord('C'), Ord('o'), Ord('n'), Ord('t'), Ord('a'), Ord('i'), Ord('n'), Ord('s'), 0);
  ID_VALUE: array[0..5] of Byte = (Ord('V'), Ord('a'), Ord('l'), Ord('u'), Ord('e'), 0);

  STANDARD_NAMES: array[STD_TYPE..STD_VALUE] of Pointer = (
    @ID_TYPE, @ID_TYPENAME, @ID_TYPEPARENT, @ID_INHERITSFROM, @ID_ASSIGN, @ID_ISREF,
    @ID_ISCONST, @ID_ISCLASS, @ID_ISINTERFACE, @ID_ISRECORD, @ID_ISARRAY, @ID_ISSET,
    @ID_ISEMPTY, @ID_FREE, @ID_CREATE, @ID_ADDREF, @ID_RELEASE, @ID_LENGTH, @ID_RESIZE,
    @ID_LOW, @ID_HIGH, @ID_INCLUDE, @ID_EXCLUDE, @ID_CONTAINS, @ID_VALUE);

  ID_LEN: array[0..5] of Byte = (Ord('_'), Ord('_'), Ord('l'), Ord('e'), Ord('n'), 0);
  ID_TOSTRING: array[0..10] of Byte = (Ord('_'), Ord('_'), Ord('t'), Ord('o'), Ord('s'), Ord('t'), Ord('r'), Ord('i'), Ord('n'), Ord('g'), 0);
  ID_CALL: array[0..6] of Byte = (Ord('_'), Ord('_'), Ord('c'), Ord('a'), Ord('l'), Ord('l'), 0);
  ID_GC: array[0..4] of Byte = (Ord('_'), Ord('_'), Ord('g'), Ord('c'), 0);
  ID_INDEX: array[0..7] of Byte = (Ord('_'), Ord('_'), Ord('i'), Ord('n'), Ord('d'), Ord('e'), Ord('x'), 0);
  ID_NEWINDEX: array[0..10] of Byte = (Ord('_'), Ord('_'), Ord('n'), Ord('e'), Ord('w'), Ord('i'), Ord('n'), Ord('d'), Ord('e'), Ord('x'), 0);




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

constructor TLua.Create;
const
  // function _FORMATWRAPPER_(...) return string.format(...)  end
  FORMATWRAPPER_CODE: array[0..59] of Byte = (Ord('f'), Ord('u'), Ord('n'), Ord('c'), Ord('t'), Ord('i'), Ord('o'), Ord('n'),
    Ord(' '), Ord('_'), Ord('F'), Ord('O'), Ord('R'), Ord('M'), Ord('A'), Ord('T'), Ord('W'), Ord('R'), Ord('A'), Ord('P'),
    Ord('P'), Ord('E'), Ord('R'), Ord('_'), Ord('('), Ord('.'), Ord('.'), Ord('.'), Ord(')'), Ord(' '), Ord('r'), Ord('e'),
    Ord('t'), Ord('u'), Ord('r'), Ord('n'), Ord(' '), Ord('s'), Ord('t'), Ord('r'), Ord('i'), Ord('n'), Ord('g'), Ord('.'),
    Ord('f'), Ord('o'), Ord('r'), Ord('m'), Ord('a'), Ord('t'), Ord('('), Ord('.'), Ord('.'), Ord('.'), Ord(')'), Ord(' '),
    Ord(' '), Ord('e'), Ord('n'), Ord('d'));
var
  i: Integer;
  StandardNames: array[STD_TYPE..STD_VALUE] of __luaname;

  procedure AddSystemClosure(const Name: LuaString; const Callback: Pointer; const P1, P2: __luapointer);
  begin
    lua_pushnil(Handle);
    push_lua_string(Name);
    alloc_push_luafunction(Callback, P1, P2);
    __global_newindex(0, 0);
    lua_settop(Handle, 0);
    with PLuaGlobalEntity(GetGlobalEntity(Name, True))^ do
    begin
      Kind := gkConst;
      Constant := True;
    end;
  end;

  procedure FillStandardNameSpace(var ANameSpace: __TLuaDictionary; const Values: array of Integer);
  var
    i: Integer;

    procedure NameSpaceAdd(Value: Integer);
    begin
      case Value of
        STD_INHERITS_FROM, STD_ASSIGN, STD_CREATE, STD_FREE,
        STD_RESIZE, STD_INCLUDE, STD_EXCLUDE, STD_CONTAINS: Value := Value or NAMESPACE_STD_PROC;
      end;

      TLuaDictionary(ANameSpace).Add(StandardNames[Value and $7f], NAMESPACE_STD_MASK or Value);
    end;
  begin
    for i := STD_TYPE to STD_FREE do
      NameSpaceAdd(i);

    for i := Low(Values) to High(Values) do
      NameSpaceAdd(Values[i]);
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
  TLuaNames(FNames).Init(Self);
  TLuaRegisteredTypeNames(FRegisteredTypeNames).Init(Self);
  InternalRegStandardTypeNames;

  // Lua initialization
  if (not InitializeLua) then
    raise ELua.CreateFmt('Lua library was not initialized: "%s"', [LuaPath]);
  FHandle := lua_open;
  luaL_openlibs(Handle);

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
    FFormatWrapper := PLuaGlobalEntity(TLuaMemoryHeap(FMemoryHeap).Unpack(FItems[Count - 1].Value)).Ptr;

  // print(f) wrappers
  AddSystemClosure('_PNT_', @TLua.__print, 0, 0);
  AddSystemClosure('printf', @TLua.__printf, 0, 0);

  // standard name spaces
  for i := Low(STANDARD_NAMES) to High(STANDARD_NAMES) do
  begin
    lua_pushlstring(Handle, STANDARD_NAMES[i], LStrLen(STANDARD_NAMES[i]));
    StandardNames[i] := lua_tolstring(Handle, -1, nil);
    global_fill_value(global_alloc_ref);
  end;
  FillStandardNameSpace(FStdObjectNameSpace, [STD_CREATE]);
  FillStandardNameSpace(FStdInterfaceNameSpace, [STD_ADDREF, STD_RELEASE]);
  FillStandardNameSpace(FStdRecordNameSpace, []);
  FillStandardNameSpace(FStdArrayNameSpace, [STD_LENGTH, STD_RESIZE, STD_LOW, STD_HIGH, STD_INCLUDE]);
  FillStandardNameSpace(FStdSetNameSpace, [STD_LOW, STD_HIGH, STD_INCLUDE, STD_EXCLUDE, STD_CONTAINS]);

  // basic classes meta tpe
  FObjectMetaType := InternalAddClass(TObject, True, True);

  // managed closures
  FClosureMetaTable := InternalNewMetaTable;
  global_push_value(FClosureMetaTable);
  InternalRegisterCallback(Pointer(@ID_GC), @TLua.__closuregc);
  lua_settop(Handle, 0);


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

  // TLuaVariable
  InternalAddClass(TLuaVariable, false, nil);   *)
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
  for i := Low(FStackFrames) to High(FStackFrames) do
  begin
    FStackFrames[i].Arguments.Finalize;
    FStackFrames[i].Buffer := nil;
  end;

  // finalize names and dynamic arrays
  TLuaDictionary(FStdObjectNameSpace).Clear;
  TLuaDictionary(FStdInterfaceNameSpace).Clear;
  TLuaDictionary(FStdRecordNameSpace).Clear;
  TLuaDictionary(FStdArrayNameSpace).Clear;
  TLuaDictionary(FStdSetNameSpace).Clear;
  for i := 0 to TLuaDictionary(FMetaTypes).Count - 1 do
  begin
    MetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(TLuaDictionary(FMetaTypes).FItems[i].Value);
    MetaType.FName := '';
    case MetaType.Kind of
      mtClass, mtInterface, mtRecord:
      begin
        TLuaDictionary(MetaType.FNameSpace).Clear;
      end;
    end;
  end;

  // containers
  TLuaStack(FEmptyRefs).Clear;
  TLuaMemoryHeap(FMemoryHeap).Clear;
  TLuaBuffer(FInternalBuffer).Clear;
  TLuaInvokableBuilder(FInvokableBuilder).Clear;
  TLuaNames(FNames).Clear;
  TLuaDictionary(FMetaTypes).Clear;
  TLuaDictionary(FClosureTypes).Clear;
  TLuaDictionary(FGlobalEntities).Clear;
  TLuaRegisteredTypeNames(FRegisteredTypeNames).Clear;

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
      Inc(X, $04040404);
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

function TLua.AnsiFromUnicode(ATarget: PAnsiChar; ACodePage: Word; ASource: PWideChar; ALength: Integer): Integer;
const
  CHARS_PER_ITERATION = SizeOf(Integer) div SizeOf(WideChar);
var
  Dest: PAnsiChar;
  Source: PWideChar;
  Count, X: Integer;
begin
  Count := ALength;
  Dest := ATarget;
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

  Result := NativeInt(Dest) - NativeInt(ATarget);
end;

procedure TLua.UnicodeFromAnsi(ATarget: PWideChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer);
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
  Dest := ATarget;
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

function Utf8FromUnicode(ATarget: PAnsiChar; ASource: PWideChar; ALength: Integer): Integer;
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
  Dest := ATarget;
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
  Result := NativeInt(Dest) - NativeInt(ATarget);
end;

function UnicodeFromUtf8(ATarget: PWideChar; ASource: PAnsiChar; ALength: Integer): Integer;
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
  Dest := ATarget;
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
  Result := (NativeInt(Dest) - NativeInt(ATarget)) shr 1;
end;

function TLua.Utf8FromAnsi(ATarget: PAnsiChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer): Integer;
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
  Dest := ATarget;
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
  Result := NativeUInt(Dest) - NativeUInt(ATarget);
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
  ascii_write, unicode_write, non_ascii, unknown;
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
    non_ascii:
      U := UTF8CHAR_SIZE[Byte(X)];
      if (U <= Count) then
      begin
        case U of
          1:
          begin
            X := PByte(Source)^;
            Dec(Count);
            Inc(Source);
          end;
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

function TLua.AnsiFromUtf8(ATarget: PAnsiChar; ACodePage: Word; ASource: PAnsiChar; ALength: Integer): Integer;
var
  ConvertDesc: TUnicodeConvertDesc;
begin
  if (ACodePage = 0) then ACodePage := CODEPAGE_DEFAULT;
  ConvertDesc.CodePage := ACodePage;
  ConvertDesc.Dest := ATarget;
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

function TLua.AnsiFromAnsi(ATarget: PAnsiChar; ATargetCodePage: Word; ASource: PAnsiChar;
  ASourceCodePage: Word; ALength: Integer): Integer;
var
  ConvertDesc: TUnicodeConvertDesc;
  SourceCodePage: Word;
begin
  ConvertDesc.Dest := ATarget;
  ConvertDesc.Source := ASource;
  ConvertDesc.Count := ALength;

  if (ATargetCodePage = 0) then ATargetCodePage := CODEPAGE_DEFAULT;
  SourceCodePage := ASourceCodePage;
  if (SourceCodePage = 0) then SourceCodePage := CODEPAGE_DEFAULT;

  if (ATargetCodePage = SourceCodePage) then
  begin
    System.Move(ConvertDesc.Source^, ConvertDesc.Dest^, ConvertDesc.Count);
    Result := ConvertDesc.Count;
  end else
  begin
    ConvertDesc.CodePage := ATargetCodePage;
    if (SourceCodePage <> FCodePage) then SetCodePage(SourceCodePage);
    Result := InternalAnsiFromAnsi(ConvertDesc, @Self.FUnicodeTable);
  end;
end;

procedure TLua.unpack_lua_string(var Result: LuaString; const RttiName: ShortString);
var
  Count: NativeInt;
  Chars: PLuaChar;
  {$if Defined(LUA_UNICODE) or Defined(NEXTGEN) or Defined(UNICODE)}
  Buffer: array[Low(Byte)..High(Byte) + 3] of LuaChar;
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
      Size := Count + 3;
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
    Size := Count + 3;
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
      Size := Count + 3;
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
    Size := Count + 3;
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

function TLua.push_metatype_instance(const AMetaType: PLuaMetaType; const ASource;
  const AOwner, ATemporary: Boolean): Pointer{PLuaUserData};
label
  simple_case;
var
  AInstance: Pointer;
  LUserData: PLuaUserData;
  LSize: Integer;
begin
  AInstance := @ASource;
  if (AMetaType.F.Kind in [mtClass, mtInterface]) then
  begin
    if (Assigned(AInstance)) then
    begin
      AInstance := PPointer(AInstance)^;
    end;
    goto simple_case;
  end;

  if (not AOwner) then
  begin
  simple_case:
    LUserData := lua_newuserdata(Handle, SizeOf(TLuaUserData));
    LUserData.Instance := AInstance;
    if (Assigned(AInstance)) and (AOwner) then
    begin
      if (AMetaType.F.Kind = mtInterface) then
      begin
        if (ATemporary) then
        begin
          PPointer(@ASource)^ := nil;
        end else
        begin
          IInterface(LUserData.Instance)._AddRef;
        end;
      end
      {$ifdef AUTOREFCOUNT}
      else // mtClass
      begin
        if (ATemporary) then
        begin
          PPointer(@ASource)^ := nil;
        end else
        begin
          TObject(AInstance).__ObjAddRef;
        end;
      end
      {$endif}
      ;
    end;
  end else
  begin
    LSize := (AMetaType.Size + SizeOf(NativeUInt) - 1) and Integer(-SizeOf(NativeUInt));
    LUserData := lua_newuserdata(Handle, SizeOf(TLuaUserData) + LSize);
    LUserData.Instance := Pointer(NativeUInt(LUserData) + SizeOf(TLuaUserData));

    if (LSize <= 4 * SizeOf(NativeUInt)) then
    begin
      with PMemoryItems(LUserData.Instance)^ do
      begin
        if (LSize > 1 * SizeOf(NativeUInt)) then
        begin
          if (LSize > 2 * SizeOf(NativeUInt)) then
          begin
            if (LSize > 3 * SizeOf(NativeUInt)) then
            begin
              NativeUInts[3] := 0;
            end;
            NativeUInts[2] := 0;
          end;
          NativeUInts[1] := 0;
        end;
        NativeUInts[0] := 0;
      end;
    end else
    begin
      System.FillChar(LUserData.Instance^, LSize, #0);
    end;

    if (Assigned(AInstance)) then
    begin
      if (AMetaType.Managed) then
      begin
        case (AMetaType.F.Kind) of
          mtRecord:
          begin
            CopyRecord(LUserData.Instance, AInstance, PLuaRecordInfo(AMetaType).F.TypeInfo);
          end;
          mtArray:
          begin
            if (PLuaArrayInfo(AMetaType).FIsDynamic) then
            begin
              PPointer(LUserData.Instance)^ := PPointer(AInstance)^;
              if (PPointer(AInstance)^ <> nil) then
                DynArrayAddRef(PPointer(AInstance)^);
            end else
            begin
              CopyArray(LUserData.Instance, AInstance, PLuaArrayInfo(AMetaType).FFinalTypeInfo,
                PLuaArrayInfo(AMetaType).FFinalItemsCount);
            end;
          end;
        end;
      end else
      begin
        System.Move(AInstance^, LUserData.Instance^, AMetaType.Size);
      end;
    end;
  end;

  // done
  PInteger(@LUserData.Kind)^ := Byte(AMetaType.F.Kind);
  LUserData.Owner := AOwner;
  LUserData.MetaType := AMetaType.Ptr;
  if (AMetaType.F.Kind = mtArray) then
  begin
    LUserData.Counts := PLuaArrayInfo(AMetaType).Dimention shl 4;
  end;
  lua_rawgeti(Handle, LUA_REGISTRYINDEX, AMetaType.Ref);
  lua_setmetatable(Handle, -2);
  Result := LUserData;
end;

function TLua.push_complex_property(const PropertyInfo; const Instance: Pointer): Pointer{PLuaUserData};
begin
  Result := nil{ToDo};
end;
          (*
// запушить свойство
function  TLua.push_complex_property(const Instance: pointer; const PropertyInfo: TLuaPropertyInfo): PLuaUserData;
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
     ltString: push_lua_string(FString);
    ltPointer: lua_pushlightuserdata(Handle, F.VPointer);
  (*    ltClass: lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassesInfo[internal_class_index(pointer(Data[0]), true)].Ref);
     ltObject: begin
                 if (TClass(pointer(Data[0])^) = TLuaVariable) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaVariable(Data[0]).Index)
                 else
                 push_metatype_instance(ClassesInfo[internal_class_index(TClass(pointer(Data[0])^), true)], false, pointer(Data[0]));
               end;
     ltRecord: begin
                 with PLuaRecord(@FLuaType)^, Info^ do
                 push_metatype_instance(ClassesInfo[FClassIndex], not IsRef, Data).is_const := IsConst;
               end;
      ltArray: begin
                 with PLuaArray(@FLuaType)^, Info^ do
                 push_metatype_instance(ClassesInfo[FClassIndex], not IsRef, Data).is_const := IsConst;
               end;
        ltSet: begin
                 with PLuaSet(@FLuaType)^, Info^ do
                 push_metatype_instance(ClassesInfo[FClassIndex], not IsRef, Data).is_const := IsConst;
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
                   if (TClass(pointer(VObject)^) = TLuaVariable) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaVariable(VObject).Index)
                   else
                   push_metatype_instance(ClassesInfo[internal_class_index(TClass(pointer(VObject)^), true)], false, pointer(VObject));
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
                    end
                    {$endif}
                    ;
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
      stack_lua_string(Ret.FString, StackIndex);
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
      // ToDo
      Ret.F.LuaType := ltPointer;
      Ret.F.VPointer := MetaType;
      (*case (UserData.InstanceKind) of
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
          if (UserData.Counts and $f = 0) then
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
            FIsConst := UserData.Constant;
          end;
        end;
        ukProperty:
        begin *)
        fail_user_data:
          UserData.GetDescription(FStringBuffer.Default, Self);
          Result := False;
    (*    end;
      end;   *)
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
          Ret.F.VPointer := Pointer(PLuaClassInfo(MetaType).ClassType);
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
        if (is_class) and (property_info.Base.Kind = vkRecord) and (property_info.Parameters = nil) and
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
              if (Base.Kind = vkObject) and (read_mode >= 0) then
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
// TLuaVariable.Create/Initialize добавляется в список ссылок и производит инициализауию в Lua
function __TLuaCreateReference(const Self: TLua; const global_name: string{=''}; const ReturnAddr: pointer): TLuaVariable;
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
    Result := TLuaVariable.Create;
    Result.Initialize(Self);
  end;
end;    *)
                       (*
function TLua.CreateReference(const global_name: string=''): TLuaVariable;
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
  FFrames := nil;
  __TLuaGarbageCollection(Self);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaGarbageCollection
end;
{$endif}

procedure TLua.NativeFrameEnter(var Frame{: TLuaNativeFrame}; const Name: PShortString; const Critical: Boolean);
var
  LFrame: PLuaNativeFrame;
begin
  LFrame := Pointer(@Frame);
  LFrame.Parent := FFrames;
  LFrame.Name := Name;
  LFrame.Critical := Critical;

  FFrames := LFrame;
end;

(*procedure TLua.NativeFrameEnterParentCritical(var Frame{: TLuaNativeFrame}; const Name: PShortString);
begin
  NativeFrameEnter(Frame, Name, (Assigned(FFrames) and PLuaNativeFrame(FFrames).Critical));
end;*)

procedure TLua.NativeFrameLeave;
var
  LFrame: PLuaNativeFrame;
begin
  LFrame := FFrames;
  FFrames := LFrame.Parent;
end;

procedure TLua.NativeFrameRename(const Name: PShortString);
var
  LFrame: PLuaNativeFrame;
begin
  LFrame := FFrames;
  LFrame.Name := Name;
end;

procedure TLua.NativeFrameRename(var NameBuffer: ShortString; const Name: LuaString);
var
  Length, Size, Count: Integer;
  Buffer: ^TLuaBuffer;
  {$if Defined(LUA_ANSI) and Defined(INTERNALCODEPAGE)}
  CodePage: Word;
  {$ifend}
begin
  Length := System.Length(Name);
  Buffer := Pointer(@FInternalBuffer);
  Buffer.Size := 0;

  Count := 0;
  if (Length <> 0) then
  begin
    {$ifdef LUA_ANSI}
      Size := Length + 3;
      if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
      {$ifdef INTERNALCODEPAGE}
      CodePage := PWord(NativeInt(Name) - ASTR_OFFSET_CODEPAGE)^;
      if (CodePage = CODEPAGE_UTF8) then
      begin
        System.Move(Pointer(Name)^, Pointer(Buffer.FBytes)^, Length);
        Count := Length;
      end else
      {$endif}
      begin
        Count := AnsiFromAnsi(Pointer(Buffer.FBytes), 0, Pointer(Name),
          {$ifdef INTERNALCODEPAGE}CodePage{$else}0{$endif}, Length);
      end;
    {$else .LUA_UNICODE}
      Size := Length * 3 + 1;
      if (Buffer.Capacity < Size) then Buffer.Alloc(Size);
      Count := Utf8FromUnicode(Pointer(Buffer.FBytes), Pointer(Name), Length);
    {$endif}
  end;

  FillShortString(NameBuffer, Pointer(Buffer.FBytes), Count, High(Byte));
  NativeFrameRename(Pointer(@NameBuffer));
end;

procedure TLua.RegisterError(const Text: LuaString);
var
  Count: Integer;
  LFrame: PLuaNativeFrame;
  Critical: Boolean;
  Path: LuaString;
  Mode: Integer;
begin
  Count := 0;
  LFrame := FFrames;
  Critical := LFrame.Critical;
  while (Assigned(LFrame)) do
  begin
    if (Count = 0) then
    begin
      unpack_lua_string(Path, LFrame.Name^);
    end else
    begin
      unpack_lua_string(FStringBuffer.Lua, LFrame.Name^);
      FStringBuffer.Lua := FStringBuffer.Lua + ' - ';
      FStringBuffer.Lua := FStringBuffer.Lua + Path;
      Path := FStringBuffer.Lua;
    end;

    Inc(Count);
    LFrame := LFrame.Parent;
  end;

  if (Assigned(FOnRegisterError)) then
  begin
    Mode := Ord(not FOnRegisterError(Path, Text, Count, Critical)) shl 1;
  end else
  begin
    Mode := (Ord(Critical) shl 1) + Ord(FPrintRegisterError and System.IsConsole);
  end;

  if (Mode <> 0) then
  begin
    if (Pointer(Path) <> nil) then Path := Path + ': ';
    Path := Path + Text;

    if (Mode and 1 <> 0) then
    begin
      Writeln(Path);
    end;

    if (Mode and 2 <> 0) then
    begin
      InternalError(Path);
    end;
  end;
end;

procedure TLua.RegisterErrorFmt(const FmtStr: LuaString; const Args: array of const);
begin
  RegisterError(LuaString({$ifdef UNICODE}Format{$else}WideFormat{$endif}(UnicodeString(FmtStr), Args)));
end;

procedure TLua.RegisterErrorTypeExists(const Name: LuaString);
begin
  RegisterErrorFmt('type "%s" is already registered', [Name]);
end;

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
  FFrames := nil;
  InternalError(Text);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalError
end;
{$endif}

procedure TLua.Error(const FmtStr: LuaString; const Args: array of const);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  InternalErrorFmt(FmtStr, Args);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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

function TLua.EnterScript(const AReturnAddress: Pointer): Pointer;
var
  LIndex: NativeUInt;
  LFrame: PLuaScriptFrame;
begin
  Result := FReturnAddress;
  FReturnAddress := AReturnAddress;
  LIndex := FStackFrameIndex;
  if (LIndex >= High(FStackFrames)) then
    raise EInvalidScriptFrameIndex(LIndex) at FReturnAddress;

  Inc(LIndex);
  FStackFrameIndex := LIndex;
  LFrame := @FStackFrames[LIndex];
  Pointer(FArguments){PLuaScriptFrame} := LFrame;

  if (LFrame.Arguments.FHeapEmpty <> TLuaMemoryHeap(LFrame.Arguments.FHeap).FCurrent) or
    (LFrame.Arguments.ParamCount or LFrame.Arguments.ResultCount <> 0) then
    LFrame.Arguments.Clear;
end;

procedure TLua.LeaveScriptStack(const AReturnAddress: Pointer);
var
  LIndex: NativeUInt;
begin
  FReturnAddress := AReturnAddress;
  LIndex := FStackFrameIndex;
  Dec(LIndex);
  if (LIndex > High(FStackFrames) - 1{Low(FStackFrames)}) then
    raise EInvalidScriptFrameIndex(LIndex + 1) at FReturnAddress;

  FStackFrameIndex := LIndex;
  if (LIndex = 0) then
  begin
    FArguments := nil;
  end else
  begin
    Pointer(FArguments){PLuaScriptFrame} := @FStackFrames[LIndex];
  end;
end;

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
  Count: Integer;
  Ptr: __luapointer;
begin
  if (ModeCreate) then
  begin
    LuaName := TLuaNames(FNames).Add(Name);
  end else
  begin
    LuaName := TLuaNames(FNames).FindValue(Name);

    if (not Assigned(LuaName)) then
    begin
      // try to find and cache identifier
      Count := TLuaBuffer(FInternalBuffer).Size;
      if (Count > 255) then Count := 255;
      lua_pushlstring(Handle, Pointer(TLuaBuffer(FInternalBuffer).FBytes), Count);
      if Assigned(TLuaDictionary(FGlobalEntities).InternalFind(lua_tolstring(Handle, -1, nil), False)) then
      begin
        LuaName := TLuaNames(FNames).InternalFind(True).Value;
      end;
      lua_settop(Handle, -1 -1);
    end;
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
  FFrames := nil;
  __TLuaGetVariable(Self, Name, Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaSetVariable(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaGetVariableEx(Self, Name, Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaSetVariableEx(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaGetTableVariable(NativeInt(Self) or Ord(True), Table, Name, @Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  or eax, 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  or rcx, 1
  {$endif}
  jmp __TLuaGetTableVariable
end;
{$endif}

function TLua.GetTableVariableEx(const Table, Name: LuaString): TLuaArg;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaGetTableVariable(NativeInt(Self) or Ord(False), Table, Name, @Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaSetTableVariable(NativeInt(Self) or Ord(True), Table, Name, @Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  or eax, 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  or rcx, 1
  {$endif}
  jmp __TLuaSetTableVariable
end;
{$endif}

procedure TLua.SetTableVariableEx(const Table, Name: LuaString; const Value: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaSetTableVariable(NativeInt(Self) or Ord(False), Table, Name, @Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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

procedure InternalLuaCall(const ASelf: TLua; const AOptions: TLuaCallOptions; var AResult: TLuaArgs);
label
  clear_not_found, not_found, fail_argument;
var
  i: Integer;
  V: PVarRec;
  LCount, LErrCode: Integer;
  LLastReturnAddress: Pointer;
  LEntity: PLuaGlobalEntity;
  LProc: ^TLuaNamespaceMethod;
  LArg: PLuaArg;
  LArguments: PLuaArguments;
begin
  LLastReturnAddress := ASelf.EnterScript(AOptions.ReturnAddress);
  try
    if (Assigned(AOptions.Table)) then
    begin
      // push table function
      LEntity := ASelf.GetGlobalEntity(AOptions.Table^, False);
      if (Assigned(LEntity)) then
      begin
        if (LEntity.Kind <> gkScriptVariable) then goto not_found;
        ASelf.global_push_value(LEntity.Ref);
        if (lua_type(ASelf.Handle, -1) <> LUA_TTABLE) then goto clear_not_found;
        ASelf.push_lua_string(AOptions.ProcName^);
        lua_rawget(ASelf.Handle, -2);
        if (lua_type(ASelf.Handle, -1) <> LUA_TFUNCTION) then goto clear_not_found;
        lua_insert(ASelf.Handle, -2);
      end;
    end else
    begin
      // push global function
      LEntity := ASelf.GetGlobalEntity(AOptions.ProcName^, False);
      if (Assigned(LEntity)) then
      begin
        case LEntity.Kind of
          gkProc:
          begin
            LProc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(ASelf.FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
            ASelf.push_luafunction(LProc.Func);
          end;
          gkScriptVariable:
          begin
            ASelf.global_push_value(LEntity.Ref);
            if (lua_type(ASelf.Handle, -1) <> LUA_TFUNCTION) then
            begin
            clear_not_found:
              lua_settop(ASelf.Handle, 0);
              goto not_found;
            end;
          end;
        else
        not_found:
          LEntity := nil;
        end;
      end;
    end;

    // check not found
    if (not Assigned(LEntity)) then
    begin
      if (Assigned(AOptions.Table)) then
      begin
        raise ELua.CreateFmt('Lua table function "%s:%s" not found',
          [AOptions.Table^, AOptions.ProcName^]) at AOptions.ReturnAddress;
      end else
      begin
        raise ELua.CreateFmt('Lua function "%s" not found', [AOptions.ProcName^]) at AOptions.ReturnAddress;
      end;
    end;

    // push arguments
    if (AOptions.VarRecMode) then
    begin
      V := AOptions.V;
      for i := 1 to AOptions.Count do
      begin
        if (not ASelf.push_argument(V^)) then
        begin
          LErrCode := i;
          goto fail_argument;
        end;
        Inc(V);
      end;
    end else
    begin
      LArg := AOptions.Arg;
      for i := 1 to AOptions.Count do
      begin
        if (not ASelf.push_luaarg(LArg^)) then
        begin
          LErrCode := i;
        fail_argument:
          lua_settop(ASelf.Handle, 0);
          raise ELua.CreateFmt('Unknown argument type "%s" (arg N%d)', [ASelf.FStringBuffer.Default, LErrCode]) at AOptions.ReturnAddress;
        end;
        Inc(LArg);
      end;
    end;

    // call
    LErrCode := lua_pcall(ASelf.Handle, AOptions.Count + Ord(Assigned(AOptions.Table)), LUA_MULTRET, 0);
    if (LErrCode = 0) then LErrCode := lua_gc(ASelf.Handle, 2{LUA_GCCOLLECT}, 0);
    if (LErrCode <> 0) then ASelf.CheckScriptError(LErrCode, nil);

    // fill result
    LCount := lua_gettop(ASelf.Handle);
    if (LCount = 0) then
    begin
      AResult.Items := nil;
      AResult.Count := 0;
    end else
    begin
      LArguments := ASelf.FArguments;
      LArguments.ResultCount := LCount;
      for i := 0 to LCount - 1 do
      begin
        if (not ASelf.stack_luaarg(LArguments.Results[i], i + 1, False)) then
          LArguments.Results[i].F.LuaType := ltEmpty;
      end;
      AResult := LArguments.F.ResultArgs;
    end;

    // clear stack
    lua_settop(ASelf.Handle, 0);
  finally
    ASelf.LeaveScriptStack(LLastReturnAddress);
  end;
end;

procedure __TLuaCall_noargs(const ASelf: TLua; const AProcName: LuaString; var AResult: TLuaArgs);
var
  LOptions: TLuaCallOptions;
begin
  LOptions.Table := nil;
  LOptions.ProcName := @AProcName;
  LOptions.ReturnAddress := Pointer(ASelf.FTempBuffer.N);
  LOptions.Count := 0;
  LOptions.VarRecMode := False;
  LOptions.Arg := nil;
  InternalLuaCall(ASelf, LOptions, AResult);
end;

function TLua.Call(const ProcName: LuaString): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaCall_noargs(Self, ProcName, Result);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FTempBuffer.N
  {$endif}
  jmp __TLuaCall_noargs
end;
{$endif}

procedure __TLuaCall_luaargs(const ASelf: TLua; const AProcName: LuaString;
  const AArgs: array of TLuaArg; var AResult: TLuaArgs);
var
  LOptions: TLuaCallOptions;
begin
  LOptions.Table := nil;
  LOptions.ProcName := @AProcName;
  LOptions.ReturnAddress := Pointer(ASelf.FTempBuffer.N);
  LOptions.Count := Length(AArgs);
  LOptions.VarRecMode := False;
  LOptions.Arg := Pointer(@AArgs[0]);
  InternalLuaCall(ASelf, LOptions, AResult);
end;

function TLua.Call(const ProcName: LuaString; const Args: array of TLuaArg): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaCall_luaargs(Self, ProcName, Args, Result);
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

procedure __TLuaCall__arguments(const ASelf: TLua; const AProcName: LuaString;
  const AArgs: array of const; var AResult: TLuaArgs);
var
  LOptions: TLuaCallOptions;
begin
  LOptions.Table := nil;
  LOptions.ProcName := @AProcName;
  LOptions.ReturnAddress := Pointer(ASelf.FTempBuffer.N);
  LOptions.Count := Length(AArgs);
  LOptions.VarRecMode := True;
  LOptions.Arg := Pointer(@AArgs[0]);
  InternalLuaCall(ASelf, LOptions, AResult);
end;

function TLua.Call(const ProcName: LuaString; const Args: array of const): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaCall__arguments(Self, ProcName, Args, Result);
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

procedure __TLuaTableCall_noargs(const ASelf: TLua; const ATable, AProcName: LuaString;
  var AResult: TLuaArgs);
var
  LOptions: TLuaCallOptions;
begin
  LOptions.Table := @ATable;
  LOptions.ProcName := @AProcName;
  LOptions.ReturnAddress := Pointer(ASelf.FTempBuffer.N);
  LOptions.Count := 0;
  LOptions.VarRecMode := False;
  LOptions.Arg := nil;
  InternalLuaCall(ASelf, LOptions, AResult);
end;

function TLua.Call(const Table, ProcName: LuaString): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaTableCall_noargs(Self, Table, ProcName, Result);
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
  jmp __TLuaTableCall_noargs
end;
{$endif}

procedure __TLuaTableCall_luaargs(const ASelf: TLua; const ATable, AProcName: LuaString;
  const AArgs: array of TLuaArg; var AResult: TLuaArgs);
var
  LOptions: TLuaCallOptions;
begin
  LOptions.Table := @ATable;
  LOptions.ProcName := @AProcName;
  LOptions.ReturnAddress := Pointer(ASelf.FTempBuffer.N);
  LOptions.Count := Length(AArgs);
  LOptions.VarRecMode := False;
  LOptions.Arg := Pointer(@AArgs[0]);
  InternalLuaCall(ASelf, LOptions, AResult);
end;

function TLua.Call(const Table, ProcName: LuaString; const Args: array of TLuaArg): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaTableCall_luaargs(Self, Table, ProcName, Args, Result);
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

procedure __TLuaTableCall__arguments(const ASelf: TLua; const ATable, AProcName: LuaString;
  const AArgs: array of const; var AResult: TLuaArgs);
var
  LOptions: TLuaCallOptions;
begin
  LOptions.Table := @ATable;
  LOptions.ProcName := @AProcName;
  LOptions.ReturnAddress := Pointer(ASelf.FTempBuffer.N);
  LOptions.Count := Length(AArgs);
  LOptions.VarRecMode := True;
  LOptions.Arg := Pointer(@AArgs[0]);
  InternalLuaCall(ASelf, LOptions, AResult);
end;

function TLua.Call(const Table, ProcName: LuaString; const Args: array of const): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaTableCall__arguments(Self, Table, ProcName, Args, Result);
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
  if (Pointer(Data) <> nil) then
  begin
    S := Pointer(Data);
    Inc(S, Size);
    S^ := 0;
  end;
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
    LastReturnAddress := EnterScript(ReturnAddress);
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
      LeaveScriptStack(LastReturnAddress);
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
  FFrames := nil;
  __TLuaRunScript(Self, Script);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
    S: {$ifdef FPC}Stat{$else}_stat{$endif};
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
      if ({$ifdef FPC}FpFStat{$else}fstat{$endif}(Handle, S) = 0) then
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
  FFrames := nil;
  __TLuaLoadFileScript(Self, FileName);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaLoadBufferScript(Self, Buffer, BufferSize, BOM, UnitName);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaLoadStreamScript(Self, Stream, UnitName, ASize);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaLoadKOLStreamScript(Self, Stream, UnitName, ASize);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaLoadResourceScript(Self, Instance, Name, ResType, UnitName);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
    Lua: Pointer;
    Callback: function(const Lua: Pointer; const P1, P2: __luapointer): Integer;
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

function TLua.alloc_luafunction(const Callback: Pointer; const P1, P2: __luapointer): __luafunction;
{$ifdef LUA_NATIVEFUNC}
begin
  Result := TLuaCFunctionHeap(FCFunctionHeap).Alloc(Self, Callback, P1, P2);
end;
{$else}
begin
  alloc_push_luafunction(Callback, P1, P2);
  Result := global_alloc_ref;
  global_fill_value(Result);
end;
{$endif}

function TLua.alloc_luafunction(const AMethod: Pointer{PLuaMethod}): __luafunction;
begin
  Result := alloc_luafunction(@TLua.__MethodCallback,
    __luapointer(Cardinal(NativeUInt(AMethod))),
    {$ifdef SMALLINT}0{$else .LARGEINT}__luapointer(Cardinal(NativeUInt(AMethod) shr 32)){$endif});
end;

procedure TLua.alloc_push_luafunction(const Callback: Pointer; const P1, P2: __luapointer);
{$ifdef LUA_NATIVEFUNC}
begin
  lua_pushcclosure(Handle, alloc_luafunction(Callback, P1, P2), 0);
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
end;
{$endif}

procedure TLua.push_luafunction(const Func: __luafunction);
begin
  {$ifdef LUA_NATIVEFUNC}
    lua_pushcclosure(Handle, Func, 0);
  {$else}
    // global_push_value(Func);
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Func);
  {$endif}
end;

(*procedure TLua.release_function(const Func: __luafunction);
begin
  {$ifdef LUA_NATIVEFUNC}
    TLuaCFunctionHeap(FCFunctionHeap).Free(Func);
  {$else}
    global_free_ref(Func);
  {$endif}
end;*)

function TLua.function_topointer(const StackIndex: Integer): Pointer;
{$ifdef LUA_NATIVEFUNC}
var
  Data: PLuaCFunctionData;
  Offset: NativeInt;
  {$ifdef LARGEINT}
  Heap: ^TLuaMemoryHeap;
  {$endif}
  MetaType: ^TLuaMetaType;
  Proc: ^TLuaMethod;
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
  if (Result <> @TLua.UniversalMethodCallback{ToDo}) then Exit;

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
    Result := PPointer(NativeUInt(MetaType.F.ClassType) + NativeUInt(Result) and PROPSLOT_CLEAR)^;
end;
{$else}
var
  Params: PLuaFunctionParams;
  {$ifdef LARGEINT}
  Heap: ^TLuaMemoryHeap;
  {$endif}
  MetaType: ^TLuaMetaType;
  Proc: ^TLuaClosureType;
begin
  Result := Pointer(lua_tocfunction(Handle, StackIndex));
  if (not Assigned(Result)) or (Result <> @LuaFuntionWrapper) then Exit;

  // callback address
  lua_getupvalue(Handle, StackIndex, 1);
  Params := lua_touserdata(Handle, -1);
  stack_pop;
  Result := @Params.Callback;
  if (Result <> @TLua.UniversalMethodCallback{ToDo}) then Exit;

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
    Result := PPointer(NativeInt(MetaType.F.ClassType) + NativeInt(Result) and PROPSLOT_CLEAR)^;
end;
{$endif}

function LuaClosureWrapper(L: Plua_State): Integer; cdecl;
var
  LLua: Pointer;
  LClosure: Pointer{PLuaClosureMethod};
begin
  LClosure := lua_touserdata(L, LUA_UPVALUEINDEX);
  LLua := PPointer(LClosure)^;
  Inc(NativeUInt(LClosure), SizeOf(Pointer));

  Result := TLua(LLua).__MethodCallback(
    __luapointer(Cardinal(NativeUInt(LClosure))),
    {$ifdef SMALLINT}0{$else .LARGEINT}__luapointer(Cardinal(NativeUInt(LClosure) shr 32)){$endif});
end;

function TLua.alloc_push_closure(const AReferenceOwner: Boolean): Pointer{PLuaClosureMethod};
begin
  Result := lua_newuserdata(Handle, SizeOf(Pointer) + SizeOf(TLuaClosureMethod));
  PPointer(Result)^ := Pointer(Self);
  Inc(NativeUInt(Result), SizeOf(Pointer));

  if (AReferenceOwner) then
  begin
    PLuaClosureMethod(Result).Instance := nil;
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, FClosureMetaTable);
    lua_setmetatable(Handle, -2);
  end;

  lua_pushcclosure(Handle, LuaClosureWrapper, 1);
end;

procedure TLua.InternalRegisterCallback(const Name: __luaname; const Callback: Pointer; const P1, P2: __luapointer);
begin
  lua_pushlstring(Handle, Pointer(Name), LStrLen(Name));
  alloc_push_luafunction(Callback, P1, P2);
  lua_rawset(Handle, -3);
end;

(*procedure TLua.InternalUnregisterCallback(const Name: __luaname);
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
end;*)

procedure TLua.InternalRegTypeName(const TypeName: LuaString;
  const TypeInfo: PTypeInfo; const PointerDepth: Integer);
begin
  TLuaRegisteredTypeNames(FRegisteredTypeNames).Add(TypeName, TypeInfo, PointerDepth);
end;

procedure TLua.InternalRegStandardTypeNames;

  procedure Add(const TypeName: LuaString; const TypeInfo: PTypeInfo);
  begin
    InternalRegTypeName(TypeName, TypeInfo, 0);
  end;
begin
  Add('TClass', TypeInfoTClass);
  Add('Pointer', TypeInfoPointer);
  Add('AnsiChar', TypeInfo(AnsiChar));
  Add('WideChar', TypeInfo(WideChar));
  Add('UTF8Char', TypeInfo(AnsiChar));
  Add('Char', TypeInfo(Char));
  Add('LuaChar', TypeInfo(LuaChar));
  Add('PAnsiChar', TypeInfoPAnsiChar);
  Add('PWideChar', TypeInfoPWideChar);
  Add('PUTF8Char', TypeInfoPUTF8Char);
  Add('PChar', {$ifdef UNICODE}TypeInfoPWideChar{$else}TypeInfoPAnsiChar{$endif});
  Add('PLuaChar', {$if Defined(LUA_UNICODE) or Defined(NEXTGEN)}TypeInfoPWideChar{$else}TypeInfoPAnsiChar{$ifend});
  Add('bool', TypeInfo(Boolean));
  Add('Boolean', TypeInfo(Boolean));
  Add('ByteBool', TypeInfo(ByteBool));
  Add('WordBool', TypeInfo(WordBool));
  Add('LongBool', TypeInfo(LongBool));
  Add('ShortInt', TypeInfo(ShortInt));
  Add('Byte', TypeInfo(Byte));
  Add('SmallInt', TypeInfo(SmallInt));
  Add('Word', TypeInfo(Word));
  Add('Integer', TypeInfo(Integer));
  Add('LongInt', TypeInfo(LongInt));
  Add('Cardinal', TypeInfo(Cardinal));
  Add('LongWord', TypeInfo(LongWord));
  Add('Int64', TypeInfo(Int64));
  Add('UInt64', TypeInfo(UInt64));
  Add('QWord', TypeInfo(UInt64));
  Add('NativeInt', TypeInfo(NativeInt));
  Add('NativeUInt', TypeInfo(NativeUInt));
  Add('THandle', TypeInfo(THandle));
  Add('Single', TypeInfo(Single));
  Add('Double', TypeInfo(Double));
  Add('Extended', TypeInfo(Extended));
  Add('Comp', TypeInfo(Comp));
  Add('Currency', TypeInfo(Currency));
  Add('Real', TypeInfo(Real));
  Add('TDateTime', TypeInfo(TDateTime));
  Add('TDate', TypeInfo(TDateTime));
  Add('TTime', TypeInfo(TDateTime));
  Add('ShortString', TypeInfo(ShortString));
  Add('AnsiString', TypeInfo(AnsiString));
  Add('UTF8String', TypeInfo(UTF8String));
  Add('WideString', TypeInfo(WideString));
  Add('UnicodeString', TypeInfo(UnicodeString));
  Add('string', TypeInfo({$ifdef UNICODE}UnicodeString{$else}AnsiString{$endif}));
  Add('LuaString', TypeInfo(LuaString));
  Add('TFileName', TypeInfo(TFileName));
  Add('Variant', TypeInfo(Variant));
  Add('OleVariant', TypeInfo(OleVariant));
  Add('IInterface', TypeInfo(IInterface));
  Add('IUnknown', TypeInfo(IUnknown));
  Add('IDispatch', TypeInfo(IDispatch));
  Add('Exception', TypeInfo(Exception));
  {$ifdef EXTENDEDRTTI}
  Add('TProcedure', TypeInfo(TProcedure));
  Add('TProc', TypeInfo(TProc));
  {$endif}
end;

function TLua.InternalNewMetaTable: Integer;
begin
  Result := global_alloc_ref;

  lua_createtable(Handle, 0, 0);
  global_fill_value(Result);
end;

function TLua.InternalRegisterMetaTable(const MetaType: PLuaMetaType): Integer;
const
  LUA_GLOBALSINDEX = -10002;
  LUA_RIDX_GLOBALS = 2;
begin
  Result := InternalNewMetaTable;

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
    lua_pushlightuserdata(Handle, MetaType);
    lua_rawseti(Handle, -2, 0);

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
      if (PLuaMetaType(Ptr).F.Marker = LUA_METATYPE_MARKER) then
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
  Entity.Constant := (Kind in CONST_GLOBAL_KINDS);
  Result := Entity;
end;

function TLua.InternalAddMetaType(const Kind: TLuaMetaKind; const Name: LuaString;
  const TypeInfo: Pointer; const InstanceSize: Integer; const AdditionalSize: NativeInt): PLuaMetaType;
const
  SIZES: array[TLuaMetaKind] of Integer = (SizeOf(TLuaClassInfo), SizeOf(TLuaInterfaceInfo),
    SizeOf(TLuaRecordInfo), SizeOf(TLuaArrayInfo), SizeOf(TLuaSetInfo));
var
  LuaName: __luaname;
  Size: NativeInt;
  Ptr: __luapointer;
  Entity: PLuaGlobalEntity;

  procedure RegisterOperators(const Values: array of Integer);
  var
    i, Value: Integer;
  begin
    for i := Low(Values) to High(Values) do
    begin
      Value := Values[i];
      InternalRegisterCallback(OPERATOR_NAMES[Value], @TLua.__operator, Ptr, Value);
    end;
  end;
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
  Result.FLua := Self;
  Result.FName := Name;
  Result.F.Size := InstanceSize;
  Result.F.TypeInfo := TypeInfo;
  Result.F.Weak := IsWeakTypeInfo(TypeInfo);
  Result.FillManagedValue;
  Result.FillHFAValue;

  // metatable (Ref)
  {Result.F.Ref := } InternalRegisterMetaTable(Result);

  // metatypes dictionary
  TLuaDictionary(FMetaTypes).Add(LuaName, Ptr);
  if (Assigned(TypeInfo)) then TLuaDictionary(FMetaTypes).Add(TypeInfo, Ptr);
  if (Result.F.Kind = mtClass) then TLuaDictionary(FMetaTypes).Add(Result.F.ClassType.ClassInfo, Ptr);

  // clear instance (specific fields)
  System.FillChar(Pointer(NativeInt(Result) + SizeOf(TLuaMetaType))^,
    Size - SizeOf(TLuaMetaType), #0);

  // registered type names
  InternalRegTypeName(Name, Pointer(Result), 0);

  // metatable callbacks
  Ptr := Result.Ptr;
  global_push_value(Result.F.Ref);
  begin
    // common
    InternalRegisterCallback(Pointer(@ID_LEN), @TLua.__len, Ptr);
    InternalRegisterCallback(Pointer(@ID_TOSTRING), @TLua.__tostring, Ptr);
    InternalRegisterCallback(Pointer(@ID_CALL), @TLua.__call, Ptr, Ord(False));
    InternalRegisterCallback(Pointer(@ID_GC), @TLua.__gc, Ptr);
    InternalRegisterCallback(Pointer(@ID_INDEX), @TLua.__index, Ptr);
    InternalRegisterCallback(Pointer(@ID_NEWINDEX), @TLua.__newindex, Ptr, Ord(False));

    // operators
    case Kind of
      mtRecord:
      begin
        RegisterOperators([OPERATOR_NEG, OPERATOR_ADD, OPERATOR_SUB, OPERATOR_MUL, OPERATOR_DIV,
          OPERATOR_MOD, OPERATOR_POW, OPERATOR_EQUAL, OPERATOR_LESS, OPERATOR_LESS_EQUAL]);
      end;
      mtArray:
      begin
        if (Assigned(TypeInfo)) and (PTypeInfo(TypeInfo).Kind = tkDynArray) then
          RegisterOperators([OPERATOR_CONCAT]);
      end;
      mtSet:
      begin
        RegisterOperators([OPERATOR_NEG, OPERATOR_ADD, OPERATOR_SUB, OPERATOR_MUL, OPERATOR_EQUAL,
          OPERATOR_LESS_EQUAL]);
      end;
    end;
  end;
  lua_settop(Handle, 0);
end;

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

function TLua.InternalAddMethod(const MetaType: PLuaMetaType; const Name: LuaString;
  Address: Pointer; const MethodKind: TLuaMethodKind; const Invokable: __luapointer;
  const Critical: Boolean; {Universal} const MinArgsCount, MaxArgsCount: Word): __luapointer;
const
  STR_METHOD: array[0..6] of Byte = (6, Ord('M'), Ord('e'), Ord('t'), Ord('h'), Ord('o'), Ord('d'));
label
  new_or_replace, done;
var
  VmtOffset: NativeInt;
  ProcName: __luaname;
  TypeName: ^LuaString;
  Value: TLuaNamespaceMethod;
  Item: PLuaDictionaryItem;
  Entity: PLuaGlobalEntity;
  LastPtr, Ptr: __luapointer;
  Proc: ^TLuaNamespaceMethod;
  Frame: TLuaNativeFrame;
  NameBuffer: ShortString;
begin
  Result := LUA_POINTER_INVALID;
  NativeFrameEnter(Frame, PShortString(@STR_METHOD), Critical);
  try
    if (NativeUInt(Address) <= $FFFF) then
    begin
      RegisterError('address not defined');
      Exit;
    end;

    if (Invokable = LUA_POINTER_INVALID) and
      ((MinArgsCount > MaxArgsCount) or (MaxArgsCount > 20)) then
    begin
      RegisterErrorFmt('non-available argument count range: %d..%d', [MinArgsCount, MaxArgsCount]);
      Exit;
    end;

    // method name
    if (not IsValidIdent(Name)) then
    begin
      RegisterErrorFmt('non-supported name ("%s")', [Name]);
      Exit;
    end;
    ProcName := TLuaNames(FNames).Add(Name);
    NativeFrameRename(NameBuffer, Name);

    if Assigned(MetaType) and (MetaType.F.Kind = mtClass) then
    begin
      VmtOffset := PLuaClassInfo(MetaType).VmtOffset(Address, False);
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
    Value.ItemName := ProcName;
    Value.Address := Address;
    Value.Kind := MethodKind;
    Value.Mode := TLuaMethodMode(Invokable <> LUA_POINTER_INVALID);
    Value.ScriptInstance := (MethodKind <> mkStatic);
    Value.Invokable := Invokable;
    if (Value.Mode = mmUniversal) then
    begin
      Value.MinArgsCount := MinArgsCount;
      Value.MaxArgsCount := MaxArgsCount;
    end;
    Value.Func := LUA_FUNC_INVALID;

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
      Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaNamespaceMethod));
      if (MetaType = nil) then
      begin
        Entity.Ptr := Ptr;
      end else
      begin
        Item.Value := Ptr or NAMESPACE_FLAG_PROC;
      end;
      Proc := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
      Proc^ := Value;
      Proc.Func := alloc_luafunction(Proc);

      if (Assigned(MetaType) and (MetaType.F.Kind = mtClass)) then
      begin
        InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(MetaType.F.ClassType)),
          ProcName, LastPtr, Ptr or NAMESPACE_FLAG_INHERITED, False);
      end
    end else
    if (LastPtr and NAMESPACE_STD_MASK = NAMESPACE_STD_MASK) then
    begin
      // ToDo?
      Exit;
    end else
    if (LastPtr and NAMESPACE_FLAG_PROC = 0) then
    begin
      RegisterErrorFmt('property "%s" is already contained in %s', [Name, TypeName^]);
      Exit;
    end else
    begin
      if (LastPtr and NAMESPACE_FLAG_INHERITED = 0) then
      begin
        // own method
        Ptr := LastPtr;
        Proc := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr and NAMESPACE_FLAGS_CLEAR);
        Proc^ := Value;
        Proc.Func := alloc_luafunction(Proc);
      end else
      begin
        // inherited method: done if not changed
        Proc := TLuaMemoryHeap(FMemoryHeap).Unpack(LastPtr and NAMESPACE_FLAGS_CLEAR);
        if (Value.Address = Proc.Address) and (Value.Kind = Proc.Kind) and
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
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddProperty(const AName: LuaString;
  const AOptions: Pointer{PLuaPropertyOptions};
  const AAutoRegister, ACritical: Boolean): __luapointer;
const
  STR_PROPERTY: array[0..8] of Byte = (8, Ord('P'), Ord('r'), Ord('o'), Ord('p'),
    Ord('e'), Ord('r'), Ord('t'), Ord('y'));
label
  invalid_getter, invalid_getter_reference, invalid_setter, invalid_setter_reference,
  auto_increment_getter, default_owned_getter, meta_pointable_getter,
  interface_setter, unsafe_setter, meta_pointable_setter,
  new_or_replace, done;
var
  LOptions: ^TLuaPropertyOptions;
  LFrame: TLuaNativeFrame;
  LNameBuffer: ShortString;
  LPropName: __luaname;
  LValue: TLuaProperty;
  LClassType: TClass;
  LMinFieldOffset, LMaxFieldOffset: NativeUInt;
  LIsStatic: Boolean;
  LSize: NativeUInt;
  LPropMode: TLuaPropertyMode;
  LPropAddress: NativeUInt;
  LPropGetterProc: Boolean;
  LPropSetterProc: Boolean;
  LVmtOffset: NativeInt;
  LEntity: PLuaGlobalEntity;
  LItem: PLuaDictionaryItem;
  LLastPtr, LPtr: __luapointer;
  LProp: ^TLuaProperty;
  LReplaceDefault: Boolean;
begin
  Result := LUA_POINTER_INVALID;
  LOptions := AOptions;
  NativeFrameEnter(LFrame, PShortString(@STR_PROPERTY), ACritical);
  try
    // property name
    if (not IsValidIdent(AName)) then
    begin
      RegisterErrorFmt('non-supported name "%s"', [AName]);
      Exit;
    end;
    NativeFrameRename(LNameBuffer, AName);
    LPropName := TLuaNames(FNames).Add(AName);

    // parameters validation
    if (Byte(LOptions.Getter.Mode) > Byte(High(TLuaPropAccessMode))) or
      (Byte(LOptions.Setter.Mode) > Byte(High(TLuaPropAccessMode))) or
      (Byte(LOptions.Getter.Reference) > Byte(High(TLuaReference))) or
      (Byte(LOptions.Setter.Reference) > Byte(High(TLuaReference))) or
      ((LOptions.IsDefault) and (not LOptions.IsComplex)) or
      ((LOptions.IsComplex) and (LOptions.ComplexPtr = LUA_POINTER_INVALID)) or
      ((LOptions.IsGetterVirtual) and (LOptions.Getter.Mode <> amInstProc)) or
      ((LOptions.IsSetterVirtual) and (LOptions.Setter.Mode <> amInstProc)) or
      (
        (LOptions.IsComplex) and
        (
          not (LOptions.Getter.Mode in [amNone, amInstProc, amStaticProc]) or
          not (LOptions.Setter.Mode in [amNone, amInstProc, amStaticProc])
        )
      ) or
      (
        (LOptions.Index <> Low(Integer)) and
        (LOptions.Getter.Mode in [amNone, amInstField, amStaticField]) and
        (LOptions.Setter.Mode in [amNone, amInstField, amStaticField])
      ) or
      (
        (Assigned(LOptions.MetaType)) and
        (
          (NativeUInt(LOptions.MetaType) <= $ffff) or
          (LOptions.MetaType.F.Marker <> LUA_METATYPE_MARKER) or
          not (LOptions.MetaType.Kind in [mtClass, mtRecord])
        )
      ) or
      (
        (not Assigned(LOptions.MetaType)) and
        (
          not (LOptions.Getter.Mode in [amNone, amStaticField]) or
          not (LOptions.Setter.Mode in [amNone, amStaticField])
        )
      ) then
    begin
      RegisterError('invalid parameters');
      Exit;
    end;

    // type information, references, meta type pointer
    if (NativeUInt(LOptions.TypeInfo) <= $ffff) then
    begin
      RegisterError('TypeInfo not defined');
      Exit;
    end;
    if (LValue.InternalSetTypeInfo(Self, LOptions.TypeInfo, False) < 0) then
    begin
      RegisterError('unknown TypeInfo');
      Exit;
    end;

    // class type, instance data range
    LClassType := nil;
    if (not Assigned(LOptions.MetaType)) then
    begin
      LMinFieldOffset := High(NativeUInt);
      LMaxFieldOffset := High(NativeUInt);
    end else
    begin
      if (LOptions.MetaType.Kind = mtClass) then
      begin
        LClassType := LOptions.MetaType.F.ClassType;
        LMinFieldOffset := SizeOf(Pointer) {$ifdef AUTOREFCOUNT}* 2{$endif};
        LMaxFieldOffset := Cardinal(LClassType.InstanceSize);
      end else
      begin
        LMinFieldOffset := 0;
        LMaxFieldOffset := Cardinal(LOptions.MetaType.Size);
      end;
    end;

    // static mode
    if (LOptions.Getter.Mode = amNone) or (LOptions.Setter.Mode = amNone) then
    begin
      if (LOptions.Getter.Mode = LOptions.Setter.Mode) then
      begin
        RegisterError('there is no getter/setter');
        Exit;
      end else
      if (LOptions.Getter.Mode <> amNone) then
      begin
        LIsStatic := (LOptions.Getter.Mode in [amStaticField, amStaticProc]);
      end else
      begin
        LIsStatic := (LOptions.Setter.Mode in [amStaticField, amStaticProc]);
      end;
    end else
    if ((LOptions.Getter.Mode in [amInstField, amInstProc]) <> (LOptions.Setter.Mode in [amInstField, amInstProc])) then
    begin
      RegisterError('can not detect class/instance property');
      Exit;
    end else
    if (LOptions.Getter.Mode in [amInstField, amStaticField]) and
      (LOptions.Setter.Mode in [amInstField, amStaticField]) and
      (LOptions.Getter.Reference <> LOptions.Setter.Reference) then
    begin
      RegisterError('uncompatible field reference modes');
      Exit;
    end else
    begin
      LIsStatic := (LOptions.Getter.Mode in [amStaticField, amStaticProc]);
    end;

    // getter/setter
    LSize := Cardinal(LValue.Size);
    begin
      LPropAddress := LOptions.Getter.Offset;
      case LOptions.Getter.Mode of
        amInstField:
        begin
          LPropMode := pmField;
          if (NativeUInt(LPropAddress) < LMinFieldOffset) or
            (NativeUInt(LPropAddress) > LMaxFieldOffset) or
            (NativeUInt(LPropAddress) > LMaxFieldOffset - LSize) then
            goto invalid_getter;
        end;
        amInstProc:
        begin
          LPropMode := pmStaticProc;
          if (LOptions.IsGetterVirtual) then
          begin
            LPropMode := pmVirtualProc;
          end else
          if (LPropAddress <= $ffff) then
          begin
            goto invalid_getter;
          end else
          if (LOptions.VirtualAddressHint) and (Assigned(LClassType)) then
          begin
            LVmtOffset := PLuaClassInfo(LOptions.MetaType).VmtOffset(Pointer(LPropAddress), False);
            if (LVmtOffset >= 0) then
            begin
              LPropMode := pmVirtualProc;
              LPropAddress := NativeUInt(LVmtOffset);
            end;
          end;

          if (LOptions.Getter.Reference = lrWeak) then goto invalid_getter_reference;
        end;
        amStaticField:
        begin
          LPropMode := pmField;
          if (LPropAddress <= $ffff) then goto invalid_getter;
        end;
        amStaticProc:
        begin
          LPropMode := pmStaticProc;
          if (LPropAddress <= $ffff) then goto invalid_getter;
          if (LOptions.Getter.Reference = lrWeak) then goto invalid_getter_reference;
        end;
      else
        // amNone:
        LPropMode := pmNone;
        if (LPropAddress <> 0) then
        begin
        invalid_getter:
          if (LOptions.Getter.Mode = amInstField) then
          begin
            RegisterError('invalid getter field');
          end else
          begin
            RegisterError('invalid getter address');
          end;
          Exit;
        end;
        if (LOptions.Getter.Reference <> lrDefault) then
        begin
        invalid_getter_reference:
          RegisterError('invalid getter reference');
          Exit;
        end;
      end;
      if (LPropMode in [pmStaticProc, pmVirtualProc]) and (not LOptions.IsComplex) and
        (LOptions.Index <> Low(Integer)) then
      begin
        Inc(LPropMode);
      end;
      LValue.GetterMode := LPropMode;
      LValue.Getter := LPropAddress;

      LPropAddress := LOptions.Setter.Offset;
      case LOptions.Setter.Mode of
        amInstField:
        begin
          LPropMode := pmField;
          if (NativeUInt(LPropAddress) < LMinFieldOffset) or
            (NativeUInt(LPropAddress) > LMaxFieldOffset) or
            (NativeUInt(LPropAddress) > LMaxFieldOffset - LSize) then
            goto invalid_setter;
        end;
        amInstProc:
        begin
          LPropMode := pmStaticProc;
          if (LOptions.IsSetterVirtual) then
          begin
            LPropMode := pmVirtualProc;
          end else
          if (LPropAddress <= $ffff) then
          begin
            goto invalid_setter;
          end else
          if (LOptions.VirtualAddressHint) and (Assigned(LClassType)) then
          begin
            LVmtOffset := PLuaClassInfo(LOptions.MetaType).VmtOffset(Pointer(LPropAddress), False);
            if (LVmtOffset >= 0) then
            begin
              LPropMode := pmVirtualProc;
              LPropAddress := NativeUInt(LVmtOffset);
            end;
          end;

          if (LOptions.Setter.Reference <> lrDefault) then goto invalid_setter_reference;
        end;
        amStaticField:
        begin
          LPropMode := pmField;
          if (LPropAddress <= $ffff) then goto invalid_setter;
        end;
        amStaticProc:
        begin
          LPropMode := pmStaticProc;
          if (LPropAddress <= $ffff) then goto invalid_setter;
          if (LOptions.Setter.Reference <> lrDefault) then goto invalid_setter_reference;
        end;
      else
        // amNone:
        LPropMode := pmNone;
        if (LPropAddress <> 0) then
        begin
        invalid_setter:
          if (LOptions.Setter.Mode = amInstField) then
          begin
            RegisterError('invalid setter field');
          end else
          begin
            RegisterError('invalid setter address');
          end;
          Exit;
        end;
        if (LOptions.Setter.Reference <> lrDefault) then
        begin
        invalid_setter_reference:
          RegisterError('invalid setter reference');
          Exit;
        end;
      end;
      if (LPropMode in [pmStaticProc, pmVirtualProc]) and (not LOptions.IsComplex) and
        (LOptions.Index <> Low(Integer)) then
      begin
        Inc(LPropMode);
      end;
      LValue.SetterMode := LPropMode;
      LValue.Setter := LPropAddress;
    end;

    // flags
    begin
      LValue.IsStatic := LIsStatic;
      LValue.IsComplex := LOptions.IsComplex;
      LValue.FIndex.Value := LOptions.Index;
      LPropGetterProc := (LValue.GetterMode > pmField);
      LPropSetterProc := (LValue.SetterMode > pmField);

      // getter
      if (LValue.F.Kind >= vkVariant) and (LValue.GetterMode <> pmNone) and
        (LValue.ExtendedGetter <> egMetaReference) then
      begin
        case LValue.Kind of
          vkVariant, {todo: TVarData,} vkLuaArg:
          begin
            goto default_owned_getter;
          end;
          {$ifdef AUTOREFCOUNT}
          vkObject,
          {$endif}
          vkInterface:
          begin
            goto auto_increment_getter;
          end;
          vkClosure:
          begin
            if (LValue.ClosureMode = mmReference) then
            begin
            auto_increment_getter:
              {$ifdef WEAKINTFREF}
              if (LOptions.Getter.Reference in [lrWeak, lrUnsafe]) then
              begin
                LValue.ExtendedGetter := egUnsafe;
              end else
              {$endif}
              begin
                // lrDefault
                goto default_owned_getter;
              end;
            end;
          end;
          vkArray:
          begin
            if (PLuaArrayInfo(LValue.MetaType).IsDynamic) then
            begin
            default_owned_getter:
              {LValue.ExtendedGetter := egOwned;}
              LValue.IsExtendedGetterTemporary := LPropGetterProc;
            end else
            begin
              goto meta_pointable_getter;
            end;
          end;
          vkRecord, vkSet:
          begin
          meta_pointable_getter:
            if (LPropGetterProc) then
            begin
              if (LValue.F.MetaType.HFA) then
              begin
                LValue.ExtendedGetter := egHFAOwned;
              end else
              begin
                LValue.ExtendedGetter := egOwned;
                LValue.IsExtendedGetterTemporary := LValue.F.MetaType.ReturnReferenced;
              end;
            end else
            begin
              {Self.ExtendedGetter := egUnsafe;}
            end;

            if (LOptions.ConstRefHint) and (not LPropGetterProc{Field}) and (LPropSetterProc) then
            begin
              LValue.IsExtendedGetterConstant := True;
            end;
          end;
        end;
      end;

      // setter
      if (LValue.F.Kind >= vkVariant) and (LValue.SetterMode <> pmNone) and
        (LValue.ExtendedSetter <> esMetaReference) then
      begin
        if (not LPropSetterProc) then
        begin
          // setter field
          case LValue.Kind of
            vkVariant, {todo: TVarData,} vkLuaArg: {LValue.ExtendedSetter := esManaged;};
            {$ifdef WEAKINSTREF}
            vkObject:
            begin
              case LOptions.Setter.Reference of
                lrWeak: LValue.ExtendedSetter := esWeakManaged;
                lrUnsafe: LValue.ExtendedSetter := esUnsafe;
              else
                {LValue.ExtendedSetter := esManaged;}
              end;
            end;
            {$endif}
            vkInterface:
            begin
              goto interface_setter;
            end;
            vkClosure:
            begin
              case LValue.ClosureMode of
                {$ifdef WEAKINSTREF}
                mmMethod:
                begin
                  if (LOptions.Setter.Reference = esUnsafe) then
                  begin
                    LValue.ExtendedSetter := esUnsafe;
                  end;
                end;
                {$endif}
                mmReference:
                begin
                interface_setter:
                  {$ifdef WEAKINTFREF}
                  case LOptions.Setter.Reference of
                    lrWeak: LValue.ExtendedSetter := esWeakManaged;
                    lrUnsafe: LValue.ExtendedSetter := esUnsafe;
                  else
                    {LValue.ExtendedSetter := esManaged;}
                  end;
                  {$endif}
                end;
              end;
            end;
            vkArray, vkRecord, vkSet: {LValue.ExtendedSetter := esUnsafe/esManaged/esWeakManaged;};
          end;
        end else
        begin
          // setter proc
          case LValue.Kind of
            vkVariant, {todo: TVarData,} vkLuaArg: {LValue.ExtendedSetter := esManaged;};
            vkObject, vkInterface:
            begin
              goto unsafe_setter;
            end;
            vkClosure:
            begin
              goto unsafe_setter;
            end;
            vkArray:
            begin
              if (PLuaArrayInfo(LValue.MetaType).IsDynamic) then
              begin
              unsafe_setter:
                LValue.ExtendedSetter := esUnsafe;
              end else
              begin
                goto meta_pointable_setter;
              end;
            end;
            vkRecord, vkSet:
            begin
            meta_pointable_setter:
              if (LValue.F.MetaType.ParamReferenced(ccReg, True)) then
              begin
                LValue.ExtendedSetter := esMetaReference;
              end else
              if (LValue.F.MetaType.HFA) then
              begin
                LValue.ExtendedSetter := esHFA;
              end else
              begin
                LValue.ExtendedSetter := esUnsafe;
              end;
            end;
          end;
        end;
      end;
    end;

    // find existing/add item
    LEntity := nil;
    LItem := nil;
    if (LOptions.MetaType = nil) then
    begin
      LEntity := InternalAddGlobal(Ord(gkVariable), LPropName);
      LEntity.Constant := LOptions.ConstRefHint;
      LLastPtr := LEntity.Ptr;
    end else
    begin
      LItem := TLuaDictionary(PLuaRecordInfo(LOptions.MetaType).FNameSpace).InternalFind(LPropName, True);
      LLastPtr := LItem.Value;
    end;

    // fill/update routine
    //  not found: fill and add childs (replace invalid)
    //  not found and defaut: fill+default and add childs (replace invalid defaults)
    //  found inherited: replace to new, replace last value to new
    //  found inherited and default: replace to new, replace last value to new, replace last default to new
    //  found and set default: (replace last value to new), replace last default to new
    //  found and not set default: fill
    if (LLastPtr = LUA_POINTER_INVALID) then
    begin
    new_or_replace:
      LPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaProperty));
      if (LOptions.MetaType = nil) then
      begin
        LEntity.Ptr := LPtr;
      end else
      begin
        LItem.Value := LPtr {or 0};
      end;
      LProp := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr);
      LProp^ := LValue;

      if (Assigned(LOptions.MetaType) and (LOptions.MetaType.F.Kind = mtClass)) then
      begin
        LReplaceDefault := (LOptions.IsDefault) or (PLuaClassInfo(LOptions.MetaType).DefaultProperty = LLastPtr and NAMESPACE_FLAGS_CLEAR);
        InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(LOptions.MetaType.F.ClassType)),
          LPropName, LLastPtr, LPtr or NAMESPACE_FLAG_INHERITED, LReplaceDefault);
      end;
    end else
    if (LLastPtr and NAMESPACE_STD_MASK = NAMESPACE_STD_MASK) then
    begin
      // ToDo?
      Exit;
    end else
    if (LLastPtr and NAMESPACE_FLAG_PROC <> 0) then
    begin
      RegisterErrorFmt('method "%s" is already contained in %s', [AName, LOptions.MetaType.Name]);
      Exit;
    end else
    begin
      if (LLastPtr and NAMESPACE_FLAG_INHERITED = 0) then
      begin
        // own property
        LPtr := LLastPtr;
        LProp := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr {and NAMESPACE_FLAGS_CLEAR});
        LProp^ := LValue;
        if (LOptions.IsDefault) and (PLuaClassInfo(LOptions.MetaType).DefaultProperty <> LPtr) then
        begin
          // set default
          InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(LOptions.MetaType.F.ClassType)),
              LPropName, LPtr or NAMESPACE_FLAG_INHERITED, LPtr or NAMESPACE_FLAG_INHERITED, True);
        end;
      end else
      begin
        // inherited property: done if not changed
        LProp := TLuaMemoryHeap(FMemoryHeap).Unpack(LLastPtr and NAMESPACE_FLAGS_CLEAR);
        if (LValue.TypeInfo = LProp.TypeInfo) and (LValue.Getter = LProp.Getter) and
          (LValue.Setter = LProp.Setter) and (LValue.Index = LProp.Index) and (LValue.Options = LProp.Options) then
        begin
          if (not LOptions.IsDefault) or
            (PLuaClassInfo(LOptions.MetaType).DefaultProperty = LLastPtr and NAMESPACE_FLAGS_CLEAR) then
          begin
            LPtr := LLastPtr;
            goto done;
          end;
        end;

        // replace to new allocated property
        goto new_or_replace;
      end;
    end;

  done:
    Result := LPtr {and NAMESPACE_FLAGS_CLEAR};
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddField(const AMetaType: PLuaMetaType; const AName: LuaString;
  const AOffset: NativeUInt; const ATypeInfo: PTypeInfo; const AReference: TLuaReference;
  const AClassField, AAutoRegister, ACritical: Boolean): __luapointer;
var
  LOptions: TLuaPropertyOptions;
begin
  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.MetaType := AMetaType;
  LOptions.TypeInfo := ATypeInfo;
  LOptions.Index := Low(Integer);
  LOptions.Getter.Reference := AReference;
  LOptions.Setter.Reference := AReference;
  LOptions.Getter.Offset := AOffset;
  LOptions.Setter.Offset := AOffset;

  if (AClassField) then
  begin
    LOptions.Getter.Mode := amStaticField;
    LOptions.Setter.Mode := amStaticField;
  end else
  begin
    LOptions.Getter.Mode := amInstField;
    LOptions.Setter.Mode := amInstField;
  end;

  Result := InternalAddProperty(AName, @LOptions, AAutoRegister, ACritical);
end;

procedure __AddClassRtti(const Lua: TLua; const ClassInfo: PLuaClassInfo;
  const Value: TClass; const UseExtendedRtti: Boolean);
type
  PClassField = ^TClassField;
  TClassField = packed record
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
    Fields: array[0..0] of TClassField;
  end;
  {$ifdef EXTENDEDRTTI}
  PClassFieldEx = ^TClassFieldEx;
  TClassFieldEx = packed record
    Flags: Byte;
    TypeRef: PPTypeInfo;
    Offset: Cardinal;
    Name: ShortString;
  end;
  {$endif}
var
  Ptr: PByte;
  Count, i: Integer;
  FieldTable: PClassFieldTable;
  Field: PClassField;
  ItemName: LuaString;
  PropInfo: {$ifdef UNITSCOPENAMES}System.{$endif}TypInfo.PPropInfo;
  {$ifdef EXTENDEDRTTI}
  MemberVisibility: TMemberVisibility;
  FieldEx: PClassFieldEx;
  Reference: TLuaReference;
  MethodEx: PExtendedClassMethodEntry;
  MethodKind: TLuaMethodKind;
  VmtOffset: NativeInt;
  ChildFrame: TLuaNativeFrame;
  Invokable: __luapointer;
  {$endif}

  procedure AddPropInfo;
  var
    LOptions: TLuaPropertyOptions;
    LGetter, LSetter: NativeUInt;
  begin
    Lua.unpack_lua_string(ItemName, PShortString(@PropInfo.Name)^);

    LGetter := NativeUInt(PropInfo.GetProc);
    LSetter := NativeUInt(PropInfo.SetProc);
    FillChar(LOptions, SizeOf(LOptions), #0);
    LOptions.MetaType := ClassInfo;
    LOptions.TypeInfo := PropInfo.PropType^;
    {$ifdef EXTENDEDRTTI}
    LOptions.Getter.Reference := Reference;
    LOptions.Setter.Reference := Reference;
    {$endif}
    LOptions.Index := PropInfo.Index;

    if (LGetter <> 0) then
    begin
      if (LGetter >= PROPSLOT_FIELD) then
      begin
        LOptions.Getter.Mode := amInstField;
        LGetter := LGetter and PROPSLOT_CLEAR;
      end else
      if (LGetter >= PROPSLOT_VIRTUAL) then
      begin
        LOptions.Getter.Mode := amInstProc;
        LOptions.IsGetterVirtual := True;
        LGetter := LGetter and PROPSLOT_CLEAR;
      end else
      begin
        LOptions.Getter.Mode := amInstProc;
      end;
    end;
    LOptions.Getter.Offset := LGetter;

    if (LSetter <> 0) then
    begin
      if (LSetter >= PROPSLOT_FIELD) then
      begin
        LOptions.Setter.Mode := amInstField;
        LSetter := LSetter and PROPSLOT_CLEAR;
      end else
      if (LSetter >= PROPSLOT_VIRTUAL) then
      begin
        LOptions.Setter.Mode := amInstProc;
        LOptions.IsSetterVirtual := True;
        LSetter := LSetter and PROPSLOT_CLEAR;
      end else
      begin
        LOptions.Setter.Mode := amInstProc;
      end;
    end;
    LOptions.Setter.Offset := LSetter;

    {$ifdef EXTENDEDRTTI}
    if (not (LOptions.Getter.Mode in [amInstField, amStaticField])) then
    begin
      LOptions.Getter.Reference := lrDefault;
    end;
    if (not (LOptions.Setter.Mode in [amInstField, amStaticField])) then
    begin
      LOptions.Setter.Reference := lrDefault;
    end;
    {$endif}

    if (PTypeInfo(LOptions.TypeInfo).Kind in [tkArray, tkRecord, tkSet{$ifdef FPC}, tkObject{$endif}]) then
    begin
      if (LOptions.Getter.Mode in [amInstField, amStaticField]) then
      begin
        if (LOptions.Getter.Mode <> LOptions.Setter.Mode) or
          (LOptions.Getter.Address <> (LOptions.Setter.Address)) then
        begin
          LOptions.ConstRefHint := True;
        end;
      end;
    end;

    Lua.InternalAddProperty(ItemName, @LOptions, True, False);
  end;

  function AddUniversalMethod(const Method: PClassMethodEntry; const MethodKind: TLuaMethodKind): Boolean;
  var
    Buffer: ShortString;
    NameLength: Integer;
  begin
    NameLength := Method.NameLength;
    Result := (NameLength > 3) and (Method.NameChars[1] = Byte('l')) and
      (Method.NameChars[2] = Byte('u')) and (Method.NameChars[3] = Byte('a'));

    if (Result) then
    begin
      Dec(NameLength, 3);
      PByte(@Buffer)^ := NameLength;
      System.Move(Method.NameChars[4], Buffer[1], NameLength);
      Lua.unpack_lua_string(ItemName, Buffer);

      Lua.InternalAddMethod(ClassInfo, ItemName, Method.Address, MethodKind, LUA_POINTER_INVALID, False);
    end;
  end;
begin
  // registrators hierarchy
  if (ClassInfo.F.ClassType <> Value) and (ClassInfo.F.ClassType <> Value.ClassParent) then
  begin
    __AddClassRtti(Lua, ClassInfo, Value.ClassParent, UseExtendedRtti);
  end;

  // published (standard) fields
  Ptr := PPointer(NativeInt(Value) + vmtFieldTable)^;
  if (Assigned(Ptr)) then
  begin
    FieldTable := Pointer(Ptr);
    if (FieldTable.Classes <> nil) then
    for i := 0 to Integer(FieldTable.Classes.Count) - 1 do
    begin
      Lua.InternalAutoRegister(FieldTable.Classes.Values[i].ClassInfo, UseExtendedRtti);
    end;

    Ptr := Pointer(@FieldTable.Fields[0]);
    for i := 0 to Integer(FieldTable.Count) - 1 do
    begin
      Field := Pointer(Ptr);
      Lua.unpack_lua_string(ItemName, Field.Name);
      Lua.InternalAddField(ClassInfo, ItemName,
        Field.Offset, FieldTable.Classes.Values[Field.TypeIndex].ClassInfo,
        lrDefault, False, True, False);

      Ptr := GetTail(Field.Name);
    end;
  end;

  // extended fields
  {$ifdef EXTENDEDRTTI}
  if (Assigned(Ptr)) and (UseExtendedRtti) then
  begin
    Count := PWord(Ptr)^;
    Inc(Ptr, SizeOf(Word));

    for i := 1 to Count do
    begin
      FieldEx := Pointer(Ptr);
      Ptr := GetTail(FieldEx.Name);
      Ptr := SkipAttributes(Ptr, Reference);

      MemberVisibility := TMemberVisibility(FieldEx.Flags and 3);
      case (MemberVisibility) of
        mvPublic, mvPublished:
        begin
          Lua.unpack_lua_string(ItemName, FieldEx.Name);
          Lua.InternalAddField(ClassInfo, ItemName, FieldEx.Offset,
            GetTypeInfo(FieldEx.TypeRef), Reference, False, True, False);
        end;
      end;
    end;
  end;
  {$endif}

  // published (standard) properties
  Ptr := GetTail(GetTypeData(Value.ClassInfo).UnitName);
  Count := PWord(Ptr)^;
  Inc(Ptr, SizeOf(Word));
  for i := 1 to Count do
  begin
    PropInfo := Pointer(Ptr);
    {$ifdef EXTENDEDRTTI}
    Reference := lrDefault;
    {$endif}
    AddPropInfo;
    Ptr := GetTail(PropInfo.Name);
  end;

  // extended properties
  {$ifdef EXTENDEDRTTI}
  if (UseExtendedRtti) then
  begin
    Count := PWord(Ptr)^;
    Inc(Ptr, SizeOf(Word));

    for i := 1 to Count do
    begin
      MemberVisibility := TMemberVisibility(Ptr^ and 3);
      Inc(Ptr);

      PropInfo := PPointer(Ptr)^;
      Inc(Ptr, SizeOf(Pointer));
      Ptr := SkipAttributes(Ptr, Reference);

      case (MemberVisibility) of
        mvPublic, mvPublished:
        begin
          AddPropInfo;
        end;
      end;
    end;
  end;
  {$endif}

  // published (standard) methods
  Ptr := PPointer(NativeInt(Value) + vmtMethodTable)^;
  if (Assigned(Ptr)) then
  begin
    Count := PWord(Ptr)^;
    Inc(Ptr, SizeOf(Word));

    for i := 1 to Count do
    begin
      AddUniversalMethod(Pointer(Ptr), mkInstance);
      Inc(Ptr, PClassMethodEntry(Ptr).Size);
    end;
  end;

  // extended methods
  {$ifdef EXTENDEDRTTI}
  if (Assigned(Ptr)) and (UseExtendedRtti) then
  begin
    Count := PWord(Ptr)^;
    Inc(Ptr, SizeOf(Word));

    for i := 1 to Count do
    begin
      MethodEx := PExtendedClassMethodEntry(Ptr);

      MemberVisibility := MethodEx.MemberVisibility;
      case (MemberVisibility) of
        mvPublic, mvPublished:
        begin
          MethodKind := MethodEx.DefaultMethodKind;
          if (not AddUniversalMethod(MethodEx.Entry, MethodKind)) then
          begin
            VmtOffset := ClassInfo.VmtOffset(MethodEx.Entry.Address, True);
            if (VmtOffset = -1) or (VmtOffset > {$ifdef FPC}vmtToString{$else .DELPHI}vmtDestroy{$endif}) then
            begin
              Lua.NativeFrameEnter(ChildFrame, PShortString(@MethodEx.Entry.Name), False);
              try
                Invokable := TLuaInvokableBuilder(Lua.FInvokableBuilder).BuildClassMethod(
                  ClassInfo, MethodEx.Entry, MethodKind, MethodEx.IsHasSelf);
              finally
                Lua.NativeFrameLeave;
              end;

              if (Invokable <> LUA_POINTER_INVALID) then
              begin
                Lua.unpack_lua_string(ItemName, MethodEx.Entry.Name);
                Lua.InternalAddMethod(ClassInfo, ItemName, MethodEx.Entry.Address, MethodKind, Invokable, False);
              end;
            end;
          end;
        end;
      end;

      Inc(Ptr, SizeOf(TExtendedClassMethodEntry));
    end;
  end;
  {$endif}
end;

// add class, if not already exists
// UseRtti means to register all published properties
// in registrator-Class case:
// addition sub-registered class using published registrator-Class methods, fields and properties
function TLua.InternalAddClass(const AClass: TClass; const UseRtti: Boolean;
  const Critical: Boolean): PLuaClassInfo;
const
  STR_CLASS: array[0..5] of Byte = (5, Ord('C'), Ord('l'), Ord('a'), Ord('s'), Ord('s'));
var
  i: Integer;
  ClassName: LuaString;
  LuaClassName: __luaname;
  ClassRegistrator, ClassValue, ClassChild: TClass;
  ClassPtr: __luapointer;
  ClassInfo, Parent: PLuaClassInfo;
  Item: PLuaDictionaryItem;
  Frame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(Frame, PShortString(@STR_CLASS), Critical);
  try
    if (NativeUInt(AClass) <= $FFFF) then
    begin
      RegisterError('parameter not defined');
      Exit;
    end;

    // find exists
    ClassPtr := TLuaDictionary(FMetaTypes).FindValue(Pointer(AClass));
    if (ClassPtr <> LUA_POINTER_INVALID) and (not UseRtti) then
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(ClassPtr);
      if (Result.F.Kind <> mtClass) then
      begin
        ClassName := Result.Name;
        RegisterErrorFmt('invalid class "%s" parameter', [ClassName]);
      end;
      Exit;
    end;

    // registrator, class, class name
    ClassValue := AClass;
    ClassRegistrator := nil;
    NativeFrameRename(PShortString(PPointer(NativeInt(ClassValue) + vmtClassName)^));
    repeat
      ClassName := LuaString(ClassValue.ClassName);
      if (Length(ClassName) > 3) and (ClassName[1] = 'l') and
        (ClassName[2] = 'u') and (ClassName[3] = 'a') then
      begin
        if (ClassRegistrator = nil) then ClassRegistrator := AClass;
        ClassChild := ClassValue;
        ClassValue := ClassValue.ClassParent;
        if (ClassValue = nil) then
        begin
          RegisterError('ClassRegistrator is defined, but real class not found');
          Exit;
        end;

        if (ClassValue.InstanceSize <> ClassChild.InstanceSize) then
        begin
          ClassName := LuaString(ClassChild.ClassName);
          RegisterErrorFmt('Class registrator "%s" can''t have own fields', [ClassName]);
          Exit;
        end;
      end else
      begin
        Break;
      end;
    until (False);

    // existing or new class
    if (ClassValue <> AClass) then
    begin
      ClassPtr := TLuaDictionary(FMetaTypes).FindValue(Pointer(ClassValue));
    end;
    if (ClassPtr <> LUA_POINTER_INVALID) then
    begin
      ClassInfo := TLuaMemoryHeap(FMemoryHeap).Unpack(ClassPtr);
    end else
    begin
      // check existing name (type)
      LuaClassName := TLuaNames(FNames).Add(ClassName);
      if (TLuaDictionary(FMetaTypes).Find(LuaClassName) <> nil) then
      begin
        RegisterErrorTypeExists(ClassName);
        Exit;
      end;

      // globals, metatypes dictionary, metatable
      ClassInfo := Pointer(InternalAddMetaType(mtClass, ClassName, Pointer(ClassValue), SizeOf(Pointer)));

      // register parents, inherits name space
      if (ClassValue.ClassParent = nil) then
      begin
        // TObject: default name space
        TLuaDictionary(ClassInfo.FNameSpace).Assign(TLuaDictionary(FStdObjectNameSpace));
        ClassInfo.Parent := LUA_POINTER_INVALID;
        ClassInfo.DefaultProperty := LUA_POINTER_INVALID;
       end else
      begin
        Parent := InternalAddClass(AClass.ClassParent, UseRtti, Critical);
        TLuaDictionary(ClassInfo.FNameSpace).Assign(TLuaDictionary(Parent.FNameSpace));
        Item := Pointer(TLuaDictionary(ClassInfo.FNameSpace).FItems);
        for i := 1 to TLuaDictionary(ClassInfo.FNameSpace).Count do
        begin
          if (Item.Value > 0) then
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

    // class rtti
    if (ClassInfo.Parent <> LUA_POINTER_INVALID) and (UseRtti) then
    begin
      __AddClassRtti(Self, ClassInfo, ClassValue, True);
    end;

    // class registrators
    if (ClassRegistrator <> nil) then
    begin
      __AddClassRtti(Self, ClassInfo, ClassRegistrator, UseRtti);
    end;

    // result
    Result := ClassInfo;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalGetClassInfo(const AClass: TClass): PLuaClassInfo;
var
  Item: PLuaDictionaryItem;
begin
  if (Assigned(AClass)) then
  begin
    Item := TLuaDictionary(FMetaTypes).InternalFind(Pointer(AClass), False);
    if (Assigned(Item)) then
    begin
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Item.Value);
      Exit;
    end else
    begin
      Result := InternalAddClass(AClass, False, False);
    end;
  end else
  begin
    Result := nil;
  end;
end;

function TLua.InternalAddRecord(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaRecordInfo;
const
  STR_RECORD: array[0..6] of Byte = (6, Ord('R'), Ord('e'), Ord('c'), Ord('o'), Ord('r'), Ord('d'));
//var
//  Frame: TLuaNativeFrame;
var
  Name: LuaString;
begin
// NativeFrameEnter(Frame, PShortString(@STR_RECORD));
//  try
    // ToDo
    unpack_lua_string(Name, PShortString(@TypeInfo.Name)^);
    Result := InternalAddRecordEx(Name, TypeInfo, True, Critical);
//  finally
//    NativeFrameLeave;
//  end;
end;

// TypeInfo can be the following:
//  - TypeInfo(record)
//  - TypeInfo(Dynamic array of record type)
//  - Pointer(SizeOf(record))
function TLua.InternalAddRecordEx(const Name: LuaString; const TypeInfo: Pointer;
  const UseRtti: Boolean; const Critical: Boolean): PLuaRecordInfo;
const
  STR_RECORD: array[0..6] of Byte = (6, Ord('R'), Ord('e'), Ord('c'), Ord('o'), Ord('r'), Ord('d'));
{$ifdef EXTENDEDRTTI}
type
  PRecordTypeField = ^TRecordTypeField;
  TRecordTypeField = packed record
    Header: TAnonymousFieldInfo;
    Flags: Byte;
    Name: ShortString;
  end;

  PRecordTypeMethod = ^TRecordTypeMethod;
  TRecordTypeMethod = packed record
    Flags: Byte;
    Address: Pointer;
    Name: ShortString;
  end;
const
  METHOD_KINDS: array[0..3] of TLuaMethodKind = (mkInstance, mkStatic, mkConstructor, mkOperator);
{$endif}
var
  RecordTypeInfo: PTypeInfo;
  RecordSize: Integer;
  TypeData: PTypeData;
  RecordPtr: __luapointer;
  LuaName: __luaname;
  {$ifdef EXTENDEDRTTI}
  Ptr: PByte;
  Count, i, j: Integer;
  ItemName: LuaString;
  ItemTypeInfo: PTypeInfo;
  Field: PRecordTypeField;
  Reference: TLuaReference;
  Method: PRecordTypeMethod;
  MethodKind: TLuaMethodKind;
  NameBuffer: ShortString;
  ChildFrame: TLuaNativeFrame;
  Invokable: __luapointer;
  {$endif}
  Frame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(Frame, PShortString(@STR_RECORD), Critical);
  try
    if (not IsValidIdent(Name)) then
    begin
      RegisterErrorFmt('non-supported name ("%s")', [Name]);
      Exit;
    end;

    if (TypeInfo = nil) then
    begin
      RegisterErrorFmt('TypeInfo of record "%s" not defined', [Name]);
      Exit;
    end;

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
            RegisterErrorFmt('sub dynamic type "%s" is not record type (%s)',
              [FStringBuffer.Lua, FStringBuffer.Default]);
            Exit;
          end;
        end;
      end else
      begin
        GetTypeKindName(FStringBuffer.Default, PTypeInfo(TypeInfo).Kind);
        RegisterErrorFmt('type "%s" is not record or subdynamic type (%s)',
          [Name, FStringBuffer.Default]);
        Exit;
      end;
    end;

    // find existing, name item
    if (Assigned(RecordTypeInfo)) then
    begin
      unpack_lua_string(FStringBuffer.Lua, PShortString(@RecordTypeInfo.Name)^);

      if (FStringBuffer.Lua <> Name) then
      begin
        RegisterErrorFmt('mismatch of names TypeInfo "%s" and "%s" as parameter "Name"',
          [FStringBuffer.Lua, Name]);
        Exit;
      end;

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
      begin
        RegisterErrorTypeExists(Name);
        Exit;
      end;

      if (Result.Size <> RecordSize) then
      begin
        RegisterErrorFmt('size of %s (%d) differs from the previous value (%d)',
          [Name, RecordSize, Result.Size]);
        Exit;
      end;

      if (Assigned(RecordTypeInfo)) then
      begin
        if (Result.F.TypeInfo = nil) then
        begin
          Result.F.TypeInfo := RecordTypeInfo;
          Result.FillManagedValue;
          Result.FillHFAValue;
          TLuaDictionary(FMetaTypes).Add(RecordTypeInfo, RecordPtr);
        end else
        if (Result.F.TypeInfo <> RecordTypeInfo) then
        begin
          RegisterErrorFmt('TypeInfo of "%s" differs from the previous value', [Name]);
          Exit;
        end;
      end;
    end else
    begin
      Result := Pointer(InternalAddMetaType(mtRecord, Name, RecordTypeInfo, RecordSize));
      TLuaDictionary(Result.FNameSpace).Assign(TLuaDictionary(FStdRecordNameSpace));
    end;

    if (not UseRtti) or (not Assigned(RecordTypeInfo)) then
      Exit;

    {$ifdef EXTENDEDRTTI}
    // name
    NativeFrameRename(NameBuffer, Name);

    // skip managed (anonymous) fields
    Ptr := Pointer(@GetTypeData(RecordTypeInfo).ManagedFldCount);
    Count := PInteger(Ptr)^;
    Inc(Ptr, SizeOf(Integer));
    Inc(Ptr, Count * SizeOf(TAnonymousFieldInfo));

    // skip "ops"
    Count := Ptr^;
    Inc(Ptr);
    Inc(Ptr, Count * SizeOf(Pointer));

    // fields
    Count := PInteger(Ptr)^;
    Inc(Ptr, SizeOf(Integer));
    for i := 1 to Count do
    begin
      Field := Pointer(Ptr);
      Ptr := GetTail(Field.Name);
      Ptr := SkipAttributes(Ptr, Reference);

      case TMemberVisibility(Field.Flags and 3) of
        mvPublic, mvPublished:
        begin
          ItemTypeInfo := GetTypeInfo(Field.Header.TypeInfo); // ToDo

          if (Assigned(ItemTypeInfo)) then
          begin
            unpack_lua_string(ItemName, Field.Name);
            InternalAddField(Result, ItemName, Field.Header.Offset, ItemTypeInfo,
              Reference, False, True, False);
          end;
        end;
      end;
    end;

    // methods
    Ptr := SkipAttributes(Ptr);
    Count := PWord(Ptr)^;
    Inc(Ptr, SizeOf(Word));
    for i := 1 to Count do
    begin
      Method := Pointer(Ptr);
      Ptr := GetTail(Method.Name);

      case TMemberVisibility((Method.Flags shr 2) and 3) of
        mvPublic, mvPublished:
        begin
          MethodKind := METHOD_KINDS[Method.Flags and 3];
          if (MethodKind <> mkOperator) then
          begin
            NativeFrameEnter(ChildFrame, PShortString(@Method.Name), False);
            try
              Invokable := TLuaInvokableBuilder(FInvokableBuilder).BuildProcedureSignature(Result, Pointer(Ptr), MethodKind);
            finally
              NativeFrameLeave;
            end;

            if (Invokable <> LUA_POINTER_INVALID) then
            begin
              unpack_lua_string(ItemName, Method.Name);
              InternalAddMethod(Result, ItemName, Method.Address, MethodKind, Invokable, False);
            end;
          end;
        end;
      end;

      // skip parameters
      Ptr := @PProcedureSignature(Ptr).ParamCount;
      j := Ptr^;
      Inc(Ptr);
      while (j <> 0) do
      begin
        Ptr := GetTail(PProcedureParam(Ptr).Name);
        Ptr := SkipAttributes(Ptr);
        Dec(j);
      end;

      Ptr := SkipAttributes(Ptr);
    end;
    {$endif}
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddInterface(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaInterfaceInfo;
const
  STR_INTERFACE: array[0..9] of Byte = (9, Ord('I'), Ord('n'), Ord('t'), Ord('e'),
    Ord('r'), Ord('f'), Ord('a'), Ord('c'), Ord('e'));
var
  Ptr: PByte;
  IsFunc: Boolean;
  Count, i, j: Integer;
  MetaTypePtr, InvokablePtr, MethodPtr: __luapointer;
  Parent: PLuaInterfaceInfo;
  ParentTypeInfo: PTypeInfo;
  ItemName: LuaString;
  LuaItemName: __luaname;
  MethodEntry: PIntfMethodEntry;
  Item: PLuaDictionaryItem;
  Method: ^TLuaNamespaceMethod;
  VmtOffset: NativeInt;
  Frame: TLuaNativeFrame;
  ChildFrame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(Frame, PShortString(@STR_INTERFACE), Critical);
  try
    // check type info
    if (TypeInfo = nil) then
    begin
      RegisterError('TypeInfo not defined');
      Exit;
    end;
    if (TypeInfo.Kind <> tkInterface) or (IsReferenceMethodType(TypeInfo)) then
    begin
      unpack_lua_string(FStringBuffer.Lua, PShortString(@TypeInfo.Name)^);
      GetTypeKindName(FStringBuffer.Default, TypeInfo.Kind);
      RegisterErrorFmt('type "%s" is not interface type (%s)',
        [FStringBuffer.Lua, FStringBuffer.Default]);
      Exit;
    end;

    // try to find
    MetaTypePtr := TLuaDictionary(FMetaTypes).FindValue(TypeInfo);
    if (MetaTypePtr <> LUA_POINTER_INVALID) then
    begin
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(MetaTypePtr);
      Exit;
    end;

    // name
    NativeFrameRename(PShortString(@TypeInfo.Name));

    // parent
    Parent := nil;
    ParentTypeInfo := GetTypeInfo(GetTypeData(TypeInfo).IntfParent);
    if (Assigned(ParentTypeInfo)) then
      Parent := InternalAddInterface(ParentTypeInfo, Critical);

    // name, check existing
    unpack_lua_string(ItemName, PShortString(@TypeInfo.Name)^);
    LuaItemName := TLuaNames(FNames).Add(ItemName);
    if (TLuaDictionary(FMetaTypes).Find(LuaItemName) <> nil) then
    begin
      Result := Parent;
      Exit;
    end;

    // global metatype, inherits name space, base VMT offset
    Result := Pointer(InternalAddMetaType(mtInterface, ItemName, TypeInfo, SizeOf(Pointer)));
    Result.Parent := LUA_POINTER_INVALID;
    if (Assigned(Parent)) then
    begin
      Result.Count := Parent.Count;
      Result.Parent := Parent.Ptr;
      TLuaDictionary(Result.FNameSpace).Assign(TLuaDictionary(Parent.FNameSpace));
      VmtOffset := Parent.Count * SizeOf(Pointer);
    end else
    begin
      TLuaDictionary(Result.FNameSpace).Assign(TLuaDictionary(FStdInterfaceNameSpace));
      VmtOffset := 0;
    end;

    // method table
    Ptr := GetTail(GetTypeData(TypeInfo).IntfUnit);
    Inc(Result.Count, PWord(Ptr)^);
    Inc(Ptr, SizeOf(Word));
    Count := PWord(Ptr)^;
    Inc(Ptr, SizeOf(Word));
    if (Count = 0) or (Count = $ffff) then
      Exit;
    for i := 1 to Count do
    begin
      MethodEntry := Pointer(Ptr);
      NativeFrameEnter(ChildFrame, PShortString(@MethodEntry.Name), False);
      try
        InvokablePtr := TLuaInvokableBuilder(FInvokableBuilder).BuildIntfMethod(Result, MethodEntry);
      finally
        NativeFrameLeave;
      end;

      // InternalAddMethod
      if (InvokablePtr <> LUA_POINTER_INVALID) then
      begin
        LuaItemName := TLuaNames(FNames).Add(PShortString(@MethodEntry.Name)^);
        Item := TLuaDictionary(Result.FNameSpace).InternalFind(LuaItemName, True);
        if (Item.Value <> LUA_POINTER_INVALID) then
        begin
          Method := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Item.Value);
          Method.Invokable := InvokablePtr;
        end else
        begin
          MethodPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaNamespaceMethod));
          Item.Value := MethodPtr or NAMESPACE_FLAG_PROC;
          Method := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(MethodPtr);

          Method.ItemName := LuaItemName;
          Method.Address := Pointer(NativeInt(PROPSLOT_VIRTUAL) or VmtOffset);
          Method.Kind := mkInstance;
          Method.Mode := mmInvokable;
          Method.ScriptInstance := True;
          Method.Invokable := InvokablePtr;
          Method.Func := alloc_luafunction(Method);
        end;
      end;

      // skip method
      Inc(VmtOffset, SizeOf(Pointer));
      Ptr := GetTail(MethodEntry.Name);
      IsFunc := (Ptr^ = 1);
      Inc(Ptr);
      Inc(Ptr);
      j := Ptr^;
      Inc(Ptr);
      while (j <> 0) do
      begin
        Inc(Ptr);
        Ptr := GetTail(Ptr^);
        Ptr := GetTail(Ptr^);
        Inc(Ptr, SizeOf(Pointer));
        Ptr := SkipAttributes(Ptr);

        Dec(j);
      end;
      if (IsFunc) then
      begin
        Ptr := GetTail(Ptr^);
        Inc(Ptr, SizeOf(Pointer));
      end;
      Ptr := SkipAttributes(Ptr);
    end;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddArray(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaArrayInfo;
const
  STR_ARRAY: array[0..5] of Byte = (5, Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'));
var
  Frame: TLuaNativeFrame;
begin
  NativeFrameEnter(Frame, PShortString(@STR_ARRAY), Critical);
  try
    Result := nil{ToDo};
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddArrayEx(const Identifier, ItemTypeInfo: Pointer;
  const ABounds: array of Integer; const Critical: Boolean): PLuaArrayInfo;
const
  STR_ARRAY: array[0..5] of Byte = (5, Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'));
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
  Frame: TLuaNativeFrame;
  NameBuffer: ShortString;
begin
  Result := nil;
  NativeFrameEnter(Frame, PShortString(@STR_ARRAY), Critical);
  try
    // identifier: name or typeinfo
    begin
      if (NativeUInt(Identifier) <= $FFFF) then
      begin
        RegisterError('array identifier not defined');
        Exit;
      end;
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
        RegisterError('array identifier is invalid');
        Exit;
      end;
      if (not IsValidIdent(FStringBuffer.Lua)) then
      begin
        RegisterErrorFmt('non-supported array type name ("%s")', [FStringBuffer.Lua]);
        Exit;
      end;
      NativeFrameRename(NameBuffer, FStringBuffer.Lua);
    end;

    // static/dynamic
    IsDynamic := Assigned(ArrayTypeInfo) and (ArrayTypeInfo.Kind = tkDynArray);
    if (IsDynamic <> (Length(ABounds) = 0)) then
    begin
      if (IsDynamic) then
      begin
        RegisterErrorFmt('dynamic array "%s" has no bounds', [FStringBuffer.Lua]);
        Exit;
      end else
      begin
        RegisterErrorFmt('array information of "%s" not defined', [FStringBuffer.Lua]);
        Exit;
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

        RegisterErrorFmt('incorrect ItemTypeInfo of dynamic array "%s"', [FStringBuffer.Lua]);
        Exit;
      end;
    end else
    begin
      Dimention := Length(ABounds);
      if (Dimention and 1 = 1) then
      begin
        RegisterErrorFmt('"%s" registering... bounds size should be even. %d is an incorrect size',
          [FStringBuffer.Lua, Dimention]);
        Exit;
      end;
      Dimention := Dimention shr 1;

      for i := 0 to Dimention-1 do
      if (ABounds[i * 2] > ABounds[i * 2 + 1]) then
      begin
        RegisterErrorFmt('"%s" registering... incorrect bounds: "%d..%d"',
          [FStringBuffer.Lua, ABounds[i * 2], ABounds[i * 2 + 1]]);
        Exit;
      end;
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
        Result.FNameSpace := FStdRecordNameSpace;

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
        begin
          RegisterErrorTypeExists(FStringBuffer.Lua);
          Exit;
        end;

        if (Assigned(ArrayTypeInfo)) then
          TLuaDictionary(FMetaTypes).Add(ArrayTypeInfo, ArrayPtr);
      end;
    end else
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(ArrayPtr);
    end;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddSet(const TypeInfo: PTypeInfo; const Critical: Boolean): PLuaSetInfo;
const
  STR_SET: array[0..3] of Byte = (3, Ord('S'), Ord('e'), Ord('t'));
  MASK_3 = $FF shl 3;
var
  Ptr: __luapointer;
  ItemTypeInfo: PTypeInfo;
  TypeData: PTypeData;
  Low, High: Integer;
  LuaName: __luaname;
  Frame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(Frame, PShortString(@STR_SET), Critical);
  try
    if (NativeUInt(TypeInfo) <= $FFFF) then
    begin
      RegisterError('TypeInfo of set not defined');
      Exit;
    end;

    if (PTypeInfo(TypeInfo).Kind <> tkSet) then
    begin
      GetTypeKindName(FStringBuffer.Default, PTypeInfo(TypeInfo).Kind);
      RegisterErrorFmt('TypeInfo of set is invalid: TypeKind = %s', [FStringBuffer.Default]);
      Exit;
    end;

    Ptr := TLuaDictionary(FMetaTypes).FindValue(TypeInfo);
    if (Ptr = LUA_POINTER_INVALID) then
    begin
      ItemTypeInfo := Pointer(GetTypeData(TypeInfo).CompType);
      {$ifNdef FPC}
      if (Assigned(ItemTypeInfo)) then
      begin
        ItemTypeInfo := PPointer(ItemTypeInfo)^;
      end;
      {$endif}
      if (not Assigned(ItemTypeInfo)) then
      begin
        Exit; // ToDo?
      end;

      TypeData := GetTypeData(ItemTypeInfo);
      Low := TypeData.MinValue;
      High := TypeData.MaxValue;

      unpack_lua_string(FStringBuffer.Lua, PShortString(@PTypeInfo(TypeInfo).Name)^);
      LuaName := TLuaNames(FNames).Add(FStringBuffer.Lua);
      Ptr := TLuaDictionary(FMetaTypes).FindValue(LuaName);
      if (Ptr = LUA_POINTER_INVALID) then
      begin
        NativeFrameRename(PShortString(@PTypeInfo(TypeInfo).Name));
        Result := Pointer(InternalAddMetaType(mtSet, FStringBuffer.Lua, TypeInfo, 0{fill later}));
        Result.FNameSpace := FStdSetNameSpace;

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

        if (ItemTypeInfo.Kind = tkEnumeration) and (not IsBooleanType(ItemTypeInfo)) then
        begin
          InternalAutoRegister(ItemTypeInfo);
        end;
      end else
      begin
        Result := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
        if (Result.Kind <> mtSet) or (Result.Low <> Low) or (Result.High <> High) then
        begin
          RegisterErrorTypeExists(FStringBuffer.Lua);
          Exit;
        end;

        TLuaDictionary(FMetaTypes).Add(TypeInfo, Ptr);
      end;
    end else
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
    end;
  finally
    NativeFrameLeave;
  end;
end;

procedure TLua.InternalAddEnum(const TypeInfo: PTypeInfo; const Critical: Boolean);
const
  STR_ENUMERATION: array[0..11] of Byte = (11, Ord('E'), Ord('n'), Ord('u'), Ord('m'),
    Ord('e'), Ord('r'), Ord('a'), Ord('t'), Ord('i'), Ord('o'), Ord('n'));
var
  i, VMin, VMax, Ref: Integer;
  Ptr: PByte;
  LuaName: __luaname;
  Frame: TLuaNativeFrame;
begin
  Self.NativeFrameEnter(Frame, PShortString(@STR_ENUMERATION), Critical);
  try
    if (NativeUInt(TypeInfo) <= $FFFF) then
    begin
      Self.RegisterError('TypeInfo not defined');
      Exit;
    end;

    if (TypeInfo.Kind <> tkEnumeration) or (IsBooleanType(TypeInfo)) then
    begin
      Self.unpack_lua_string(Self.FStringBuffer.Lua, PShortString(@TypeInfo.Name)^);
      GetTypeKindName(Self.FStringBuffer.Default, TypeInfo.Kind);
      Self.RegisterErrorFmt('type "%s" (kind: %s) is not enumeration',
        [Self.FStringBuffer.Lua, Self.FStringBuffer.Default]);
      Exit;
    end;

    // check fake (enumeration) storage
    if (Assigned(TLuaDictionary(Self.FGlobalEntities).InternalFind(TypeInfo, False))) then
      Exit;

    // frame
    Self.NativeFrameRename(PShortString(@TypeInfo.Name));

    // each enumeration value
    with GetTypeData(TypeInfo)^ do
    begin
      VMin := MinValue;
      VMax := MaxValue;
      Ptr := Pointer(@GetTypeData(BaseType^)^.NameList);

      for i := VMin to VMax do
      begin
        LuaName := TLuaNames(Self.FNames).Add(PShortString(Ptr)^);
        Ref := PLuaGlobalEntity(Self.InternalAddGlobal(Ord(gkConst), LuaName)).Ref;
        lua_pushinteger(Self.Handle, i);
        Self.global_fill_value(Ref);

        Ptr := GetTail(Ptr^);
      end;
    end;

    // fake (enumeration) storage
    TLuaDictionary(Self.FGlobalEntities).Add(TypeInfo, LUA_POINTER_INVALID);
  finally
    Self.NativeFrameLeave;
  end;
end;

function TLua.InternalInvokableBuilderEnter: __TLuaInvokableBuilder;
var
  LBuilder: ^TLuaInvokableBuilder;
begin
  Result := FInvokableBuilder;
  FillChar(FInvokableBuilder, SizeOf(FInvokableBuilder), #0);
  LBuilder := @TLuaInvokableBuilder(FInvokableBuilder);
  LBuilder.FLua := Self;
end;

procedure TLua.InternalInvokableBuilderLeave(const AValue: __TLuaInvokableBuilder);
var
  LBuilder: ^TLuaInvokableBuilder;
begin
  LBuilder := @TLuaInvokableBuilder(FInvokableBuilder);
  LBuilder.Clear;
  FInvokableBuilder := AValue;
end;

function TLua.InternalAutoRegister(const TypeInfo: PTypeInfo; const UseRtti: Boolean;
  const Critical: Boolean): Pointer{PLuaMetaType/PLuaClosureType};
label
  invokable;
var
  Ptr: __luapointer;
  LClass: TClass;
  LDictionary: ^TLuaDictionary;
  LClosure: ^TLuaClosureType;
  LInvokableBuilder: __TLuaInvokableBuilder;
//  Frame: TLuaNativeFrame;
begin
//  NativeFrameEnter(Frame, PShortString(@));
//  try
    Ptr := LUA_POINTER_INVALID;

    case TypeInfo.Kind of
      tkEnumeration:
      begin
        if (not IsBooleanType(TypeInfo)) then
          InternalAddEnum(TypeInfo, Critical);
      end;
      //? tkVariant:
      tkMethod{$ifdef EXTENDEDRTTI}, tkProcedure{$endif}:
      begin
      invokable:
        LDictionary := @TLuaDictionary(FClosureTypes);
        Ptr := LDictionary.FindValue(TypeInfo);
        if (Ptr = LUA_POINTER_INVALID) then
        begin
          Ptr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaClosureType));
          LClosure := TLuaMemoryHeap(FMemoryHeap).Unpack(Ptr);
          LClosure.Name := PShortString(@TypeInfo.Name);
          LInvokableBuilder := InternalInvokableBuilderEnter;
          try
            LClosure.Invokable := InternalBuildInvokable(nil, TypeInfo, TLuaMethodKind($ff), Critical);
          finally
            InternalInvokableBuilderLeave(LInvokableBuilder);
          end;
          LDictionary.Add(TypeInfo, Ptr);
        end;
      end;
      tkSet, tkClass, tkArray, tkRecord{$ifdef FPC}, tkObject{$endif},
      tkInterface, tkDynArray{$ifdef EXTENDEDRTTI}, tkClassRef{$endif}:
      begin
        LDictionary := @TLuaDictionary(FMetaTypes);
        Ptr := LDictionary.FindValue(TypeInfo);

        if (Ptr = LUA_POINTER_INVALID) then
        begin
          if (TypeInfo.Kind = tkInterface) and (IsReferenceMethodType(TypeInfo)) then
            goto invokable;

          LInvokableBuilder := InternalInvokableBuilderEnter;
          try
            case TypeInfo.Kind of
              tkClass:
              begin
                LClass := GetTypeData(TypeInfo).ClassType;
                Ptr := LDictionary.FindValue(Pointer(LClass));
                if (Ptr = LUA_POINTER_INVALID) then
                begin
                  Ptr := InternalAddClass(LClass, UseRtti, Critical).Ptr;
                end else
                begin
                  LDictionary.Add(TypeInfo, Ptr);
                end;
              end;
              {$ifdef EXTENDEDRTTI}
              tkClassRef:
              begin
                LClass := GetTypeData(GetTypeInfo(GetTypeData(TypeInfo).InstanceType)).ClassType;
                Ptr := LDictionary.FindValue(Pointer(LClass));
                if (Ptr = LUA_POINTER_INVALID) then
                  Ptr := InternalAddClass(LClass, UseRtti, Critical).Ptr;

                LDictionary.Add(TypeInfo, Ptr);
              end;
              {$endif}
              tkSet:
              begin
                // todo
                Ptr := InternalAddSet(TypeInfo, Critical).Ptr;
              end;
              tkArray:
              begin
                // todo
                Ptr := InternalAddArray(TypeInfo, Critical).Ptr;
              end;
              tkDynArray:
              begin
                // todo
                Ptr := InternalAddArray(TypeInfo, Critical).Ptr;
              end;
              tkRecord{$ifdef FPC}, tkObject{$endif}:
              begin
                // todo
                Ptr := InternalAddRecord(TypeInfo, Critical).Ptr;
              end;
              tkInterface:
              begin
                // todo
                Ptr := InternalAddInterface(TypeInfo, Critical).Ptr;
              end;
            end;
          finally
            InternalInvokableBuilderLeave(LInvokableBuilder);
          end;
        end;
      end;
    end;

    // done
    Result := nil;
    if (Ptr <> LUA_POINTER_INVALID) then
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
//  finally
//    NativeFrameLeave;
//  end;
end;

function TLua.InternalBuildInvokable(const MetaType: PLuaMetaType; const TypeInfo: PTypeInfo;
  const MethodKind: TLuaMethodKind; const Critical: Boolean): __luapointer;
var
  Builder: ^TLuaInvokableBuilder;
var
  Frame: TLuaNativeFrame;
begin
  NativeFrameEnter(Frame, PShortString(@TypeInfo.Name), Critical);
  try
    Result := LUA_POINTER_INVALID;
    Builder := @TLuaInvokableBuilder(FInvokableBuilder);

    if (Assigned(TypeInfo)) then
    case TypeInfo.Kind of
      tkMethod:
      begin
        Result := Builder.BuildMethod(MetaType, TypeInfo, MethodKind);
      end;
      {$ifdef EXTENDEDRTTI}
      tkProcedure:
      begin
        Result := Builder.BuildProcedure(MetaType, TypeInfo, MethodKind);
      end;
      tkInterface:
      begin
        if (IsReferenceMethodType(TypeInfo)) then
          Result := Builder.BuildReferenceMethod(MetaType, TypeInfo, MethodKind);
      end;
      {$endif}
    end;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalBuildInvokable(const MetaType: PLuaMetaType; const Name: LuaString;
  const Params: array of TLuaProcParam; const ResultType: PTypeInfo; const IsResultUnsafe: Boolean;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv; const Critical: Boolean): __luapointer;
var
  Builder: ^TLuaInvokableBuilder;
  Frame: TLuaNativeFrame;
  NameBuffer: ShortString;
begin
  NativeFrameEnter(Frame, nil, Critical);
  try
    NativeFrameRename(NameBuffer, Name);
    Builder := @TLuaInvokableBuilder(FInvokableBuilder);
    Result := Builder.BuildCustom(MetaType, Params, ResultType, IsResultUnsafe, MethodKind, CallConv);
  finally
    NativeFrameLeave;
  end;
end;

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

function TLua.__GetUserData(const AMetaType: __luapointer; const CheckAlreadyDestroyed: Boolean): Pointer;
label
  invalid_instance;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
begin
  UserData := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: ;
 LUA_TUSERDATA: begin
                  UserData := lua_touserdata(Handle, 1);
                  if (not Assigned(UserData)) then goto invalid_instance;
                  if (not Assigned(UserData.Instance)) and (CheckAlreadyDestroyed) then
                  begin
                    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
                    Self.InternalErrorFmt('%s instance is already destroyed', [MetaType.FName]);
                  end;
                end;
  else
  invalid_instance:
    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    Self.InternalErrorFmt('%s: invalid self argument', [MetaType.FName]);
  end;

  Result := UserData;
end;

function TLua.__GetSelfInstance(const AMethodName: __luaname): Pointer;
label
  invalid_instance;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
  LName: ^LuaString;
begin
  UserData := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: ;
 LUA_TUSERDATA: begin
                  UserData := lua_touserdata(Handle, 1);
                  if (not Assigned(UserData)) then goto invalid_instance;
                  if (not Assigned(UserData.Instance)) then
                  begin
                    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(UserData.MetaType);
                    Self.InternalErrorFmt('%s instance is already destroyed', [MetaType.FName]);
                  end;
                end;
  else
  invalid_instance:
    LName := @Self.FStringBuffer.Lua;
    Self.unpack_lua_string(LName^, AMethodName);
    Self.InternalErrorFmt('%s: invalid self instance argument', [LName]);
  end;

  Result := UserData.Instance;
end;

function TLua.__GetSelfClass(const AMethodName: __luaname; const AAllowInstance: Boolean): TClass;
label

  invalid_instance;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
  LName: ^LuaString;
begin
  Result := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: begin
                  MetaType := InternalTableToMetaType(1);
                  if (not Assigned(MetaType)) or (MetaType.Kind <> mtClass) then goto invalid_instance;
                  Result := MetaType.F.ClassType;
                end;
 LUA_TUSERDATA: begin
                  if (not AAllowInstance) then goto invalid_instance;
                  UserData := lua_touserdata(Handle, 1);
                  if (not Assigned(UserData)) then goto invalid_instance;
                  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(UserData.MetaType);
                  if (not Assigned(UserData.Instance)) then
                  begin
                    Self.InternalErrorFmt('%s instance is already destroyed', [MetaType.FName]);
                  end;
                  if (MetaType.Kind <> mtClass) then goto invalid_instance;
                  Result := TClass(PPointer(UserData.Instance)^);
                end;
  else
  invalid_instance:
    LName := @Self.FStringBuffer.Lua;
    Self.unpack_lua_string(LName^, AMethodName);
    Self.InternalErrorFmt('%s: invalid self class argument', [LName]);
  end;
end;

function TLua.__InitTableValues(const AUserData: Pointer{PLuaUserData}; const StackIndex: Integer): Integer;
label
  done;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
  Item: PLuaDictionaryItem;
  Ptr: __luapointer;
  Prop: PLuaProperty;
  LuaType: Integer;
  TempUserData: TLuaUserData;

  procedure ErrorKeyValue(const MetaType: PLuaMetaType; const Description: string);
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, -2);
    stack_force_unicode_string(FStringBuffer.UnicodeReserved, -1);

    Error('Failure changing %s.%s to "%s": %s', [MetaType.Name,
      FStringBuffer.Unicode, FStringBuffer.UnicodeReserved, Description]);
  end;

  procedure ErrorProperty(const MetaType: PLuaMetaType; const Prop: PLuaProperty; const Description: string);
  const
    PROPERTY_KINDS: array[0..3] of string = ('property', 'class property', 'field', 'class field');
  var
    Kind: Integer;
  begin
    Kind := Ord(Prop.IsField) * 2 + Ord(Prop.IsStatic);
    ErrorKeyValue(MetaType, PROPERTY_KINDS[Kind] + ' ' + Description);
  end;

begin
  UserData := AUserData;
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(UserData.MetaType);
  if (not (MetaType.F.Kind in [mtClass, mtRecord])) then
  begin
    Error('%s instance can not be initialized by a table', [MetaType.Name]);
    goto done;
  end;
  if (UserData.Constant) then
  begin
    Error('"%s" instance is a constant', [MetaType.Name]);
  end;

  // for each
  lua_pushnil(Handle);
  while (lua_next(Handle, StackIndex) <> 0) do
  begin
    if (lua_type(Handle, -2) <> LUA_TSTRING) then
    begin
      ErrorKeyValue(MetaType, 'invalid key');
      goto done;
    end;

    Item := TLuaDictionary(MetaType.FNameSpace).InternalFind(lua_tolstring(Handle, -2, nil), False);
    if (Assigned(Item)) then
    begin
      Ptr := Item.Value;
      if (Ptr >= 0) then
      begin
        if (Ptr and NAMESPACE_FLAG_PROC <> 0) then
        begin
          ErrorKeyValue(MetaType, 'method can not be changed');
          goto done;
        end;

        Ptr := Ptr and NAMESPACE_FLAGS_CLEAR;
        Prop := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr);
        if (Prop.F.Options and PROP_SLOTSETTER_MASK = 0{Prop.SetterMode = pmNone}) then
        begin
          ErrorProperty(MetaType, Prop, 'is a constant');
          goto done;
        end;

        if (Prop.F.Kind <> vkObject) and (Prop.F.Options and PROP_SLOTSETTER_MASK = 0{Prop.SetterMode = pmNone}) then
        begin
          ErrorProperty(MetaType, Prop, 'is read only');
          goto done;
        end;

        if (Prop.Options and PROP_COMPLEX_MODE <> 0{Prop.IsComplex}) then
        begin
          ErrorProperty(MetaType, Prop, 'is complicated to initialize');
          goto done;
        end;

        LuaType := lua_type(Handle, -1);
        if (LuaType = LUA_TTABLE) and (InternalTableToMetaType(-1) = nil) then
        begin
          // sub table initialization
          if (not (Prop.F.Kind in [vkObject, vkRecord])) then
          begin
            ErrorProperty(MetaType, Prop, 'can not be initialized by a table');
            goto done;
          end;

          begin
            if (Prop.F.Options and PROP_SLOTGETTER_MASK = 0{Prop.GetterMode = pmNone}) then
            begin
              ErrorProperty(MetaType, Prop, 'is write only');
              goto done;
            end;

            if (Prop.F.Options and PROP_SLOTGETTER_MASK <> Cardinal(pmField){Prop.GetterMode <> pmField}) or
              (not {Prop.SetterMode in [pmNone, pmField]}
               (TLuaPropertyMode((Prop.F.Options shr PROP_SLOTSETTER_SHIFT) and PROP_SLOT_MASK) in [pmNone, pmField])
              ) then
            begin
              ErrorProperty(MetaType, Prop, 'is not a field');
              goto done;
            end;
          end;

          TempUserData.Instance := Pointer(NativeUInt(UserData.Instance) + Prop.Getter);
          PInteger(@TempUserData.Kind)^ := 0;
          TempUserData.MetaType := Prop.MetaType.Ptr;
          if (Prop.F.Kind = vkObject) then
          begin
            TempUserData.Instance := PPointer(TempUserData.Instance)^;
            if (not Assigned(TempUserData.Instance)) then
            begin
              ErrorProperty(MetaType, Prop, 'is nil');
              goto done;
            end;
          end;
          __InitTableValues(@TempUserData, lua_gettop(Handle));
        end else
        begin
          // set property value
          FParamBuffer.UserData := UserData;
          FParamBuffer.Prop := Prop;
          __newindex(UserData.MetaType, True);
        end;
      end else
      begin
        // standard item
        stack_lua_string(FStringBuffer.Lua, -2);
        ErrorKeyValue(MetaType, 'standard item can not be changed');
        goto done;
      end;
    end else
    begin
      ErrorKeyValue(MetaType, 'item not found');
      goto done;
    end;

    // next iteration
    lua_settop(Handle, - 1 - 1);
  end;

done:
  Result := 0;
end;

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
  ScriptAssert('"%s" instance is a constant', [ClassInfo._ClassName]);

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
      if (not(PropertyInfo.Base.Kind in [vkObject, vkRecord])) then ThrowKeyValue('Incompatible types');

      // если поле/свойство нечитабельное
      if (PropertyInfo.read_mode = MODE_NONE_USE) then ThrowFieldProp('is writeonly');

      // если свойство по геттеру
      if (PropertyInfo.read_mode < 0) then ThrowFieldProp('is difficult to initialize, because it has a getter function');

      // подготовка данных для рекурсивного вызова
      pinteger(@recursive_userdata.kind)^ := 0; // сразу все поля
      if (PropertyInfo.Base.Kind = vkObject) then
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

function _SetContainsWrapper(Dest, Left, Right: PByte; ASize: Integer): Boolean;
begin
  Result := _SetLe(Right, Left, ASize);
end;

function TLua.__ExecuteSetMethod(const AUserData: Pointer{PLuaUserData}; const FirstArgIndex, ArgsCount, Method: Integer): Integer;
label
  invalid_argument;
const
  METHOD_NAMES: array[0..3] of string = ('.Include() method', '.Exclude() method',
    '.Contains() method', ' constructor');
var
  i, LuaType: Integer;
  Ret: Boolean;
  Instance: Pointer;
  SetInfo: PLuaSetInfo;
  BitProc: function(Value: PByte; Bit: Integer): Boolean;
  SetProc: function(Dest, Left, Right: PByte; ASize: Integer): Boolean;
  ValueUserData: PLuaUserData;
begin
  SetInfo := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(PLuaUserData(AUserData).MetaType);
  case Method of
    1:
    begin
      // exclude
      BitProc := Pointer(@SetBitExclude);
      SetProc := Pointer(@SetsDifference);
    end;
    2:
    begin
      // contains
      BitProc := SetBitContains;
      SetProc := _SetContainsWrapper;
    end;
  else
    // include, constructor
    BitProc := Pointer(@SetBitInclude);
    SetProc := Pointer(@SetsUnion);
  end;

  Ret := False;
  Instance := PLuaUserData(AUserData).Instance;
  for i := FirstArgIndex to (FirstArgIndex + ArgsCount - 1) do
  begin
    LuaType := lua_type(Handle, i);
    case LuaType of
      LUA_TNUMBER:
      begin
        FTempBuffer.D := lua_tonumber(Handle, i);
        if (NumberToInteger(FTempBuffer.D, FTempBuffer.I)) and
          (FTempBuffer.I >= SetInfo.FLow) and (FTempBuffer.I <= SetInfo.FHigh) then
        begin
          Ret := BitProc(Instance, FTempBuffer.I - SetInfo.FCorrection);
        end else
        begin
          goto invalid_argument;
        end;
      end;
      LUA_TUSERDATA:
      begin
        ValueUserData := lua_touserdata(Handle, i);
        if (Assigned(ValueUserData)) and (Assigned(ValueUserData.Instance)) and
          (ValueUserData.MetaType = SetInfo.Ptr) then
        begin
          Ret := SetProc(Instance, Instance, ValueUserData.Instance, SetInfo.Size);
        end else
        begin
          goto invalid_argument;
        end;
      end;
    else
    invalid_argument:
      stack_force_unicode_string(FStringBuffer.Unicode, i);
      Error('Failure %s%s: invalid argument %d (%s)', [SetInfo.Name, METHOD_NAMES[Method],
        i, FStringBuffer.Unicode]);
      Ret := False;
    end;

    if (Method = 2) and (not Ret) then Break;
  end;

  if (Method = 2) then
  begin
    lua_pushboolean(Handle, Ret);
    Result := 1;
  end else
  begin
    Result := 0;
  end;
end;

function TLua.__len(const AMetaType: __luapointer): Integer;
var
  MetaType: PLuaMetaType;
begin
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  lua_pushinteger(Handle, MetaType.F.Size);
  Result := 1;
end;

function TLua.__tostring(const AMetaType: __luapointer): Integer;
label
  push_type_name, invalid_user_data;
type
  TObjectToString = procedure(Instance: Pointer; var Result: string);
  TSetDescription = procedure(MetaType, Instance: Pointer; var Result: LuaString);
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
  (*
  procedure FillPropertyDescription();
  begin
    with userdata^, FBufferArg do
    str_data := __arrayindex_description('NON FINISHED COMPLEX PROPERTY: ' + PropertyInfo.PropertyName,
               'xxx', integer(array_params) and $F - 1, array_params shr 4);
  end;

  procedure FillArrayDescription();
  begin
    with userdata^, FBufferArg do
    str_data := __arrayindex_description(ArrayInfo.Name, 'xxx', integer(array_params) and $F - 1, array_params shr 4);
  end;
  *)
begin
  UserData := __GetUserData(AMetaType, False);
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);

  if (UserData = nil) then
  begin
    goto push_type_name;
  end else
  if (UserData.Instance = nil) then
  begin
    goto invalid_user_data;
  end else
  case Byte(UserData.Kind) of
    Ord(mtComplexProperty):
    begin
      push_lua_string('ikComplexProperty: ToDo');
    end;
    Ord(mtClass):
    begin
      {$ifdef EXTENDEDRTTI}
      TObjectToString(PPointer(PNativeUInt(UserData.Instance)^ + NativeUInt(vmtToString))^)(UserData.Instance, FStringBuffer.Default);
      push_unicode_string(FStringBuffer.Default);
      {$else}
      push_lua_string(InternalGetClassInfo(TClass(PPointer(UserData.Instance)^)).FName);
      {$endif}
    end;
    Ord(mtRecord), Ord(mtInterface), Ord(mtArray):
    begin
    push_type_name:
      push_lua_string(MetaType.FName);
    end;
    Ord(mtSet):
    begin
      TSetDescription(@TLuaSetInfo.Description)(MetaType, UserData.Instance, FStringBuffer.Lua);
      push_lua_string(FStringBuffer.Lua);
    end;
  else
  invalid_user_data:
    push_lua_string('Invalid user data');
  end;

  Result := 1;
end;

function TLua.__gc(const AMetaType: __luapointer): Integer;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
begin
  UserData := __GetUserData(AMetaType, False);

  if (Assigned(UserData)) and (UserData.Owner) and
    (Assigned(UserData.Instance)) then
  begin
    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    if (MetaType.F.Kind = mtClass) or (MetaType.F.Managed) then
      MetaType.Dispose(UserData.Instance);
  end;

  Result := 0;
end;

function TLua.__call(const AMetaType: __luapointer; const ParamMode: Boolean): Integer;
label
  construct, class_record_construct, done;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
  ArgsCount: Integer;
  FirstArgIndex{, i}: Integer;
begin
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  ArgsCount := lua_gettop(Handle) - 1;
  if (ParamMode) then
  begin
    Inc(ArgsCount);
    goto construct;
  end;

  UserData := __GetUserData(AMetaType);
  if (Assigned(UserData)) then
  begin
    // initialization by a table
    if (ArgsCount = 1) and (lua_type(Handle, -1) = LUA_TTABLE) and
      (InternalTableToMetaType(-1) = nil) then
    begin
      if (MetaType.F.Kind in [mtClass, mtRecord]) then
      begin
        Result := __InitTableValues(UserData, -1);
      end else
      begin
        Error('%s instance can not be initialized by a table.', [MetaType.Name]);
        Result := 0;
      end;
    end else
    begin
      Error('Incorrect usage of %s constructor.', [MetaType.Name]);
      Result := 0;
    end;
    Exit;
  end;

  // constructor (default gc-destroyed instance)
construct:
  UserData := push_metatype_instance(MetaType, nil^, not ParamMode, False);
  FirstArgIndex := 2 - Ord(ParamMode);
  case MetaType.F.Kind of
    mtClass:
    begin
      UserData.Instance := Pointer(PLuaClassInfo(MetaType).F.ClassType.NewInstance);
      goto class_record_construct;
    end;
    mtInterface:
    begin
     // ToDo
    end;
    mtRecord:
    begin
    class_record_construct:
      // ToDo
      if (ArgsCount = 1) and (lua_type(Handle, -1) = LUA_TTABLE) and
        (InternalTableToMetaType(-1) = nil) then
      begin
        __InitTableValues(UserData, -1);
        goto done;
      end;
      // ToDo
    end;
    mtArray:
    begin
     // ToDo
    end;
    mtSet:
    begin
      __ExecuteSetMethod(UserData, FirstArgIndex, ArgsCount, 3{constructor});
    end;
  end;

done:
  Result := 1;
end;

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
  userdata := push_metatype_instance(ClassInfo, not __create, nil);
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

    // TLuaVariable - особый случай, для него используется свой конструктор (из стека)
    if (ClassInfo._ClassIndex = TLUA_REFERENCE_CLASS_INDEX) then
    begin
      if (ArgsCount < 0) or (ArgsCount > 1) then ScriptAssert('Wrong arguments count(%d) of TLuaVariable() constructor', [ArgsCount]);

      // проинициализировать ссылку
      if (ArgsCount = 0) then lua_pushnil(Handle) else lua_pushvalue(Handle, -2);
      TLuaVariable(userdata.instance).Initialize(Self);

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

function TLua.__operator(const AMetaType: __luapointer; const Kind: Cardinal{Byte}): Integer;
label
  instance_destroyed, failure_operation, push_compare, done;
const
  RECORD_OPERATORS: array[OPERATOR_NEG..OPERATOR_LESS_EQUAL] of TLuaOperator = (
    loNeg, loAdd, loSub, loMul, loDiv, loMod, loPow, loCompare, loCompare, loCompare);
  OPERATION_NAMES: array[TLuaOperator] of string = ('neg', '+', '-', '*', '/', '%', '^', 'compare');
var
  MetaType: PLuaMetaType;
  Inverted: Boolean;
  Compare: Integer;
  LeftLuaType, RightLuaType, TempLuaType: Integer;
  LeftUserData, RightUserData, TempUserData, TargetUserData: PLuaUserData;
  RecordOperator: TLuaOperator;
  RecordOperatorCallback: TLuaOperatorCallback;
begin
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);

  // check operator arguments: user data
  LeftLuaType := lua_type(Handle, 1);
  RightLuaType := LUA_TNONE;
  if (Kind <> OPERATOR_NEG) then RightLuaType := lua_type(Handle, 2);
  LeftUserData := nil;
  RightUserData := nil;
  if (LeftLuaType = LUA_TUSERDATA) then
  begin
    LeftUserData := lua_touserdata(Handle, 1);
    if (not Assigned(LeftUserData)) or (not Assigned(LeftUserData.Instance)) then
    begin
      MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LeftUserData.MetaType);
    instance_destroyed:
      // __GetUserData like error
      InternalErrorFmt('%s instance is already destroyed', [MetaType.FName]);
      goto done;
    end else
    begin
      if (LeftUserData.MetaType <> MetaType.Ptr) then
      begin
      failure_operation:
        stack_force_unicode_string(FStringBuffer.Unicode, 1);
        if (RightLuaType = LUA_TNONE) then
        begin
          InternalErrorFmt('Failure operation "%s" with "%s" operand', [
            OPERATION_NAMES[RECORD_OPERATORS[Kind]], FStringBuffer.Unicode]);
        end else
        begin
          stack_force_unicode_string(FStringBuffer.UnicodeReserved, 2);
          InternalErrorFmt('Failure operation "%s" with "%s" and "%s" operands', [
            OPERATION_NAMES[RECORD_OPERATORS[Kind]], FStringBuffer.Unicode, FStringBuffer.UnicodeReserved]);
        end;
        goto done;
      end;
    end;
  end;
  if (RightLuaType = LUA_TUSERDATA) then
  begin
    RightUserData := lua_touserdata(Handle, 2);
    if (not Assigned(RightUserData)) or (not Assigned(RightUserData.Instance)) then
    begin
      MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(RightUserData.MetaType);
      goto instance_destroyed;
    end else
    begin
      if (RightUserData.MetaType <> MetaType.Ptr) then
        goto failure_operation;
    end;
  end;

  // check operator arguments: compatibility
  Inverted := (not Assigned(LeftUserData));
  if (Inverted) then
  begin
    TempLuaType := LeftLuaType;
    {.$HINTS OFF LeftLuaType := RightLuaType; }
    RightLuaType := TempLuaType;
    TempUserData := LeftUserData;
    LeftUserData := RightUserData;
    RightUserData := TempUserData;

    if (not (Kind in [OPERATOR_DIV..OPERATOR_POW])) then
      goto failure_operation;
  end;

  case MetaType.F.Kind of
    mtRecord:
    begin
      RecordOperator := RECORD_OPERATORS[Kind];
      RecordOperatorCallback := PLuaRecordInfo(MetaType).FOperatorCallback;
      if (RecordOperator in PLuaRecordInfo(MetaType).FOperators) and Assigned(RecordOperatorCallback) then
      begin
        if (Kind = OPERATOR_NEG) then
        begin
          TargetUserData := push_metatype_instance(MetaType, nil^, True, False);
          RecordOperatorCallback(TargetUserData.Instance^, LeftUserData.Instance^,
            LeftUserData.Instance^, loNeg);
        end else
        case RightLuaType of
          LUA_TUSERDATA:
          begin
            case (Kind) of
              OPERATOR_ADD, OPERATOR_SUB:
              begin
                TargetUserData := push_metatype_instance(MetaType, nil^, True, False);
                RecordOperatorCallback(TargetUserData.Instance^, LeftUserData.Instance^,
                  RightUserData.Instance^, RecordOperator);
              end;
              OPERATOR_EQUAL..OPERATOR_LESS_EQUAL:
              begin
                //compare
                RecordOperatorCallback(Compare, LeftUserData.Instance^,
                  RightUserData.Instance^, RecordOperator);
                goto push_compare;
              end;
            else
              goto failure_operation;
            end;
          end;
          LUA_TNUMBER:
          begin
            if (not (Kind in [OPERATOR_MUL..OPERATOR_POW])) then goto failure_operation;
            FTempBuffer.D := lua_tonumber(Handle, 2 - Ord(Inverted));
            TargetUserData := push_metatype_instance(MetaType, nil^, True, False);
            RecordOperatorCallback(TargetUserData.Instance^, LeftUserData.Instance^,
              FTempBuffer.D, RecordOperator);
          end;
        else
          goto failure_operation;
        end;
      end else
      begin
        // Error: unsupported operator
      end;
    end;
    mtArray{..}:
    begin
      // array concatenation
      // ToDo
    end;
    mtSet:
    begin
      if (Kind = OPERATOR_NEG) then
      begin
        TargetUserData := push_metatype_instance(MetaType, nil^, True, False);
        SetInvert(TargetUserData.Instance, LeftUserData.Instance, PLuaSetInfo(MetaType).FAndMasks, PLuaSetInfo(MetaType).FRealSize);
      end else
      case RightLuaType of
        LUA_TUSERDATA:
        begin
          if (RECORD_OPERATORS[Kind] = loCompare) then
          begin
            Compare := SetsCompare(LeftUserData.Instance, RightUserData.Instance, PLuaSetInfo(MetaType).Size, Kind = OPERATOR_LESS_EQUAL);
          push_compare:
            if (Inverted) then Compare := -Compare;
            case (Kind) of
              OPERATOR_EQUAL: lua_pushboolean(Handle, (Compare = 0));
              OPERATOR_LESS: lua_pushboolean(Handle, (Compare < 0));
              OPERATOR_LESS_EQUAL: lua_pushboolean(Handle, (Compare <= 0));
            end;
          end else
          begin
            TargetUserData := push_metatype_instance(MetaType, nil^, True, False);
            case (Kind) of
              OPERATOR_ADD: SetsUnion(TargetUserData.Instance, LeftUserData.Instance, RightUserData.Instance, MetaType.Size);
              OPERATOR_SUB: SetsDifference(TargetUserData.Instance, LeftUserData.Instance, RightUserData.Instance, MetaType.Size);
              OPERATOR_MUL: SetsIntersection(TargetUserData.Instance, LeftUserData.Instance, RightUserData.Instance, MetaType.Size);
            end;
          end;
        end;
        LUA_TNUMBER:
        begin
          FTempBuffer.D := lua_tonumber(Handle, 2 - Ord(Inverted));
          if (not NumberToInteger(FTempBuffer.D, FTempBuffer.I)) or
            (FTempBuffer.I < PLuaSetInfo(MetaType).FLow) or (FTempBuffer.I > PLuaSetInfo(MetaType).FHigh) then
            goto failure_operation;

          Dec(FTempBuffer.I, PLuaSetInfo(MetaType).FCorrection);
          TargetUserData := push_metatype_instance(MetaType, nil^, True, False);
          case (Kind) of
            OPERATOR_ADD: SetsUnion(TargetUserData.Instance, LeftUserData.Instance, FTempBuffer.I, MetaType.Size);
            OPERATOR_SUB: SetsDifference(TargetUserData.Instance, LeftUserData.Instance, FTempBuffer.I, MetaType.Size, Inverted);
            OPERATOR_MUL: SetsIntersection(TargetUserData.Instance, LeftUserData.Instance, FTempBuffer.I, MetaType.Size);
          end;
        end;
      else
        goto failure_operation;
      end;
    end;
  end;

done:
  Result := 1;
end;

function TLua.__closuregc: Integer;
var
  LClosure: Pointer{PLuaClosureMethod};
begin
  // reference
  LClosure := lua_touserdata(Handle, 1);
  if (Assigned(LClosure)) then
  begin
    Inc(NativeUInt(LClosure), SizeOf(Pointer));
    IInterface(PLuaClosureMethod(LClosure).Instance) := nil;
  end;

  Result := 0;
end;

function TLua.push_standard(const MetaType: PLuaMetaType; const StdIndex: Cardinal): Boolean;
label
  ret_false, is_filled_zero, done;
var
  Ptr: __luapointer;
  ArrayIndex, Count: Integer;
  UserData: PLuaUserData;

  procedure ErrorConst;
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    Error('%s() method can''t be called, because %s instance is a constant', [FStringBuffer.Lua, MetaType.Name]);
  end;
begin
  UserData := __GetUserData(MetaType.Ptr, Cardinal(StdIndex - STD_IS_EMPTY) > 1{STD_IS_EMPTY/STD_FREE});

  case (StdIndex) of
    STD_TYPE:
    begin
      global_push_value(MetaType.Ref);
      goto done;
    end;
    STD_TYPE_NAME:
    begin
      push_lua_string(MetaType.FName);
      goto done;
    end;
    STD_TYPE_PARENT:
    begin
      Ptr := LUA_POINTER_INVALID;
      case MetaType.F.Kind of
        mtClass:
        begin
          Ptr := PLuaClassInfo(MetaType).Parent;
        end;
        mtInterface:
        begin
          Ptr := PLuaInterfaceInfo(MetaType).Parent;
        end;
      end;

      if (Ptr = LUA_POINTER_INVALID) then
      begin
        lua_pushnil(Handle);
      end else
      begin
        Ptr := PLuaMetaType({$ifdef LARGEINT}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Ptr)).Ptr;
        global_push_value(Ptr);
      end;

      goto done;
    end;
    STD_INHERITS_FROM:
    begin
      {ToDo} lua_pushnil(FHandle);
      //push_closure(Pointer(@ID_INHERITSFROM), Ord(ckClassInstance), Ord(cmInternal),
      //  MetaType, @TLua.__InheritsFrom, 1 + (1 shl 16));
      goto done;
    end;
    STD_ASSIGN:
    begin
      if (Assigned(UserData)) then
      begin
        if (MetaType.F.Kind <> mtClass) and (UserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_ASSIGN), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__Assign, 1 + (1 shl 16));
        goto done;
      end;
    end;
    STD_IS_REF:
    begin
      lua_pushboolean(Handle, (Assigned(UserData)) and
        (UserData.Kind <> mtClass) and (not UserData.Owner));
      goto done;
    end;
    STD_IS_CONST:
    begin
      lua_pushboolean(Handle, (Assigned(UserData)) and (UserData.Constant));
      goto done;
    end;
    STD_IS_CLASS:
    begin
      lua_pushboolean(Handle, MetaType.F.Kind = mtClass);
      goto done;
    end;
    STD_IS_INTERFACE:
    begin
      lua_pushboolean(Handle, MetaType.F.Kind = mtInterface);
      goto done;
    end;
    STD_IS_RECORD:
    begin
      lua_pushboolean(Handle, MetaType.F.Kind = mtRecord);
      goto done;
    end;
    STD_IS_SET:
    begin
      lua_pushboolean(Handle, MetaType.F.Kind = mtSet);
      goto done;
    end;
    STD_IS_EMPTY:
    begin
      if (not Assigned(UserData)) or (UserData.Instance = nil) then
      begin
        lua_pushboolean(Handle, True);
      end else
      case MetaType.F.Kind of
        mtClass, mtInterface:
        begin
        ret_false:
          lua_pushboolean(Handle, False);
        end;
        mtArray:
        begin
          if (not PLuaArrayInfo(MetaType).IsDynamic) then goto is_filled_zero;
          lua_pushboolean(Handle, PPointer(UserData.Instance)^ = nil);
        end
      else
      is_filled_zero:
        lua_pushboolean(Handle, IsMemoryFilledZero(UserData.Instance, MetaType.F.Size));
      end;

      goto done;
    end;
    STD_FREE:
    begin
      if (Assigned(UserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_FREE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__Free, 0 + (0 shl 16));
        goto done;
      end;
    end;
    STD_CREATE:
    begin
      if (not Assigned(UserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_CREATE), Ord(ckClassInstance), Ord(cmInternal),
        //  MetaType, @TLua.__ClassCreate, {???} 0 + ($ffff shl 16));
        goto done;
      end;
    end;
    STD_ADDREF:
    begin
      if (Assigned(UserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_ADDREF), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__IntfAddRef, 0 + (0 shl 16));
        goto done;
      end;
    end;
    STD_RELEASE:
    begin
      if (Assigned(UserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_RELEASE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__IntfRelease, 0 + (0 shl 16));
        goto done;
      end;
    end;
    STD_LENGTH:
    begin
      with PLuaArrayInfo(MetaType)^ do
      if (IsDynamic) then
      begin
        Count := 0;
        if (Assigned(UserData)) then Count := DynArrayLength(PPointer(UserData.Instance)^);
      end else
      begin
        ArrayIndex := 0;
        if (Assigned(UserData)) then ArrayIndex := (UserData.Counts and $F) * 2;
        Count := Integer(PMemoryItems(FBounds).Cardinals[ArrayIndex + 1]) -
          Integer(PMemoryItems(FBounds).Cardinals[ArrayIndex]) + 1;
      end;

      lua_pushinteger(Handle, Count);
      goto done;
    end;
    STD_RESIZE:
    begin
      if (Assigned(UserData)) and (PLuaArrayInfo(MetaType).IsDynamic) then
      begin
        if (UserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_RESIZE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__DynArrayResize, 1 + (PLuaArrayInfo(MetaType).Dimention shl 16));
        goto done;
      end;
    end;
    STD_LOW:
    begin
      if (MetaType.F.Kind = mtArray) then
      begin
        with PLuaArrayInfo(MetaType)^ do
        begin
          Count := 0;
          if (not IsDynamic) then
          begin
            if (not Assigned(UserData)) then
            begin
              Count := FBounds^;
            end else
            begin
              ArrayIndex := (UserData.Counts and $F) * 2;
              Count := Integer(PMemoryItems(FBounds).Cardinals[ArrayIndex]);
            end;
          end;

          lua_pushinteger(Handle, Count);
          goto done;
        end;
      end else
      begin
        // mtSet
        lua_pushinteger(Handle, PLuaSetInfo(MetaType).FLow);
        goto done;
      end;
    end;
    STD_HIGH:
    begin
      if (MetaType.F.Kind = mtArray) then
      begin
        with PLuaArrayInfo(MetaType)^ do
        begin
          if (IsDynamic) then
          begin
            Count := -1;
            if (Assigned(UserData)) then Count := DynArrayLength(PPointer(UserData.Instance)^) - 1;
          end else
          begin
            ArrayIndex := 0;
            if (Assigned(UserData)) then
            begin
              ArrayIndex := (UserData.Counts and $F) * 2;
            end;

            Inc(ArrayIndex);
            Count := Integer(PMemoryItems(FBounds).Cardinals[ArrayIndex]);
          end;

          lua_pushinteger(Handle, Count);
          goto done;
        end;
      end else
      begin
        // mtSet
        lua_pushinteger(Handle, PLuaSetInfo(MetaType).FHigh);
        goto done;
      end;
    end;
    STD_INCLUDE:
    begin
      if (Assigned(UserData)) then
      begin
        if (MetaType.F.Kind = mtArray) then
        begin
          if (UserData.Counts and $F + 1 = UserData.Counts shr 4) then
          begin
            if (UserData.Constant) then ErrorConst;
            {ToDo} lua_pushnil(FHandle);
            //push_closure(Pointer(@ID_INCLUDE), Ord(ckInstance), Ord(cmInternal),
            //  UserData, @TLua.__DynArrayInclude, 1 + ($ffff shl 16));
            goto done;
          end;
        end else
        begin
          // mtSet
          if (UserData.Constant) then ErrorConst;
          {ToDo} lua_pushnil(FHandle);
          //push_closure(Pointer(@ID_INCLUDE), Ord(ckInstance), Ord(cmInternal),
          //  UserData, @TLua.__SetInclude, 1 + ($ffff shl 16));
          goto done;
        end;
      end;
    end;
    STD_EXCLUDE:
    begin
      if (Assigned(UserData)) then
      begin
        if (UserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_EXCLUDE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__SetExclude, 1 + ($ffff shl 16));
        goto done;
      end;
    end;
    STD_CONTAINS:
    begin
      if (Assigned(UserData)) then
      begin
        if (UserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_CONTAINS), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__SetContains, 1 + ($ffff shl 16));
        goto done;
      end;
    end;
   (* STD_VALUE:
    begin
      lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaVariable(UserData.Instance).Index);
      goto done;
    end;   *)
  end;

  Result := False;
  Exit;
done:
  Result := True;
end;

function TLua.set_standard(const MetaType: PLuaMetaType; const StdIndex: Cardinal): Boolean;
label
  invalid_array_length, done;
var
  UserData: PLuaUserData;

  procedure ErrorConst;
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    Error('%s can''t be called, because %s instance is a constant', [FStringBuffer.Lua, MetaType.Name]);
  end;
begin
  case (StdIndex) of
    STD_LENGTH:
    begin
      if (MetaType.F.Kind = mtArray) and (PLuaArrayInfo(MetaType).FIsDynamic) then
      begin
        UserData := __GetUserData(MetaType.Ptr);
        if (Assigned(UserData)) then
        begin
          if (not UserData.Constant) then
          begin
            if (lua_type(Handle, 3) = LUA_TNUMBER) then
            begin
              FTempBuffer.D := lua_tonumber(Handle, 3);
              if (not NumberToInteger(FTempBuffer.D, FTempBuffer.I)) or
               (FTempBuffer.I < 0) then goto invalid_array_length;

              {$ifdef LARGEINT}
              TPoint(FTempBuffer.I64).Y := 0;
              {$endif}
              DynArraySetLength(PPointer(UserData.Instance)^,
                PTypeInfo(PLuaArrayInfo(MetaType).FMultiplies[UserData.Counts and $F]),
                1, @FTempBuffer.N);
              goto done;
            end else
            begin
            invalid_array_length:
              stack_force_unicode_string(FStringBuffer.Unicode, 3);
              Error('Invalid %s.Length value "%s"', [MetaType.Name, FStringBuffer.Unicode]);
            end;
          end else
          begin
            Error('Length can''t be changed, because %s instance is a constant', [MetaType.Name]);
          end;
        end;
      end;
    end;
   (* STD_VALUE:
    begin
      lua_pushvalue(Handle, 3);
      lua_rawseti(Handle, LUA_REGISTRYINDEX, TLuaVariable(userdata.instance).Index);
      goto done;
    end;   *)
  end;

  Result := False;
  Exit;
done:
  Result := True;
end;

procedure TLua.push_prop_tempgetter(const AInstance: Pointer; const AProp: Pointer);
label
  push_value;
type
  TProcStd = procedure(const P1, P2, P3: NativeInt);
var
  LProp: ^TLuaProperty;
  LUserData: ^TLuaUserData;
  LProc: Pointer;
  LMode: Cardinal;
  LType: Integer;
  P1, P2, P3: NativeInt;
begin
  LProp := AProp;
  try
    // result arguments (P3)
    P3 := 0;
    case LProp.Kind of
      vkVariant:
      begin
        P3 := NativeInt(@FTempBuffer.V);
      end;
      {TVarData: ToDo}
      vkLuaArg:
      begin
        P3 := NativeInt(@FTempBuffer.A);
      end;
      {$ifdef AUTOREFCOUNT}
      vkObject:
      begin
        P3 := NativeInt(@FTempBuffer.O);
      end;
      {$endif}
      vkInterface,
      vkClosure:
      begin
        P3 := NativeInt(@FTempBuffer.Intf);
      end;
      vkRecord, vkArray, vkSet:
      begin
        LUserData := push_metatype_instance(LProp.MetaType, nil^, True, False);
        P3 := NativeInt(LUserData.Instance);
      end;
    end;

    // instance, proc, index, unused arguments
    P1 := NativeInt(AInstance);
    LProc := Pointer(LProp.Getter);
    LMode := LProp.Options and PROP_SLOTGETTER_MASK;
    if (LMode >= Cardinal(pmVirtualProc)) then
    begin
      Inc(NativeUInt(LProc), PNativeUInt(P1)^);
      LProc := PPointer(LProc)^;
      Dec(LMode, 3);
    end;
    if (LMode = Cardinal(pmStaticProcIndex)) then
    begin
      P2 := Cardinal(LProp.Index);
    end else
    begin
      // pmStaticProc
      P2 := P3;
    end;
    if (LProp.Options and PROP_STATIC_MODE <> 0) then
    begin
      P1 := P2;
      P2 := P3;
    end;

    // call
    begin
      TProcStd(LProc)(P1, P2, P3);
    end;

    // push
    case LProp.Kind of
      vkArray:
      begin
        if (PLuaArrayInfo(LProp.MetaType).IsDynamic) then goto push_value;
      end;
      vkRecord, vkSet: ;
    else
    push_value:
      LProp.GetValue(PPointer(P3)^, Self);
    end;
  finally
    case LProp.Kind of
      vkVariant:
      begin
        LType := PVarData(@FTempBuffer.V).VType;
        if (LType and varDeepData <> 0) then
        case LType of
          varBoolean, varUnknown+1..$15{varUInt64}: ;
        else
          System.VarClear(FTempBuffer.V);
        end;
      end;
      {TVarData: ToDo}
      vkLuaArg:
      begin
        FTempBuffer.A.Empty := True;
      end;
      {$ifdef AUTOREFCOUNT}
      vkObject:
      begin
        if (FTempBuffer.O <> nil) then
        begin
          FTempBuffer.O := nil;
        end;
      end;
      {$endif}
      vkInterface,
      vkClosure:
      begin
        if (FTempBuffer.Intf <> nil) then
        begin
          FTempBuffer.Intf := nil;
        end;
      end;
    end;
  end;
end;

function TLua.call_prop_getter(const AInstance: Pointer; const AProp: Pointer): Pointer;
type
  TProcStd = function(const P1, P2, P3: NativeInt): NativeInt;
  TProcInt64 = function(const P1, P2, P3: NativeInt): Int64;
  TProcSingle = function(const P1, P2, P3: NativeInt): Single;
  TProcDouble = function(const P1, P2, P3: NativeInt): Double;
  TProcComp = function(const P1, P2, P3: NativeInt): Comp;
  TProcCurrency = function(const P1, P2, P3: NativeInt): Currency;
var
  LProp: ^TLuaProperty;
  LProc: Pointer;
  LMode: Cardinal;
  P1, P2, P3: NativeInt;
begin
  // result arguments (P3)
  LProp := AProp;
  P3 := 0;
  case LProp.Kind of
    vkString:
    begin
      case LProp.StringType of
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
    vkClosure:
    begin
      if (LProp.ClosureMode = mmMethod) then
      begin
        {$ifdef WEAKINSTREF}
        if (LProp.Options and OPTIONS_GETTER_MASK <> 0{ExtendedGetter <> lrDefault}) then
        begin
          P3 := NativeInt(@FTempBuffer.MUnsafe);
        end else
        {$endif}
        begin
          P3 := NativeInt(@FTempBuffer.M);
        end;
      end;
    end;
  end;

  // instance, proc, index, unused arguments
  P1 := NativeInt(AInstance);
  LProc := Pointer(LProp.Getter);
  LMode := LProp.Options and PROP_SLOT_MASK;
  if (LMode >= Cardinal(pmVirtualProc)) then
  begin
    Inc(NativeUInt(LProc), PNativeUInt(P1)^);
    LProc := PPointer(LProc)^;
    Dec(LMode, 3);
  end;
  if (LMode = Cardinal(pmStaticProcIndex)) then
  begin
    P2 := Cardinal(LProp.Index);
  end else
  begin
    // pmStaticProc
    P2 := P3;
  end;
  if (LProp.Options and PROP_STATIC_MODE <> 0) then
  begin
    P1 := P2;
    P2 := P3;
  end;

  // call
  if (P3 = 0) then
  begin
    case LProp.Kind of
      vkInt64:
      begin
        Result := @FTempBuffer.I64;
        PInt64(Result)^ := TProcInt64(LProc)(P1, P2, P3);
      end;
      vkFloat:
      begin
        Result := @FTempBuffer.D;
        case LProp.FloatType of
          ftSingle: PDouble(Result)^ := TProcSingle(LProc)(P1, P2, P3);
            ftComp: PDouble(Result)^ := TProcComp(LProc)(P1, P2, P3);
            ftCurr: PDouble(Result)^ := TProcCurrency(LProc)(P1, P2, P3);
        else
          // ftDouble, ftExtended
          PDouble(Result)^ := TProcDouble(LProc)(P1, P2, P3);
        end;
      end;
    else
      Result := @FTempBuffer.N;
      PNativeInt(Result)^ := TProcStd(LProc)(P1, P2, P3);
    end;
  end else
  begin
    Result := Pointer(P3);
    TProcStd(LProc)(P1, P2, P3);
  end;
end;

procedure TLua.call_prop_setter(const AInstance: Pointer; const AProp: Pointer; const AValue: NativeInt);
label
  specific_call, call_standard;
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
  LProp: ^TLuaProperty;
  LProc: Pointer;
  LMode, LArgsCount: Cardinal;
  P1, P2, P3: NativeInt;
begin
  // basic
  LProp := AProp;
  P3 := AValue;

  // instance, proc, index, unused arguments
  P1 := NativeInt(AInstance);
  LProc := Pointer(LProp.Setter);
  LMode := (LProp.Options shr PROP_SLOTSETTER_SHIFT) and PROP_SLOT_MASK;
  if (LMode >= Cardinal(pmVirtualProc)) then
  begin
    Inc(NativeUInt(LProc), PNativeUInt(P1)^);
    LProc := PPointer(LProc)^;
    Dec(LMode, 3);
  end;
  LArgsCount := 3;
  if (LMode = Cardinal(pmStaticProcIndex)) then
  begin
    P2 := Cardinal(LProp.Index);
  end else
  begin
    // pmStaticProc
    P2 := P3;
    Dec(LArgsCount);
  end;
  if (LProp.Options and PROP_STATIC_MODE <> 0) then
  begin
    P1 := P2;
    P2 := P3;
    Dec(LArgsCount);
  end;

  // call specific(Int64, Float) or standard
  case LProp.Kind of
    {$ifdef CPUX86}
    vkClosure:
    begin
      if (LProp.F.ClosureMode = mmMethod) then
      begin
        LMode := Succ(Ord(High(TFloatType))){Int64};
        goto specific_call;
      end;
      goto call_standard;
    end;
    {$endif}
    vkInt64:
    begin
      LMode := Succ(Ord(High(TFloatType))){Int64};
      goto specific_call;
    end;
    vkFloat:
    begin
      LMode := Ord(LProp.FloatType);
    specific_call:
      case (LMode * 3 + (LArgsCount - 1)) of
        // ftSingle
        0: TProcSingle1(LProc)(PSingle(P3)^);
        1: TProcSingle2(LProc)(P1, PSingle(P3)^);
        2: TProcSingle3(LProc)(P1, P2, PSingle(P3)^);
        // ftDouble
        3: TProcDouble1(LProc)(PDouble(P3)^);
        4: TProcDouble2(LProc)(P1, PDouble(P3)^);
        5: TProcDouble3(LProc)(P1, P2, PDouble(P3)^);
        // ftExtended
        6: TProcExtended1(LProc)(PExtended(P3)^);
        7: TProcExtended2(LProc)(P1, PExtended(P3)^);
        8: TProcExtended3(LProc)(P1, P2, PExtended(P3)^);
        // ftComp
        9: TProcComp1(LProc)(PComp(P3)^);
       10: TProcComp2(LProc)(P1, PComp(P3)^);
       11: TProcComp3(LProc)(P1, P2, PComp(P3)^);
        // tCurr
       12: TProcCurrency1(LProc)(PCurrency(P3)^);
       13: TProcCurrency2(LProc)(P1, PCurrency(P3)^);
       14: TProcCurrency3(LProc)(P1, P2, PCurrency(P3)^);
        // vkInt64
       15: TProcInt641(LProc)(PInt64(P3)^);
       16: TProcInt642(LProc)(P1, PInt64(P3)^);
       17: TProcInt643(LProc)(P1, P2, PInt64(P3)^);
      end;
    end;
  else
  call_standard:
    TProcStd(LProc)(P1, P2, P3);
  end;
end;

function TLua.__index(const AMetaType: __luapointer): Integer;
label
  not_found, property_push, field_push, done;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaClassInfo{PLuaMetaType};
  LLuaType: Integer;
  LLuaName: __luaname;
  LItem: PLuaDictionaryItem;
  LPtr: __luapointer;
  LProp: ^TLuaProperty;
  LProc: ^TLuaNamespaceMethod;
  LOptions: Integer;
  LInstance, LValue: Pointer;
begin
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);

  // argument
  LLuaName := nil;
  LLuaType := lua_type(Handle, 2);
  if (LLuaType = LUA_TSTRING) then LLuaName := lua_tolstring(Handle, 2, nil);

  // try to find
  if (Assigned(LLuaName)) then
  begin
    LItem := TLuaDictionary(LMetaType.FNameSpace).InternalFind(LLuaName, False);
    if (Assigned(LItem)) then
    begin
      LPtr := LItem.Value;
      if (LPtr >= 0) then
      begin
        LUserData := __GetUserData(AMetaType);

        if (LPtr and NAMESPACE_FLAG_PROC = 0) then
        begin
          // property
          LPtr := LPtr and NAMESPACE_FLAGS_CLEAR;
          LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
          goto property_push;
        end else
        begin
          // method
          LPtr := LPtr and NAMESPACE_FLAGS_CLEAR;
          LProc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
          if (LProc.Kind in [mkInstance, {mkConstructor, }mkDestructor]) then
          begin
            if (not Assigned(LUserData)) then
            begin
              stack_lua_string(FStringBuffer.Lua, 2);
              Self.Error('%s.%s method is not a class method', [LMetaType.FName, FStringBuffer.Lua]);
              goto done;
            end;
          end;

          push_luafunction(LProc.Func);
          goto done;
        end;
      end else
      begin
        // standard item
        if push_standard(LMetaType, LPtr and $7f) then
        begin
          goto done;
        end else
        begin
          LUserData := __GetUserData(AMetaType);
          goto not_found;
        end;
      end;
    end;
  end;

  // try look default property
  LUserData := __GetUserData(AMetaType);
  if (LMetaType.F.Kind = mtClass) and (LMetaType.DefaultProperty <> LUA_POINTER_INVALID) then
  begin
    LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LMetaType.DefaultProperty);
    // ToDo
    goto property_push;
  end;

  // not found
not_found:
  stack_force_unicode_string(FStringBuffer.Unicode, 2);
  Self.InternalErrorFmt('"%s" item not found in %s %s', [FStringBuffer.Unicode, LMetaType.FName,
    INSTANCE_MODES[Byte(LUserData = nil) + (Byte(LUserData = nil) and Byte(LMetaType.F.Kind = mtClass))]]);
  goto done;

property_push:
  LOptions := LProp.Options;
  if (Assigned(LUserData)) >= (LOptions and PROP_STATIC_MODE = 0) then
  begin
    LInstance := nil;
    if (LOptions and PROP_STATIC_MODE = 0) then LInstance := LUserData.Instance;
    if (LOptions and PROP_COMPLEX_MODE = 0) then
    begin
      if (LOptions and $ff00{TLuaVariableKind} < (Ord(vkVariant) shl 8)) or
        (LOptions and OPTIONS_GETTER_TEMPORARY_MODE = 0) then
      begin
        LOptions := LOptions and PROP_SLOTGETTER_MASK;
        if (LOptions = Ord(pmField)) then
        begin
          // field
          LValue := Pointer(LProp.Getter);
          Inc(NativeUInt(LValue), NativeUInt(LInstance));
        field_push:
          LProp.GetValue(LValue^, Self);
        end else
        if (LOptions <> Ord(pmNone)) then
        begin
          // getter
          LValue := call_prop_getter(LInstance, LProp);
          if (LProp.Kind <> vkFloat) then
          begin
            goto field_push;
          end else
          begin
            lua_pushnumber(Handle, PDouble(LValue)^);
          end;
        end else
        begin
          stack_lua_string(FStringBuffer.Lua, 2);
          Self.InternalErrorFmt('%s.%s property is a writeonly property', [LMetaType.FName, FStringBuffer.Lua]);
        end;
      end else
      begin
        // temporary getter
        push_prop_tempgetter(LInstance, LProp);
      end;
    end else
    begin
      // complex property
      // ToDo
    end;
  end else
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    Self.InternalErrorFmt('%s.%s property is not a class property', [LMetaType.FName, FStringBuffer.Lua]);
  end;

done:
  Result := 1;
end;

function TLua.__newindex(const AMetaType: __luapointer; const AParamMode: Boolean): Integer;
label
  failure_set_method, property_set, failure_set, done;
const
  PROP_FIELD = Ord(pmField) shl PROP_SLOTSETTER_SHIFT;
var
  LMetaType: ^TLuaClassInfo;
  LUserData: PLuaUserData;
  LLuaType: Integer;
  LLuaName: __luaname;
  LItem: PLuaDictionaryItem;
  LPtr: __luapointer;
  LProp: ^TLuaProperty;
  LFlags: Integer;
  LSetter, LValue: NativeInt;

  function CallVariantSetter(const ASelf: TLua; const AInstance: Pointer; const AProp: PLuaProperty): Boolean; far;
  begin
    Result := AProp.SetValue(ASelf.FTempBuffer.V, ASelf, -1);
    if (Result) then
    try
      ASelf.call_prop_setter(AInstance, AProp, NativeInt(@ASelf.FTempBuffer.V));
    finally
      VarClear(ASelf.FTempBuffer.V);
    end;
  end;

  function CallIntfSetter(const ASelf: TLua; const AInstance: Pointer; const AProp: PLuaProperty): Boolean; far;
  begin
    Result := AProp.SetValue(ASelf.FTempBuffer.Intf, ASelf, -1);
    if (Result) then
    try
      ASelf.call_prop_setter(AInstance, AProp, NativeInt(ASelf.FTempBuffer.Intf));
    finally
      ASelf.FTempBuffer.Intf := nil;
    end;
  end;

  procedure CallEmptyMetaTypeSetter(const ASelf: TLua; AMetaType: PLuaMetaType;
    const AInstance: Pointer; const AProp: Pointer); far;
  var
    LSize: Integer;
    LBuffer: array[0..511] of Byte;
    LPtr: Pointer;
  begin
    LSize := (AMetaType.F.Size + SizeOf(Pointer)) and -SizeOf(Pointer);

    if (LSize <= SizeOf(LBuffer)) then
    begin
      if (LSize > SizeOf(Pointer)) then
      begin
        FillChar(LBuffer, LSize, #0);
        ASelf.call_prop_setter(AInstance, AProp, NativeInt(@LBuffer));
      end else
      begin
        ASelf.call_prop_setter(AInstance, AProp, 0);
      end;
    end else
    begin
      GetMem(LPtr, LSize);
      try
        FillChar(LPtr^, LSize, #0);
        ASelf.call_prop_setter(AInstance, AProp, NativeInt(LPtr));
      finally
        FreeMem(LPtr);
      end;
    end;
  end;

  function AssignMetaType(const ASelf: TLua; const AInstance: Pointer; const AProp: PLuaProperty): Boolean; far;
  label
    failure, done;
  var
    LUserData: PLuaUserData;
    LMetaType: PLuaMetaType;
    LBuffer: array[0..SizeOf(Pointer) - 1] of Byte;
  begin
    LMetaType := AProp.MetaType;
    case lua_type(ASelf.Handle, -1) of
      LUA_TNIL:
      begin
        CallEmptyMetaTypeSetter(ASelf, LMetaType, AInstance, AProp);
      end;
      LUA_TUSERDATA:
      begin
        LUserData := lua_touserdata(ASelf.Handle, -1);
        if (LUserData = nil) or (LUserData.Instance = nil) or
          (LUserData.MetaType <> {$ifdef SMALLINT}__luapointer(LMetaType){$else}LMetaType.Ptr{$endif}) then
          goto failure;

        if (LMetaType.F.Size <= SizeOf(Pointer)) then
        begin
          PNativeInt(@LBuffer)^ := 0;
          System.Move(LUserData.Instance^, LBuffer, LMetaType.F.Size);
          ASelf.call_prop_setter(AInstance, AProp, NativeInt(LBuffer));
        end else
        begin
          ASelf.call_prop_setter(AInstance, AProp, NativeInt(LUserData.Instance));
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
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  if (AParamMode) then
  begin
    LUserData := FParamBuffer.UserData;
    LProp := FParamBuffer.Prop;
    goto property_set;
  end;

  // argument
  LLuaName := nil;
  LLuaType := lua_type(Handle, 2);
  if (LLuaType = LUA_TSTRING) then LLuaName := lua_tolstring(Handle, 2, nil);

  // try to find
  if (Assigned(LLuaName)) then
  begin
    LItem := TLuaDictionary(LMetaType.FNameSpace).InternalFind(LLuaName, False);
    if (Assigned(LItem)) then
    begin
      LPtr := LItem.Value;
      if (LPtr >= 0) then
      begin
        LUserData := __GetUserData(AMetaType);

        if (LPtr and NAMESPACE_FLAG_PROC = 0) then
        begin
          // property
          LPtr := LPtr and NAMESPACE_FLAGS_CLEAR;
          LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
          goto property_set;
        end else
        begin
          // method
        failure_set_method:
          stack_lua_string(FStringBuffer.Lua, 2);
          Self.InternalErrorFmt('%s.%s method can not be changed', [LMetaType.FName, FStringBuffer.Lua]);
          goto done;
        end;
      end else
      begin
        // standard item
        if set_standard(LMetaType, LPtr and $7f) then
        begin
          goto done;
        end else
        begin
          if (LPtr and NAMESPACE_STD_PROC <> 0) then
          begin
            goto failure_set_method;
          end else
          begin
            stack_lua_string(FStringBuffer.Lua, 2);
            Self.InternalErrorFmt('%s.%s property can not be changed', [LMetaType.FName, FStringBuffer.Lua]);
            goto done;
          end;
        end;
      end;
    end;
  end;

  // try look default property
  LUserData := __GetUserData(AMetaType);
  if (LMetaType.F.Kind = mtClass) and (LMetaType.DefaultProperty <> LUA_POINTER_INVALID) then
  begin
    LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LMetaType.DefaultProperty);
    // ToDo
    goto property_set;
  end;

  // not found
  stack_force_unicode_string(FStringBuffer.Unicode, 2);
  Self.InternalErrorFmt('"%s" not found in %s %s', [FStringBuffer.Unicode, LMetaType.FName,
    INSTANCE_MODES[Byte(LUserData = nil) + (Byte(LUserData = nil) and Byte(LMetaType.F.Kind = mtClass))]]);
  goto done;

property_set:
  LFlags := LProp.Flags;
  if (Assigned(LUserData)) >= (LFlags and PROP_STATIC_MODE = 0) then
  begin
    LSetter := 0;
    if (LFlags and PROP_STATIC_MODE = 0) then LSetter := NativeInt(LUserData.Instance);
    LFlags := LFlags and (PROP_SLOT_MASK shl PROP_SLOTSETTER_SHIFT);
    if (LFlags <> 0) then
    begin
      if (LFlags = PROP_FIELD) then
      begin
        if (not LProp.SetValue(Pointer(NativeUInt(LSetter) + LProp.Setter)^, Self, -1)) then
          goto failure_set;
      end else
      begin
        case LProp.Kind of
          vkBoolean,
          vkInteger,
          vkPointer,
          vkClass:
          begin
            if (not LProp.SetValue(Pointer(@FTempBuffer.N)^, Self, -1)) then goto failure_set;
            LValue := FTempBuffer.N;
          end;
          vkInt64,
          vkFloat:
          begin
            if (not LProp.SetValue(Pointer(@FTempBuffer.E)^, Self, -1)) then goto failure_set;
            LValue := NativeInt(@FTempBuffer.E);
          end;
          vkString:
          begin
            case LProp.StringType of
              {$ifNdef NEXTGEN}
              stShortString:
              begin
                stack_short_string(FStringBuffer.Short, -1, LProp.MaxShortLength);
                LValue := NativeInt(@FStringBuffer.Short);
              end;
              stAnsiString:
              begin
                stack_ansi_string(FStringBuffer.Ansi, -1,
                  {$ifdef INTERNALCODEPAGE}GetTypeData(LProp.TypeInfo).CodePage{$else}0{$endif});
                LValue := PNativeInt(@FStringBuffer.Ansi)^;
              end;
              stWideString:
              begin
                stack_wide_string(FStringBuffer.Wide, -1);
                LValue := PNativeInt(@FStringBuffer.Wide)^;
              end;
              {$endif}
              stUnicodeString:
              begin
                stack_unicode_string(FStringBuffer.Unicode, -1);
                LValue := PNativeInt(@FStringBuffer.Unicode)^;
              end;
            else
              if (not LProp.SetValue(Pointer(@FTempBuffer.N)^, Self, -1)) then goto failure_set;
              LValue := FTempBuffer.N;
            end;
          end;
          vkVariant:
          begin
            if (not CallVariantSetter(Self, Pointer(LSetter), LProp)) then goto failure_set;
            goto done;
          end;
          vkInterface:
          begin
            {ToDo}
            if (not CallIntfSetter(Self, Pointer(LSetter), LProp)) then goto failure_set;
            goto done;
          end;
          vkObject:
          begin
            {ToDo}
            if (not LProp.SetValue(Pointer(@FTempBuffer.N)^, Self, -1)) then
              goto failure_set;
            LValue := FTempBuffer.N;
          end;
          vkClosure:
          begin
            // ToDo
            LValue := 0;
          end;
        else
          // vkRecord, vkArray, vkSet, vkLuaArg
          if (not AssignMetaType(Self, Pointer(LSetter), LProp)) then goto failure_set;
          goto done;
        end;

        call_prop_setter(Pointer(LSetter), LProp, LValue);
      end;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
      Self.InternalErrorFmt('%s.%s property is a readonly property', [LMetaType.FName, FStringBuffer.Lua]);
    end;
  end else
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    InternalErrorFmt('%s.%s property is not a class property', [LMetaType.FName, FStringBuffer.Lua]);
    goto done;
  failure_set:
    stack_lua_string(FStringBuffer.Lua, 2);
    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    Error('Can not change %s.%s %s to %s ("%s")', [LMetaType.FName,
      FStringBuffer.Lua, PROPERTY_MODES[Ord(LMetaType.F.Kind = mtRecord)],
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
  Proc: ^TLuaNamespaceMethod;
begin
  NativeUInt(NativeModify) := {$ifdef LARGEINT}(NativeUInt(ModifyHigh) shl 32) +{$endif} ModifyLow;

  // entity
  if (Assigned(NativeModify)) then
  begin
    Entity := GetGlobalEntity(NativeModify.Name^, False);
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
      Prop.GetValue(Pointer(Prop.Getter)^, Self);
    end;
    gkProc:
    begin
      Proc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(Entity.Ptr);
      push_luafunction(Proc.Func);
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
  if (Entity.Constant) then
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
    ScriptAssert('%s() method can''t be called, because %s instance is a constant', [S, ClassInfo._ClassName]);
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
    {дополнительный множитель} if (ItemKind = vkArray) then ItemsCount := PLuaArrayInfo(ItemInfomation).FItemsCount else ItemsCount := 1;

    // для быстрого определения в userdata-случаях
    case (ItemKind) of
      vkObject: item_class_index := 0;
      vkRecord: item_class_index := PLuaRecordInfo(ItemInfomation).FClassIndex;
       vkArray: item_class_index := integer(ItemInfomation);
         vkSet: item_class_index := integer(ItemInfomation);
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

function TLua.__InheritsFrom(const AMetaType: PLuaMetaType; const ArgsCount: Integer{ = 1}): Integer;
label
  done;
var
  Ret: Boolean;
  Value: PLuaMetaType;
begin
  Value := InternalTableToMetaType(1);
  if (not Assigned(Value)) then
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, 1);
    Error('Invalid meta type value "%s"', [FStringBuffer.Unicode]);
    Result := 0;
    Exit;
  end;

  Ret := (AMetaType = Value);
  if (Ret) then goto done;

  if (AMetaType.F.Kind = Value.F.Kind) then
  case AMetaType.F.Kind of
    mtClass:
    begin
      Ret := PLuaClassInfo(AMetaType).ClassType.InheritsFrom(PLuaClassInfo(Value).ClassType);
    end;
    mtInterface:
    begin
      Ret := PLuaInterfaceInfo(AMetaType).InheritsFrom(PLuaInterfaceInfo(Value));
    end;
  end;

done:
  lua_pushboolean(Handle, Ret);
  Result := 1;
end;

function TLua.__Assign(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer{ = 1}): Integer;
label
  invalid_argument, done;
var
  LuaType: Integer;
  UserData, ValueUserData: PLuaUserData;
  MetaType, ValueMetaType: PLuaMetaType;

  procedure ErrorParameter(const MetaType: PLuaMetaType; const Description: string);
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, 1);
    Error('Failure %s.Assign(%s): %s.', [MetaType.Name, FStringBuffer.Unicode, Description]);
  end;
begin
  LuaType := lua_type(Handle, 1);
  UserData := AUserData;
  MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(UserData.MetaType);

  if (LuaType <> LUA_TUSERDATA) then
  begin
    if (LuaType = LUA_TTABLE) and (InternalTableToMetaType(1) = nil) then
    begin
      __InitTableValues(UserData, 1);
      goto done;
    end else
    begin
      goto invalid_argument;
    end;
  end else
  begin
    ValueUserData := lua_touserdata(Handle, 1);
    if (ValueUserData.Kind = mtComplexProperty) then goto invalid_argument;
    if (not Assigned(ValueUserData.Instance)) then
    begin
      ErrorParameter(MetaType, 'instance is already destroyed');
      goto done;
    end;

    ValueMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(ValueUserData.MetaType);
    if (MetaType.F.Kind <> ValueMetaType.F.Kind) then goto invalid_argument;
  end;

  // check compatibility
  if (MetaType <> ValueMetaType) then
  case MetaType.F.Kind of
    mtClass:
    begin
      if (MetaType.F.ClassType.InheritsFrom(TPersistent)) and (ValueMetaType.F.ClassType.InheritsFrom(TPersistent))  then
      begin
        TPersistent(UserData.Instance).Assign(TPersistent(ValueUserData.Instance));
        goto done;
      end else
      if (not MetaType.F.ClassType.InheritsFrom(ValueMetaType.F.ClassType)) and
        (not ValueMetaType.F.ClassType.InheritsFrom(MetaType.F.ClassType)) then
      begin
        goto invalid_argument;
      end;
    end;
    mtInterface:
    begin
      if (not PLuaInterfaceInfo(ValueMetaType).InheritsFrom(PLuaInterfaceInfo(MetaType))) then
        goto invalid_argument;
    end;
  else
  invalid_argument:
    ErrorParameter(MetaType, 'invalid argument');
    goto done;
  end;

  // assign
  MetaType.Assign(UserData.Instance^, ValueUserData.Instance);

done:
  Result := 0;
end;

function TLua.__Free(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
var
  UserData: PLuaUserData;
  MetaType: PLuaMetaType;
  Instance: Pointer;
begin
  UserData := AUserData;
  Instance := UserData.Instance;
  UserData.Instance := nil;
  if (Assigned(Instance)) then
  begin
    MetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(UserData.MetaType);
    MetaType.Dispose(Instance);
  end;

  Result := 0;
end;

function TLua.__ClassCreate(const AMetaType: PLuaMetaType; const ArgsCount: Integer): Integer;
begin
  Result := __call(AMetaType.Ptr, True);
end;

function TLua.__IntfAddRef(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
var
  UserData: PLuaUserData;
  Count: Integer;
begin
  UserData := AUserData;
  Count := IInterface(UserData.Instance)._AddRef;
  lua_pushinteger(Handle, Count);
  Result := 1;
end;

function TLua.__IntfRelease(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
var
  UserData: PLuaUserData;
  Count: Integer;
begin
  UserData := AUserData;
  Count := IInterface(UserData.Instance)._Release;
  if (Count = 0) then
  begin
    UserData.Instance := nil;
  end;
  lua_pushinteger(Handle, Count);
  Result := 1;
end;

function TLua.__DynArrayInclude(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
begin
  Result := 0{ToDo};
end;

function TLua.__DynArrayResize(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
begin
  Result := 0{ToDo};
end;

function TLua.__SetInclude(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
begin
  Result := __ExecuteSetMethod(AUserData, 1, ArgsCount, 0);
end;

function TLua.__SetExclude(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
begin
  Result := __ExecuteSetMethod(AUserData, 1, ArgsCount, 1);
end;

function TLua.__SetContains(const AUserData: Pointer{PLuaUserData}; const ArgsCount: Integer): Integer;
begin
  Result := __ExecuteSetMethod(AUserData, 1, ArgsCount, 2);
end;

function TLua.UniversalMethodCallback(const AMethod: Pointer{PLuaMethod}; const ArgsCount: Integer): Integer;
begin
  Result := 0;
end;

function TLua.InvokableMethodCallback(const AMethod: Pointer{PLuaMethod}; const AInvokableData: Pointer): Integer;
label
  type_pointer, type_userdata, type_standard, invalid_argument;
const
  META_TYPE_INSTANCE_KINDS = [vkObject, vkInterface, vkRecord, vkArray, vkSet];
var
  i: Integer;
  LMethod: PLuaMethod;
  LInvokable: PLuaInvokable;
  LInvokableData: NativeInt;
  LParam: PLuaInvokableParam;
  LLuaType, LFlags: Integer;
  LPtr, LValue: Pointer;
  LUserData: PLuaUserData;
  LInstance: Pointer;
  LAddress: Pointer;
begin
  LMethod := PLuaMethod(AMethod);
  LInvokable := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LMethod.Invokable);
  LInvokableData := NativeInt(AInvokableData);

  // instance
  if (LInvokable.Instance <> VALUE_NOT_DEFINED) then
  begin
    if (LMethod.Mode = mmInvokable) then
    begin
      // script instance
      // structures?
      case LMethod.Kind of
        mkInstance, mkDestructor: LInstance := __GetSelfInstance(LMethod.ItemName);
        mkClass: LInstance := Pointer(__GetSelfClass(LMethod.ItemName, True));
        mkConstructor:
        begin
          LInstance := Pointer(__GetSelfClass(LMethod.ItemName, False));
          PByte(LInvokableData + LInvokable.ConstructorFlag)^ := $01;
        end;
      else
        LInstance := nil{error};
      end;
    end else
    begin
      // mmMethod, mmReference
      LInstance := PLuaClosureMethod(LMethod).Instance;
    end;

    PPointer(LInvokableData + LInvokable.Instance)^ := LInstance;
  end;

  // parameters
  LParam := @LInvokable.Params[0];
  for i := 1 + Integer(LMethod.ScriptInstance) to Integer(LInvokable.MaxParamCount) + Integer(LMethod.ScriptInstance) do
  begin
    LLuaType := lua_type(Handle, i);
    LFlags := LParam.Flags;
    case LLuaType of
      LUA_TNONE:
      begin
        {ToDo: default params}Break;
      end;
      LUA_TNIL:
      begin
        // nil
        LValue := nil;
        if (LFlags and PARAM_POINTER_MODE <> 0) then goto type_pointer;

        if (LFlags and PARAM_ARRAY_MODE <> 0) then
        begin
          PPointer(LInvokableData + LParam.DataValue)^ := nil;
          Inc(LParam);
          PInteger(LInvokableData + LParam.DataValue)^ := -1;
        end else
        if (LParam.F.Kind in META_TYPE_INSTANCE_KINDS) then
        begin
          lua_remove(Handle, i);
          {$ifdef CPUARM}LUserData :={$endif} push_metatype_instance(LParam.F.MetaType, nil^, True, False);
          lua_insert(Handle, i);
          goto type_userdata;
        end else
        begin
          LPtr := Pointer(LInvokableData + LParam.DataValue);
          if (not LParam.SetValue(LPtr^, Self, i, LLuaType)) then
            goto invalid_argument;
        end;
      end;
      LUA_TLIGHTUSERDATA:
      begin
        // pointer
        if (LFlags and PARAM_POINTER_MODE = 0) then goto invalid_argument;
        LValue := lua_touserdata(Handle, i);
      type_pointer:
        if (LFlags and PARAM_ARRAY_MODE <> 0) then goto invalid_argument;
        if (LFlags and PARAM_REFERENCE_MODE <> 0) then
        begin
          PPointer(PPointer(LInvokableData + LParam.Value)^)^ := LValue;
        end else
        begin
          PPointer(LInvokableData + LParam.Value)^ := LValue;
        end;
      end;
      LUA_TTABLE:
      begin
        // array
        if (Assigned(InternalTableToMetaType(i))) then goto type_standard;
        if (LFlags and PARAM_ARRAY_MODE = 0) then goto invalid_argument;

        {ToDo Initialize Insurance array by table}
      end;
      LUA_TUSERDATA:
      begin
        // meta type instance
        if (LFlags and PARAM_ARRAY_MODE <> 0) then goto invalid_argument;
        if (not (LParam.F.Kind in META_TYPE_INSTANCE_KINDS)) then goto invalid_argument;
        LUserData := lua_touserdata(Handle, i);
        if (not Assigned(LUserData)) and (not Assigned(LUserData.Instance)) then goto invalid_argument;
      type_userdata:

        {ToDo MetaType check}

        {$ifdef CPUARM}
        if (LFlags and PARAM_ARMPARTIAL_MODE <> 0) then
        begin
          LInvokable.FillARMPartialData(LParam, LUserData.Instance, Pointer(LInvokableData));
        end else
        {$endif}
        begin
          LPtr := Pointer(LInvokableData + LParam.DataValue);
          if (not LParam.SetValue(LPtr^, Self, i, LLuaType)) then
            goto invalid_argument;
        end;
      end;
    else
    type_standard:
      if (LFlags and (PARAM_POINTER_MODE or PARAM_ARMPARTIAL_MODE) <> 0) then goto invalid_argument;
      if (LFlags and PARAM_INSURANCE_MODE <> 0) then
      begin
        if (LFlags and PARAM_ARRAY_MODE <> 0) then
        begin
          if (LParam.F.Kind <> vkArray) then goto invalid_argument;

          {ToDo MetaType check}

        end else
        begin
          // PAnsiChar, PWideChar
          LPtr := Pointer(LInvokableData + LParam.InsuranceValue);
          if (not LParam.SetValue(LPtr^, Self, i, LLuaType)) then
            goto invalid_argument;

          LValue := PPointer(LInvokableData + LParam.InsuranceValue)^;
          if (LValue = nil) then LValue := @NULL_CHAR;
          PPointer(LInvokableData + LParam.DataValue)^ := LValue;
        end;
      end else
      begin
        if (LParam.F.Kind in META_TYPE_INSTANCE_KINDS) then
          goto invalid_argument;

        LPtr := Pointer(LInvokableData + LParam.DataValue);
        if (not LParam.SetValue(LPtr^, Self, i, LLuaType)) then
        begin
        invalid_argument:
          if (LMethod.IsClosure) then
          begin
            Self.unpack_lua_string(FStringBuffer.Lua, LMethod.ClosureName^);
          end else
          begin
            Self.unpack_lua_string(FStringBuffer.Lua, LMethod.ItemName);
          end;
          unpack_lua_string(FStringBuffer.LuaReserved, LParam.Name^);
          Self.Error('Invalid %s.%s argument value: "%s" (%s)',
            [FStringBuffer.Lua, FStringBuffer.LuaReserved, FStringBuffer.Unicode, FStringBuffer.Default]);
        end;
      end;
    end;

    Inc(LParam);
  end;

  // invoke address
  LAddress := LMethod.Address;
  if (NativeUInt(LAddress) >= PROPSLOT_VIRTUAL) then
  begin
    LInstance := PPointer(LInvokableData + LInvokable.Instance)^;
    if not (LMethod.Kind in [mkClass, mkConstructor]) then
    begin
      LInstance := PPointer(LInstance)^;
    end;
    LAddress := PPointer(NativeUInt(LInstance) + NativeUInt(LAddress) and PROPSLOT_CLEAR)^;
  end;

  // invoke: function/procedure
  if (LInvokable.ResultMode <> rmNone) then
  begin
    // optional prepare result user data
    if (LInvokable.ResultMode = rmUserData) then
    begin
      LUserData := push_metatype_instance(LInvokable.Result.MetaType, nil^, True, False);
      PPointer(LInvokableData + LInvokable.Result.Value)^ := LUserData.Instance;
    end;

    // invoke
    LInvokable.Invoke(LAddress, Pointer(LInvokableData));

    // optional get value
    if (LInvokable.ResultMode <> rmUserData) then
      LInvokable.Result.GetValue(Pointer(LInvokableData + LInvokable.Result.DataValue)^, Self);
  end else
  begin
    // procedure
    LInvokable.Invoke(LAddress, Pointer(LInvokableData));
  end;

  // done
  Result := Byte(LInvokable.ResultMode <> rmNone);
end;

function TLua.__MethodCallback(const AMethodLow, AMethodHigh: Cardinal): Integer;
label
  invalid_signature, invalid_args_count, invalid_result;
//type
//  TInternalCallback = function(const Lua: Pointer;
//    const AUserDataMetaType: Pointer{PLuaUserData/PLuaMetaType}; const ArgsCount: Integer): Integer;
const
  METHOD_KINDS: array[TLuaMethodKind] of string = (
    'static method', 'method', 'class method', 'constructor', 'destructor', 'class operator');
var
  LMethod: PLuaMethod;
  LFrame: PLuaScriptFrame;
  LArgsCount, LMinArgsCount, LMaxArgsCount: Integer;
  LInvokable: PLuaInvokable;
  LInvokableBuffer: Pointer;
  LName: ^LuaString;
begin
  NativeUInt(LMethod) := {$ifdef LARGEINT}(NativeUInt(AMethodHigh) shl 32) +{$endif} AMethodLow;
  LArgsCount := lua_gettop(Handle);

  if (LMethod.Mode = mmUniversal) then
  begin
    LMinArgsCount := LMethod.MinArgsCount;
    LMaxArgsCount := LMethod.MaxArgsCount;
    if (LArgsCount < LMinArgsCount) or (LArgsCount > LMaxArgsCount) then
      goto invalid_args_count;

   { if (LMethod.Mode = cmInternal) then
    begin
      Result := TInternalCallback(LMethod.Address)(Self, LMethod.Instance, LArgsCount);
    end else  }
    begin
      Result := Self.UniversalMethodCallback(LMethod, LArgsCount);
    end;
    Exit;
  end else
  begin
    if (LMethod.Invokable = LUA_POINTER_INVALID) then
    begin
      goto invalid_signature;
    end;
    LInvokable := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(Self.FMemoryHeap).Unpack{$endif}(LMethod.Invokable);
    LMinArgsCount := LInvokable.MinParamCount + Integer(LMethod.ScriptInstance);
    LMaxArgsCount := LInvokable.MaxParamCount + Integer(LMethod.ScriptInstance);

    if (LArgsCount < LMinArgsCount) or (LArgsCount > LMaxArgsCount) then
      goto invalid_args_count;

    LFrame := PLuaScriptFrame(FArguments);
    if (LInvokable.TotalDataSize > Integer(LFrame.BufferCapacity)) then
    begin
      NativeInt(LFrame.BufferCapacity) := (NativeInt(LInvokable.TotalDataSize) + 127) and -128;
      SetLength(LFrame.Buffer, LFrame.BufferCapacity);
      LInvokableBuffer := Pointer(PLuaScriptFrame(FArguments).Buffer);
    end else
    begin
      LInvokableBuffer := Pointer(LFrame.Buffer);
    end;

    if (LInvokable.InitialCount <> 0) then
      LInvokable.Initialize(LInvokableBuffer);

    if (LInvokable.FinalCount = 0) then
    begin
      Result := Self.InvokableMethodCallback(LMethod, LInvokableBuffer);
    end else
    begin
      try
        Result := Self.InvokableMethodCallback(LMethod, LInvokableBuffer);
      finally
        LInvokable.Finalize(LInvokableBuffer);
      end;
    end;
  end;

  Exit;
invalid_signature:
  LName := LMethod.UnpackName(Self);
  Self.Error('Invoked %s "%s" signature not found',
    [
      METHOD_KINDS[LMethod.Kind], LName^
    ]);
  goto invalid_result;
invalid_args_count:
  LName := LMethod.UnpackName(Self);
  if (LMinArgsCount = LMaxArgsCount) then
  begin
    Self.Error('Invalid arguments count (%d) of %s "%s", reqired: %d',
      [
        LArgsCount, METHOD_KINDS[LMethod.Kind], LName^, {Min/}LMaxArgsCount
      ]);
  end else
  if (LMaxArgsCount = High(Word)) then
  begin
    Self.Error('Invalid arguments count (%d) of %s "%s", reqired: %d..inf',
      [
        LArgsCount, METHOD_KINDS[LMethod.Kind], LName^, LMinArgsCount
      ]);
  end else
  begin
    Self.Error('Invalid arguments count (%d) of %s "%s", reqired: %d..%d',
      [
        LArgsCount, METHOD_KINDS[LMethod.Kind], LName^, LMinArgsCount, LMaxArgsCount
      ]);
  end;

invalid_result:
  Result := 0;
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

procedure TLua.RegTypeName(const TypeName: LuaString; const TypeInfo: PTypeInfo;
  const PointerDepth: Integer);
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Self.InternalRegTypeName(TypeName, TypeInfo, PointerDepth);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalRegTypeName
end;
{$endif}

procedure TLua.RegClass(const AClass: TClass; const UseRtti: Boolean);
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Self.InternalAddClass(AClass, UseRtti, True);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  push [esp]
  mov [esp + 4], 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  mov r9d, 1
  {$endif}
  jmp TLua.InternalAddClass
end;
{$endif}

procedure __TLuaRegClasses(const Self: TLua; const AClasses: array of TClass; const UseRtti: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(AClasses) do
  Self.InternalAddClass(AClasses[i], UseRtti, True);
end;

procedure TLua.RegClasses(const AClasses: array of TClass; const UseRtti: Boolean);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegClasses(Self, AClasses, UseRtti);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegClasses
end;
{$endif}

function TLua.RegRecord(const TypeInfo: PTypeInfo): PLuaRecordInfo;
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Result := InternalAddRecord(TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddRecord
end;
{$endif}

function TLua.RegRecord(const Name: LuaString; const TypeInfo: PTypeInfo; const UseRtti: Boolean): PLuaRecordInfo;
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Result := InternalAddRecordEx(Name, TypeInfo, UseRtti);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddRecordEx
end;
{$endif}

function TLua.RegArray(const TypeInfo: PTypeInfo): PLuaArrayInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  Result := InternalAddArray(TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddArray
end;
{$endif}

function TLua.RegArray(const Identifier: Pointer; const ItemTypeInfo: PTypeInfo; const ABounds: array of Integer): PLuaArrayInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  Result := InternalAddArrayEx(Identifier, ItemTypeInfo, ABounds);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddArrayEx
end;
{$endif}

function TLua.RegSet(const TypeInfo: PTypeInfo): PLuaSetInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  Result := InternalAddSet(TypeInfo, True);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  mov ecx, 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  mov r8d, 1
  {$endif}
  jmp TLua.InternalAddSet
end;
{$endif}

procedure __TLuaRegUniversalProc(const Lua: TLua; const ProcName: LuaString;
  const Proc: TLuaProcCallback; const MinArgsCount, MaxArgsCount: Word);
begin
  Lua.InternalAddMethod(nil, ProcName, @Proc, mkStatic, LUA_POINTER_INVALID, True, MinArgsCount, MaxArgsCount);
end;

procedure TLua.RegProc(const ProcName: LuaString; const Callback: TLuaProcCallback;
  const MinArgsCount, MaxArgsCount: Word);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegUniversalProc(Self, ProcName, Callback, MinArgsCount, MaxArgsCount);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegUniversalProc
end;
{$endif}

procedure __TLuaRegRttiProc(const Lua: TLua; const ProcName: LuaString;
  const Address: Pointer; const TypeInfo: PTypeInfo);
var
  Invokable: __luapointer;
begin
  Invokable := Lua.InternalBuildInvokable(nil, TypeInfo, mkStatic, True);
  if (Invokable <> LUA_POINTER_INVALID) then
  begin
    Lua.InternalAddMethod(nil, ProcName, Address, mkStatic, Invokable, True);
  end;
end;

procedure TLua.RegProc(const ProcName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegRttiProc(Self, ProcName, Address, TypeInfo);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegRttiProc
end;
{$endif}

procedure __TLuaRegCustomProc(const Lua: TLua; const ProcName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam;
  const ResultType: PTypeInfo; const IsResultUnsafe: Boolean; const CallConv: TCallConv);
var
  Invokable: __luapointer;
begin
  Invokable := Lua.InternalBuildInvokable(nil, ProcName, Params,
    ResultType, IsResultUnsafe, mkStatic, CallConv, True);
  if (Invokable <> LUA_POINTER_INVALID) then
  begin
    Lua.InternalAddMethod(nil, ProcName, Address, mkStatic, Invokable, True);
  end;
end;

procedure TLua.RegProc(const ProcName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam;
  const ResultType: PTypeInfo; const IsResultUnsafe: Boolean; const CallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegCustomProc(Self, ProcName, Address, Params, ResultType, IsResultUnsafe, CallConv);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegCustomProc
end;
{$endif}

procedure __TLuaRegUniversalClassMethod(const Lua: TLua; const AClass: TClass;
  const MethodName: LuaString; const Method: TLuaMethodCallback;
  const MinArgsCount, MaxArgsCount: Word; const MethodKind: TLuaMethodKind);
begin
  Lua.InternalAddMethod(Lua.InternalAddClass(AClass, False, True), MethodName, @Method,
    MethodKind, LUA_POINTER_INVALID, True, MinArgsCount, MaxArgsCount);
end;

procedure TLua.RegMethod(const AClass: TClass; const MethodName: LuaString; const Callback: TLuaMethodCallback;
  const MinArgsCount, MaxArgsCount: Word; const MethodKind: TLuaMethodKind);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegUniversalClassMethod(Self, AClass, MethodName, Callback, MinArgsCount, MaxArgsCount, MethodKind);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegUniversalClassMethod
end;
{$endif}

procedure __TLuaRegRttiClassMethod(const Lua: TLua; const AClass: TClass;
  const MethodName: LuaString; const Address: Pointer; const TypeInfo: PTypeInfo;
  const MethodKind: TLuaMethodKind);
var
  ClassInfo: PLuaClassInfo;
  Invokable: __luapointer;
begin
  ClassInfo := Lua.InternalAddClass(AClass, False, True);
  Invokable := Lua.InternalBuildInvokable(ClassInfo, TypeInfo, MethodKind, True);
  if (Invokable <> LUA_POINTER_INVALID) then
  begin
    Lua.InternalAddMethod(ClassInfo, MethodName, Address, MethodKind, Invokable, True);
  end;
end;

procedure TLua.RegMethod(const AClass: TClass; const MethodName: LuaString; const Address: Pointer;
  const TypeInfo: PTypeInfo; const MethodKind: TLuaMethodKind);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegRttiClassMethod(Self, AClass, MethodName, Address, TypeInfo, MethodKind);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegRttiClassMethod
end;
{$endif}

procedure __TLuaRegCustomClassMethod(const Lua: TLua; const AClass: TClass; const MethodName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam;
  const ResultType: PTypeInfo; const IsResultUnsafe: Boolean;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv);
var
  ClassInfo: PLuaClassInfo;
  Invokable: __luapointer;
begin
  ClassInfo := Lua.InternalAddClass(AClass, False, True);
  Invokable := Lua.InternalBuildInvokable(ClassInfo, MethodName, Params,
    ResultType, IsResultUnsafe, mkStatic, CallConv, True);
  if (Invokable <> LUA_POINTER_INVALID) then
  begin
    Lua.InternalAddMethod(ClassInfo, MethodName, Address, MethodKind, Invokable, True);
  end;
end;

procedure TLua.RegMethod(const AClass: TClass; const MethodName: LuaString;
  const Address: Pointer; const Params: array of TLuaProcParam;
  const ResultType: PTypeInfo; const IsResultUnsafe: Boolean;
  const MethodKind: TLuaMethodKind; const CallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegCustomClassMethod(Self, AClass, MethodName, Address, Params, ResultType, IsResultUnsafe, MethodKind, CallConv);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegCustomClassMethod
end;
{$endif}

procedure __TLuaRegProperty(const ASelf: TLua; const AClass: TClass; const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const ATypeInfo: PTypeInfo;
  const AConstRefHint: Boolean; const AIndex: Integer);
var
  LOptions: TLuaPropertyOptions;
begin
  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.MetaType := ASelf.InternalAddClass(AClass, False, True);
  LOptions.TypeInfo := ATypeInfo;
  LOptions.VirtualAddressHint := True;
  LOptions.ConstRefHint := AConstRefHint;
  LOptions.Index := AIndex;
  LOptions.Getter := AGetter;
  LOptions.Setter := ASetter;

  ASelf.InternalAddProperty(AName, @LOptions, True, True);
end;

procedure TLua.RegProperty(const AClass: TClass; const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const ATypeInfo: PTypeInfo;
  const AConstRefHint: Boolean; const AIndex: Integer);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegProperty(Self, AClass, AName, AGetter, ASetter, ATypeInfo, AConstRefHint, AIndex);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegProperty
end;
{$endif}

procedure __TLuaRegComplexProperty(const ASelf: TLua; const AClass: TClass; const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam;
  const ATypeInfo: PTypeInfo; const ADefault, AConstRefHint: Boolean; const AIndex: Integer);
var
  LOptions: TLuaPropertyOptions;
begin
  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.MetaType := ASelf.InternalAddClass(AClass, False, True);
  LOptions.TypeInfo := ATypeInfo;
  LOptions.IsComplex := True;
  LOptions.IsDefault := ADefault;
  LOptions.ConstRefHint := AConstRefHint;
  LOptions.Getter := AGetter;
  LOptions.Setter := ASetter;

  //ToDo AArguments/AIndex
  LOptions.ComplexPtr := LUA_POINTER_INVALID;

  ASelf.InternalAddProperty(AName, @LOptions, True, True);
end;

procedure TLua.RegComplexProperty(const AClass: TClass; const AName: LuaString;
  const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam;
  const ATypeInfo: PTypeInfo; const ADefault, AConstRefHint: Boolean; const AIndex: Integer);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegComplexProperty(Self, AClass, AName, AGetter, ASetter, AArguments,
     ATypeInfo, ADefault, AConstRefHint, AIndex);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegComplexProperty
end;
{$endif}

procedure __TLuaRegVariableClass(const ASelf: TLua; const AName: LuaString; const AInstance;
  const AClass: TClass; const AIsConst: Boolean; const AReference: TLuaReference);
var
  LOptions: TLuaPropertyOptions;
begin
  ASelf.InternalAddClass(AClass, True, True);

  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.TypeInfo := AClass.ClassInfo;
  LOptions.Index := Low(Integer);
  LOptions.Getter.Mode := amStaticField;
  LOptions.Getter.Reference := AReference;
  LOptions.Getter.Address := @AInstance;
  LOptions.ConstRefHint := AIsConst;
  if (not AIsConst) then
  begin
    LOptions.Setter := LOptions.Getter;
  end;

  ASelf.InternalAddProperty(AName, @LOptions, True, True);
end;

procedure TLua.RegVariable(const Name: LuaString; const Instance; const AClass: TClass;
  const IsConst: Boolean; const Reference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegVariableClass(Self, Name, Instance, AClass, IsConst, Reference);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegVariableClass
end;
{$endif}

procedure __TLuaRegVariableTypeInfo(const ASelf: TLua; const AName: LuaString; const AInstance;
  const ATypeInfo: PTypeInfo; const AIsConst: Boolean; const AReference: TLuaReference);
var
  LOptions: TLuaPropertyOptions;
begin
  FillChar(LOptions, SizeOf(LOptions), #0);
  LOptions.TypeInfo := ATypeInfo;
  LOptions.Index := Low(Integer);
  LOptions.Getter.Mode := amStaticField;
  LOptions.Getter.Reference := AReference;
  LOptions.Getter.Address := @AInstance;
  LOptions.ConstRefHint := AIsConst;
  if (not AIsConst) then
  begin
    LOptions.Setter := LOptions.Getter;
  end;

  ASelf.InternalAddProperty(AName, @LOptions, True, True);
end;

procedure TLua.RegVariable(const Name: LuaString; const Instance; const TypeInfo: PTypeInfo;
  const IsConst: Boolean; const Reference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegVariableTypeInfo(Self, Name, Instance, TypeInfo, IsConst, Reference);
end;
{$else}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegVariableTypeInfo
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
  FFrames := nil;
  __TLuaRegConstVariant(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
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
  FFrames := nil;
  __TLuaRegConstLuaArg(Self, Name, Value);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegConstLuaArg
end;
{$endif}

procedure TLua.RegEnum(const TypeInfo: PTypeInfo);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  InternalAddEnum(TypeInfo, True);
end;
{$else}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  mov ecx, 1
  {$else .CPUX64} .NOFRAME
  push [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  mov r8d, 1
  {$endif}
  jmp TLua.InternalAddEnum
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
