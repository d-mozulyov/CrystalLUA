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

// compiler directives
{$ifdef FPC}
  {$mode delphiunicode}
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
{$ifdef KOLERROR}
  {$define KOL_MCK}
{$endif}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}
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
         KOL,
       {$endif}
       {$ifdef KOLERROR}
         err
       {$else}
         {$ifdef UNITSCOPENAMES}System.SysUtils{$else}SysUtils{$endif}
       {$endif};

type
  // standard types
  {$ifdef FPC}
    PUInt64 = ^UInt64;
    PBoolean = ^Boolean;
    PString = ^string;
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
  TBytes = {$if (not Defined(FPC)) and (CompilerVersion >= 23)}TArray<Byte>{$else}array of Byte{$ifend};
  PBytes = ^TBytes;
  {$ifNdef EXTENDEDRTTI}
    TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall);
  {$endif}

  // exception class
  ELua = class(Exception)
  {$ifdef KOLERROR}
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
  LuaString = UnicodeString;
  PLuaString = PUnicodeString;
  {$ifNdef UNICODE}
    {$define LUA_LENGTH_SHIFT}
  {$endif}
  LuaChar = WideChar;
  PLuaChar = PWideChar;

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

  // record instance information: Pointer/IsRef/IsConst/RecordInfo
  PLuaRecordInfo = ^TLuaRecordInfo;
  TLuaRecord = object(TLuaDifficultType)
  public
    Info: PLuaRecordInfo;
  end;
  PLuaRecord = ^TLuaRecord;

  // array instance information: Pointer/IsRef/IsConst/ArrayInfo
  PLuaArrayInfo = ^TLuaArrayInfo;
  TLuaArray = object(TLuaDifficultType)
  public
    Info: PLuaArrayInfo;
  end;
  PLuaArray = ^TLuaArray;

  // set instance information: Pointer/IsRef/IsConst/SetInfo
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
      AdvancedData: array[0..2] of Byte; { used in TLuaRecord/TLuaArray etc }
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
    procedure CheckDifficultSetter(const AValue: TLuaDifficultType; const AName: PChar; const AReturnAddress: Pointer);
    function  GetLuaTypeName: string;
    function  GetEmpty: Boolean;
    procedure SetEmpty(const AValue: Boolean);
    function  GetBoolean: Boolean;
    procedure SetBoolean(const AValue: Boolean);
    function  GetInteger: Integer;
    procedure SetInteger(const AValue: Integer);
    function  GetDouble: Double;
    procedure SetDouble(AValue: Double);
    function  GetString: LuaString;
    procedure SetString(const AValue: LuaString);
    function  GetPointer: Pointer;
    procedure SetPointer(const AValue: Pointer);
    function  GetVariant: Variant;
    procedure SetVariant(const AValue: Variant);
    function  GetClass: TClass;
    procedure SetClass(const AValue: TClass);
    function  GetObject: TObject;
    procedure SetObject(const AValue: TObject);
    function  GetRecord: TLuaRecord;
    procedure SetRecord(const AValue: TLuaRecord);
    function  GetArray: TLuaArray;
    procedure SetArray(const AValue: TLuaArray);
    function  GetSet: TLuaSet;
    procedure SetSet(const AValue: TLuaSet);
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
    procedure CheckInstance(const AKind: TLuaMetaKind; const AReturnAddress: Pointer);
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
    procedure Dispose(const AValue: Pointer{/TObject/IInterface});
    procedure Clear(var AInstance; const AFillZero: Boolean);
    procedure Assign(var AInstance; const AValue: Pointer{/TObject/IInterface});
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
  TLuaOperatorCallback = procedure(var AResult, ALeft, ARight; const AKind: TLuaOperator);

  // all information (such as name, field, methods)
  // you should use it to operate records between native and script
  TLuaRecordInfo = object(TLuaMetaType)
  protected
    FOperators: TLuaOperators;
    FOperatorCallback: TLuaOperatorCallback;

    procedure SetOperators(const AValue: TLuaOperators);
    procedure SetOperatorCallback(const AValue: TLuaOperatorCallback);
  public
    procedure RegMethod(const AMethodName: LuaString; const ACallback: TLuaMethodCallback; const AMinArgsCount: Word = 0; const AMaxArgsCount: Word = High(Word); const AIsStatic: Boolean = False); overload;
    procedure RegMethod(const AMethodName: LuaString; const AAddress: Pointer; const ATypeInfo: PTypeInfo; const AIsStatic: Boolean = False); overload;
    procedure RegMethod(const AMethodName: LuaString; const AAddress: Pointer;
      const AParams: array of TLuaProcParam;
      const AResultType: PTypeInfo = nil; const AIsResultUnsafe: Boolean = False;
      const AIsStatic: Boolean = False; const ACallConv: TCallConv = ccReg); overload;
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

    function Description(const AValue: Pointer): LuaString;
  public
    property Low: Integer read FLow;
    property High: Integer read FHigh;
  end;

  // (internal) information needed to use interfaces between native and script side
  PLuaInterfaceInfo = ^TLuaInterfaceInfo;
  TLuaInterfaceInfo = object(TLuaMetaType)
    Parent: __luapointer;
    Count: Integer;

    function InheritsFrom(const AValue: PLuaInterfaceInfo): Boolean;
  end;

  // (internal) information needed to use classes between native and script side
  PLuaClassInfo = ^TLuaClassInfo;
  TLuaClassInfo = object(TLuaMetaType)
  protected
    function VmtOffset(const AProc: Pointer; const AStandardProcs: Boolean): NativeInt;
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
  TLuaBufferUnique = function(var AData: __luabuffer): NativeInt;
  TLuaOnPreprocessScript = procedure(var AData, ASource: __luabuffer; var AOffset: Integer; const AUnitName: LuaString; const AUnique: TLuaBufferUnique) of object;

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

    function GetItem(const AIndex: Integer): LuaString;
    function GetLine(const AIndex: Integer): TLuaUnitLine;
    procedure InitializeLines;
  public
    procedure SaveToFile(const AFileName: string); overload;
    procedure SaveToFile; overload;

    property Lua: TLua read FLua;
    property Index: Integer read FIndex;
    property Name: LuaString read FName;
    property FileName: string read FFileName;
    property Data: __luabuffer read FData;
    property LinesCount: Integer read FLinesCount;
    property Items[const Index: Integer]: LuaString read GetItem; default;
    property Lines[const Index: Integer]: TLuaUnitLine read GetLine;
  end;


{ TLua class
  Script and registered types/functions manager }

  TLuaOnRegisterError = function(const APath, AError: LuaString; const ACallDepth: Integer; const ACritical: Boolean): Boolean of object;

  TLua = class(TInterfacedObject)
  protected
    // unicode routine
    FCodePage: Word;
    FUnicodeTable: array[Byte] of Word;
    FUTF8Table: array[Byte] of Cardinal;

    procedure SetCodePage(AValue: Word);
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

    procedure unpack_lua_string(var AResult: LuaString; const ARttiName: ShortString); overload;
    procedure unpack_lua_string(var AResult: LuaString; const AChars: __luadata; const ACount: Integer); overload;
    procedure unpack_lua_string(var AResult: LuaString; const AName: __luaname); overload;
    procedure unpack_lua_string(var AResult: LuaString; const ABuffer: __luabuffer); overload;

    procedure push_ansi_chars(const S: PAnsiChar; const ACodePage: Word; const ACount: Integer);
    procedure push_utf8_chars(const S: PAnsiChar; const ACount: Integer);
    procedure push_wide_chars(const S: PWideChar; const ACount: Integer);
    {$ifNdef NEXTGEN}
    procedure push_short_string(const S: ShortString);
    procedure push_ansi_string(const S: AnsiString);
    procedure push_wide_string(const S: WideString);
    {$endif}
    procedure push_unicode_string(const S: UnicodeString);
    procedure push_lua_string(const S: LuaString); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}

    {$ifNdef NEXTGEN}
    procedure stack_short_string(var S: ShortString; const AStackIndex: Integer; const AMaxLength: Integer);
    procedure stack_ansi_string(var S: AnsiString; const AStackIndex: Integer; const ACodePage: Word);
    procedure stack_wide_string(var S: WideString; const AStackIndex: Integer);
    {$endif}
    procedure stack_unicode_string(var S: UnicodeString; const AStackIndex: Integer);
    procedure stack_lua_string(var S: LuaString; const AStackIndex: Integer); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure stack_force_unicode_string(var S: UnicodeString; const AStackIndex: Integer; const AExtendedMode: Boolean = True);

    function  push_metatype_instance(const AMetaType: PLuaMetaType; const ASource; const AOwner, ATemporary: Boolean): Pointer{PLuaUserData};
    function  push_complex_property(const APropertyInfo; const AInstance: Pointer): Pointer{PLuaUserData};
    function  push_luaarg(const AValue: TLuaArg): Boolean;
    function  push_variant(const AValue: Variant): Boolean;
    function  push_argument(const AValue: TVarRec): Boolean;
    procedure stack_pop(const ACount: Integer = 1);
    function  stack_variant(var AResult: Variant; const AStackIndex: Integer; const AOleMode: Boolean): Boolean;
    function  stack_luaarg(var AResult: TLuaArg; const AStackIndex: integer; const AAllowLuaTable: Boolean): Boolean;
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
    procedure global_free_ref(var ARef: Integer);
    procedure global_fill_value(const ARef: Integer);
    procedure global_push_value(const ARef: Integer);

    // public globals
    function GetGlobalEntity(const AName: LuaString; const AModeCreate: Boolean): Pointer{PLuaGlobalEntity};
    function GetRecordInfo(const AName: LuaString): PLuaRecordInfo;
    function GetArrayInfo(const AName: LuaString): PLuaArrayInfo;
    function GetSetInfo(const AName: LuaString): PLuaSetInfo;
    function GetVariable(const AName: LuaString): Variant;
    procedure SetVariable(const AName: LuaString; const AValue: Variant);
    function GetVariableEx(const AName: LuaString): TLuaArg;
    procedure SetVariableEx(const AName: LuaString; const AValue: TLuaArg);
    function GetTableVariable(const ATable, AName: LuaString): Variant;
    procedure SetTableVariable(const ATable, AName: LuaString; const AValue: Variant);
    function GetTableVariableEx(const ATable, AName: LuaString): TLuaArg;
    procedure SetTableVariableEx(const ATable, AName: LuaString; const AValue: TLuaArg);
  private
    // registrations
    {$ifdef LUA_NATIVEFUNC}
    FCFunctionHeap: array[1..8{$ifdef LARGEINT}* 2{$endif}] of Byte{TLuaCFunctionHeap};
    {$endif}
    FRegisteredTypeNames: __TLuaStringDictionary {TLuaRegisteredTypeNames: <LuaString,PLuaRegisteredTypeName>};

    function alloc_luafunction(const ACallback: Pointer; const P1, P2: __luapointer): __luafunction; overload;
    function alloc_luafunction(const AMethod: Pointer{PLuaMethod}): __luafunction; overload;
    procedure alloc_push_luafunction(const ACallback: Pointer; const P1, P2: __luapointer);
    procedure push_luafunction(const AFunction: __luafunction);
    function function_topointer(const AStackIndex: Integer): Pointer;
    function alloc_push_closure(const AReferenceOwner: Boolean): Pointer{PLuaClosureMethod};

    procedure InternalRegisterCallback(const AName: __luaname; const ACallback: Pointer; const P1: __luapointer = -1; const P2: __luapointer = -1);
    procedure InternalRegTypeName(const ATypeName: LuaString; const ATypeInfo: PTypeInfo; const APointerDepth: Integer);
    procedure InternalRegStandardTypeNames;
    function InternalNewMetaTable: Integer;
    function InternalRegisterMetaTable(const AMetaType: PLuaMetaType = nil): Integer;
    function InternalTableToMetaType(const AStackIndex: Integer): PLuaMetaType;
    function InternalAddGlobal(const AKind: Byte{TLuaGlobalKind}; const AName: __luaname): Pointer{PLuaGlobalEntity};
    function InternalAddMetaType(const AKind: TLuaMetaKind; const AName: LuaString; const ATypeInfo: Pointer;
      const AInstanceSize: Integer; const AAdditionalSize: NativeInt = 0): PLuaMetaType;
    procedure InternalReplaceChildNameSpace(const AParentMetaItem: Pointer{PLuaDictionaryItem};
      const AName: __luaname; const ALastValue, AValue: __luapointer; const AIsDefaultProp: Boolean);
    function  InternalAddMethod(const AMetaType: PLuaMetaType; const AName: LuaString;
      const AAddress: Pointer; const AMethodKind: TLuaMethodKind; const AInvokable: __luapointer; const ACritical: Boolean;
      {Universal} const AMinArgsCount: Word = 0; const AMaxArgsCount: Word = High(Word)): __luapointer;
    function InternalAddProperty(const AName: LuaString; const AOptions: Pointer{PLuaPropertyOptions};
      const AAutoRegister, ACritical: Boolean): __luapointer;
    function InternalAddField(const AMetaType: PLuaMetaType; const AName: LuaString;
      const AOffset: NativeUInt; const ATypeInfo: PTypeInfo; const AReference: TLuaReference;
      const AClassField, AAutoRegister, ACritical: Boolean): __luapointer;
    function  InternalAddClass(const AClass: TClass; const AUseRtti: Boolean; const ACritical: Boolean): PLuaClassInfo;
    function  InternalGetClassInfo(const AClass: TClass): PLuaClassInfo;
    function  InternalAddRecord(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaRecordInfo;
    function  InternalAddRecordEx(const AName: LuaString; const ATypeInfo: Pointer; const AUseRtti: Boolean; const ACritical: Boolean): PLuaRecordInfo;
    function  InternalAddInterface(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaInterfaceInfo;
    function  InternalAddArray(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaArrayInfo;
    function  InternalAddArrayEx(const AIdentifier, AItemTypeInfo: Pointer; const ABounds: array of Integer; const ACritical: Boolean): PLuaArrayInfo;
    function  InternalAddSet(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaSetInfo;
    procedure InternalAddEnum(const ATypeInfo: PTypeInfo; const ACritical: Boolean);
    function  InternalInvokableBuilderEnter: __TLuaInvokableBuilder;
    procedure InternalInvokableBuilderLeave(const AValue: __TLuaInvokableBuilder);
    function  InternalAutoRegister(const ATypeInfo: PTypeInfo; const AUseRtti: Boolean = True; const ACritical: Boolean = False): Pointer{PLuaMetaType/PLuaClosureType};
    function  InternalBuildInvokable(const AMetaType: PLuaMetaType; const ATypeInfo: PTypeInfo; const AMethodKind: TLuaMethodKind; const ACritical: Boolean): __luapointer; overload;
    function  InternalBuildInvokable(const AMetaType: PLuaMetaType; const AName: LuaString; const AParams: array of TLuaProcParam; const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean; const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv; const ACritical: Boolean): __luapointer; overload;
  private
    // script callbacks helpers
    function __print: Integer;
    function __printf: Integer;
    function __GetUserData(const AMetaType: __luapointer; const ACheckAlreadyDestroyed: Boolean = True): Pointer;
    function __GetSelfInstance(const AMethodName: __luaname): Pointer;
    function __GetSelfClass(const AMethodName: __luaname; const AAllowInstance: Boolean): TClass;
    function __InitTableValues(const AUserData: Pointer{PLuaUserData}; const AStackIndex: Integer): Integer;
    function __ExecuteSetMethod(const AUserData: Pointer{PLuaUserData}; const AFirstArgIndex, AArgsCount, AMethod: Integer): Integer;

    // specific script callbacks
    function __len(const AMetaType: __luapointer): Integer;
    function __tostring(const AMetaType: __luapointer): Integer;
    function __gc(const AMetaType: __luapointer): Integer;
    function __call(const AMetaType: __luapointer; const AParamMode: Boolean): Integer;
    function __operator(const AMetaType: __luapointer; const AKind: Cardinal{Byte}): Integer;
    function __closuregc: Integer;

    // index/newindex helpers
    function push_standard(const AMetaType: PLuaMetaType; const AStdIndex: Cardinal): Boolean;
    function set_standard(const AMetaType: PLuaMetaType; const AStdIndex: Cardinal): Boolean;
    procedure push_prop_tempgetter(const AInstance: Pointer; const AProp: Pointer);
    function call_prop_getter(const AInstance: Pointer; const AProp: Pointer): Pointer;
    procedure call_prop_setter(const AInstance: Pointer; const AProp: Pointer; const AValue: NativeInt);

    // index/newindex
    function __index(const AMetaType: __luapointer): Integer;
    function __newindex(const AMetaType: __luapointer; const AParamMode: Boolean): Integer;
    function __global_index(const AModifyLow, AModifyHigh: Cardinal): Integer;
    function __global_newindex(const AModifyLow, AModifyHigh: Cardinal): Integer;
  protected
    function __InheritsFrom(const AMetaType: PLuaMetaType; const AArgsCount: Integer): Integer;
    function __Assign(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __Free(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __ClassCreate(const AMetaType: PLuaMetaType; const AArgsCount: Integer): Integer;
    function __IntfAddRef(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __IntfRelease(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __DynArrayInclude(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __DynArrayResize(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __SetInclude(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __SetExclude(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
    function __SetContains(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
  private
    // method callbacks
    function UniversalMethodCallback(const AMethod: Pointer{PLuaMethod}; const AArgsCount: Integer): Integer;
    function InvokableMethodCallback(const AMethod: Pointer{PLuaMethod}; const AInvokableData: Pointer): Integer;
    function __MethodCallback(const AMethodLow, AMethodHigh: Cardinal): Integer;
  private
    // errors
    FReturnAddress: Pointer;
    FFrames: Pointer{PLuaNativeFrame};
    FPrintRegisterError: Boolean;
    FOnRegisterError: TLuaOnRegisterError;

    procedure NativeFrameEnter(var AFrame{: TLuaNativeFrame}; const AName: PShortString; const ACritical: Boolean);
    procedure NativeFrameLeave;
    procedure NativeFrameRename(const AName: PShortString);
    procedure NativeFrameBufferedRename(var ANameBuffer: ShortString; const AName: LuaString);
    procedure InternalError(const AText: LuaString);
    procedure InternalErrorFmt(const AFmtStr: LuaString; const AArgs: array of const);
    procedure RegisterError(const AText: LuaString);
    procedure RegisterErrorFmt(const AFmtStr: LuaString; const AArgs: array of const);
    procedure RegisterErrorTypeExists(const AName: LuaString);
  private
    // scripts and units routine
    FOnPreprocessScript: TLuaOnPreprocessScript;
    FStackFrames: array[1 - 1..16] of TLuaScriptFrame;
    FStackFrameIndex: NativeUInt;
    FArguments: PLuaArguments{PLuaScriptFrame};

    FUnitCount: Integer;
    FUnits: array of TLuaUnit;
    procedure CheckScriptError(const AErrCode: Integer; AUnit: TLuaUnit = nil);
    function EnterScript(const AReturnAddress: Pointer): Pointer;
    procedure LeaveScriptStack(const AReturnAddress: Pointer);
    function  InternalConvertScript(var AData: __luabuffer): Integer;
    procedure InternalPreprocessScript(var ABuffer: __luabuffer; const AOffset: Integer);
    function  InternalRunScript(var AData: __luabuffer; const AOffset: Integer; const AUnitName: __luaname; const AUnit: TLuaUnit; const AMakeResult: Boolean; var AExceptionAddress: Pointer; const AReturnAddress: Pointer): Exception;
    procedure InternalLoadScript(var AData: __luabuffer; const AUnitName: LuaString; const AFileName: string);
    function  GetUnit(const AIndex: Integer): TLuaUnit;
    function  GetUnitByName(const AName: LuaString): TLuaUnit;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GarbageCollection;

    // errors
    procedure Error(const AText: LuaString);
    procedure ErrorFmt(const AFmtStr: LuaString; const AArgs: array of const);

    // scripts
    procedure RunScript(const AScript: LuaString);
    procedure LoadScript(const AFileName: string); overload;
    procedure LoadScript(const ABuffer: Pointer; const ABufferSize: Integer; const ABOM: TLuaScriptBOM; const AUnitName: LuaString = ''); overload;
    procedure LoadScript(const AInstance: THandle; const AName, AResType: PChar; const UnitName: LuaString); overload;
    procedure LoadScript(const ABuffer: TBytes; const ABOM: TLuaScriptBOM = sbNone; const AUnitName: LuaString = ''); overload;

    // methods
    function VariableExists(const AName: LuaString): Boolean; overload;
    function ProcExists(const AName: LuaString): Boolean; overload;
    function VariableExists(const ATable, AName: LuaString): Boolean; overload;
    function ProcExists(const ATable, AName: LuaString): Boolean; overload;
    function Call(const AProcName: LuaString): TLuaArgs; overload;
    function Call(const AProcName: LuaString; const AArgs: array of TLuaArg): TLuaArgs; overload;
    function Call(const AProcName: LuaString; const AArgs: array of const): TLuaArgs; overload;
    function Call(const ATable, AProcName: LuaString): TLuaArgs; overload;
    function Call(const ATable, AProcName: LuaString; const AArgs: array of TLuaArg): TLuaArgs; overload;
    function Call(const ATable, AProcName: LuaString; const AArgs: array of const): TLuaArgs; overload;

    // registrations
    procedure RegTypeName(const ATypeName: LuaString; const ATypeInfo: PTypeInfo; const APointerDepth: Integer = 0);
    procedure RegClass(const AClass: TClass; const AUseRtti: Boolean = True);
    procedure RegClasses(const AClasses: array of TClass; const AUseRtti: Boolean = True);
    function  RegRecord(const ATypeInfo: PTypeInfo): PLuaRecordInfo; overload;
    function  RegRecord(const AName: LuaString; const ATypeInfo: PTypeInfo; const AUseRtti: Boolean = True): PLuaRecordInfo; overload;
    function  RegArray(const ATypeInfo: PTypeInfo): PLuaArrayInfo; overload;
    function  RegArray(const AIdentifier: Pointer; const AItemTypeInfo: PTypeInfo; const ABounds: array of Integer): PLuaArrayInfo; overload;
    function  RegSet(const ATypeInfo: PTypeInfo): PLuaSetInfo;
    procedure RegProc(const AProcName: LuaString; const ACallback: TLuaProcCallback; const AMinArgsCount: Word = 0; const AMaxArgsCount: Word = High(Word)); overload;
    procedure RegProc(const AProcName: LuaString; const AAddress: Pointer; const ATypeInfo: PTypeInfo); overload;
    procedure RegProc(const AProcName: LuaString; const AAddress: Pointer; const AParams: array of TLuaProcParam;
      const AResultType: PTypeInfo = nil; const AIsResultUnsafe: Boolean = False;
      const ACallConv: TCallConv = ccReg); overload;
    procedure RegMethod(const AClass: TClass; const AMethodName: LuaString; const ACallback: TLuaMethodCallback; const AMinArgsCount: Word = 0; const AMaxArgsCount: Word = High(Word); const AMethodKind: TLuaMethodKind = mkInstance); overload;
    procedure RegMethod(const AClass: TClass; const AMethodName: LuaString; const AAddress: Pointer; const ATypeInfo: PTypeInfo; const AMethodKind: TLuaMethodKind = mkInstance); overload;
    procedure RegMethod(const AClass: TClass; const AMethodName: LuaString; const AAddress: Pointer;
      const AParams: array of TLuaProcParam; const AResultType: PTypeInfo = nil; const AIsResultUnsafe: Boolean = False;
      const AMethodKind: TLuaMethodKind = mkInstance; const ACallConv: TCallConv = ccReg); overload;
    procedure RegProperty(const AClass: TClass; const AName: LuaString;
      const AGetter, ASetter: TLuaPropAccess; const ATypeInfo: PTypeInfo;
      const AConstRefHint: Boolean = True; const AIndex: Integer = Low(Integer));
    procedure RegComplexProperty(const AClass: TClass; const AName: LuaString;
      const AGetter, ASetter: TLuaPropAccess; const AArguments: array of TLuaProcParam; const ATypeInfo: PTypeInfo;
      const ADefault: Boolean = False; const AConstRefHint: Boolean = True; const AIndex: Integer = Low(Integer));
    procedure RegVariable(const AName: LuaString; const AInstance; const AClass: TClass;
      const AIsConst: Boolean = False; const AReference: TLuaReference = lrDefault); overload;
    procedure RegVariable(const AName: LuaString; const AInstance; const ATypeInfo: PTypeInfo;
      const AIsConst: Boolean = False; const AReference: TLuaReference = lrDefault); overload;
    procedure RegConst(const AName: LuaString; const AValue: Variant); overload;
    procedure RegConst(const AName: LuaString; const AValue: TLuaArg); overload;
    procedure RegEnum(const ATypeInfo: PTypeInfo);

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
function LuaArg(const AValue: Boolean): TLuaArg; overload;
function LuaArg(const AValue: Integer): TLuaArg; overload;
function LuaArg(const AValue: Double): TLuaArg; overload;
function LuaArg(const AValue: LuaString): TLuaArg; overload;
function LuaArg(const AValue: Pointer): TLuaArg; overload;
function LuaArg(const AValue: TClass): TLuaArg; overload;
function LuaArg(const AValue: TObject): TLuaArg; overload;
function LuaArg(const AValue: TLuaRecord): TLuaArg; overload;
function LuaArg(const AValue: TLuaArray): TLuaArg; overload;
function LuaArg(const AValue: TLuaSet): TLuaArg; overload;
function LuaArg(const AValue: Variant): TLuaArg; overload;
function LuaArgDynArray(const ACount: Integer): TLuaArgDynArray;
function LuaProcCallback(const AAddress: Pointer): TLuaProcCallback;
function LuaMethodCallback(const AAddress: Pointer): TLuaMethodCallback;
function LuaProcParam(const AName: LuaString; const ATypeInfo: PTypeInfo;
  const AFlags: TParamFlags = [pfConst]; const APointerDepth: Integer = 0): TLuaProcParam;


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

implementation


{$ifdef FPC}
procedure VarClear(var V: Variant);
begin
  if (not Assigned(System.VarClearProc)) then
    System.Error(reAccessViolation);

  System.VarClearProc(PVarData(@V)^);
end;
{$endif}

{ ELua }

{$ifdef KOLERROR}
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

{$if Defined(FPC) or (CompilerVersion < 27)}
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

function lua_toint64(L: Plua_State; idx: Integer): Int64; register; {$ifdef FPC}assembler;nostackframe;{$endif}
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

procedure GetLuaTypeName(var AResult: string; const ALuaType: Integer);
begin
  case (ALuaType) of
    LUA_TNONE         : AResult := 'LUA_TNONE';
    LUA_TNIL          : AResult := 'LUA_TNIL';
    LUA_TBOOLEAN      : AResult := 'LUA_TBOOLEAN';
    LUA_TLIGHTUSERDATA: AResult := 'LUA_TLIGHTUSERDATA';
    LUA_TNUMBER       : AResult := 'LUA_TNUMBER';
    LUA_TSTRING       : AResult := 'LUA_TSTRING';
    LUA_TTABLE        : AResult := 'LUA_TTABLE';
    LUA_TFUNCTION     : AResult := 'LUA_TFUNCTION';
    LUA_TUSERDATA     : AResult := 'LUA_TUSERDATA';
  else
    AResult := Format('UNKNOWN (%d)', [ALuaType]);
  end;
end;

function LoadLuaLibrary: THandle;
var
  S: string;
  LBuffer: array[0..1024] of Char;
  LBufferPtr: PChar;
begin
  if (LuaPath = '') then
  begin
    if (FileExists(LuaLibraryPath)) then
    begin
      LuaPath := ExpandFileName(LuaLibraryPath);
    end else
    begin
      LBufferPtr := @LBuffer[0];
      SetString(S, LBufferPtr, {$ifdef FPC}GetModuleFileNameW{$else}GetModuleFileName{$endif}(hInstance, LBufferPtr, High(LBuffer)));
      LuaPath := ExtractFilePath(S) + ExtractFileName(LuaLibraryPath);
    end;

    LuaHandle := {$ifdef FPC}LoadLibraryW{$else}LoadLibrary{$endif}(PChar(LuaPath));
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
  LBuffer: Pointer;

  function FailLoad(var AProc; const AProcName: PAnsiChar): Boolean;
  begin
    Pointer(AProc) := GetProcAddress(LuaHandle, AProcName);
    Result := (Pointer(AProc) = nil);
  end;

begin
  Result := False;
  if (not LuaInitialized) then
  begin
    if (LoadLuaLibrary = 0) then Exit;

    LUA_VERSION_52 := not FailLoad(LBuffer, 'lua_tounsignedx');
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
  LNativeArr: ^TNativeUIntArray;
begin
  CODEPAGE_DEFAULT := GetACP;
  LNativeArr := Pointer(@UTF8CHAR_SIZE);

  // 0..127
  X := {$ifdef LARGEINT}$0101010101010101{$else}$01010101{$endif};
  for i := 0 to 128 div SizeOf(NativeUInt) - 1 do
  LNativeArr[i] := X;

  // 128..191 (64) fail (0)
  Inc(NativeUInt(LNativeArr), 128);
  X := 0;
  for i := 0 to 64 div SizeOf(NativeUInt) - 1 do
  LNativeArr[i] := X;

  // 192..223 (32)
  Inc(NativeUInt(LNativeArr), 64);
  X := {$ifdef LARGEINT}$0202020202020202{$else}$02020202{$endif};
  for i := 0 to 32 div SizeOf(NativeUInt) - 1 do
  LNativeArr[i] := X;

  // 224..239 (16)
  Inc(NativeUInt(LNativeArr), 32);
  X := {$ifdef LARGEINT}$0303030303030303{$else}$03030303{$endif};
  {$ifdef LARGEINT}
    LNativeArr[0] := X;
    LNativeArr[1] := X;
  {$else}
    LNativeArr[0] := X;
    LNativeArr[1] := X;
    LNativeArr[2] := X;
    LNativeArr[3] := X;
  {$endif}

  // 240..247 (8)
  Inc(NativeUInt(LNativeArr), 16);
  {$ifdef LARGEINT}
    LNativeArr[0] := $0404040404040404;
  {$else}
    LNativeArr[0] := $04040404;
    LNativeArr[1] := $04040404;
  {$endif}

  // 248..251 (4) --> 5
  // 252..253 (2) --> 6
  // 254..255 (2) --> fail (0)
  {$ifdef LARGEINT}
    LNativeArr[1] := $0000060605050505;
  {$else}
    LNativeArr[2] := $05050505;
    LNativeArr[3] := $00000606;
  {$endif}
end;


{ Sets routine }

type
  TSet1 = set of 0..7;
  TSet2 = set of 0..15;
  TSet4 = set of 0..31;
  TSetBuffer = array[0..32 div SizeOf(NativeUInt) - 1] of NativeUInt;

function SetBitInitialize(ABit: Integer): TSetBuffer;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  LNull: NativeUInt;
  LDest: PNativeUInt;
begin
  LNull := 0;
  Result[0] := LNull;
  Result[1] := LNull;
  Result[2] := LNull;
  Result[3] := LNull;
  {$ifdef SMALLINT}
  Result[4] := LNull;
  Result[5] := LNull;
  Result[6] := LNull;
  Result[7] := LNull;
  {$endif}

  LDest := @Result[ABit shr {$ifdef SMALLINT}5{$else .LARGEINT}6{$endif}];
  ABit := ABit and {$ifdef SMALLINT}31{$else .LARGEINT}63{$endif};
  LDest^ := LDest^ or (NativeUInt(1) shl ABit);
end;
{$endif}

procedure SetBitInclude(AValue: PByte; ABit: Integer);
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  bts [eax], edx
end;
{$else}
begin
  Inc(AValue, ABit shr 3);
  ABit := ABit and 7;
  AValue^ := AValue^ or (1 shl ABit);
end;
{$endif}

procedure SetBitExclude(AValue: PByte; ABit: Integer);
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  btr [eax], edx
end;
{$else}
begin
  Inc(AValue, ABit shr 3);
  ABit := ABit and 7;
  AValue^ := AValue^ and (not (1 shl ABit));
end;
{$endif}

function SetBitContains(AValue: PByte; ABit: Integer): Boolean;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  bt [eax], edx
  setc al
end;
{$else}
begin
  Inc(AValue, ABit shr 3);
  ABit := ABit and 7;
  Result := (AValue^ and (1 shl ABit) <> 0);
end;
{$endif}

function  _SetLe(ALeft, ARight: PByte; ASize: Integer): Boolean;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  if (ASize >= SizeOf(NativeUInt)) then
  repeat
    L := PNativeUInt(ALeft)^;
    R := PNativeUInt(ARight)^;
    Dec(ASize, SizeOf(NativeUInt));
    Inc(ALeft, SizeOf(NativeUInt));
    Inc(ARight, SizeOf(NativeUInt));
    if (L and (not R) <> 0) then goto ret_false;
  until (ASize < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (ASize >= SizeOf(Cardinal)) then
  begin
    L := PCardinal(ALeft)^;
    R := PCardinal(ARight)^;
    Dec(ASize, SizeOf(Cardinal));
    Inc(ALeft, SizeOf(Cardinal));
    Inc(ARight, SizeOf(Cardinal));
    if (L and (not R) <> 0) then goto ret_false;
  end;
  {$endif}

  if (ASize >= SizeOf(Byte)) then
  repeat
    L := PByte(ALeft)^;
    R := PByte(ARight)^;
    Dec(ASize, SizeOf(Byte));
    Inc(ALeft, SizeOf(Byte));
    Inc(ARight, SizeOf(Byte));
    if (L and (not R) <> 0) then goto ret_false;
  until (ASize < SizeOf(Byte));

  Result := True;
  Exit;
ret_false:
  Result := False;
end;
{$endif}

function _SetEq(ALeft, ARight: PByte; ASize: Integer): Boolean;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  if (ASize >= SizeOf(NativeUInt)) then
  repeat
    if (PNativeUInt(ALeft)^ <> PNativeUInt(ARight)^) then goto ret_false;
    Dec(ASize, SizeOf(NativeUInt));
    Inc(ALeft, SizeOf(NativeUInt));
    Inc(ARight, SizeOf(NativeUInt));
  until (ASize < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (ASize >= SizeOf(Cardinal)) then
  begin
    if (PCardinal(ALeft)^ <> PCardinal(ARight)^) then goto ret_false;
    Dec(ASize, SizeOf(Cardinal));
    Inc(ALeft, SizeOf(Cardinal));
    Inc(ARight, SizeOf(Cardinal));
  end;
  {$endif}

  if (ASize >= SizeOf(Byte)) then
  repeat
    if (PByte(ALeft)^ <> PByte(ARight)^) then goto ret_false;
    Dec(ASize, SizeOf(Byte));
    Inc(ALeft, SizeOf(Byte));
    Inc(ARight, SizeOf(Byte));
  until (ASize < SizeOf(Byte));

  Result := True;
  Exit;
ret_false:
  Result := False;
end;
{$endif}

// Result 0 means "equal"
function SetsCompare(const ALeft, ARight: Pointer; const ASize: Integer; const ASubsetMode: Boolean): Integer; overload;
var
  LResult: Boolean;
begin
  if (ASubsetMode) then
  begin
    case (ASize) of
      1: LResult := (TSet1(ALeft^) <= TSet1(ARight^));
      2: LResult := (TSet2(ALeft^) <= TSet2(ARight^));
      4: LResult := (TSet4(ALeft^) <= TSet4(ARight^));
    else
      LResult := _SetLe(ALeft, ARight, ASize);
    end;
  end else
  begin
    case (ASize) of
      1: LResult := (TSet1(ALeft^) = TSet1(ARight^));
      2: LResult := (TSet2(ALeft^) = TSet2(ARight^));
      4: LResult := (TSet4(ALeft^) = TSet4(ARight^));
    else
      LResult := _SetEq(ALeft, ARight, ASize);
    end;
  end;

  Result := Ord(not LResult);
end;

function SetsCompare(const ALeft: Pointer; const ABit, ASize: Integer; const ASubsetMode: Boolean): Integer; overload;
var
  LRight: TSetBuffer;
  LRet: Boolean;
begin
  if (ASubsetMode) then
  begin
    case (ASize) of
      1: LRet := (TSet1(ALeft^) <= TSet1(Byte(1 shl ABit)));
      2: LRet := (TSet2(ALeft^) <= TSet2(Word(1 shl ABit)));
      4: LRet := (TSet4(ALeft^) <= TSet4(Integer(1 shl ABit)));
    else
      LRight := SetBitInitialize(ABit);
      LRet := _SetLe(ALeft, Pointer(@LRight), ASize);
    end;
  end else
  begin
    case (ASize) of
      1: LRet := (TSet1(ALeft^) = TSet1(Byte(1 shl ABit)));
      2: LRet := (TSet2(ALeft^) = TSet2(Word(1 shl ABit)));
      4: LRet := (TSet4(ALeft^) = TSet4(Integer(1 shl ABit)));
    else
      LRight := SetBitInitialize(ABit);
      LRet := _SetEq(ALeft, Pointer(@LRight), ASize);
    end;
  end;

  Result := Ord(not LRet);
end;

procedure SetsUnion(ADest, ALeft, ARight: PByte; ASize: Integer); overload;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  LSize: Integer;
begin
  LSize := ASize;

  if (LSize >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(ADest)^ := PNativeUInt(ALeft)^ or PNativeUInt(ARight)^;
    Dec(LSize, SizeOf(NativeUInt));
    Inc(ADest, SizeOf(NativeUInt));
    Inc(ALeft, SizeOf(NativeUInt));
    Inc(ARight, SizeOf(NativeUInt));
  until (LSize < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (LSize >= SizeOf(Cardinal)) then
  begin
    PCardinal(ADest)^ := PCardinal(ALeft)^ or PCardinal(ARight)^;
    Dec(LSize, SizeOf(Cardinal));
    Inc(ADest, SizeOf(Cardinal));
    Inc(ALeft, SizeOf(Cardinal));
    Inc(ARight, SizeOf(Cardinal));
  end;
  {$endif}

  if (LSize >= SizeOf(Byte)) then
  repeat
    PByte(ADest)^ := PByte(ALeft)^ or PByte(ARight)^;
    Dec(LSize, SizeOf(Byte));
    Inc(ADest, SizeOf(Byte));
    Inc(ALeft, SizeOf(Byte));
    Inc(ARight, SizeOf(Byte));
  until (LSize < SizeOf(Byte));
end;
{$endif}

procedure SetsUnion(ADest, ALeft: Pointer; ABit, ASize: Integer); overload;
var
  LRight: TSetBuffer;
begin
  case ASize of
    1: TSet1(ADest^) := TSet1(ALeft^) + TSet1(Byte(1 shl ABit));
    2: TSet2(ADest^) := TSet2(ALeft^) + TSet2(Word(1 shl ABit));
    4: TSet4(ADest^) := TSet4(ALeft^) + TSet4(Integer(1 shl ABit));
  else
    LRight := SetBitInitialize(ABit);
    SetsUnion(ADest, ALeft, Pointer(@LRight), ASize);
  end;
end;

procedure SetsDifference(ADest, ALeft, ARight: PByte; ASize: Integer); overload;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  LSize: Integer;
begin
  LSize := ASize;

  if (LSize >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(ADest)^ := PNativeUInt(ALeft)^ and (not PNativeUInt(ARight)^);
    Dec(LSize, SizeOf(NativeUInt));
    Inc(ADest, SizeOf(NativeUInt));
    Inc(ALeft, SizeOf(NativeUInt));
    Inc(ARight, SizeOf(NativeUInt));
  until (LSize < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (LSize >= SizeOf(Cardinal)) then
  begin
    PCardinal(ADest)^ := PCardinal(ALeft)^ and (not PCardinal(ARight)^);
    Dec(LSize, SizeOf(Cardinal));
    Inc(ADest, SizeOf(Cardinal));
    Inc(ALeft, SizeOf(Cardinal));
    Inc(ARight, SizeOf(Cardinal));
  end;
  {$endif}

  if (LSize >= SizeOf(Byte)) then
  repeat
    PByte(ADest)^ := PByte(ALeft)^ and (not PByte(ARight)^);
    Dec(LSize, SizeOf(Byte));
    Inc(ADest, SizeOf(Byte));
    Inc(ALeft, SizeOf(Byte));
    Inc(ARight, SizeOf(Byte));
  until (LSize < SizeOf(Byte));
end;
{$endif}

procedure SetsDifference(ADest, ALeft: Pointer; ABit, ASize: Integer; AExchange: Boolean); overload;
var
  LRight: TSetBuffer;
begin
  if (not AExchange) then
  begin
    case ASize of
      1: TSet1(ADest^) := TSet1(ALeft^) - TSet1(Byte(1 shl ABit));
      2: TSet2(ADest^) := TSet2(ALeft^) - TSet2(Word(1 shl ABit));
      4: TSet4(ADest^) := TSet4(ALeft^) - TSet4(Integer(1 shl ABit));
    else
      LRight := SetBitInitialize(ABit);
      SetsDifference(ADest, ALeft, Pointer(@LRight), ASize);
    end;
  end else
  begin
    case ASize of
      1: TSet1(ADest^) := TSet1(Byte(1 shl ABit)) - TSet1(ALeft^);
      2: TSet2(ADest^) := TSet2(Word(1 shl ABit)) - TSet2(ALeft^);
      4: TSet4(ADest^) := TSet4(Integer(1 shl ABit))- TSet4(ALeft^);
    else
      LRight := SetBitInitialize(ABit);
      SetsDifference(ADest, Pointer(@LRight), ALeft, ASize);
    end;
  end;
end;

procedure SetsIntersection(ADest, ALeft, ARight: PByte; ASize: Integer); overload;
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  LSize: Integer;
begin
  LSize := ASize;

  if (LSize >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(ADest)^ := PNativeUInt(ALeft)^ and PNativeUInt(ARight)^;
    Dec(LSize, SizeOf(NativeUInt));
    Inc(ADest, SizeOf(NativeUInt));
    Inc(ALeft, SizeOf(NativeUInt));
    Inc(ARight, SizeOf(NativeUInt));
  until (LSize < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (LSize >= SizeOf(Cardinal)) then
  begin
    PCardinal(ADest)^ := PCardinal(ALeft)^ and PCardinal(ARight)^;
    Dec(LSize, SizeOf(Cardinal));
    Inc(ADest, SizeOf(Cardinal));
    Inc(ALeft, SizeOf(Cardinal));
    Inc(ARight, SizeOf(Cardinal));
  end;
  {$endif}

  if (LSize >= SizeOf(Byte)) then
  repeat
    PByte(ADest)^ := PByte(ALeft)^ and PByte(ARight)^;
    Dec(LSize, SizeOf(Byte));
    Inc(ADest, SizeOf(Byte));
    Inc(ALeft, SizeOf(Byte));
    Inc(ARight, SizeOf(Byte));
  until (LSize < SizeOf(Byte));
end;
{$endif}

procedure SetsIntersection(ADest, ALeft: Pointer; ABit, ASize: Integer); overload;
var
  LRight: TSetBuffer;
begin
  case ASize of
    1: TSet1(ADest^) := TSet1(ALeft^) * TSet1(Byte(1 shl ABit));
    2: TSet2(ADest^) := TSet2(ALeft^) * TSet2(Word(1 shl ABit));
    4: TSet4(ADest^) := TSet4(ALeft^) * TSet4(Integer(1 shl ABit));
  else
    LRight := SetBitInitialize(ABit);
    SetsIntersection(ADest, ALeft, Pointer(@LRight), ASize);
  end;
end;

procedure SetInvert(ADest, ALeft: PByte; AAndMasks, ASize: Integer);
{$ifdef CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
  LSize: Integer;
begin
  LSize := ASize;

  if (LSize >= SizeOf(NativeUInt)) then
  repeat
    PNativeUInt(ADest)^ := not PNativeUInt(ALeft)^;
    Dec(LSize, SizeOf(NativeUInt));
    Inc(ADest, SizeOf(NativeUInt));
    Inc(ALeft, SizeOf(NativeUInt));
  until (LSize < SizeOf(NativeUInt));

  {$ifdef LARGEINT}
  if (LSize >= SizeOf(Cardinal)) then
  begin
    PCardinal(ADest)^ := not PCardinal(ALeft)^;
    Dec(LSize, SizeOf(Cardinal));
    Inc(ADest, SizeOf(Cardinal));
    Inc(ALeft, SizeOf(Cardinal));
  end;
  {$endif}

  case LSize of
    3:
    begin
      PWord(ADest)^ := (not Integer(PWord(ALeft)^));
      Inc(ALeft, SizeOf(Word));
      Inc(ADest, SizeOf(Word));
      PByte(ADest)^ := (not Integer(PByte(ALeft)^)) and AAndMasks;
    end;
    2:
    begin
      PWord(ADest)^ := (not Integer(PWord(ALeft)^)) and AAndMasks;
    end;
    1:
    begin
      PByte(ADest)^ := (not Integer(PByte(ALeft)^)) and AAndMasks;
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

  {$if Defined(FPC) or (CompilerVersion < 20)}
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

function IsBooleanType(const AValue: PTypeInfo): Boolean;
var
  LBase: PTypeInfo;
  LBaseNext: Pointer;
  LTypeData: PTypeData;
  S: PByte;
begin
  Result := Assigned(AValue) and (AValue.Kind = tkEnumeration);
  if (not Result) then
  begin
    {$ifdef FPC}Result := Assigned(AValue) and (AValue.Kind = tkBool);{$endif}
    Exit;
  end;

  LBase := AValue;
  repeat
    LBaseNext := GetTypeData(LBase)^.BaseType;
    {$ifNdef FPC}
    if (Assigned(LBaseNext)) then
    begin
      LBaseNext := PPointer(LBaseNext)^;
    end;
    {$endif}

    if (not Assigned(LBaseNext)) or (LBaseNext = LBase) then Break;
    LBase := LBaseNext;
  until (False);

  Result := (LBase = System.TypeInfo(Boolean)) or
    (LBase = System.TypeInfo(ByteBool)) or
    (LBase = System.TypeInfo(WordBool)) or
    (LBase = System.TypeInfo(LongBool));

  if (not Result) then
  begin
    LTypeData := GetTypeData(LBase);
    if (LTypeData.MinValue = 0) and (LTypeData.MaxValue = 1) then // match C++ bool
    begin
      S := Pointer(@LBase.Name);
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

function IsReferenceMethodType(const AValue: PTypeInfo): Boolean;
var
  LTypeData: PTypeData;
  LTable: PIntfMethodTable;
  LMethodEntry: PIntfMethodEntry;
  LName: ^HugeByteArray;
begin
  Result := False;
  if (not Assigned(AValue)) or (AValue.Kind <> tkInterface) then
    Exit;

  LTypeData := GetTypeData(AValue);
  if (not Assigned(LTypeData.IntfParent)) or (LTypeData.IntfParent{$ifNdef FPC}^{$endif} <> TypeInfo(IInterface)) then
    Exit;

  LTable := GetTail(LTypeData.IntfUnit);
  LMethodEntry := Pointer(NativeUInt(LTable) + SizeOf(TIntfMethodTable));
  if (LTable.Count <> 1) then
    Exit;

  case LTable.RttiCount of
    $ffff: Result := (PByte(@LMethodEntry.Name)^ = 2){no RTTI reference};
    1:
    begin
      LName := Pointer(@LMethodEntry.Name);
      Result := (LName[0] = 6) and (LName[1] = Ord('I')) and (LName[2] = Ord('n')) and
        (LName[3] = Ord('v')) and (LName[4] = Ord('o')) and (LName[5] = Ord('k')) and (LName[6] = Ord('e'));
    end;
  else
    Exit;
  end;
end;

function IsWeakTypeInfo(const AValue: PTypeInfo): Boolean;
{$ifdef WEAKREF}
var
  i: Cardinal;
  LWeakMode: Boolean;
  LFieldTable: PAnonymousFieldTable;
begin
  Result := False;

  if Assigned(AValue) then
  case AValue.Kind of
    {$ifdef WEAKINSTREF}
    tkMethod:
    begin
      Result := True;
    end;
    {$endif}
    tkArray{static array}:
    begin
      LFieldTable := PAnonymousFieldTable(NativeUInt(AValue) + PByte(@AValue.Name)^);
      if (LFieldTable.Fields[0].TypeInfo <> nil) then
        Result := IsWeakTypeInfo(LFieldTable.Fields[0].TypeInfo^);
    end;
    tkRecord{$ifdef FPC}, tkObject{$endif}:
    begin
      LFieldTable := PAnonymousFieldTable(NativeUInt(AValue) + PByte(@AValue.Name)^);
      if LFieldTable.Count > 0 then
      begin
        LWeakMode := False;
        for i := 0 to LFieldTable.Count - 1 do
        begin
          if LFieldTable.Fields[i].TypeInfo = nil then
          begin
            LWeakMode := True;
            Continue;
          end;
          if (not LWeakMode) then
          begin
            if (IsWeakTypeInfo(LFieldTable.Fields[i].TypeInfo^)) then
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

function IsManagedTypeInfo(const AValue: PTypeInfo): Boolean;
var
  i: Cardinal;
  {$ifdef WEAKREF}
  LWeakMode: Boolean;
  {$endif}
  LFieldTable: PAnonymousFieldTable;
begin
  Result := False;

  if Assigned(AValue) then
  case AValue.Kind of
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
      LFieldTable := PAnonymousFieldTable(NativeUInt(AValue) + PByte(@AValue.Name)^);
      if (LFieldTable.Fields[0].TypeInfo <> nil) then
        Result := IsManagedTypeInfo(LFieldTable.Fields[0].TypeInfo^);
    end;
    tkRecord{$ifdef FPC}, tkObject{$endif}:
    begin
      LFieldTable := PAnonymousFieldTable(NativeUInt(AValue) + PByte(@AValue.Name)^);
      if LFieldTable.Count > 0 then
      begin
        {$ifdef WEAKREF}
        LWeakMode := False;
        {$endif}
        for i := 0 to LFieldTable.Count - 1 do
        begin
          {$ifdef WEAKREF}
          if LFieldTable.Fields[i].TypeInfo = nil then
          begin
            LWeakMode := True;
            Continue;
          end;
          if (not LWeakMode) then
          begin
          {$endif}
            if (IsManagedTypeInfo(LFieldTable.Fields[i].TypeInfo^)) then
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

{$ifdef FPC}
function int_copy(Src, Dest, TypeInfo: Pointer): SizeInt; [external name 'FPC_COPY'];
procedure int_initialize(Data, TypeInfo: Pointer); [external name 'FPC_INITIALIZE'];
procedure int_finalize(Data, TypeInfo: Pointer); [external name 'FPC_FINALIZE'];
procedure int_safecallcheck(Value: HRESULT); [external name 'FPC_SAFECALLCHECK'];
{$endif}

procedure CopyRecord(Dest, Source, TypeInfo: Pointer);
{$if Defined(FPC)}
begin
  int_copy(Source, Dest, TypeInfo);
end;
{$elseif Defined(CPUINTEL)}
asm
  jmp System.@CopyRecord
end;
{$else}
begin
  System.CopyArray(Dest, Source, TypeInfo, 1);
end;
{$ifend}

{$if Defined(FPC) or (CompilerVersion <= 20)}
procedure CopyArray(Dest, Source, TypeInfo: Pointer; Count: NativeUInt);
{$ifdef FPC}
var
  i, LItemSize: NativeInt;
  LItemDest, LItemSrc: Pointer;
begin
  LItemDest := Dest;
  LItemSrc := Source;

  case PTypeInfo(TypeInfo).Kind of
    tkVariant: LItemSize := SizeOf(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: LItemSize := SizeOf(Pointer);
      tkArray, tkRecord, tkObject: LItemSize := PTypeData(NativeUInt(TypeInfo) + PByte(@PTypeInfo(TypeInfo).Name)^).RecSize;
  else
    Exit;
  end;

  for i := 1 to Count do
  begin
    int_copy(LItemSrc, LItemDest, TypeInfo);

    Inc(NativeInt(LItemDest), LItemSize);
    Inc(NativeInt(LItemSrc), LItemSize);
  end;
end;
{$else}
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
{$endif}

procedure InitializeArray(Source, TypeInfo: Pointer; Count: NativeUInt);
{$ifdef FPC}
var
  i, LItemSize: NativeInt;
  LItemPtr: Pointer;
begin
  LItemPtr := Source;

  case PTypeInfo(TypeInfo).Kind of
    tkVariant: LItemSize := SizeOf(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: LItemSize := SizeOf(Pointer);
      tkArray, tkRecord, tkObject: LItemSize := PTypeData(NativeUInt(TypeInfo) + PByte(@PTypeInfo(TypeInfo).Name)^).RecSize;
  else
    Exit;
  end;

  for i := 1 to Count do
  begin
    int_initialize(LItemPtr, TypeInfo);
    Inc(NativeInt(LItemPtr), LItemSize);
  end;
end;
{$else}
asm
  jmp System.@InitializeArray
end;
{$endif}

procedure FinalizeArray(Source, TypeInfo: Pointer; Count: NativeUInt);
{$ifdef FPC}
var
  i, LItemSize: NativeInt;
  LItemPtr: Pointer;
begin
  LItemPtr := Source;

  case PTypeInfo(TypeInfo).Kind of
    tkVariant: LItemSize := SizeOf(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: LItemSize := SizeOf(Pointer);
      tkArray, tkRecord, tkObject: LItemSize := PTypeData(NativeUInt(TypeInfo) + PByte(@PTypeInfo(TypeInfo).Name)^).RecSize;
  else
    Exit;
  end;

  for i := 1 to Count do
  begin
    int_finalize(LItemPtr, TypeInfo);
    Inc(NativeInt(LItemPtr), LItemSize);
  end;
end;
{$else}
asm
  jmp System.@FinalizeArray
end;
{$endif}
{$ifend}

procedure CopyObject(const ADest, ASrc: TObject);
var
  LInitTable: Pointer;
  LBaseSize, LDestSize: NativeInt;
  LBaseClass, LDestClass, LSrcClass: TClass;
begin
  if (ADest = nil) or (ASrc = nil) then Exit;

  LDestClass := TClass(Pointer(ADest)^);
  LSrcClass := TClass(Pointer(ASrc)^);

  if (LDestClass = LSrcClass) then LBaseClass := LDestClass
  else
  if (LDestClass.InheritsFrom(LSrcClass)) then LBaseClass := LSrcClass
  else
  if (LSrcClass.InheritsFrom(LDestClass)) then LBaseClass := LDestClass
  else
  begin
    LBaseClass := LDestClass;

    while (LBaseClass <> nil) and (not LSrcClass.InheritsFrom(LBaseClass)) do
    begin
      LBaseClass := LBaseClass.ClassParent;
    end;

    if (LBaseClass = nil) then Exit;
  end;

  {todo Monitor}

  LDestSize := LBaseClass.InstanceSize;
  while (LBaseClass <> TObject) do
  begin
    LInitTable := PPointer(Integer(LBaseClass) + vmtInitTable)^;
    if (LInitTable <> nil) then
    begin
      CopyRecord(Pointer(ADest), Pointer(ASrc), LInitTable);
      Break;
    end;
    LBaseClass := LBaseClass.ClassParent;
  end;

  LBaseSize := LBaseClass.InstanceSize;
  if (LBaseSize <> LDestSize) then
  begin
    System.Move(Pointer(NativeInt(ASrc) + LBaseSize)^,
      Pointer(NativeInt(ADest) + LBaseSize)^, LDestSize - LBaseSize);
  end;
end;

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

function UniqueLuaBuffer(var AData: __luabuffer): NativeInt;
begin
  Result := NativeInt(AData);
  if (Result = 0) then Exit;

  {$ifdef NEXTGEN}
    if (PDynArrayRec(Result - SizeOf(TDynArrayRec)).RefCount <> 1) then
      Data := System.Copy(Data, Low(Data), Length(Data));
  {$else}
    UniqueString(AnsiString(AData));
  {$endif}

  Result := NativeInt(AData) - Result;
end;


{ Low level helpers }

function IsMemoryFilledZero(ASource: PByte; ASize: NativeUInt): Boolean;
label
  zero, non_zero;
begin
  if (ASize <> 0) then
  begin
    if (ASize >= SizeOf(NativeUInt)) then
    repeat
      if (PNativeUInt(ASource)^ <> 0) then goto non_zero;
      Dec(ASize, SizeOf(NativeUInt));
      Inc(ASource, SizeOf(NativeUInt));
    until (ASize < SizeOf(NativeUInt));

    {$ifdef LARGEINT}
    if (ASize >= SizeOf(Cardinal)) then
    begin
      if (PCardinal(ASource)^ <> 0) then goto non_zero;
      Dec(ASize, SizeOf(Cardinal));
      Inc(ASource, SizeOf(Cardinal));
    end;
    {$endif}

    if (ASize >= SizeOf(Word)) then
    begin
      if (PWord(ASource)^ <> 0) then goto non_zero;
      Dec(ASize, SizeOf(Word));
      Inc(ASource, SizeOf(Word));
    end;

    if (ASize <> 0) then
    begin
      if (PByte(ASource)^ <> 0) then goto non_zero;
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
  LStart: PAnsiChar;
  LAlign: NativeInt;
  X, V: NativeInt;
begin
  LStart := S;
  if (S = nil) then goto done;

  LAlign := NativeInt(S) and (CHARS_IN_CARDINAL - 1);
  if (LAlign <> 0) then
  repeat
    if (Byte(S^) = 0) then goto done;
    Inc(LAlign);
    Inc(S);
  until (LAlign = CHARS_IN_CARDINAL);

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
  Result := NativeUInt(S) - NativeUInt(LStart);
end;

function WStrLen(S: PWideChar): NativeUInt;
label
  done;
var
  LStart: PWideChar;
  X: Cardinal;
begin
  LStart := S;
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
  Result := (NativeUInt(S) - NativeUInt(LStart)) shr 1;
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

function NumberToInteger({$ifdef CPUX86}var{$else}const{$endif} ANumber: Double;
  var AInteger: Integer): Boolean;
const
  DBLROUND_CONST{$ifdef CPUX86}: Double{$endif} = 6755399441055744.0;
var
  ATemp: record
  case Integer of
    0: (VDouble: Double);
    1: (VInteger: Integer);
  end;
begin
  ATemp.VDouble := ANumber + DBLROUND_CONST;
  if (ATemp.VInteger <> ANumber) then
  begin
    Result := False;
    Exit;
  end else
  begin
    AInteger := ATemp.VInteger;
    Result := True;
  end;
end;

// 0: TDateTime, 1: TDate, 2: TTime
function InspectDateTime({$ifdef CPUX86}var{$else}const{$endif} AValue: TDateTime): Integer;
const
  DBLROUND_CONST{$ifdef CPUX86}: Double{$endif} = 6755399441055744.0;
var
  LTemp: record
  case Integer of
    0: (VDouble: Double);
    1: (VInteger: Integer);
  end;
begin
  // check 2: TTime
  {$ifdef CPUX86}
  if (PPoint(@AValue)^.Y >= 0) and (PInt64(@AValue)^ < 4607182418800017408) then
  {$else}
  if (AValue >= 0) and (AValue < 1.0) then
  {$endif}
  begin
    Result := 2;
    Exit;
  end;

  // 0: TDateTime, 1: TDate
  LTemp.VDouble := AValue + DBLROUND_CONST;
  Result := Byte(LTemp.VInteger = AValue);
end;

function IsValidIdent(const AIdent: LuaString): Boolean;
var
  i, LCount: NativeInt;
  S: PLuaChar;
begin
  Result := False;
  LCount := Length(AIdent);
  if (LCount = 0) then Exit;
  S := Pointer(AIdent);

  if (S^ <> '_') and (not
    {$if Defined(FPC) or not Defined(UNICODE)}
      IsCharAlphaW(S^)
    {$elseif (CompilerVersion < 25)}
      IsLetter(S^)
    {$else}
      S^.IsLetter
    {$ifend}
  ) then Exit;

  Inc(S);
  for i := 1 to LCount - 1 do
  begin
    case S^ of
      '_'{$ifdef UNITSCOPENAMES}, '.'{$endif}: ;
    else
      if (not
        {$if Defined(FPC) or not Defined(UNICODE)}
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
    procedure Init(const ALua: TLua; const P1, P2: __luapointer; const ACallback: Pointer);
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

    function Alloc(const ALua: TLua; const ACallback: Pointer; const P1, P2: __luapointer): Pointer;
    procedure Free(const ALuaCFunction: Pointer);
    procedure Clear;
  end;

function BitScan16(const AValue: Integer{Word}): NativeInt;
begin
  if (AValue <> 0) then
  begin
    if (AValue and $ff <> 0) then
    begin
      Result := BIT_SCANS[Byte(AValue)];
    end else
    begin
      Result := 8 + BIT_SCANS[AValue shr 8];
    end;
  end else
  begin
    Result := -1;
  end;
end;

procedure TLuaCFunctionData.Init(const ALua: TLua; const P1, P2: __luapointer; const ACallback: Pointer);
var
  LOffset: NativeInt;
begin
  {$ifdef CPUX86}
    // mov eax, Lua
    Bytes[0] := $B8;
    PPointer(@Bytes[1])^ := ALua;
    // mov edx, P1
    Bytes[5] := $BA;
    PInteger(@Bytes[6])^ := P1;
    // mov ecx, P2
    Bytes[10] := $B9;
    PInteger(@Bytes[11])^ := P2;
    // jmp Callback
    LOffset := NativeInt(ACallback) - (NativeInt(@Bytes[15]) + 5);
    Bytes[15] := $E9;
    PInteger(@Bytes[16])^ := LOffset;
  {$endif}

  {$ifdef CPUX64}
    // mov rcx, Lua
    Bytes[0] := $48;
    Bytes[1] := $B9;
    PPointer(@Bytes[2])^ := ALua;
    // mov edx, P1
    Bytes[10] := $BA;
    PInteger(@Bytes[11])^ := P1;
    // mov r8d, P2
    Bytes[15] := $41;
    Bytes[16] := $B8;
    PInteger(@Bytes[17])^ := P2;
    // jump
    LOffset := NativeInt(ACallback) - (NativeInt(@Bytes[21]) + 5);
    case Integer(LOffset shr 32) of
      -1, 0:
      begin
        // jmp Callback
        Bytes[21] := $E9;
        PInteger(@Bytes[22])^ := LOffset;
      end;
    else
      // mov rax, Callback
      Bytes[21] := $48;
      Bytes[22] := $B8;
      PPointer(@Bytes[23])^ := ACallback;
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
  i, LCount: NativeInt;
  LList: ^TList;
begin
  MarkerLow := MEMORY_PAGE_MARKER_LOW;
  MarkerHigh := MEMORY_PAGE_MARKER_HIGH;

  LList := Pointer(@Self);
  if (NativeInt(LList) and MEMORY_BLOCK_TEST <> 0) then
  begin
    Inc(NativeInt(LList), SizeOf(TLuaCFunctionPage));
    LCount := (MEMORY_PAGE_SIZE - SizeOf(TLuaCFunctionPage)) div SizeOf(TLuaCFunctionData);
  end else
  begin
    Inc(NativeInt(LList), SizeOf(TLuaCFunctionBlock));
    LCount := (MEMORY_PAGE_SIZE - SizeOf(TLuaCFunctionBlock)) div SizeOf(TLuaCFunctionData);
  end;

  for i := 0 to LCount - 2 do
  begin
    PPointer(@LList[i])^ := @LList[i + 1];
  end;
  PPointer(@LList[LCount - 1])^ := nil;

  Items := @LList[0];
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
  LCount: Cardinal;
begin
  PPointer(P)^ := Items;
  Items := P;

  LCount := Allocated - 1;
  Allocated := LCount;
  Result := (LCount = 0);
end;

procedure TLuaCFunctionHeap.Clear;
var
  LBlock, LNext: PLuaCFunctionBlock;
begin
  LBlock := Blocks;
  Blocks := nil;

  while (LBlock <> nil) do
  begin
    LNext := LBlock.Next;
    VirtualFree(LBlock, 0, MEM_RELEASE);
    LBlock := LNext;
  end;
end;

function TLuaCFunctionHeap.Alloc(const ALua: TLua; const ACallback: Pointer; const P1, P2: __luapointer): Pointer;
var
  LIndex: NativeInt;
  LBlock: PLuaCFunctionBlock;
  LPage: PLuaCFunctionPage;

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

  LBlock := Self.Blocks;
  while (LBlock <> nil) do
  begin
    LIndex := BitScan16(LBlock.Empties);
    if (LIndex >= 0) then
    begin
      LPage := Pointer(NativeInt(LBlock) + LIndex * MEMORY_PAGE_SIZE);
      Result := LPage.Alloc;
      if (LPage.Items = nil) then
      begin
        LBlock.Empties := LBlock.Empties and (not (1 shl LIndex));
      end;
      Break;
    end else
    if (LBlock.Reserved <> $ffff) then
    begin
      LIndex := BitScan16(not LBlock.Reserved);
      Result := CommitPage(LBlock, LIndex).Alloc;
      Break;
    end;

    LBlock := LBlock.Next;
  end;

  if (Result = nil) then
  begin
    LBlock := VirtualAlloc(nil, MEMORY_BLOCK_SIZE, MEM_RESERVE, PAGE_EXECUTE_READWRITE);

    if (LBlock <> nil) then
    begin
      Result := CommitPage(LBlock, 0).Alloc;
      LBlock.Next := Self.Blocks;
      Self.Blocks := LBlock;
    end;
  end;

  if (Result = nil) then System.Error(reOutOfMemory);
  PLuaCFunctionData(Result).Init(ALua, P1, P2, ACallback);
end;

procedure TLuaCFunctionHeap.Free(const ALuaCFunction: Pointer);
var
  LIndex: NativeInt;
  LPage: PLuaCFunctionPage;
  LBlock, LItem: PLuaCFunctionBlock;
  LParent: ^PLuaCFunctionBlock;
begin
  LPage := Pointer(NativeInt(ALuaCFunction) and MEMORY_PAGE_CLEAR);
  LBlock := Pointer(NativeInt(LPage) and MEMORY_BLOCK_CLEAR);
  LIndex := NativeInt(NativeUInt(LPage) div MEMORY_PAGE_SIZE) and 15;
  if (not LPage.Free(ALuaCFunction)) then
  begin
    LBlock.Empties := LBlock.Empties or (1 shl LIndex);
    Exit;
  end;

  // decommit page (not first)
  if (LIndex <> 0) then
  begin
    LBlock.Empties := LBlock.Empties and (not (1 shl LIndex));
    LBlock.Reserved := LBlock.Reserved and (not (1 shl LIndex));
    VirtualFree(LPage, MEMORY_PAGE_SIZE, MEM_DECOMMIT);
  end;

  // check empty block
  if (LBlock.Empties <> 1) or (LBlock.Allocated <> 0) then
  begin
    Exit;
  end;

  // remove block
  LParent := @Blocks;
  repeat
    LItem := LParent^;
    if (LItem = LBlock) then
    begin
      LParent^ := LItem.Next;
      VirtualFree(LItem, 0, MEM_RELEASE);
      Break;
    end;

    LParent := @LItem.Next;
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
    procedure Push(const AValue: Integer);
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

    procedure SetCapacity(const AValue: NativeInt);
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
    function NewItem(const AKey: Pointer): PLuaDictionaryItem;
    function InternalFind(const AKey: Pointer; const AModeCreate: Boolean): PLuaDictionaryItem;
  public
    procedure Clear;
    procedure TrimExcess;
    procedure Assign(const AValue: TLuaDictionary);
    function Find(const AKey: Pointer): PLuaDictionaryItem; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure Add(const AKey: Pointer; const AValue: __luapointer);
    function FindValue(const AKey: Pointer): __luapointer;

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
  TLuaCustomStringDictionaryOnProcessKey = procedure(const ASelf: PLuaCustomStringDictionary;
    const AChars: PByte; const ALength: Integer);

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
    procedure InitBuffer(const AKey: LuaString); overload;
    procedure InitBuffer(const ARttiName: ShortString); overload;
    function GetHashCode(const AChars: PByte; const ALength: Integer): Integer;
    function InternalAdd(const AHashCode: Integer; const AValue: Pointer): PLuaCustomStringDictionaryItem;
  public
    procedure Clear;
    procedure TrimExcess;

    property Lua: TLua read FLua write FLua;
    property Capacity: NativeInt read FCapacity;
    property Count: NativeInt read FCount;
  end;

  TLuaNames = object(TLuaCustomStringDictionary){TDictionary<LuaString,__luaname>}
  private
    procedure InternalProcessKey(const AChars: PByte; const ALength: Integer);
    function InternalFind(const AModeCreate: Boolean): PLuaCustomStringDictionaryItem;
  public
    procedure Init(const ALua: TLua);
    function Add(const AKey: LuaString): __luaname; overload;
    function Add(const ARttiName: ShortString): __luaname; overload;
    function FindValue(const AKey: LuaString): __luaname; overload;
    function FindValue(const ARttiName: ShortString): __luaname; overload;
  end;

  PLuaRegisteredTypeName = ^TLuaRegisteredTypeName;
  TLuaRegisteredTypeName = record
    TypeInfo: PTypeInfo;
    PointerDepth: Integer;
    Chars: array[0..0] of Byte;
  end;

  TLuaRegisteredTypeNames = object(TLuaCustomStringDictionary){TDictionary<LuaString,PLuaRegisteredTypeName>}
  private
    procedure InternalProcessKey(const AChars: PByte; const ALength: Integer);
    function InternalFind(const AOffset: Integer; const AModeCreate: Boolean): PLuaCustomStringDictionaryItem;
  public
    procedure Init(const ALua: TLua);
    procedure Add(const AName: LuaString; const ATypeInfo: PTypeInfo; const APointerDepth: Integer);
    function Find(const ARttiName: ShortString): PLuaRegisteredTypeName;
  end;


// x86 architecture compatibility
{$ifNdef CPUX86}
function Swap(const X: NativeUInt): NativeUInt; inline;
begin
  Result := (Byte(X) shl 8) + Byte(X shr 8);
end;
{$endif}

procedure SwapPtr(var ALeft, ARight: Pointer);
var
  LTemp: Pointer;
begin
  LTemp := ALeft;
  ALeft := ARight;
  ARight := LTemp;
end;

procedure TLuaStack.Clear;
begin
  FItems := nil;
  FCount := 0;
  FCapacity := 0;
end;

procedure TLuaStack.Push(const AValue: Integer);
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
    FItems[C] := AValue;
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
  LBufferSize, LCount: NativeInt;
begin
  // buffer size
  LBufferSize := (ASize + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer);
  if (LBufferSize < HEAP_BUFFER_SIZE) then LBufferSize := HEAP_BUFFER_SIZE;

  // allocate buffer
  LCount := Length(FBuffers);
  SetLength(FBuffers, LCount + 1);
  SetLength(FBuffers[LCount], LBufferSize);

  // new current pointer
  FCurrent := NativeInt(FBuffers[LCount]);
  {$ifdef LARGEINT}
  if (NativeUInt(FCurrent) > NativeUInt(High(Integer))) or
    (NativeUInt(FCurrent + LBufferSize - 1) > NativeUInt(High(Integer))) then
    FCurrent := (LCount shl HEAP_BUFFER_SHIFT) + (NativeInt(1) shl 31);
  {$endif}

  // allocate size
  FMargin := LBufferSize;
  Result := Alloc(ASize);
end;

function TLuaMemoryHeap.Alloc(const ASize: NativeInt): __luapointer;
var
  LSize, LNewMargin, LNewCurrent: NativeInt;
begin
  LSize := (ASize + (SizeOf(Pointer) - 1)) and -SizeOf(Pointer);
  LNewMargin := FMargin - LSize;
  if (LNewMargin < 0) then
  begin
    Result := GrowAlloc(LSize);
  end else
  begin
    FMargin := LNewMargin;
    LNewCurrent := FCurrent + LSize;
    FCurrent := LNewCurrent;
    Result := __luapointer(LNewCurrent - LSize);
  end;
end;

{$ifdef LARGEINT}
function TLuaMemoryHeap.ExtendedUnpack(const APtr: __luapointer): Pointer;
var
  LValue: NativeInt;
begin
  LValue := NativeInt(Cardinal(APtr xor (1 shl 31)));
  Result := Pointer(FBuffers[LValue shr HEAP_BUFFER_SHIFT]);
  Inc(NativeInt(Result), LValue and HEAP_BUFFER_MASK);
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

procedure TLuaBuffer.SetCapacity(const AValue: NativeInt);
var
  LNewBytes: TBytes;
begin
  if (AValue = FCapacity) then
    Exit;
  if (AValue < FSize) then
    raise ELua.CreateFmt('Invalid capacity value %d, items count: %d', [AValue, FSize]);

  if (AValue < FSize) then
  begin
    SetLength(LNewBytes, AValue);
    System.Move(Pointer(FBytes)^, Pointer(LNewBytes)^, FSize);
    FBytes := LNewBytes;
  end else
  begin
    SetLength(FBytes, AValue);
  end;

  FCapacity := AValue;
end;

function TLuaBuffer.Alloc(const ASize: NativeInt): Pointer;
var
  LNewSize: NativeInt;

  function EInvalidSize(const ASize: NativeInt): ELua; far;
  begin
    Result := ELua.CreateFmt('Invalid allocated size: %d', [ASize]);
  end;
begin
  LNewSize := Self.FSize + ASize;

  if (ASize > 0) then
  begin
    if (LNewSize <= FCapacity) then
    begin
      FSize := LNewSize;
      Dec(LNewSize, ASize);
      Result := @FBytes[LNewSize];
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
  LNewSize, LNewCapacity: NativeInt;
begin
  LNewSize := Self.FSize + ASize;
  LNewCapacity := Self.FCapacity;
  if (LNewCapacity = 0) then LNewCapacity := 16;

  while (LNewCapacity < LNewSize) do LNewCapacity := LNewCapacity * 2;
  Self.Capacity := LNewCapacity;

  FSize := LNewSize;
  Dec(LNewSize, ASize);
  Result := @FBytes[LNewSize];
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
  LNewItems: array of TLuaDictionaryItem;
begin
  SetLength(LNewItems, FCount);
  System.Move(Pointer(FItems)^, Pointer(LNewItems)^, FCount * SizeOf(TLuaDictionaryItem));
  SwapPtr(Pointer(FItems), Pointer(LNewItems));
  FCapacity := FCount;
end;

procedure TLuaDictionary.Assign(const AValue: TLuaDictionary);
var
  LNewCount: Integer;
  LNewItems: array of TLuaDictionaryItem;
  LNewHashes: array of Integer;
begin
  LNewCount := Length(AValue.FItems);
  SetLength(LNewItems, LNewCount);
  System.Move(Pointer(AValue.FItems)^, Pointer(LNewItems)^, LNewCount * SizeOf(TLuaDictionaryItem));

  LNewCount := Length(AValue.FHashes);
  SetLength(LNewHashes, LNewCount);
  System.Move(Pointer(AValue.FHashes)^, Pointer(LNewHashes)^, LNewCount * SizeOf(Integer));

  SwapPtr(Pointer(Self.FItems), Pointer(LNewItems));
  SwapPtr(Pointer(Self.FHashes), Pointer(LNewHashes));
  Self.FHashesMask := AValue.FHashesMask;
  Self.FCapacity := AValue.FCapacity;
  Self.FCount := AValue.FCount;
end;

procedure TLuaDictionary.Grow;
var
  i: NativeInt;
  LItem: PLuaDictionaryItem;
  LHashCode: Integer;
  LParent: PInteger;
  LPow2, LNewHashesMask, LNewCapacity: NativeInt;
  LNewHashes: array of Integer;
begin
  LPow2 := FHashesMask;
  if (LPow2 <> 0) then
  begin
    Inc(LPow2);
    LNewCapacity := (LPow2 shr 2) + (LPow2 shr 1);
    if (LNewCapacity = Count) then
    begin
      LPow2 := LPow2 * 2;
      SetLength(LNewHashes, LPow2);
      FillChar(Pointer(LNewHashes)^, LPow2 * SizeOf(Integer), $ff);

      LNewHashesMask := (LPow2 - 1);
      LItem := Pointer(FItems);
      for i := 0 to Count - 1 do
      begin
        {$ifdef LARGEINT}
          LHashCode := (NativeInt(LItem.Key) shr 4) xor (NativeInt(LItem.Key) shr 32);
        {$else .SMALLINT}
          LHashCode := NativeInt(LItem.Key) shr 4;
        {$endif}
        Inc(LHashCode, (LHashCode shr 16) * -1660269137);

        LParent := @LNewHashes[NativeInt(LHashCode) and LNewHashesMask];
        LItem.Next := LParent^;
        LParent^ := i;

        Inc(LItem);
      end;

      FHashesMask := LNewHashesMask;
      LNewCapacity := (LPow2 shr 2) + (LPow2 shr 1);
      SwapPtr(Pointer(FHashes), Pointer(LNewHashes));
    end;
    SetLength(FItems, LNewCapacity);
    FCapacity := LNewCapacity;
  end else
  begin
    SetLength(FItems, 3);
    SetLength(FHashes, 4);
    System.FillChar(Pointer(FHashes)^, 4 * SizeOf(Integer), $ff);
    FHashesMask := 3;
    FCapacity := 3;
  end;
end;

function TLuaDictionary.NewItem(const AKey: Pointer): PLuaDictionaryItem;
label
  start;
var
  LIndex: NativeInt;
  LHashCode: Integer;
  LParent: PInteger;
begin
start:
  LIndex := FCount;
  if (LIndex <> FCapacity) then
  begin
    Inc(LIndex);
    FCount := LIndex;
    Dec(LIndex);

    {$ifdef LARGEINT}
      LHashCode := (NativeInt(AKey) shr 4) xor (NativeInt(AKey) shr 32);
    {$else .SMALLINT}
      LHashCode := NativeInt(AKey) shr 4;
    {$endif}
    Inc(LHashCode, (LHashCode shr 16) * -1660269137);

    LParent := @FHashes[NativeInt(LHashCode) and FHashesMask];
    Result := @FItems[LIndex];
    Result.Key := AKey;
    Result.Value := LUA_POINTER_INVALID;
    Result.Next := LParent^;
    LParent^ := LIndex;
  end else
  begin
    Grow;
    goto start;
  end;
end;

function TLuaDictionary.InternalFind(const AKey: Pointer; const AModeCreate: Boolean): PLuaDictionaryItem;
var
  LHashCode: Integer;
  LHashesMask: NativeInt;
  LIndex: NativeInt;
begin
  {$ifdef LARGEINT}
    LHashCode := (NativeInt(AKey) shr 4) xor (NativeInt(AKey) shr 32);
  {$else .SMALLINT}
    LHashCode := NativeInt(AKey) shr 4;
  {$endif}
  Inc(LHashCode, (LHashCode shr 16) * -1660269137);

  LHashesMask := FHashesMask;
  if (LHashesMask <> 0) then
  begin
    LIndex := FHashes[NativeInt(LHashCode) and LHashesMask];
    if (LIndex >= 0) then
    repeat
      Result := @FItems[LIndex];
      if (Result.Key = AKey) then Exit;
      LIndex := Result.Next;
    until (LIndex < 0);
  end;

  if (AModeCreate) then
  begin
    Result := NewItem(AKey);
  end else
  begin
    Result := nil;
  end;
end;

function TLuaDictionary.Find(const AKey: Pointer): PLuaDictionaryItem;
{$ifdef INLINESUPPORT}
begin
  Result := InternalFind(AKey, False);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  xor ecx, ecx
  jmp TLuaDictionary.InternalFind
end;
{$endif}

procedure TLuaDictionary.Add(const AKey: Pointer; const AValue: __luapointer);
begin
  InternalFind(AKey, True).Value := AValue;
end;

function TLuaDictionary.FindValue(const AKey: Pointer): __luapointer;
var
  LItem: PLuaDictionaryItem;
begin
  LItem := InternalFind(AKey, False);
  Result := LUA_POINTER_INVALID;
  if (Assigned(LItem)) then
    Result := LItem.Value;
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
  LNewItems: array of TLuaCustomStringDictionaryItem;
begin
  SetLength(LNewItems, FCount);
  System.Move(Pointer(FItems)^, Pointer(LNewItems)^, FCount * SizeOf(TLuaCustomStringDictionaryItem));
  System.FillChar(Pointer(FItems)^, FCount * SizeOf(TLuaCustomStringDictionaryItem), #0);
  SwapPtr(Pointer(FItems), Pointer(LNewItems));
  FCapacity := FCount;
end;

procedure TLuaCustomStringDictionary.Grow;
var
  i: NativeInt;
  LItem: PLuaCustomStringDictionaryItem;
  LParent: PInteger;
  LPow2, LNewHashesMask, LNewCapacity: NativeInt;
  LNewHashes: array of Integer;
begin
  LPow2 := FHashesMask;
  if (LPow2 <> 0) then
  begin
    Inc(LPow2);
    LNewCapacity := (LPow2 shr 2) + (LPow2 shr 1);
    if (LNewCapacity = Count) then
    begin
      LPow2 := LPow2 * 2;
      SetLength(LNewHashes, LPow2);
      FillChar(Pointer(LNewHashes)^, LPow2 * SizeOf(Integer), $ff);

      LNewHashesMask := (LPow2 - 1);
      LItem := Pointer(FItems);
      for i := 0 to Count - 1 do
      begin
        LParent := @LNewHashes[NativeInt(LItem.HashCode) and LNewHashesMask];
        LItem.Next := LParent^;
        LParent^ := i;

        Inc(LItem);
      end;

      FHashesMask := LNewHashesMask;
      LNewCapacity := (LPow2 shr 2) + (LPow2 shr 1);
      SwapPtr(Pointer(FHashes), Pointer(LNewHashes));
    end;
    SetLength(FItems, LNewCapacity);
    FCapacity := LNewCapacity;
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

procedure TLuaCustomStringDictionary.InitBuffer(const AKey: LuaString);
var
  LLength, LSize: Integer;
  LBuffer: ^TLuaBuffer;
begin
  LLength := System.Length(AKey);
  LBuffer := Pointer(@FLua.FInternalBuffer);
  LBuffer.Size := 0;

  if (Pointer(AKey) <> nil) then
  begin
    // Utf8 <-- Utf16
    LSize := LLength * 3 + 1;
    if (LBuffer.Capacity < LSize) then LBuffer.Alloc(LSize);
    LBuffer.Size := Utf8FromUnicode(Pointer(LBuffer.FBytes), Pointer(AKey), LLength);
  end;

  if (Assigned(FOnProcessKey)) then
    FOnProcessKey(@Self, Pointer(LBuffer.FBytes), LBuffer.Size);
end;

procedure TLuaCustomStringDictionary.InitBuffer(const ARttiName: ShortString);
var
  LLength, LSize: Integer;
  LBuffer: ^TLuaBuffer;
begin
  LLength := PByte(@ARttiName)^;
  LBuffer := Pointer(@FLua.FInternalBuffer);
  LBuffer.Size := 0;

  if (LLength <> 0) then
  begin
    {$ifdef UNICODE}
      LSize := LLength + 3;
      if (LBuffer.Capacity < LSize) then LBuffer.Alloc(LSize);
      LBuffer.Size := FLua.AnsiFromUtf8(Pointer(LBuffer.FBytes), 0, Pointer(@ARttiName[1]), LLength);
    {$else .ANSI.ASCII}
      LSize := LLength;
      if (LBuffer.Capacity < LSize) then LBuffer.Alloc(LSize);
      LBuffer.Size := LLength;
      System.Move(ARttiName[1], Pointer(LBuffer.FBytes)^, LLength);
    {$endif}
  end;
end;

function TLuaCustomStringDictionary.GetHashCode(const AChars: PByte; const ALength: Integer): Integer;
var
  S: PByte;
  X, i: Integer;
begin
  S := AChars;
  X := 63689;
  Result := ALength;
  for i := 1 to ALength do
  begin
    Result := Result * X + Integer(S^);
    Inc(S);
    X := X * 378551;
  end;

  X := ALength;
  if (ALength > 255) then X := 255;
  Result := Result and $00ffffff;
  X := X shl 24;
  Inc(Result, X);
end;

function TLuaCustomStringDictionary.InternalAdd(const AHashCode: Integer;
  const AValue: Pointer): PLuaCustomStringDictionaryItem;
label
  start;
var
  LIndex: NativeInt;
  LParent: PInteger;
begin
start:
  LIndex := FCount;
  if (LIndex <> FCapacity) then
  begin
    Inc(LIndex);
    FCount := LIndex;
    Dec(LIndex);

    LParent := @FHashes[NativeInt(AHashCode) and FHashesMask];
    Result := @FItems[LIndex];
    Result.HashCode := AHashCode;
    Result.Value := AValue;
    Result.Next := LParent^;
    LParent^ := LIndex;
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

procedure TLuaNames.InternalProcessKey(const AChars: PByte; const ALength: Integer);
{$if (not Defined(FPC)) and (CompilerVersion >= 20)}
var
  i: Integer;
  S: PByte;
begin
  S := AChars;
  for i := 1 to ALength do
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

function TLuaNames.InternalFind(const AModeCreate: Boolean): PLuaCustomStringDictionaryItem;
var
  LChars: PByte;
  LHashCode: Integer;
  LHashesMask: NativeInt;
  LIndex: NativeInt;
begin
  // chars, hash code
  LChars := Pointer(TLuaBuffer(FLua.FInternalBuffer).FBytes);
  LHashCode := GetHashCode(LChars, TLuaBuffer(FLua.FInternalBuffer).Size);

  // find
  LHashesMask := FHashesMask;
  if (LHashesMask <> 0) then
  begin
    LIndex := FHashes[NativeInt(LHashCode) and LHashesMask];
    if (LIndex >= 0) then
    repeat
      Result := @FItems[LIndex];
      if (Result.HashCode = LHashCode) and (CompareMem(Result.Value, LChars, LHashCode shr 24)) then Exit;
      LIndex := Result.Next;
    until (LIndex < 0);
  end;

  // not found
  if (AModeCreate) then
  begin
    lua_pushlstring(FLua.Handle, Pointer(LChars), LHashCode shr 24);
    LChars := Pointer(lua_tolstring(FLua.Handle, -1, nil));
    FLua.global_fill_value(FLua.global_alloc_ref);
    Result := InternalAdd(LHashCode, LChars);
  end else
  begin
    Result := nil;
  end;
end;

function TLuaNames.Add(const AKey: LuaString): __luaname;
begin
  InitBuffer(AKey);
  Result := InternalFind(True).Value;
end;

function TLuaNames.Add(const ARttiName: ShortString): __luaname;
begin
  InitBuffer(ARttiName);
  Result := InternalFind(True).Value;
end;

function TLuaNames.FindValue(const AKey: LuaString): __luaname;
begin
  InitBuffer(AKey);
  Result := Pointer(InternalFind(False));
  if (Assigned(Result)) then
    Result := PLuaCustomStringDictionaryItem(Result).Value;
end;

function TLuaNames.FindValue(const ARttiName: ShortString): __luaname;
begin
  InitBuffer(ARttiName);
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

procedure TLuaRegisteredTypeNames.InternalProcessKey(const AChars: PByte; const ALength: Integer);
var
  i: Integer;
  S: PByte;
begin
  S := AChars;
  for i := 1 to ALength do
  begin
    case S^ of
      Ord('A')..Ord('Z'): S^ := S^ or $20;
    end;
    Inc(S);
  end;
end;

function TLuaRegisteredTypeNames.InternalFind(const AOffset: Integer;
  const AModeCreate: Boolean): PLuaCustomStringDictionaryItem;
var
  LChars: PByte;
  LHashCode: Integer;
  LHashesMask: NativeInt;
  LIndex: NativeInt;
  LValue: PLuaRegisteredTypeName;
begin
  // chars, hash code
  LChars := Pointer(@TLuaBuffer(FLua.FInternalBuffer).FBytes[AOffset]);
  LHashCode := GetHashCode(LChars, TLuaBuffer(FLua.FInternalBuffer).Size - AOffset);

  // find
  LHashesMask := FHashesMask;
  if (LHashesMask <> 0) then
  begin
    LIndex := FHashes[NativeInt(LHashCode) and LHashesMask];
    if (LIndex >= 0) then
    repeat
      Result := @FItems[LIndex];
      if (Result.HashCode = LHashCode) and
        (CompareMem(@PLuaRegisteredTypeName(Result.Value).Chars, LChars, LHashCode shr 24)) then Exit;
      LIndex := Result.Next;
    until (LIndex < 0);
  end;

  // not found
  if (AModeCreate) then
  begin
    LValue := TLuaMemoryHeap(FLua.FMemoryHeap).Unpack(
      TLuaMemoryHeap(FLua.FMemoryHeap).Alloc(SizeOf(TLuaRegisteredTypeName) -
        SizeOf(Byte) + (LHashCode shr 24)));

    LValue.TypeInfo := nil;
    LValue.PointerDepth := 0;
    System.Move(LChars^, LValue.Chars, LHashCode shr 24);
    Result := InternalAdd(LHashCode, LValue);
  end else
  begin
    Result := nil;
  end;
end;

procedure TLuaRegisteredTypeNames.Add(const AName: LuaString; const ATypeInfo: PTypeInfo;
  const APointerDepth: Integer);
var
  LValue: PLuaRegisteredTypeName;
begin
  InitBuffer(AName);
  LValue := InternalFind(0, True).Value;
  LValue.TypeInfo := ATypeInfo;
  LValue.PointerDepth := APointerDepth;
end;

function TLuaRegisteredTypeNames.Find(const ARttiName: ShortString): PLuaRegisteredTypeName;
var
  LChars: PByte;
  LOffset, LLength: Integer;
  LAdditionalPointerDepth: Boolean;
  LItem: PLuaCustomStringDictionaryItem;
  LValue: PLuaRegisteredTypeName;
begin
  InitBuffer(ARttiName);
  LChars := Pointer(TLuaBuffer(FLua.FInternalBuffer).FBytes);
  LLength := TLuaBuffer(FLua.FInternalBuffer).Size;

  LItem := nil;
  LOffset := 0;
  LAdditionalPointerDepth := False;
  while (LOffset <> LLength) do
  begin
    LItem := InternalFind(LOffset, False);
    if (Assigned(LItem)) then
      Break{found};

    if (LChars^ <> Ord('p')) then
      Break{not found};

    LChars^ := Ord('t');
    LItem := InternalFind(LOffset, False);
    if (Assigned(LItem)) then
    begin
      LAdditionalPointerDepth := True;
      Break{found: additional mode};
    end;

    LChars^ := Ord('i');
    LItem := InternalFind(LOffset, False);
    if (Assigned(LItem)) then
    begin
      LAdditionalPointerDepth := True;
      Break{found: additional mode};
    end;

    Inc(LChars);
    Inc(LOffset);
  end;

  if (not Assigned(LItem)) then
  begin
    Result := nil;
    Exit;
  end;

  Result := LItem.Value;
  if (LAdditionalPointerDepth) then
  begin
    LChars^ := Ord('p');
    LValue := InternalFind(LOffset, True).Value;
    LValue.TypeInfo := Result.TypeInfo;
    LValue.PointerDepth := Result.PointerDepth + 1;
    Result := LValue;
  end;

  while (LOffset <> 0) do
  begin
    Dec(LChars);

    LChars^ := Ord('p');
    LValue := InternalFind(LOffset, True).Value;
    LValue.TypeInfo := Result.TypeInfo;
    LValue.PointerDepth := Result.PointerDepth + 1;
    Result := LValue;

    Dec(LOffset);
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
{$else .CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  xor ecx, ecx
  jmp TLuaArguments.SetArgsCount
end;
{$endif}

procedure TLuaArguments.SetResultCount(const AValue: Integer);
{$ifdef INLINESUPPORT}
begin
  SetArgsCount(AValue, 1);
end;
{$else .CPUX86} {$ifdef FPC}assembler;nostackframe;{$endif}
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
function TLuaArgumentsAllocRecord(const Self: TLuaArguments; const AInfo: PLuaRecordInfo; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  AInfo.CheckInstance(mtRecord, ReturnAddress);
  Result := Self.AllocMetaType(AInfo);
end;

{$ifNdef RETURNADDRESS}
function TLuaArguments.AllocRecord(const AInfo: PLuaRecordInfo): Pointer; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgumentsAllocRecord
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArguments.AllocArray(const AInfo: PLuaArrayInfo): Pointer;
{$else}
function TLuaArgumentsAllocArray(const Self: TLuaArguments; const AInfo: PLuaArrayInfo; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  AInfo.CheckInstance(mtArray, ReturnAddress);
  Result := Self.AllocMetaType(AInfo);
end;

{$ifNdef RETURNADDRESS}
function TLuaArguments.AllocArray(const AInfo: PLuaArrayInfo): Pointer; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgumentsAllocArray
end;
{$endif}

{$ifdef RETURNADDRESS}
function TLuaArguments.AllocSet(const AInfo: PLuaSetInfo): Pointer;
{$else}
function TLuaArgumentsAllocSet(const Self: TLuaArguments; const AInfo: PLuaSetInfo; const ReturnAddress: Pointer): Pointer;
{$endif}
begin
  AInfo.CheckInstance(mtSet, ReturnAddress);
  Result := Self.AllocMetaType(AInfo);
end;

{$ifNdef RETURNADDRESS}
function TLuaArguments.AllocSet(const AInfo: PLuaSetInfo): Pointer; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
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

function LuaArgTypeToString(const AValue: TLuaArgType; const APrefixes: Boolean): string;
begin
  Result := GetEnumName(TypeInfo(TLuaArgType), Ord(AValue));
  if (not APrefixes) and (Ord(AValue) <= Ord(High(TLuaArgType))) then
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

procedure TLuaArg.CheckDifficultSetter(const AValue: TLuaDifficultType; const AName: PChar;
  const AReturnAddress: Pointer);
var
  P: Pointer;
  B: Byte;
begin
  P := AValue.Data;
  if (NativeUInt(P) <= High(Word)) then
  raise ELua.CreateFmt('%s.Data field not defined ($%p)', [AName, P]) at AReturnAddress;

  P := PLuaRecord(@AValue).Info;
  if (NativeUInt(P) <= High(Word)) then
  raise ELua.CreateFmt('%s.Info field not defined ($%p)', [AName, P]) at AReturnAddress;

  B := Byte(AValue.FIsRef);
  if (B > 1) then
  raise ELua.CreateFmt('%s.IsRef field not defined (%d)', [AName, B]) at AReturnAddress;

  B := Byte(AValue.FIsConst);
  if (B > 1) then
  raise ELua.CreateFmt('%s.IsConst field not defined (%d)', [AName, B]) at AReturnAddress;
end;

function TLuaArg.GetLuaTypeName: string;
begin
  Result := GetEnumName(TypeInfo(TLuaArgType), Ord(F.LuaType));
end;

function TLuaArg.GetEmpty: Boolean;
begin
  GetEmpty := (F.LuaType = ltEmpty);
end;

procedure TLuaArg.SetEmpty(const AValue: Boolean);
begin
  if (AValue) then
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
function TLuaArg.GetBoolean: Boolean; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetBoolean
end;
{$endif}

procedure TLuaArg.SetBoolean(const AValue: Boolean);
begin
  F.LuaType := ltBoolean;
  F.VBoolean := AValue;
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
function TLuaArg.GetInteger: Integer; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetInteger
end;
{$endif}

procedure TLuaArg.SetInteger(const AValue: Integer);
begin
  F.LuaType := ltInteger;
  F.VInteger := AValue;
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
function TLuaArg.GetDouble: Double; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetDouble
end;
{$endif}

procedure TLuaArg.SetDouble(AValue: Double);
begin
  if (NumberToInteger(AValue, F.VInteger)) then
  begin
    F.LuaType := ltInteger;
  end else
  begin
    F.LuaType := ltDouble;
    F.VDouble := AValue;
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
function TLuaArg.GetString: LuaString; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetString
end;
{$endif}

procedure TLuaArg.SetString(const AValue: LuaString);
begin
  F.LuaType := ltString;
  FString := AValue;
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
function TLuaArg.GetPointer: Pointer; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetPointer
end;
{$endif}

procedure TLuaArg.SetPointer(const AValue: Pointer);
begin
  if (AValue = nil) then
  begin
    F.LuaType := ltEmpty;
  end else
  begin
    F.LuaType := ltPointer;
    F.VPointer := AValue;
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
function TLuaArg.GetVariant: Variant; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetVariant
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetVariant(const AValue: Variant);
{$else}
procedure TLuaArgSetVariant(var Self: TLuaArg; const AValue: Variant; const ReturnAddress: Pointer);
{$endif}

  procedure DateTimeValue(const Self: PLuaArg; AValue: TDateTime); far;
  begin
    case InspectDateTime(AValue) of
      0: Self.FString := LuaString(DateTimeToStr(AValue));
      1: Self.FString := LuaString(DateToStr(AValue));
      2: Self.FString := LuaString(TimeToStr(AValue));
    else
      Self.FString := '';
    end;
  end;

  {$ifdef UNICODE}
  procedure UnicodeValue(const Self: PLuaArg; const AValue: UnicodeString); far;
  begin
    Self.FString := LuaString(AValue);
  end;
  {$endif}

  {$ifNdef NEXTGEN}
  procedure AnsiValue(const Self: PLuaArg; const AValue: AnsiString); far;
  begin
    Self.FString := LuaString(AValue);
  end;

  procedure WideValue(const Self: PLuaArg; const AValue: WideString); far;
  begin
    Self.FString := LuaString(AValue);
  end;
  {$endif}
begin
  with TVarData(AValue) do
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
    varUString : UnicodeValue(@Self, UnicodeString(VString{VUString}));
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
procedure TLuaArg.SetVariant(const AValue: Variant); {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
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
function TLuaArg.GetClass: TClass; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetClass
end;
{$endif}

procedure TLuaArg.SetClass(const AValue: TClass);
begin
  if (AValue = nil) then
  begin
    F.LuaType := ltEmpty;
  end else
  begin
    F.LuaType := ltClass;
    F.VPointer := Pointer(AValue);
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
function TLuaArg.GetObject: TObject; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov edx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rdx, [rsp]
  {$endif}
  jmp TLuaArgGetObject
end;
{$endif}

procedure TLuaArg.SetObject(const AValue: TObject);
begin
  if (AValue = nil) then
  begin
    F.LuaType := ltEmpty;
  end else
  begin
    F.LuaType := ltObject;
    F.VPointer := Pointer(AValue);
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
function TLuaArg.GetRecord: TLuaRecord; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetRecord
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetRecord(const AValue: TLuaRecord);
{$else}
procedure TLuaArgSetRecord(var Self: TLuaArg; const AValue: TLuaRecord; const ReturnAddress: Pointer);
{$endif}
begin
  Self.CheckDifficultSetter(AValue, 'LuaRecord', ReturnAddress);
  PLuaRecord(@Self.F)^ := AValue;
  Self.F.LuaType := ltRecord;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetRecord(const AValue: TLuaRecord); {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
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
function TLuaArg.GetArray: TLuaArray; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetArray
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetArray(const AValue: TLuaArray);
{$else}
procedure TLuaArgSetArray(var Self: TLuaArg; const AValue: TLuaArray; const ReturnAddress: Pointer);
{$endif}
begin
  Self.CheckDifficultSetter(AValue, 'LuaArray', ReturnAddress);
  PLuaArray(@Self.F)^ := AValue;
  Self.F.LuaType := ltArray;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetArray(const AValue: TLuaArray); {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
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
function TLuaArg.GetSet: TLuaSet; {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgGetSet
end;
{$endif}

{$ifdef RETURNADDRESS}
procedure TLuaArg.SetSet(const AValue: TLuaSet);
{$else}
procedure TLuaArgSetSet(var Self: TLuaArg; const AValue: TLuaSet; const ReturnAddress: Pointer);
{$endif}
begin
  Self.CheckDifficultSetter(AValue, 'LuaSet', ReturnAddress);
  PLuaSet(@Self.F)^ := AValue;
  Self.F.LuaType := ltSet;
end;

{$ifNdef RETURNADDRESS}
procedure TLuaArg.SetSet(const AValue: TLuaSet); {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ecx, [esp]
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov r8, [rsp]
  {$endif}
  jmp TLuaArgSetSet
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

  function StringDefault(const AValue: LuaString): NativeInt; far;
  begin
    Result := {$ifdef LARGEINT}StrToInt64Def{$else}StrToIntDef{$endif}(string(AValue), 0);
  end;

begin
  case LuaType of
      ltEmpty: Result := 0;
     ltDouble: Result := Trunc(F.VDouble);
     ltObject: Result := TObject(F.VPointer).InstanceSize;
     ltRecord: Result := PLuaRecord(@F).Info.Size;
     ltString: Result := StringDefault(FString);
  else
    Result := NativeInt(F.VPointer);
  end;
end;

function TLuaArg.ForceDouble: Double;

  function StringDefault(const AValue: LuaString): Double; far;
  begin
    Result := StrToFloatDef(string(AValue), 0);
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

  function IntegerValue(const AValue: Integer): LuaString; far;
  begin
    Result := LuaString(IntToStr(AValue));
  end;

  function DoubleValue(const AValue: Double): LuaString; far;
  begin
    Result := LuaString(FloatToStr(AValue));
  end;

  function PointerValue(const AValue: Pointer): LuaString; far;
  begin
    Result := LuaString(IntToHex(NativeInt(AValue), {$ifdef SMALLINT}8{$else .LARGEINT}16{$endif}));
  end;

  function ClassValue(const AValue: TClass): LuaString; far;
  begin
    Result := LuaString(AValue.ClassName);
  end;

  function ObjectValue(const AValue: TObject): LuaString; far;
  begin
    Result := LuaString(AValue.ClassName);
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
  else
    Result := nil;
  end;
end;

function TLuaArg.ForceVariant: Variant;
var
  LType: Integer;
  LVarData: TVarData absolute Result;
begin
  LType := LVarData.VType;
  if (LType and varDeepData <> 0) then
  case LType of
    varBoolean, varUnknown+1..$15{varUInt64}: ;
  else
    VarClear(Result);
  end;

  case (LuaType) of
    ltBoolean: begin
                 LVarData.VType := varBoolean;
                 LVarData.VBoolean := F.VBoolean;
               end;
    ltInteger: begin
                 LVarData.VType := varInteger;
                 LVarData.VInteger := F.VInteger;
               end;
     ltDouble: begin
                 LVarData.VType := varDouble;
                 LVarData.VDouble := F.VDouble;
               end;
     ltString: begin
                 {$ifdef UNICODE}
                   LVarData.VType := varUString;
                  {$else}
                   LVarData.VType := varOleStr;
                 {$endif}
                 LVarData.VPointer := nil;

                 if (Pointer(FString) <> nil) then
                 begin
                   {$ifdef UNICODE}
                     UnicodeString(LVarData.VString{VUString}) := FString;
                    {$else}
                     WideString(Pointer(LVarData.VOleStr)) := FString;
                   {$endif}
                 end;
               end;
   else
     {ltEmpty etc:}
     LVarData.VType := varEmpty;
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

function LuaArg(const AValue: Boolean): TLuaArg;
begin
  Result.AsBoolean := AValue;
end;

function LuaArg(const AValue: Integer): TLuaArg;
begin
  Result.AsInteger := AValue;
end;

function LuaArg(const AValue: Double): TLuaArg;
begin
  Result.AsDouble := AValue;
end;

function LuaArg(const AValue: LuaString): TLuaArg;
begin
  Result.AsString := AValue;
end;

function LuaArg(const AValue: Pointer): TLuaArg;
begin
  Result.AsPointer := AValue;
end;

function LuaArg(const AValue: TClass): TLuaArg;
begin
  Result.AsClass := AValue;
end;

function LuaArg(const AValue: TObject): TLuaArg;
begin
  Result.AsObject := AValue;
end;

function LuaArg(const AValue: TLuaRecord): TLuaArg;
begin
  Result.AsRecord := AValue;
end;

function LuaArg(const AValue: TLuaArray): TLuaArg;
begin
  Result.AsArray := AValue;
end;

function LuaArg(const AValue: TLuaSet): TLuaArg;
begin
  Result.AsSet := AValue;
end;

function LuaArg(const AValue: Variant): TLuaArg;
begin
  Result.AsVariant := AValue;
end;

function LuaArgDynArray(const ACount: Integer): TLuaArgDynArray;
var
  i: Integer;
begin
  if (ACount < 0) then
    raise ELua.CreateFmt('Can''t create an array lenght of %d arguments', [ACount])
    {$ifdef RETURNADDRESS} at ReturnAddress{$endif};

  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i].F.LuaType := ltEmpty;
end;

function LuaProcCallback(const AAddress: Pointer): TLuaProcCallback;
begin
  Result := AAddress;
end;

function LuaMethodCallback(const AAddress: Pointer): TLuaMethodCallback;
begin
  Result := AAddress;
end;

function LuaProcParam(const AName: LuaString; const ATypeInfo: PTypeInfo;
  const AFlags: TParamFlags; const APointerDepth: Integer): TLuaProcParam;
begin
  Result.Name := AName;
  Result.TypeInfo := ATypeInfo;
  Result.Flags := AFlags;
  Result.PointerDepth := APointerDepth;
end;


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

procedure TLuaMetaType.CheckInstance(const AKind: TLuaMetaKind; const AReturnAddress: Pointer);
const
  KINDS: array[TLuaMetaKind] of string = ('class', 'interface', 'record', 'array', 'set');
begin
  if (NativeUInt(@Self) <= $FFFF) or (NativeInt(@Self) and (SizeOf(Pointer) - 1) <> 0) or
    (F.Marker <> LUA_METATYPE_MARKER) or (F.Kind <> AKind) then
    raise ELua.CreateFmt('Invalid %s instance: %p', [KINDS[AKind], @Self]) at AReturnAddress;
end;

procedure TLuaMetaType.FillManagedValue;
var
  LValue: Boolean;
begin
  case F.Kind of
    {$ifdef AUTOREFCOUNT}
    mtClass,
    {$endif}
    mtInterface: LValue := True;
    mtRecord:
    begin
      LValue := (Assigned(F.TypeInfo)) and (IsManagedTypeInfo(F.TypeInfo));
    end;
    mtArray:
    begin
      LValue := Assigned(PLuaArrayInfo(@Self).FFinalTypeInfo);
    end;
  else
    LValue := False;
  end;

  F.Managed := LValue;
end;

procedure TLuaMetaType.FillHFAValue;
{$ifdef CPUARM64}
  function GetRecordHFA(const ATypeInfo: PTypeInfo): Boolean; far;
  var
    LContext: TRTTIContext;
  begin
    Result := LContext.GetType(ATypeInfo).IsHFA;
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
    LContext: TRTTIContext;
    LElementType: TRttiFloatType;
  begin
    Result := High(TFloatType);
    LElementType := LContext.GetType(ATypeInfo).HFAElementType;
    if (Assigned(LElementType)) then
      Result := LElementType.FloatType;
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
    LContext: TRTTIContext;
  begin
    Result := LContext.GetType(ATypeInfo).HFAElementCount;
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

procedure TLuaMetaType.Dispose(const AValue: Pointer{/TObject/IInterface});
begin
  case F.Kind of
    mtClass:
    begin
      TObject(AValue).{$IFDEF AUTOREFCOUNT}DisposeOf{$else}Free{$endif};
    end;
    mtInterface:
    begin
      if (Assigned(AValue)) then
        IInterface(AValue)._Release;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
        FinalizeArray(AValue, F.TypeInfo, 1);
    end;
    mtArray:
    begin
      if (Self.Managed) then
        FinalizeArray(AValue, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
    end;
    mtSet: ;
  end;
end;

procedure TLuaMetaType.Clear(var AInstance; const AFillZero: Boolean);
begin
  case F.Kind of
    mtClass:
    begin
      TObject(AInstance).CleanupInstance;
      if (AFillZero) then
      begin
        FillChar(PPointer(NativeInt(AInstance) + SizeOf(Pointer))^,
          TObject(AInstance).InstanceSize - (SizeOf(Pointer) + hfFieldSize), #0);
      end;
      Exit;
    end;
    mtInterface:
    begin
      IInterface(AInstance) := nil;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
        FinalizeArray(@AInstance, F.TypeInfo, 1);
    end;
    mtArray:
    begin
      if (Self.Managed) then
        FinalizeArray(@AInstance, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
    end;
    mtSet: ;
  end;

  if (AFillZero) then
  begin
    FillChar(AInstance, F.Size, #0);
  end;
end;

procedure TLuaMetaType.Assign(var AInstance; const AValue: Pointer{/TObject/IInterface});
var
  LObj: ^TObject;
begin
  case F.Kind of
    mtClass:
    begin
      LObj := Pointer(@AInstance);
      if (not Assigned(LObj^)) then
      begin
        LObj^ := TClass(PPointer(AValue)^).NewInstance;
      end;
      CopyObject(LObj^, TObject(AValue));
      Exit;
    end;
    mtInterface:
    begin
      IInterface(AInstance) := IInterface(AValue);
      Exit;
    end;
    mtRecord:
    begin
      if (Self.Managed) then
      begin
        CopyArray(@AInstance, AValue, F.TypeInfo, 1);
        Exit;
      end;
    end;
    mtArray:
    begin
      if (Self.Managed) then
      begin
        CopyArray(@AInstance, AValue, PLuaArrayInfo(@Self).FFinalTypeInfo, PLuaArrayInfo(@Self).FFinalItemsCount);
        Exit;
      end;
    end;
    mtSet: ;
  end;

  System.Move(AValue^, AInstance, F.Size);
end;


{ TLuaRecordInfo }

const
  RECORD_METHOD_KINDS: array[Boolean{IsStatic}] of TLuaMethodKind = (mkInstance, mkStatic);

procedure __TLuaRecordInfoRegUniversalMethod(const Self: TLuaRecordInfo; const AMethodName: LuaString;
  const AMethod: TLuaMethodCallback; const AMinArgsCount, AMaxArgsCount: Word; const AIsStatic: Boolean);
begin
  Self.FLua.InternalAddMethod(@Self, AMethodName, @AMethod, RECORD_METHOD_KINDS[AIsStatic],
    LUA_POINTER_INVALID, True, AMinArgsCount, AMaxArgsCount);
end;

procedure TLuaRecordInfo.RegMethod(const AMethodName: LuaString; const ACallback: TLuaMethodCallback;
  const AMinArgsCount, AMaxArgsCount: Word; const AIsStatic: Boolean); {$ifdef FPC}assembler;nostackframe;{$endif}
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegUniversalMethod(Self, AMethodName, ACallback, AMinArgsCount, AMaxArgsCount, AIsStatic);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegUniversalMethod
end;
{$endif}

procedure __TLuaRecordInfoRegTypeInfoMethod(const Self: TLuaRecordInfo; const AMethodName: LuaString;
  const AAddress: Pointer; const ATypeInfo: PTypeInfo; const AIsStatic: Boolean);
var
  LInvokable: __luapointer;
begin
  LInvokable := Self.FLua.InternalBuildInvokable(@Self, ATypeInfo, RECORD_METHOD_KINDS[AIsStatic], True);
  if (LInvokable <> LUA_POINTER_INVALID) then
  begin
    Self.FLua.InternalAddMethod(@Self, AMethodName, AAddress, RECORD_METHOD_KINDS[AIsStatic], LInvokable, True);
  end;
end;

procedure TLuaRecordInfo.RegMethod(const AMethodName: LuaString; const AAddress: Pointer;
  const ATypeInfo: PTypeInfo; const AIsStatic: Boolean); {$ifdef FPC}assembler;nostackframe;{$endif}
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegTypeInfoMethod(Self, AMethodName, AAddress, ATypeInfo, AIsStatic);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegUniversalMethod
end;
{$endif}

procedure __TLuaRecordInfoRegCustomMethod(const Self: TLuaRecordInfo; const AMethodName: LuaString;
  const AAddress: Pointer; const AParams: array of TLuaProcParam; const AResultType: PTypeInfo;
  const AIsResultUnsafe, AIsStatic: Boolean; const ACallConv: TCallConv);
var
  LInvokable: __luapointer;
begin
  LInvokable := Self.FLua.InternalBuildInvokable(@Self, AMethodName, AParams,
    AResultType, AIsResultUnsafe, RECORD_METHOD_KINDS[AIsStatic], ACallConv, True);
  if (LInvokable <> LUA_POINTER_INVALID) then
  begin
    Self.FLua.InternalAddMethod(@Self, AMethodName, AAddress, RECORD_METHOD_KINDS[AIsStatic], LInvokable, True);
  end;
end;

procedure TLuaRecordInfo.RegMethod(const AMethodName: LuaString; const AAddress: Pointer;
  const AParams: array of TLuaProcParam; const AResultType: PTypeInfo;
  const AIsResultUnsafe, AIsStatic: Boolean; const ACallConv: TCallConv); {$ifdef FPC}assembler;nostackframe;{$endif}
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoRegCustomMethod(Self, AMethodName, AAddress, AParams, AResultType, AIsResultUnsafe, AIsStatic, ACallConv);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
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
  const ATypeInfo: Pointer; const AReference: TLuaReference); {$ifdef FPC}assembler;nostackframe;{$endif}
{$ifNdef CPUINTEL}
begin
  FLua.FReturnAddress := ReturnAddress;
  FLua.FFrames := nil;
  __TLuaRecordInfoInternalRegField(Self, AName, AOffset, ATypeInfo, AReference);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
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
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
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
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
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
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
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
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  mov ebp, [EAX].TLuaRecordInfo.FLua
  push [esp + 4]
  pop [EBP].TLua.FReturnAddress
  mov [EBP].TLua.FFrames, 0
  pop ebp
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  mov rax, [RCX].TLuaRecordInfo.FLua
  push qword ptr [rsp]
  pop [RAX].TLua.FReturnAddress
  mov [RAX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRecordInfoRegComplexProperty
end;
{$endif}

procedure TLuaRecordInfo.SetOperators(const AValue: TLuaOperators);
begin
  if (FOperators <> AValue) then
  begin
    FOperators := AValue;
    //FLua.FInitialized := false;
  end;
end;

procedure TLuaRecordInfo.SetOperatorCallback(const AValue: TLuaOperatorCallback);
begin
  if (Pointer(@FOperatorCallback) <> Pointer(@AValue)) then
  begin
    FOperatorCallback := AValue;
    //FLua.FInitialized := false;
  end;
end;


{ TLuaSetInfo }

function TLuaSetInfo.Description(const AValue: Pointer): LuaString;
var
  P1, P2, i: integer;
  LBuffer: ^string;

  procedure Add();
  const
    CHARS: array[Boolean] of string = (', ', '..');
  var
    S: string;
  begin
    if (P1 < 0) then Exit;
    if (LBuffer^ <> '') then LBuffer^ := LBuffer^ + ', ';
    S := GetEnumName(FItemTypeInfo, P1);
    LBuffer^ := LBuffer^ + S;
    if (P2 <> P1) then
    begin
      S := GetEnumName(FItemTypeInfo, P2);
      LBuffer^ := LBuffer^ + CHARS[P2 > (P1 + 1)] + S;
    end;

    P1 := -1;
    P2 := -1;
  end;
begin
  LBuffer := @FLua.FStringBuffer.Default;
  LBuffer^ := '';

  P1 := -1;
  P2 := -1;
  for i := FLow to FHigh do
  if (SetBitContains(AValue, i - FCorrection)) then
  begin
    if (P1 < 0) then P1 := i;
    P2 := i;
  end else
  begin
    if (P1 >= 0) then Add;
  end;

  Add;
  LBuffer^ := '['+ LBuffer^ + ']';

  Result := LuaString(LBuffer^);
end;


{ TLuaInterfaceInfo }

function TLuaInterfaceInfo.InheritsFrom(const AValue: PLuaInterfaceInfo): Boolean;
var
  LTempPtr, LValuePtr: __luapointer;
begin
  Result := (@Self = AValue);
  if (Result) then Exit;

  LValuePtr := AValue.Ptr;
  LTempPtr := Self.Parent;
  if (LTempPtr <> LUA_POINTER_INVALID) then
  repeat
    Result := (LTempPtr = LValuePtr);
    if (Result) then Exit;

    LTempPtr := PLuaInterfaceInfo({$ifdef LARGEINT}TLuaMemoryHeap(FLua.FMemoryHeap).Unpack{$endif}(LTempPtr)).Parent;
  until (LTempPtr = LUA_POINTER_INVALID);

  Result := False;
end;


{ TLuaClassInfo }

function TLuaClassInfo.VmtOffset(const AProc: Pointer; const AStandardProcs: Boolean): NativeInt;
const
  FIRST_VMT: array[Boolean] of NativeInt = (
    {$ifdef FPC}vmtToString + SizeOf(Pointer){$else .DELPHI}0{$endif},
    {$ifdef FPC}vmtMethodStart{$else .DELPHI}vmtParent + SizeOf(Pointer){$endif});
var
  LVmt, LTop: NativeUInt;
  LStart, LFinish, LValue: NativeUInt;
begin
  if (Assigned(AProc)) then
  begin
    LVmt := NativeUInt(ClassType);
    LStart := LVmt;
    LFinish := LVmt;
    Inc(LStart, {$ifdef FPC}vmtClassName{$else .DELPHI}(vmtSelfPtr + SizeOf(Pointer)){$endif});
    Inc(LFinish, {$ifdef FPC}vmtMsgStrPtr{$else .DELPHI}vmtClassName{$endif});

    LTop := High(NativeUInt);
    repeat
      LValue := PNativeUInt(LStart)^;
      Inc(LStart, SizeOf(Pointer));
      if (LValue >= LVmt) and (LValue < LTop) then LTop := LValue;
    until (LStart > LFinish);

    LTop := NativeUInt(NativeInt(LTop) and -SizeOf(Pointer));
    LStart := LVmt;
    Inc(NativeInt(LVmt), FIRST_VMT[AStandardProcs]);
    Dec(LVmt, SizeOf(Pointer));
    repeat
      Inc(LVmt, SizeOf(Pointer));
      if (LVmt = LTop) then Break;
      if (PPointer(LVmt)^ = AProc) then
      begin
        Result := NativeInt(LVmt) - NativeInt(LStart);
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

    procedure GetDescription(var AResult: string; const ALua: TLua);
  end;
  PLuaUserData = ^TLuaUserData;


procedure TLuaUserData.GetDescription(var AResult: string; const ALua: TLua);
begin
  if (@Self = nil) then
  begin
    AResult := 'nil userdata';
  end else
  if (Byte(Kind) > Byte(mtComplexProperty)) then
  begin
    AResult := 'unknown user data';
  end else
  if (Instance = nil) then
  begin
    AResult := 'already destroyed';
  end else
  begin
    AResult := 'ToDo';
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
  Internal closure description: name and invoke information }

  TLuaClosureType = packed record
    Name: PShortString;
    Invokable: __luapointer{PLuaInvokable};
  end;
  PLuaClosureType = ^TLuaClosureType;


{ TLuaMethod object
  Internal method description: class/record methods and function types: function, method, closure }

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
  Class/record method description }

  TLuaNamespaceMethod = object(TLuaMethod)
    Func: __luafunction;
  end;
  PLuaNamespaceMethod = ^TLuaNamespaceMethod;


{ TLuaClosureMethod object
  Function types: function, method, closure }

  TLuaClosureMethod = object(TLuaMethod)
    Instance: Pointer;
  end;
  PLuaClosureMethod = ^TLuaClosureMethod;


{ TLuaCustomVariable object
  Common native variable description }

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
        push_metatype_instance(InternalGetClassInfo(TClass(LValue^)), LSource^,
          LOptions and OPTIONS_GETTER_OWNED_MODE <> 0{Self.IsExtendedGetterOwned},
          LOptions and OPTIONS_GETTER_TEMPORARY_MODE <> 0{Self.IsExtendedGetterTemporary});
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
          ALua.stack_ansi_string(LDefault^, AStackIndex, 0);
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
          stack_lua_string(FStringBuffer.Lua, AStackIndex);
          LValue := NativeInt(FStringBuffer.Lua);
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
          if (Pointer(@lua_tocfunction(Handle, AStackIndex)) <> @LuaClosureWrapper) then
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

    procedure Initialize(const AParamBlock: PParamBlock);
    procedure Finalize(const AParamBlock: PParamBlock);
    {$ifdef CPUARM}
    procedure FillARMPartialData(const AParam: PLuaInvokableParam; const AValue: Pointer; const AParamBlock: PParamBlock);
    {$endif}
    procedure Invoke(const ACodeAddress: Pointer; const AParamBlock: PParamBlock);
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

function CheckAutoResult(AResultCode: HResult): HResult;
begin
  if AResultCode < 0 then
  begin
    if Assigned(SafeCallErrorProc) then
      SafeCallErrorProc(AResultCode, ReturnAddress);
    System.Error(reSafeCallError);
  end;
  Result := AResultCode;
end;
{$ifend}

procedure TLuaInvokable.Initialize(const AParamBlock: PParamBlock);
var
  i, LTotalPointerDepth, LOptions: Integer;
  LInitialPtr: PInteger;
  LParam: PLuaInvokableParam;
  LPtr: Pointer;
begin
  LInitialPtr := Initials;

  for i := 1 to InitialCount do
  begin
    LParam := Pointer(NativeInt(@Self) + LInitialPtr^);
    LPtr := Pointer(NativeInt(AParamBlock) + LParam.DataValue);

    PNativeInt(LPtr)^ := 0;
    LOptions := LParam.F.Options;
    if (LOptions and (PARAM_INSURANCE_MODE or PARAM_TOTALPOINTERDEPTH_MASK) <> 0) then
    begin
      // insurance
      if (LOptions and PARAM_INSURANCE_MODE <> 0) then
      begin
        Dec(NativeUInt(LPtr), SizeOf(Pointer));
        PNativeInt(LPtr)^ := 0;
      end;

      // references
      LTotalPointerDepth := LOptions and PARAM_TOTALPOINTERDEPTH_MASK;
      if (LTotalPointerDepth <> 0) then
      begin
        if (LTotalPointerDepth <> 1) then
        repeat
          Dec(NativeInt(LPtr), SizeOf(Pointer));
          PNativeInt(LPtr)^ := NativeInt(LPtr) + SizeOf(Pointer);
          Dec(LTotalPointerDepth);
        until (LTotalPointerDepth = 1);

        PPointer(NativeInt(AParamBlock) + LParam.Value)^ := LPtr;
      end;
    end;

    Inc(LInitialPtr);
  end;
end;

procedure TLuaInvokable.Finalize(const AParamBlock: PParamBlock);
label
  failure;
var
  i, LOptions, LType: Integer;
  LFinalPtr: PInteger;
  LParam: PLuaInvokableParam;
  LPtr: Pointer;

  procedure _FinalizeArray(AParam: PLuaInvokableParam; APtr: Pointer; ACount: Integer); far;
  var
    i: Integer;
    LMetaType: PLuaMetaType;
  begin
    case (AParam.F.Kind) of
      {$ifdef AUTOREFCOUNT}
      vkObject:
      begin
        FinalizeArray(APtr, TypeInfo(TObject), ACount);
      end;
      {$endif}
      vkInterface:
      begin
        FinalizeArray(APtr, TypeInfo(IInterface), ACount);
      end;
      vkVariant:
      begin
        FinalizeArray(APtr, TypeInfo(Variant), ACount);
      end;
      vkString:
      begin
        case AParam.F.StringType of
            stAnsiString: FinalizeArray(APtr, TypeInfo(AnsiString), ACount);
            stWideString: FinalizeArray(APtr, TypeInfo(WideString), ACount);
         stUnicodeString: FinalizeArray(APtr, TypeInfo(UnicodeString), ACount);
        end;
      end;
      vkClosure:
      begin
        case AParam.F.ClosureMode of
          mmReference:
          begin
            FinalizeArray(APtr, TypeInfo(IInterface), ACount);
          end;
          {$ifdef WEAKINSTREF}
          mmMethod:
          begin
            FinalizeArray(APtr, TypeInfo(TLuaOnPreprocessScript{TMethodProc}), ACount);
          end;
          {$endif}
        end;
      end;
      vkRecord, vkArray:
      begin
        LMetaType := AParam.F.MetaType;

        for i := 1 to ACount do
        begin
          LMetaType.Clear(APtr^, False);
          Inc(NativeInt(APtr), LMetaType.Size);
        end;

        Dec(NativeInt(APtr), ACount * LMetaType.Size);
      end;
      // TVarArg?
    end;

    FreeMem(APtr);
  end;
begin
  LFinalPtr := Finals;

  for i := 1 to FinalCount do
  begin
    LParam := Pointer(NativeInt(@Self) + LFinalPtr^);
    LPtr := Pointer(NativeInt(AParamBlock) + LParam.DataValue);

    LOptions := LParam.F.Options;
    if (LOptions and PARAM_INSURANCE_MODE = 0) then
    begin
      case ((LOptions shr 8) and $7f) of
        Ord(vkString):
        begin
          case LParam.F.StringType of
            {$ifNdef NEXTGEN}
            stAnsiString:
            begin
              if (PNativeInt(LPtr)^ <> 0) then
                AnsiString(LPtr^) := '';
            end;
            stWideString:
            begin
              if (PNativeInt(LPtr)^ <> 0) then
                WideString(LPtr^) := '';
            end;
            {$endif}
            stUnicodeString:
            begin
              if (PNativeInt(LPtr)^ <> 0) then
                UnicodeString(LPtr^) := '';
            end;
          else
            goto failure;
          end;
        end;
        Ord(vkVariant):
        begin
          LType := PVarData(LPtr).VType;
          if (LType and varDeepData <> 0) then
          case LType of
            varBoolean, varUnknown+1..$15{varUInt64}: ;
          else
            VarClear(PVariant(LPtr)^);
          end;
        end;
        Ord(vkClosure):
        begin
          if (LParam.F.ClosureMode = mmMethod) then
          begin
            TLuaOnPreprocessScript(LPtr^) := nil;
          end else
          begin
            // mmReference
            if (PNativeInt(LPtr)^ <> 0) then
              PLuaDefaultInterface(LPtr).Value := nil;
          end;
        end;
      else
      failure:
        raise ELua.CreateFmt('Error finalize argument %d', [LFinalPtr^]);
      end;
    end else
    begin
      // Insurance: array/PAnsiChar/PWideChar
      LPtr := Pointer(NativeInt(AParamBlock) + LParam.InsuranceValue);
      if (LOptions and PARAM_ARRAY_MODE = 0) then
      begin
        case LParam.F.Kind of
          vkString:
          begin
            case LParam.F.StringType of
              stPAnsiChar:
              begin
                if (PNativeInt(LPtr)^ <> 0) then
                  AnsiString(LPtr^) := '';
              end;
              stPWideChar:
              begin
                if (PNativeInt(LPtr)^ <> 0) then
                  UnicodeString(LPtr^) := '';
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
        LPtr := PPointer(LPtr)^;
        if (LPtr <> nil) then
        begin
          Inc(LParam);
          LOptions{Count} := PInteger(NativeInt(AParamBlock) + LParam.DataValue)^ {$ifNdef FPC} + 1{$endif};
          Dec(LParam);
          _FinalizeArray(LParam, LPtr, LOptions{Count});
        end;
      end;
    end;

    Inc(LFinalPtr);
  end;
end;

{$ifdef CPUARM}
procedure TLuaInvokable.FillARMPartialData(const AParam: PLuaInvokableParam;
  const AValue: Pointer; const AParamBlock: PParamBlock);
var
  i: Integer;
  LPartialSize: Integer;
  LSrc, LDest: PByte;
begin
  LSrc := AValue;
  LDest := Pointer(NativeInt(ParamBlock) + AParam.Value);
  LPartialSize := Self.ARMPartialSize;

  {$ifdef CPUX64}
    if (AParam.MetaType.HFA) then
    begin
      if (AParam.MetaType.HFAElementType = ftSingle) then
      begin
        for i := 1 to LPartialSize shr 2 do
        begin
          PCardinal(LDest)^ := PCardinal(LSrc)^;
          Inc(LSrc, SizeOf(Cardinal));
          Inc(LDest, 16);
        end;
      end else
      begin
        for i := 1 to LPartialSize shr 3 do
        begin
          PInt64(LDest)^ := PInt64(LSrc)^;
          Inc(LSrc, SizeOf(Int64));
          Inc(LDest, 16);
        end;
      end;
      Exit;
    end else
    begin
      for i := 1 to LPartialSize shr 3 do
      begin
        PInt64(LDest)^ := PInt64(LSrc)^;
        Inc(LSrc, SizeOf(Int64));
        Inc(LDest, 16);
      end;
    end;
  {$else .CPUARM32}
    begin
      System.Move(LSrc^, LDest^, LPartialSize);
      Inc(LSrc, LPartialSize);
      Inc(LDest, LPartialSize);
    end;
  {$endif}

  LDest := Pointer(NativeInt(AParamBlock) + AParam.DataValue);
  System.Move(LSrc^, LDest^, AParam.MetaType.Size - LPartialSize);
end;
{$endif}

procedure TLuaInvokable.Invoke(const ACodeAddress: Pointer; const AParamBlock: PParamBlock); {$ifdef FPC}assembler;nostackframe;{$endif}
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
      {$ifdef FPC}
      je int_safecallcheck
      {$else .DELPHI}
      je System.@CheckAutoResult
      {$endif}
@ret:
end;
{$elseif Defined(CPUX64) and Defined(MSWINDOWS)}
const
  PARAMBLOCK_SIZE = SizeOf(TParamBlock);

  procedure InvokeError(const AReturnAddress: Pointer); far;
  begin
    {$ifdef FPC}
      raise Exception.Create('Parameter count exceeded') at AReturnAddress;
    {$else .DELPHI}
      raise Exception.CreateRes(Pointer(@SParameterCountExceeded)) at AReturnAddress;
    {$endif}
  end;
asm
      // .PARAMS 62 // There's actually room for 64, assembler is saving locals for Self, CodeAddress & ParamBlock
      push rbp
      sub rsp, $1f0
      mov rbp, rsp

      mov [rbp + $208], rcx
      MOV     [RBP+$210], ACodeAddress
      MOV     [RBP+$218], AParamBlock
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
      {$ifdef FPC}
      je int_safecallcheck
      {$else .DELPHI}
      je System.@CheckAutoResult
      {$endif}
@ret:
end;
{$elseif Defined(CPUX64)} {!MSWINDOWS}
var
  LStoredXMM0: Double;
begin
  LStoredXMM0 := AParamBlock.RegXMM[0];
  try
    AParamBlock.StackData := Pointer(NativeInt(AParamBlock) + SizeOf(TParamBlock));
    AParamBlock.StackDataSize := Self.StackDataSize;
    RawInvoke(ACodeAddress, AParamBlock);

    AParamBlock.OutXMM0 := AParamBlock.RegXMM[0];
  finally
    AParamBlock.RegXMM[0] := LStoredXMM0;
  end;

  if (Self.CallConv = ccSafeCall) and (AParamBlock.OutSafeCall < 0) then
    CheckAutoResult(AParamBlock.OutSafeCall);
end;
{$elseif Defined(CPUARM32)}
var
  LStoredCR: Integer;
  LStoredD: Double;
begin
  LStoredCR := AParamBlock.RegCR[0];
  LStoredD := AParamBlock.RegD[0];
  try
    AParamBlock.StackData := Pointer(NativeInt(AParamBlock) + SizeOf(TParamBlock));
    AParamBlock.StackDataSize := Self.StackDataSize;
    RawInvoke(ACodeAddress, AParamBlock);

    AParamBlock.OutCR := AParamBlock.RegCR[0];
    AParamBlock.OutD := AParamBlock.RegD[0];
  finally
    AParamBlock.RegCR[0] := LStoredCR;
    AParamBlock.RegD[0] := LStoredD;
  end;

  if (Self.CallConv = ccSafeCall) and (AParamBlock.OutSafeCall < 0) then
    CheckAutoResult(AParamBlock.OutSafeCall);
end;
{$else .CPUARM64}
var
  LStoredX: Int64;
  LStored128x4: m128x4;
begin
  LStoredX := AParamBlock.RegX[0];
  LStored128x4 := AParamBlock.Regs128x4;
  try
    AParamBlock.StackData := Pointer(NativeInt(AParamBlock) + SizeOf(TParamBlock));
    AParamBlock.StackDataSize := Self.StackDataSize;
    RawInvoke(ACodeAddress, AParamBlock);

    AParamBlock.OutX := AParamBlock.RegX[0];
    AParamBlock.OutQ := AParamBlock.RegQ[0];
    if (Self.ResultMode in [rmHFASingle, rmHFADouble]) then
    begin
      if (Self.ResultMode = rmHFASingle) then
      begin
        AParamBlock.OutHFA.Singles[0] := AParamBlock.RegQ[0].LL;
        AParamBlock.OutHFA.Singles[1] := AParamBlock.RegQ[1].LL;
        AParamBlock.OutHFA.Singles[2] := AParamBlock.RegQ[2].LL;
        AParamBlock.OutHFA.Singles[3] := AParamBlock.RegQ[3].LL;
      end else
      begin
        AParamBlock.OutHFA.Doubles[0] := AParamBlock.RegQ[0].Lo;
        AParamBlock.OutHFA.Doubles[1] := AParamBlock.RegQ[1].Lo;
        AParamBlock.OutHFA.Doubles[2] := AParamBlock.RegQ[2].Lo;
        AParamBlock.OutHFA.Doubles[3] := AParamBlock.RegQ[3].Lo;
      end;
    end;
  finally
    AParamBlock.RegX[0] := LStoredX;
    AParamBlock.Regs128x4 := LStored128x4;
  end;

  if (Self.CallConv = ccSafeCall) and (AParamBlock.OutSafeCall < 0) then
    CheckAutoResult(AParamBlock.OutSafeCall);
end;
{$ifend}


type
  TLuaInvokableBuilder = object
  private
    FLua: TLua;
    FBuffer: TLuaBuffer;
    FAdvanced: TLuaBuffer;

    function NewInvokable(const AMetaType: PLuaMetaType; const AMethodKind: TLuaMethodKind;
      const ACallConv: TCallConv; const AResultType: PTypeInfo; const AResultTypeName: PShortString;
      const AResultReference: TLuaReference): PLuaInvokable;
    function InternalAddName(const AName: LuaString): NativeInt;
    function InternalAddParam(const AName: PShortString; const ATypeInfo: Pointer; const ATypeName: PShortString;
      const APointerDepth: Integer; const AParamFlags: TParamFlags): PLuaInvokableParam; overload;
    function InternalAddParam(const AParam: TLuaProcParam): PLuaInvokableParam; overload;
    procedure InternalParamsDone(var AInvokable: TLuaInvokable);
    function InternalBuildDone: __luapointer;
  public
    procedure Clear;
    function BuildMethod(const AMetaType: PLuaMetaType; const AMethod: PTypeInfo; AMethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    {$ifdef EXTENDEDRTTI}
    function BuildProcedureSignature(const AMetaType: PLuaMetaType; const ASignature: PProcedureSignature; AMethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildProcedure(const AMetaType: PLuaMetaType; const AProcedure: PTypeInfo; AMethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildReferenceMethod(const AMetaType: PLuaMetaType; const AReference: PTypeInfo; AMethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    {$endif}
    function BuildIntfMethod(const AMetaType: PLuaMetaType; const AMethodEntry: PIntfMethodEntry; AMethodKind: TLuaMethodKind = TLuaMethodKind($ff)): __luapointer;
    function BuildClassMethod(const AMetaType: PLuaMetaType; const AMethodEntry: PClassMethodEntry; AMethodKind: TLuaMethodKind = TLuaMethodKind($ff); const ASkipSelf: Boolean = True): __luapointer;
    function BuildCustom(const AMetaType: PLuaMetaType; const AParams: array of TLuaProcParam;
      const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean;
      const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv): __luapointer;
  end;

const
  LUA_METHOD_KINDS: array[{$ifdef UNITSCOPENAMES}System.{$endif}TypInfo.TMethodKind] of TLuaMethodKind = (
    {mkProcedure} mkStatic,
    {mkFunction} mkStatic,
    {mkConstructor} mkConstructor,
    {mkDestructor} mkDestructor,
    {mkClassProcedure} mkClass,
    {mkClassFunction} mkClass
    {$if Defined(EXTENDEDRTTI) or Defined(FPC)}
    {mkClassConstructor}, mkStatic
    {mkClassDestructor}, mkStatic
    {mkOperatorOverload}, mkOperator
    {$ifend}
    {$ifNdef FPC}
    {mkSafeProcedure}, mkStatic
    {mkSafeFunction}, mkStatic
    {$endif}
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

function TLuaInvokableBuilder.NewInvokable(const AMetaType: PLuaMetaType;
  const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv;
  const AResultType: PTypeInfo; const AResultTypeName: PShortString;
  const AResultReference: TLuaReference): PLuaInvokable;
{$ifdef WEAKREF}
label
  result_reference;
{$endif}
begin
  FBuffer.Size := 0;
  FAdvanced.Size := 0;

  Result := FBuffer.Alloc(SizeOf(TLuaInvokable) - SizeOf(TLuaInvokableParams));
  FillChar(Result^, SizeOf(TLuaInvokable) - SizeOf(TLuaInvokableParams), #0);

  Result.MethodKind := AMethodKind;
  Result.CallConv := ACallConv;
  Result.Instance := VALUE_NOT_DEFINED;
  Result.ConstructorFlag := VALUE_NOT_DEFINED;
  Result.Result.Value := VALUE_NOT_DEFINED;
  Result.Result.DataValue := VALUE_NOT_DEFINED;
  Result.Result.InsuranceValue := VALUE_NOT_DEFINED;

  if (AMethodKind = mkConstructor) then
  begin
    if (AMetaType.F.Kind = mtClass) then
    begin
      Result.Result.F.Kind := vkObject;
      Result.Result.F.MetaType := FLua.FObjectMetaType;
    end else
    begin
      Result.Result.F.Kind := vkRecord;
      Result.Result.F.MetaType := AMetaType;
    end;
  end else
  if (Assigned(AResultType)) or (Assigned(AResultTypeName)) then
  begin
    Result.Result.InternalSetRttiType(FLua, AResultType, AResultTypeName);
    if (Result.Result.Kind = vkUnknown) then
    begin
      FLua.FStringBuffer.Default := Format('Invalid result type "%s" (%s)', [
        FLua.FStringBuffer.Lua, FLua.FStringBuffer.Default]);
      Result := nil;
      Exit;
    end
    {$ifdef CPUX86}
    else
    if (Result.Result.Kind = vkFloat) and (ACallConv <> ccSafeCall) and
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
        if (AResultReference = TLuaReference($ff)) then
        begin
          {$ifdef EXTENDEDRTTI}
          Result.Result.ExtendedGetter := egUnsafe;
          {$endif}
        end else
        if (AResultReference <> lrDefault) then
        begin
          Result.Result.ExtendedGetter := egUnsafe;
        end;
      end;
    end;
    {$endif}
  end;

  {$ifdef CPUX86}
  if ACallConv = ccCdecl then
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

function TLuaInvokableBuilder.InternalAddName(const AName: LuaString): NativeInt;
var
  LLength, LSize: Integer;
  LBuffer: ^TLuaBuffer;
  S: PByte;
begin
  LLength := System.Length(AName);
  LBuffer := Pointer(@FLua.FInternalBuffer);
  LBuffer.Size := 0;

  // Utf8 <-- Utf16
  LSize := LLength * 3 + 1;
  if (LBuffer.Capacity < LSize) then LBuffer.Alloc(LSize);
  LBuffer.Size := Utf8FromUnicode(Pointer(LBuffer.FBytes), Pointer(AName), LLength);

  LSize := LBuffer.Size;
  if (LSize > High(Byte)) then LSize := High(Byte);

  Result := FAdvanced.Size;
  S := FAdvanced.Alloc(SizeOf(Byte) + LSize);
  PByte(S)^ := LSize;
  Inc(S);
  System.Move(Pointer(LBuffer.FBytes)^, S^, LSize);
end;

function TLuaInvokableBuilder.InternalAddParam(const AName: PShortString; const ATypeInfo: Pointer;
  const ATypeName: PShortString; const APointerDepth: Integer; const AParamFlags: TParamFlags): PLuaInvokableParam;
{$ifdef WEAKREF}
label
  managed_reference;
{$endif}
var
  LCustomVariable: TLuaCustomVariable;
  LTotalPointerDepth: Integer;
  LHighArray: PLuaInvokableParam;

  function CallConv: TCallConv;
  begin
    Result := PLuaInvokable(FBuffer.FBytes).CallConv;
  end;

  function IsConst: Boolean;
  begin
    Result := (pfConst in AParamFlags);
  end;
begin
  {$ifdef EXTENDEDRTTI}
  if (pfResult in AParamFlags) then
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

  LTotalPointerDepth := LCustomVariable.InternalSetRttiType(FLua, ATypeInfo, ATypeName, True);
  if (LTotalPointerDepth < 0) then
  begin
    Result := nil;
    Exit;
  end;
  Inc(LTotalPointerDepth, APointerDepth);

  Result := FBuffer.Alloc(SizeOf(TLuaInvokableParam));
  Inc(PLuaInvokable(FBuffer.FBytes).MaxParamCount);

  PLuaCustomVariable(Result)^ := LCustomVariable;
  Result.Name := AName;
  Result.Value := VALUE_NOT_DEFINED;
  Result.DataValue := VALUE_NOT_DEFINED;
  Result.InsuranceValue := VALUE_NOT_DEFINED;

  Result.IsArray := (pfArray in AParamFlags);
  Result.IsReference := ([pfVar, pfReference, pfOut] * AParamFlags <> []);
  Result.IsPointer := (Result.F.Kind = vkPointer) or (LTotalPointerDepth <> 0) {or (Result.IsExtendedMetaPointer)} or
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

  Result.TotalPointerDepth := LTotalPointerDepth + Ord(Result.IsReference);
  if (Result.IsArray) then
  begin
    LHighArray := FBuffer.Alloc(SizeOf(TLuaInvokableParam));
    LHighArray.InternalSetTypeInfo(FLua, System.TypeInfo(Integer));
    LHighArray.Name := Pointer(@PARAMNAME_HIGH_ARRAY);
    LHighArray.Value := VALUE_NOT_DEFINED;
    LHighArray.DataValue := VALUE_NOT_DEFINED;
    LHighArray.InsuranceValue := VALUE_NOT_DEFINED;
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

function TLuaInvokableBuilder.InternalAddParam(const AParam: TLuaProcParam): PLuaInvokableParam;
begin
  Result := InternalAddParam(Pointer(InternalAddName(AParam.Name)), AParam.TypeInfo, nil,
    AParam.PointerDepth, AParam.Flags);
end;

procedure TLuaInvokableBuilder.InternalParamsDone(var AInvokable: TLuaInvokable);
const
  MASK_BUFFERED = 1 shl 30;
  NATIVE_SHIFT = {$ifdef LARGEINT}3{$else}2{$endif};
type
  TResultArgMode = (ramNone, ramRegister, ramRefFirst, ramRefLast);
  TSelfArgMode = (samNone, samFirst, samSecond, samLast);
var
  i: Integer;
  LIsStatic, LIsConstructor, LIsBackwardArg: Boolean;
  LResultArgMode: TResultArgMode;
  LSelfArgMode: TSelfArgMode;
  LParam: ^TLuaInvokableParam;
  LRegGen, LRegExt: Integer;
  LTempParam: TLuaInvokableParam;

  function AllocRegGen(const ACount: Integer = 1): Integer;
  begin
    if (LRegGen >= REGGEN_COUNT) then
    begin
      Result := VALUE_NOT_DEFINED;
    end else
    begin
      Result := LRegGen * REGGEN_SIZE + NativeInt(@PParamBlock(nil).RegsGen);
      Inc(LRegGen, ACount);
      {$if Defined(CPUX64) and Defined(MSWINDOWS)}
      LRegExt := LRegGen;
      {$ifend}
    end;
  end;

  function AllocRegExt(const ACount: Integer = 1): Integer;
  begin
    if (LRegExt >= REGEXT_COUNT) then
    begin
      Result := VALUE_NOT_DEFINED;
    end else
    begin
      Result := LRegExt * REGEXT_SIZE + NativeInt(@PParamBlock(nil).RegsExt);
      Inc(LRegExt, ACount);
      {$if Defined(CPUX64) and Defined(MSWINDOWS)}
      LRegGen := LRegExt;
      {$ifend}
    end;
  end;

  function PutStack(var AValue: Integer; ASize: Integer): Integer;
  begin
    ASize := (ASize + SizeOf(Pointer) - 1) and (-SizeOf(Pointer));

    if (LIsBackwardArg) then
    begin
      AValue := SizeOf(TParamBlock) + AInvokable.StackDataSize;
      Inc(AInvokable.StackDataSize, ASize);
    end else
    begin
      Inc(AInvokable.StackDataSize, ASize);
      AValue := -AInvokable.StackDataSize;
    end;

    Result := AValue;
  end;

  function PutBuffer(var AValue: Integer; ASize: Integer): Integer;
  begin
    ASize := (ASize + SizeOf(Pointer) - 1) and (-SizeOf(Pointer));

    AValue := MASK_BUFFERED or AInvokable.TotalDataSize;
    Inc(AInvokable.TotalDataSize, ASize);

    Result := AValue;
  end;

  function PutGen(var AValue: Integer): Integer;
  begin
    AValue := AllocRegGen;
    Result := AValue;
  end;

  function PutExt(var AValue: Integer): Integer;
  begin
    AValue := AllocRegExt;
    Result := AValue;
  end;

  function PutPtr(var AValue: Integer): Integer;
  begin
    Result := PutGen(AValue);
    if (Result = VALUE_NOT_DEFINED) then
      Result := PutStack(AValue, SizeOf(Pointer));
  end;

  procedure PutArg;
  var
    LSize: Integer;
    {$ifdef CPUARM32}
    LRegL, LRegH: Integer;
    {$endif}
  begin
    LSize := LParam.Size;

    // array of ...
    if (LParam.IsArray) then
    begin
      LParam.DataValue := PutPtr(LParam.Value);
      PutBuffer(LParam.InsuranceValue, SizeOf(Pointer));
      Exit;
    end;

    // references: pointer + buffered data
    if (LParam.TotalPointerDepth <> 0) then
    begin
      LParam.DataValue := PutPtr(LParam.Value);

      if (LParam.TotalPointerDepth > 1) then
      begin
        PutBuffer(LParam.DataValue, (LParam.TotalPointerDepth - 1) * SizeOf(Pointer));
        Inc(LParam.DataValue, (LParam.TotalPointerDepth - 1) * SizeOf(Pointer));
      end;

      case LParam.F.Kind of
        vkRecord, vkSet, vkArray:
        begin
          if (LParam.TotalPointerDepth > 1) then
            Dec(LParam.DataValue, SizeOf(Pointer));

          LParam.TotalPointerDepth := LParam.TotalPointerDepth - 1;
        end;
      else
        PutBuffer(LParam.DataValue, LSize);
      end;

      if (LParam.IsInsurance) then
      begin
        PutBuffer(LParam.InsuranceValue, SizeOf(Pointer));
      end;

      Exit;
    end;

    // whole data: registers/stack
    if (LParam.Kind = vkFloat) and (LParam.FloatType in [ftSingle, ftDouble, ftExtended]) then
    begin
      case LSize of
        4:
        begin
          LParam.DataValue := PutExt(LParam.Value);
          if (LParam.Value <> VALUE_NOT_DEFINED) then
            Exit;
        end;
        8:
        begin
          {$ifdef CPUARM32}
            if (LRegExt + 2 <= REGEXT_COUNT) then
            begin
              LParam.Value := AllocRegExt(2);
              LParam.DataValue := LParam.Value;
              Exit;
            end;
          {$else}
            LParam.DataValue := PutExt(LParam.Value);
            if (LParam.Value <> VALUE_NOT_DEFINED) then
              Exit;
          {$endif}
        end;
      else
        {$ifdef CPUARM64}
        LParam.DataValue := PutExt(LParam.Value);
        if (LParam.Value <> VALUE_NOT_DEFINED) then
          Exit;
        {$endif}
      end;
    end
    {$ifdef CPUARM64}
    else
    if (LParam.Kind = vkRecord) and (LParam.MetaType.HFA) then
    begin
      LSize := LParam.MetaType.HFAElementCount * SizeOf(Single);
      if (LParam.MetaType.HFAElementType <> ftSingle) then LSize := LSize * 2;

      if (LRegExt + LParam.MetaType.HFAElementCount <= REGEXT_COUNT) then
      begin
        AInvokable.ARMPartialSize := LSize;
        LParam.IsARMPartial := True;
        LParam.Value := AllocRegExt(LParam.MetaType.HFAElementCount);
        LParam.DataValue := LParam.Value;
        Exit;
      end else
      begin
        RegExt := REGEXT_COUNT;
      end;
    end
    {$endif}
    {$ifdef CPUARM}
    else
    if (LParam.Kind in [vkRecord{$ifdef CPUARM32}, vkArray{$endif}]) then
    begin
      if (LRegGen + ((LSize + SizeOf(Pointer) - 1) shr NATIVE_SHIFT) <= REGGEN_COUNT) then
      begin
        LParam.Value := AllocRegGen((LSize + SizeOf(Pointer) - 1) shr NATIVE_SHIFT);
        LParam.DataValue := LParam.Value;
        Exit;
      end else
      if (LRegGen <> REGGEN_COUNT) then
      begin
        AInvokable.ARMPartialSize := (REGGEN_COUNT - LRegGen) shl NATIVE_SHIFT;
        LParam.IsARMPartial := True;
        LParam.Value := AllocRegExt(REGGEN_COUNT - LRegGen);
        PutStack(LParam.DataValue, LSize - AInvokable.ARMPartialSize);
        Exit;
      end;
    end
    {$endif};

    case LSize of
      1, 2, 4{$ifdef LARGEINT},8{$ifNdef MSWINDOWS}3,5,6,7{$endif}{$endif}:
      begin
        LParam.DataValue := PutPtr(LParam.Value);
        Exit;
      end;
      {$if Defined(CPUX64) and (not Defined(MSWINDOWS))}
      9..16:
      begin
        if (PutGen(LParam.Value)) and (AllocRegGen >= 0) then
        begin
          LParam.DataValue := LParam.Value;
          Exit;
        end;
      end;
      {$ifend}
      {$ifdef CPUARM32}
      8:
      begin
        {$ifNdef IOS}
        Inc(LRegGen, LRegGen and 1);
        {$endif}
        LRegL := AllocRegGen;
        LRegH := AllocRegGen;

        if (LRegL >= 0) then
        begin
          if (LRegH >= 0) then
          begin
            LParam.Value := LRegL;
            LParam.DataValue := LParam.Value;
            Exit;
          end else
          begin
            AInvokable.ARMPartialSize := 4;
            LParam.IsARMPartial := True;
            LParam.Value := LRegL;
            PutStack(LParam.DataValue, 4);
            Exit;
          end;
        end else
        begin
          {$ifNdef IOS}
          Inc(AInvokable.StackDataSize,A Invokable.StackDataSize and 4);
          {$endif}
        end;
      end;
      {$endif}
    end;

    LParam.DataValue := PutStack(LParam.Value, LSize);
    if (LParam.IsInsurance{PAnsiChar, PWideChar}) then
      PutBuffer(LParam.InsuranceValue, SizeOf(Pointer));
  end;

  procedure PutResult;
  var
    LPtr: Pointer;
  begin
    LParam := @AInvokable.Result;
    LParam.Name := Pointer(@PARAMNAME_RESULT);

    if (LResultArgMode = ramRegister) then
    begin
      AInvokable.ResultMode := rmRegister;
      LPtr :=
        {$if Defined(CPUINTEL)}
          @PParamBlock(nil).OutEAX
        {$elseif Defined(CPUARM32)}
          @PParamBlock(nil).OutCR
        {$else .CPUARM64}
          @PParamBlock(nil).OutX
        {$ifend}
        ;

      if (LParam.F.Kind = vkFloat) then
      begin
        {$ifdef CPUX86}
        if (LParam.F.FloatType = ftExtended{ftSingle, ftDouble}) then
        begin
          LPtr := @PParamBlock(nil).OutFPU;
        end else
        begin
          LPtr := @PParamBlock(nil).OutInt64{ftComp, ftCurr};
        end;
        {$endif}

        {$ifdef CPUX64}
        if (not (LParam.F.FloatType in [ftComp, ftCurr])) then
        begin
          LPtr := @PParamBlock(nil).OutXMM0;

          {$ifNdef MSWINDOWS}
          if (LParam.F.FloatType = ftExtended) then
          begin
            LPtr := @PParamBlock(nil).OutFPU;
          end;
          {$endif}
        end;
        {$endif}

        {$if (Defined(CPUARM32) and (not Defined(ARM_NO_VFP_USE))) or Defined(CPUARM64)}
        if {$ifdef CPUARM32}(AInvokable.CallConv = ccReg) and {$endif}
          (LParam.F.FloatType in [ftSingle, ftDouble, ftExtended]) then
        begin
          LPtr := @PParamBlock(nil).{$ifdef CPUARM32}OutD{$else .CPUARM64}OutQ{$endif};
        end;
        {$ifend}
      end;

      {$ifdef CPUARM64}
      if (LParam.F.Kind = vkRecord) and (LParam.MetaType.HFA) then
      begin
        LParam.IsARMPartial := True;
        LPtr := @PParamBlock(nil).OutHFA;

        if (LParam.MetaType.HFAElementType = ftSingle) then
        begin
          AInvokable.ResultMode := rmHFASingle;
        end else
        begin
          AInvokable.ResultMode := rmHFADouble;
        end;
      end;
      {$endif}

      LParam.Value := NativeInt(LPtr);
      LParam.DataValue := LParam.Value;
      Exit;
    end;

    {$ifdef CPUARM64}
    if (ResultMode = rmRefFirst) then
    begin
      LParam.Value := NativeInt(@PParamBlock(nil).RegX8);
    end else
    {$endif}
    begin
      PutPtr(LParam.Value);
    end;

    if (LParam.F.Kind in [vkObject, vkInterface, vkRecord, vkArray, vkSet]) then
    begin
      // user data
      AInvokable.ResultMode := rmUserData;
    end else
    begin
      // buffer
      LParam.TotalPointerDepth := 1;
      AInvokable.ResultMode := rmBuffer;
      PutBuffer(LParam.DataValue, LParam.Size);

      // is managed
      case LParam.F.Kind of
        vkString:
        begin
          if (LParam.F.StringType in [stAnsiString, stWideString, stUnicodeString]) then
            AInvokable.ResultMode := rmManagedBuffer;
        end;
        vkVariant:
        begin
          AInvokable.ResultMode := rmManagedBuffer;
        end;
        vkClosure:
        begin
          if (LParam.ExtendedGetter <> egUnsafe) then
            AInvokable.ResultMode := rmManagedBuffer;
        end;
      end;

      // temporary hint
      if (AInvokable.ResultMode = rmManagedBuffer) then
      begin
        LParam.IsExtendedGetterTemporary := True;
      end;
    end;
  end;

  procedure PutSelf;
  begin
    PutPtr(AInvokable.Instance);
  end;

  procedure ApplyOffset(var AValue: Integer); overload;
  var
    LFrameSize: Integer;
  begin
    LFrameSize := SizeOf(TParamBlock) + AInvokable.StackDataSize;

    if (AValue < 0) then
    begin
      if (AValue <> VALUE_NOT_DEFINED) then
        Inc(AValue, LFrameSize);
    end else
    if (AValue and MASK_BUFFERED <> 0) then
    begin
      AValue := (AValue - MASK_BUFFERED) + LFrameSize;
    end;
  end;

  procedure ApplyOffset; overload;
  begin
    ApplyOffset(LParam.Value);
    ApplyOffset(LParam.DataValue);
    ApplyOffset(LParam.InsuranceValue);
  end;
begin
  // flags
  LIsStatic := (AInvokable.MethodKind = mkStatic) or
    ((AInvokable.Result.Kind <> vkClass) and (AInvokable.MethodKind = mkConstructor));
  LIsConstructor := (AInvokable.MethodKind = mkConstructor);
  LIsBackwardArg := {$ifdef CPUX86}(AInvokable.CallConv in [ccCdecl, ccStdCall, ccSafeCall]){$else}True{$endif};

  // result argument mode
  LResultArgMode := ramNone;
  LParam := @AInvokable.Result;
  if (LParam.F.Kind <> vkUnknown) then
  begin
    LResultArgMode := ramRegister;
    if (LParam.IsReference) then
    begin
      LResultArgMode := ramRefLast;

      {$if not Defined(CPUX86)}
      if (AInvokable.CallConv <> ccSafeCall) then
      begin
        LResultArgMode := ramRefFirst;
      end;
      {$ifend}
    end;
  end;

  // self argument mode
  LSelfArgMode := samNone;
  if (not LIsStatic) then
  begin
    {$ifdef CPUX86}
    if (AInvokable.CallConv = ccPascal) then
    begin
      LSelfArgMode := samLast;
    end else
    {$endif}
    begin
      LSelfArgMode := samFirst;

      {$if Defined(CPUARM)}
      if (LResultArgMode = ramRefFirst) then
      begin
        SelfArgMode := samSecond;
      end;
      {$ifend}
    end;
  end;

  // registers
  LRegGen := 0;
  LRegExt := 0;
  {$ifdef CPUX86}
  if (AInvokable.CallConv <> ccReg) then
    LRegGen := REGGEN_COUNT;
  {$endif}
  {$ifdef CPUARM32}
  if (AInvokable.CallConv <> ccReg) then
    LRegExt := REGEXT_COUNT;
  {$endif}

  // self, first result
  if (LSelfArgMode = samFirst) then PutSelf;
  if (LResultArgMode = ramRefFirst) then PutResult;
  if (LSelfArgMode = samSecond) then PutSelf;

  // constructor flag
  if (LIsConstructor) and (AInvokable.Result.Kind = vkClass) then
  begin
    LTempParam.InternalSetTypeInfo(nil, TypeInfo(ShortInt));
    LParam := @LTempParam;
    PutArg;
    AInvokable.ConstructorFlag := LTempParam.DataValue;
  end;

  // arguments
  LParam := @AInvokable.Params[Ord(LSelfArgMode = samLast)];
  for i := 0 to Integer(AInvokable.MaxParamCount) - 1 do
  begin
    PutArg;

    if (LParam.IsArray) then
    begin
      Inc(LParam);
      PutArg;
    end;

    Inc(LParam);
  end;

  // last result/self
  if (LResultArgMode = ramRefLast) then PutResult;
  if (LSelfArgMode = samLast) then PutSelf;
  if (LResultArgMode = ramRegister) then PutResult;

  // total buffer (stack) size
  // calculate real offsets
  Inc(AInvokable.TotalDataSize, SizeOf(TLuaInvokable) + AInvokable.StackDataSize);
  LParam := @AInvokable.Params[0];
  for i := 0 to Integer(AInvokable.MaxParamCount) - 1 do
  begin
    ApplyOffset;

    if (LParam.IsArray) then
    begin
      Inc(LParam);
      ApplyOffset;
    end;

    Inc(LParam);
  end;
  if (LResultArgMode in [ramRefFirst, ramRefLast]) then
  begin
    LParam := @AInvokable.Result;
    ApplyOffset;
  end;
  ApplyOffset(AInvokable.Instance);
  ApplyOffset(AInvokable.ConstructorFlag);
end;

function TLuaInvokableBuilder.InternalBuildDone: __luapointer;
var
  i: Integer;
  LOffset: NativeInt;
  LInvokable: PLuaInvokable;
  LParam: ^TLuaInvokableParam;

  function IsParamHFAManaged: Boolean;
  begin
    Result := (LParam.F.Kind in [vkRecord, vkArray]) and
      (LParam.F.MetaType.HFA) and (LParam.F.MetaType.Managed);
  end;

  function IsParamFinal: Boolean;
  begin
    Result := False;

    if (LParam.IsInsurance) or (IsParamHFAManaged) then
    begin
      Result := True;
      Exit;
    end;

    case LParam.F.Kind of
      vkVariant:
      begin
        Result := True;
      end;
      vkString:
      begin
        Result := (LParam.F.StringType in [stAnsiString, stWideString, stUnicodeString]);
      end;
      vkClosure:
      begin
        if (LParam = @LInvokable.Result) then
        begin
          if (LParam.ExtendedGetter <> egUnsafe) then
            Result := True;
        end else
        begin
          if (LParam.ExtendedSetter <> esUnsafe) then
            Result := True;
        end;
      end;
    end;
  end;

  function IsParamInitial: Boolean;
  begin
    Result := (LParam.TotalPointerDepth <> 0) or (LParam.IsInsurance) or (IsParamFinal);
  end;
begin
  // result param reference and getter mode
  LInvokable := Pointer(FBuffer.FBytes);
  LParam := @LInvokable.Result;
  if (LParam.F.Kind <> vkUnknown) then
  begin
    if (LParam.IsExtendedMetaPointer) then
    begin
      LParam.IsPointer := True;
    end;

    if (LInvokable.CallConv = ccSafeCall) then
    begin
      LParam.IsReference := True;
    end else
    case LParam.Kind of
      {$ifdef AUTOREFCOUNT}
      vkObject:
      begin
        if (AInvokable.MethodKind = mkConstructor) then
        begin
          LParam.ExtendedGetter := egUnsafe;
        end else
        begin
          if (LParam.ExtendedGetter <> egUnsafe) then
            LParam.IsReference := True;
        end;
      end;
      {$endif}
      vkString:
      begin
        if (LParam.F.StringType in [stShortString, stAnsiString, stWideString, stUnicodeString]) then
          LParam.IsReference := True;
      end;
      vkVariant:
      begin
        LParam.IsReference := True;
      end;
      vkRecord, vkArray, vkSet:
      begin
        if (LParam.F.MetaType.ReturnReferenced) then
          LParam.IsReference := True;
      end;
      vkInterface:
      begin
        if (LParam.ExtendedGetter <> egUnsafe) then
          LParam.IsReference := True;
      end;
      vkClosure:
      begin
        case LParam.F.ClosureMode of
          mmMethod:
          begin
            LParam.IsReference := True;
          end;
          mmReference:
          begin
            if (LParam.ExtendedGetter <> egUnsafe) then
              LParam.IsReference := True;
          end;
        end;
      end;
    end;
  end;

  // buffering
  InternalParamsDone(LInvokable^);

  // align advanced buffer
  if (FAdvanced.Size and (SizeOf(Pointer) - 1) <> 0) then
  begin
    FAdvanced.Alloc(SizeOf(Pointer) - (FAdvanced.Size and (SizeOf(Pointer) - 1)));
  end;

  // initial params
  LInvokable := Pointer(FBuffer.FBytes);
  LInvokable.MinParamCount := LInvokable.MaxParamCount;
  LInvokable.Initials := Pointer(FBuffer.Size + FAdvanced.Size);
  LParam := @LInvokable.Params[0];
  for i := 0 to Integer(LInvokable.MaxParamCount) - 1 do
  begin
    if (IsParamInitial) then
    begin
      PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(LParam) - NativeInt(LInvokable);
      Inc(LInvokable.InitialCount);
    end;
    if (LParam.IsArray) then
      Inc(LParam);

    Inc(LParam);
  end;
  LParam := @LInvokable.Result;
  if (LInvokable.ResultMode in [rmBuffer, rmManagedBuffer]) or (IsParamHFAManaged) then
  begin
    PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(LParam) - NativeInt(LInvokable);
    Inc(LInvokable.InitialCount);
  end;

  // final params
  LParam := @LInvokable.Params[0];
  LInvokable.Finals := Pointer(FBuffer.Size + FAdvanced.Size);
  for i := 0 to Integer(LInvokable.MaxParamCount) - 1 do
  begin
    if (IsParamFinal) then
    begin
      PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(LParam) - NativeInt(LInvokable);
      Inc(LInvokable.FinalCount);
    end;
    if (LParam.IsArray) then
      Inc(LParam);

    Inc(LParam);
  end;
  LParam := @LInvokable.Result;
  if (LInvokable.ResultMode = rmManagedBuffer) or (IsParamHFAManaged) then
  begin
    PInteger(FAdvanced.Alloc(SizeOf(Integer)))^ := NativeInt(LParam) - NativeInt(LInvokable);
    Inc(LInvokable.FinalCount);
  end;

  // concatenate buffers, unpack internal names
  LOffset := FBuffer.Size;
  if (FAdvanced.Size <> 0) then
  begin
    System.Move(Pointer(FAdvanced.FBytes)^, FBuffer.Alloc(FAdvanced.Size)^, FAdvanced.Size);
  end;
  LInvokable := Pointer(FBuffer.FBytes);
  LParam := @LInvokable.Params[0];
  LOffset := LOffset{Last FBuffer.Size} + NativeInt(LInvokable);
  for i := 0 to Integer(LInvokable.MaxParamCount) - 1 do
  begin
    if (NativeUInt(LParam.Name) <= $ffff) then
      Inc(NativeInt(LParam.Name), LOffset);

    if (LParam.IsArray) then
      Inc(LParam){"High(Array)"};

    Inc(LParam);
  end;

  // hash comparison
  // ToDo

  // result
  Result := TLuaMemoryHeap(FLua.FMemoryHeap).Alloc(FBuffer.Size);
  LInvokable := TLuaMemoryHeap(FLua.FMemoryHeap).Unpack(Result);
  System.Move(Pointer(FBuffer.FBytes)^, LInvokable^, FBuffer.Size);
  Inc(NativeInt(LInvokable.Initials), NativeInt(LInvokable));
  Inc(NativeInt(LInvokable.Finals), NativeInt(LInvokable));

  // result names (internal)
  LParam := @LInvokable.Params[0];
  LOffset := NativeInt(LInvokable) - NativeInt(Pointer(FBuffer.FBytes));
  for i := 0 to Integer(LInvokable.MaxParamCount) - 1 do
  begin
    if (NativeUInt(LParam.Name) >= NativeUInt(FBuffer.FBytes)) and
      (NativeUInt(LParam.Name) < NativeUInt(FBuffer.FBytes) + NativeUInt(FBuffer.Size)) then
      Inc(NativeInt(LParam.Name), LOffset);

    if (LParam.IsArray) then
      Inc(LParam){"High(Array)"};

    Inc(LParam);
  end;
end;

function TLuaInvokableBuilder.BuildMethod(const AMetaType: PLuaMetaType; const AMethod: PTypeInfo;
  AMethodKind: TLuaMethodKind): __luapointer;
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
  LPtr: PByte;
  LTypeData: PTypeData;
  LParamCount, i: Integer;
  LMethodInfo: TMethodInfo;
  LParam: TMethodParam;

  procedure ReadParam(const AIndex: Integer);
  begin
    LParam.Flags := TParamFlags(Pointer(LPtr)^);
    Inc(LPtr, SizeOf(TParamFlags));
    LParam.Name := Pointer(LPtr);
    LPtr := GetTail(LPtr^);
    LParam.TypeName := Pointer(LPtr);
    LPtr := GetTail(LPtr^);
    if (AIndex >= 0) then
    begin
      LParam.TypeInfo := GetTypeInfo(LMethodInfo.ParamTypeRefs[AIndex]);
    end;
  end;
begin
  Result := LUA_POINTER_INVALID;
  if (AMethodKind = TLuaMethodKind($ff)) then
  begin
    AMethodKind := mkInstance;
  end;

  // skip parameters, fill method information
  LTypeData := GetTypeData(AMethod);
  LParamCount := LTypeData.ParamCount;
  LPtr := Pointer(@LTypeData.ParamList);
  for i := 0 to LParamCount - 1 do
  begin
    ReadParam(-1);
  end;
  if (LTypeData.MethodKind = mkFunction) then
  begin
    LMethodInfo.ResultTypeName := Pointer(LPtr);
    LPtr := GetTail(LPtr^);
    LMethodInfo.ResultTypeInfo := GetTypeInfo(PPointer(LPtr)^);
    Inc(LPtr, SizeOf(Pointer));
  end else
  begin
    LMethodInfo.ResultTypeName := nil;
    LMethodInfo.ResultTypeInfo := nil;
  end;
  LMethodInfo.CC := TCallConv(Pointer(LPtr)^);
  Inc(LPtr, SizeOf(TCallConv));
  LMethodInfo.ParamTypeRefs := Pointer(LPtr);

  // initialization
  if (not Assigned(NewInvokable(AMetaType, AMethodKind, LMethodInfo.CC,
    LMethodInfo.ResultTypeInfo, LMethodInfo.ResultTypeName, lrDefault{TLuaReference($ff)}))) then
    Exit;

  // parameters
  LPtr := Pointer(@LTypeData.ParamList);
  for i := 0 to LParamCount - 1 do
  begin
    ReadParam(i);
    if (InternalAddParam(LParam.Name, LParam.TypeInfo, LParam.TypeName, 0, LParam.Flags) = nil) then
      Exit;
  end;

  // done
  Result := InternalBuildDone;
end;

{$ifdef EXTENDEDRTTI}
function TLuaInvokableBuilder.BuildProcedureSignature(const AMetaType: PLuaMetaType;
  const ASignature: PProcedureSignature; AMethodKind: TLuaMethodKind): __luapointer;
var
  i: Integer;
  LCallConv: TCallConv;
  LResultType: PTypeInfo;
  LParam: PProcedureParam;
  LTypeInfo: PTypeInfo;
begin
  Result := LUA_POINTER_INVALID;
  if (ASignature.Flags = 255) then
  begin
    // error todo
    Exit;
  end;

  // method information
  if (AMethodKind = TLuaMethodKind($ff)) then
  begin
    AMethodKind := mkStatic;
  end;
  LCallConv := ASignature.CC;
  LResultType := GetTypeInfo(ASignature.ResultType);
  if (not Assigned(NewInvokable(AMetaType, AMethodKind, LCallConv, LResultType, nil, lrDefault{TLuaReference($ff)}))) then
    Exit;

  // parameters
  LParam := Pointer(NativeUInt(ASignature) + SizeOf(TProcedureSignature));
  for i := 0 to Integer(ASignature.ParamCount) - 1 do
  begin
    LTypeInfo := GetTypeInfo(LParam.ParamType);
    if (InternalAddParam(Pointer(@LParam.Name), LTypeInfo, nil, 0, TParamFlags(LParam.Flags)) = nil) then
      Exit;

    LParam := SkipAttributes(GetTail(LParam.Name));
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildProcedure(const AMetaType: PLuaMetaType; const AProcedure: PTypeInfo;
  AMethodKind: TLuaMethodKind): __luapointer;
begin
  Result := BuildProcedureSignature(AMetaType, GetTypeData(AProcedure).ProcSig, AMethodKind);
end;

function TLuaInvokableBuilder.BuildReferenceMethod(const AMetaType: PLuaMetaType; const AReference: PTypeInfo;
  AMethodKind: TLuaMethodKind): __luapointer;
var
  LTypeData: PTypeData;
  LTable: PIntfMethodTable;
  LMethodEntry: PIntfMethodEntry;
begin
  LTypeData := GetTypeData(AReference);
  LTable := GetTail(LTypeData.IntfUnit);
  if (Assigned(LTable)) and (LTable.Count = 1) and (LTable.RttiCount = 1) then
  begin
    LMethodEntry := Pointer(NativeUInt(LTable) + SizeOf(TIntfMethodTable));
    Result := BuildIntfMethod(AMetaType, LMethodEntry, AMethodKind);
  end else
  begin
    Result := LUA_POINTER_INVALID;
  end;
end;
{$endif}

function TLuaInvokableBuilder.BuildIntfMethod(const AMetaType: PLuaMetaType; const AMethodEntry: PIntfMethodEntry;
  AMethodKind: TLuaMethodKind): __luapointer;
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
  LPtr: PByte;
  LParamCount, i: Integer;
  LMethodInfo: TMethodInfo;
  LParam: TMethodParam;

  procedure ReadParam;
  begin
    LParam.Flags := TParamFlags(LPtr^);
    Inc(LPtr);
    LParam.Name := Pointer(LPtr);
    LPtr := GetTail(LPtr^);
    LParam.TypeName := Pointer(LPtr);
    LPtr := GetTail(LPtr^);
    LParam.TypeInfo := GetTypeInfo(PPointer(LPtr)^);
    Inc(LPtr, SizeOf(Pointer));
    LPtr := SkipAttributes(LPtr);
  end;
begin
  Result := LUA_POINTER_INVALID;
  if (AMethodKind = TLuaMethodKind($ff)) then
  begin
    AMethodKind := mkInstance;
  end;

  // skip parameters, fill method information
  LPtr := GetTail(AMethodEntry.Name);
  LMethodInfo.IsFunc := (LPtr^ = 1);
  Inc(LPtr);
  LMethodInfo.CC := TCallConv(LPtr^);
  Inc(LPtr);
  LParamCount := LPtr^;
  Inc(LPtr);
  LMethodInfo.ResultTypeName := nil;
  LMethodInfo.ResultTypeInfo := nil;
  if (LMethodInfo.IsFunc) then
  begin
    ReadParam;
    for i := 1 to LParamCount - 1 do
    begin
      ReadParam;
    end;
    if (LPtr^ <> 0) then
    begin
      LMethodInfo.ResultTypeName := Pointer(LPtr);
      LPtr := GetTail(LPtr^);
      LMethodInfo.ResultTypeInfo := GetTypeInfo(PPointer(LPtr)^);
    end;
  end;

  if (not Assigned(NewInvokable(AMetaType, AMethodKind, LMethodInfo.CC,
    LMethodInfo.ResultTypeInfo, LMethodInfo.ResultTypeName, lrDefault{TLuaReference($ff)}))) then
    Exit;

  // parameters
  LPtr := GetTail(AMethodEntry.Name);
  Inc(LPtr, SizeOf(Byte) + SizeOf(TCallConv) + SizeOf(Byte));
  ReadParam;
  for i := 1 to LParamCount - 1 do
  begin
    ReadParam;
    if (InternalAddParam(Pointer(@LParam.Name), LParam.TypeInfo, LParam.TypeName, 0, LParam.Flags) = nil) then
      Exit;
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildClassMethod(const AMetaType: PLuaMetaType;
  const AMethodEntry: PClassMethodEntry; AMethodKind: TLuaMethodKind;
  const ASkipSelf: Boolean): __luapointer;
type
  PClassMethodParam = ^TClassMethodParam;
  TClassMethodParam = packed record
    Flags: TParamFlags;
    ParamType: PPTypeInfo;
    Location: Word;
    Name: ShortString;
  end;
var
  LPtr: PByte;
  LCount, i: Integer;
  LCallConv: TCallConv;
  LResultType: PTypeInfo;
  LParam: PClassMethodParam;
begin
  Result := LUA_POINTER_INVALID;
  LPtr := GetTail(AMethodEntry.Name);
  if (LPtr = Pointer(NativeUInt(AMethodEntry) + AMethodEntry.Size)) then
  begin
    // todo?
    Exit;
  end;

  // method information
  if (AMethodKind = TLuaMethodKind($ff)) then
  begin
    AMethodKind := mkInstance;
  end;
  Inc(LPtr); // Version
  LCallConv := TCallConv(LPtr^);
  Inc(LPtr);
  LResultType := GetTypeInfo(PPointer(LPtr)^);
  Inc(LPtr, SizeOf(Pointer));
  if (not Assigned(NewInvokable(AMetaType, AMethodKind, LCallConv, LResultType, nil, TLuaReference($ff)))) then
    Exit;

  // parameters
  Inc(LPtr, SizeOf(Word)); // ParOff
  LCount := LPtr^;
  Inc(LPtr);
  if (ASkipSelf) then
  begin
    Dec(LCount);
    LPtr := GetTail(PClassMethodParam(LPtr).Name);
    LPtr := SkipAttributes(LPtr);
  end;
  for i := 1 to LCount do
  begin
    LParam := Pointer(LPtr);
    if (InternalAddParam(Pointer(@LParam.Name), GetTypeInfo(LParam.ParamType), nil, 0, LParam.Flags) = nil) then
      Exit;

    LPtr := GetTail(LParam.Name);
    LPtr := SkipAttributes(LPtr);
  end;

  // done
  Result := InternalBuildDone;
end;

function TLuaInvokableBuilder.BuildCustom(const AMetaType: PLuaMetaType; const AParams: array of TLuaProcParam;
  const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean;
  const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv): __luapointer;
var
  i: Integer;
begin
  Result := LUA_POINTER_INVALID;
  if (not Assigned(NewInvokable(AMetaType, AMethodKind, ACallConv, AResultType, nil,
    TLuaReference(Ord(AIsResultUnsafe) * 2{lrDefault/lrUnsafe})))) then
    Exit;

  for i := Low(AParams) to High(AParams) do
  if (not Assigned(InternalAddParam(AParams[i]))) then
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

function TLuaUnit.GetLine(const AIndex: Integer): TLuaUnitLine;
var
  LIndex: Integer;
begin
  LIndex := AIndex - 1;
  if (Cardinal(LIndex) >= Cardinal(FLinesCount)) then
    raise ELua.CreateFmt('Can''t get line %d from unit "%s". Lines count = %d', [LIndex + 1, Name, FLinesCount]);

  Result := FLines[LIndex];
end;

function TLuaUnit.GetItem(const AIndex: Integer): LuaString;
var
  LLine: TLuaUnitLine;
begin
  LLine := Self.GetLine(AIndex);
  FLua.unpack_lua_string(Result, LLine.Chars, LLine.Count);
end;

procedure TLuaUnit.InitializeLines;
var
  LChars: PByte;
  X: NativeUInt;
  LIndex, LSize: Integer;
begin
  // calculate lines count
  LIndex := 0;
  LSize := Length(FData) - FDataOffset;
  NativeInt(LChars) := NativeInt(FData) + FDataOffset;
  if (LSize > 0) then
  begin
    repeat
      if (LSize = 0) then Break;
      X := LChars^;
      Inc(LChars);
      Dec(LSize);

      if (X <= 13) then
      case X of
        10, 13:
        begin
          Inc(LIndex);
          if (LSize > 0) and (LChars^ = X xor 7) then
          begin
            Inc(LChars);
            Dec(LSize);
          end;
        end;
      end;
    until (False);
    Inc(LIndex);
  end;

  // fill lines
  FLinesCount := LIndex;
  SetLength(FLines, LIndex);
  LIndex := 0;
  LSize := Length(FData) - FDataOffset;
  NativeInt(LChars) := NativeInt(FData) + FDataOffset;
  if (LSize > 0) then
  begin
    FLines[LIndex].Chars := Pointer(LChars);
    repeat
      if (LSize = 0) then Break;
      X := LChars^;
      Inc(LChars);
      Dec(LSize);

      if (X <= 13) then
      case X of
        10, 13:
        begin
          FLines[LIndex].Count := NativeInt(LChars) - NativeInt(FLines[LIndex].Chars) - 1;
          Inc(LIndex);
          if (LSize > 0) and (LChars^ = X xor 7) then
          begin
            Inc(LChars);
            Dec(LSize);
          end;
          FLines[LIndex].Chars := Pointer(LChars);
        end;
      end;
    until (False);
    FLines[LIndex].Count := NativeInt(LChars) - NativeInt(FLines[LIndex].Chars);
  end;
end;

procedure TLuaUnit.SaveToFile(const AFileName: string);
var
  LChars: PByte;
  LSize: Integer;
  LHandle: THandle;
begin
  LSize := Length(FData) - FDataOffset;
  NativeInt(LChars) := NativeInt(FData) + FDataOffset;

  LHandle := FileCreate(AFileName);
  if (LHandle = INVALID_HANDLE_VALUE) then
  begin
    raise ELua.CreateResFmt
      (Pointer(@SFCreateErrorEx), [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;

  try
    if (FileWrite(LHandle, BOM_INFO[sbUTF8], 3) <> 3) or
      ((LSize > 0) and (FileWrite(LHandle, LChars^, LSize) <> LSize)) then
    begin
      {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};
    end;
  finally
    FileClose(LHandle);
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
  LStandardNames: array[STD_TYPE..STD_VALUE] of __luaname;

  procedure AddSystemClosure(const AName: LuaString; const ACallback: Pointer; const P1, P2: __luapointer);
  begin
    lua_pushnil(Handle);
    push_lua_string(AName);
    alloc_push_luafunction(ACallback, P1, P2);
    __global_newindex(0, 0);
    lua_settop(Handle, 0);
    with PLuaGlobalEntity(GetGlobalEntity(AName, True))^ do
    begin
      Kind := gkConst;
      Constant := True;
    end;
  end;

  procedure FillStandardNameSpace(var ANameSpace: __TLuaDictionary; const AValues: array of Integer);
  var
    i: Integer;

    procedure NameSpaceAdd(AValue: Integer);
    begin
      case AValue of
        STD_INHERITS_FROM, STD_ASSIGN, STD_CREATE, STD_FREE,
        STD_RESIZE, STD_INCLUDE, STD_EXCLUDE, STD_CONTAINS: AValue := AValue or NAMESPACE_STD_PROC;
      end;

      TLuaDictionary(ANameSpace).Add(LStandardNames[AValue and $7f], NAMESPACE_STD_MASK or AValue);
    end;
  begin
    for i := STD_TYPE to STD_FREE do
      NameSpaceAdd(i);

    for i := Low(AValues) to High(AValues) do
      NameSpaceAdd(AValues[i]);
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
    LStandardNames[i] := lua_tolstring(Handle, -1, nil);
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
end;

destructor TLua.Destroy;
var
  i: Integer;
  LMetaType: PLuaMetaType;
begin
  // Lua
  if (FHandle <> nil) then lua_close(FHandle);

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
    LMetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(TLuaDictionary(FMetaTypes).FItems[i].Value);
    LMetaType.FName := '';
    case LMetaType.Kind of
      mtClass, mtInterface, mtRecord:
      begin
        TLuaDictionary(LMetaType.FNameSpace).Clear;
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

  inherited;
end;

procedure TLua.SetCodePage(AValue: Word);
var
  i, X, Y: Integer;
  LDest, LSrc, LTopSrc: Pointer;
  LBuffer: array[128..255] of AnsiChar;
begin
  // code page
  if (AValue = 0) then AValue := CODEPAGE_DEFAULT;
  FCodePage := AValue;

  // unicode table (128..255)
  if (AValue = $ffff) then
  begin
    LDest := @FUnicodeTable[128];
    X := 128 + (129 shl 16);
    for i := 1 to (128 div (SizeOf(Integer) div SizeOf(WideChar))) do
    begin
      PInteger(LDest)^ := X;
      Inc(X, $00010001);
      Inc(NativeInt(LDest), SizeOf(Integer));
    end;
  end else
  begin
    LDest := @LBuffer;
    X := 128 + (129 shl 8) + (130 shl 16) + (131 shl 24);
    for i := 1 to (128 div SizeOf(Integer)) do
    begin
      PInteger(LDest)^ := X;
      Inc(X, $04040404);
      Inc(NativeInt(LDest), SizeOf(Integer));
    end;

    {$ifdef MSWINDOWS}
      MultiByteToWideChar(AValue, 0, Pointer(@LBuffer), 128, Pointer(@FUnicodeTable[128]), 128);
    {$else}
      UnicodeFromLocaleChars(AValue, 0, Pointer(@LBuffer), 128, Pointer(@FUnicodeTable[128]), 128);
    {$endif}
  end;

  // utf8 table (128..255)
  LSrc := @FUnicodeTable[128];
  LTopSrc := @FUnicodeTable[High(FUnicodeTable)];
  LDest := Pointer(@FUTF8Table[128]);
  Dec(NativeInt(LSrc), SizeOf(WideChar));
  Dec(NativeInt(LDest), SizeOf(Cardinal));
  repeat
    if (LSrc = LTopSrc) then Break;
    Inc(NativeInt(LSrc), SizeOf(WideChar));
    Inc(NativeInt(LDest), SizeOf(Cardinal));

    X := PWord(LSrc)^;
    if (X <= $7ff) then
    begin
      if (X > $7f) then
      begin
        Y := (X and $3f) shl 8;
        X := (X shr 6) + $020080c0;
        Inc(X, Y);
        PCardinal(LDest)^ := X;
      end else
      begin
        X := X + $01000000;
        PCardinal(LDest)^ := X;
      end;
    end else
    begin
      Y := ((X and $3f) shl 16) + ((X and ($3f shl 6)) shl (8-6));
      X := (X shr 12) + $038080E0;
      Inc(X, Y);
      PCardinal(LDest)^ := X;
    end;
  until (False);
end;

function TLua.AnsiFromUnicode(ATarget: PAnsiChar; ACodePage: Word; ASource: PWideChar; ALength: Integer): Integer;
const
  CHARS_PER_ITERATION = SizeOf(Integer) div SizeOf(WideChar);
var
  LDest: PAnsiChar;
  LSource: PWideChar;
  LCount, X: Integer;
begin
  LCount := ALength;
  LDest := ATarget;
  LSource := ASource;

  if (LCount >= CHARS_PER_ITERATION) then
  repeat
    X := PInteger(LSource)^;
    if (X and $ff80ff80 <> 0) then Break;

    Inc(X, X shr 8);
    Dec(LCount, CHARS_PER_ITERATION);
    PWord(LDest)^ := X;
    Inc(LSource, CHARS_PER_ITERATION);
    Inc(LDest, CHARS_PER_ITERATION);
  until (LCount < CHARS_PER_ITERATION);

  if (LCount <> 0) then
  begin
    X := PWord(LSource)^;
    if (X and $ff80 = 0) then
    begin
      PByte(LDest)^ := X;
      Dec(LCount);
      Inc(LSource);
      Inc(LDest);
    end;

    if (LCount <> 0) then
    Inc(LDest,
      {$ifdef MSWINDOWS}
        WideCharToMultiByte(ACodePage, 0, LSource, LCount, Pointer(LDest), LCount, nil, nil)
      {$else}
        LocaleCharsFromUnicode(ACodePage, 0, LSource, LCount, Pointer(LDest), LCount, nil, nil)
      {$endif} );
  end;

  Result := NativeInt(LDest) - NativeInt(ATarget);
end;

procedure TLua.UnicodeFromAnsi(ATarget: PWideChar; ASource: PAnsiChar; ACodePage: Word; ALength: Integer);
const
  CHARS_PER_ITERATION = SizeOf(Integer) div SizeOf(AnsiChar);
type
  TUnicodeTable = array[Byte] of Word;
var
  LDest: PWideChar;
  LSource: PAnsiChar;
  LCount, X: Integer;
  LUnicodeTable: ^TUnicodeTable;
begin
  if (ACodePage = 0) then ACodePage := CODEPAGE_DEFAULT;
  if (ACodePage <> FCodePage) then SetCodePage(ACodePage);

  LCount := ALength;
  LDest := ATarget;
  LSource := ASource;
  LUnicodeTable := Pointer(@FUnicodeTable);

  if (LCount >= CHARS_PER_ITERATION) then
  repeat
    X := PInteger(LSource)^;
    if (X and $8080 = 0) then
    begin
      if (X and $80808080 = 0) then
      begin
        PCardinal(LDest)^ := Byte(X) + (X and $ff00) shl 8;
        X := X shr 16;
        Dec(LCount, 2);
        Inc(LSource, 2);
        Inc(LDest, 2);
      end;

      PCardinal(LDest)^ := Byte(X) + (X and $ff00) shl 8;
      Dec(LCount, 2);
      Inc(LSource, 2);
      Inc(LDest, 2);

      if (LCount < CHARS_PER_ITERATION) then Break;
    end else
    begin
      X := Byte(X);
      {$ifdef CPUX86}if (X > $7f) then{$endif} X := LUnicodeTable[X];
      PWord(LDest)^ := X;

      Dec(LCount);
      Inc(LSource);
      Inc(LDest);
      if (LCount < CHARS_PER_ITERATION) then Break;
    end;
  until (False);

  if (LCount <> 0) then
  repeat
    X := PByte(LSource)^;
    {$ifdef CPUX86}if (X > $7f) then{$endif} X := LUnicodeTable[X];
    PWord(LDest)^ := X;

    Dec(LCount);
    Inc(LSource);
    Inc(LDest);
  until (LCount = 0);
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
  X, U, LCount: NativeUInt;
  LDest: PAnsiChar;
  LSource: PWideChar;
{$ifdef CPUX86}
const
  MASK_FF80 = MASK_FF80_SMALL;
{$else .CPUX64/.CPUARM}
var
  MASK_FF80: NativeUInt;
{$endif}
begin
  LCount := ALength;
  LDest := ATarget;
  LSource := ASource;

  if (LCount = 0) then goto done;
  Inc(LCount, LCount);
  Inc(LCount, NativeUInt(LSource));
  Dec(LCount, (2 * SizeOf(Cardinal)));

  {$ifNdef CPUX86}
  MASK_FF80 := {$ifdef LARGEINT}MASK_FF80_LARGE{$else}MASK_FF80_SMALL{$endif};
  {$endif}

  // conversion loop
  if (NativeUInt(LSource) > LCount{TopSource}) then goto small_length;
  {$ifdef SMALLINT}
    X := PCardinal(@LSource[0])^;
    U := PCardinal(@LSource[2])^;
    if ((X or U) and Integer(MASK_FF80) = 0) then
  {$else}
    X := PNativeUInt(LSource)^;
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

      Inc(LSource, SizeOf(Cardinal));
      PCardinal(LDest)^ := X;
      Inc(LDest, SizeOf(Cardinal));

      if (NativeUInt(LSource) > LCount{TopSource}) then goto small_length;
    {$ifdef SMALLINT}
      X := PCardinal(@LSource[0])^;
      U := PCardinal(@LSource[2])^;
    until ((X or U) and Integer(MASK_FF80) <> 0);
    {$else}
      X := PNativeUInt(LSource)^;
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
    Inc(LSource);
    U := Word(X);
    if (X and $FF80 = 0) then
    begin
      if (X and MASK_FF80 = 0) then
      begin
        // ascii_2
        X := X shr 8;
        Inc(LSource);
        Inc(X, U);
        PWord(LDest)^ := X;
        Inc(LDest, 2);
      end else
      begin
        // ascii_1
        PByte(LDest)^ := X;
        Inc(LDest);
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
          PWord(LDest)^ := X;
          Inc(LDest, 2);
        end else
        begin
          PByte(LDest)^ := U;
          Inc(LDest);
        end;
      end else
      begin
        X := (U and $0fc0) shl 2;
        Inc(X, (U and $3f) shl 16);
        U := (U shr 12);
        Inc(X, $8080E0);
        Inc(X, U);

        PWord(LDest)^ := X;
        Inc(LDest, 2);
        X := X shr 16;
        PByte(LDest)^ := X;
        Inc(LDest);
      end;
    end else
    begin
      if (U >= $e000) then goto process_character;
      if (U >= $dc00) then
      begin
      unknown:
        PByte(LDest)^ := UNKNOWN_CHARACTER;
        Inc(LDest);
      end else
      begin
        Inc(LSource);
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

        PCardinal(LDest)^ := X;
        Inc(LDest, 4);
      end;
    end;
  end;

  if (NativeUInt(LSource) > LCount{TopSource}) then goto small_length;
  {$ifdef SMALLINT}
    X := PCardinal(@LSource[0])^;
    U := PCardinal(@LSource[2])^;
    if ((X or U) and Integer(MASK_FF80) = 0) then goto process4;
  {$else}
    X := PNativeUInt(LSource)^;
    if (X and MASK_FF80 = 0) then goto process4;
  {$endif}
  goto look_first;

small_length:
  U := LCount{TopSource} + (2 * SizeOf(Cardinal));
  if (U = NativeUInt(LSource)) then goto done;
  Dec(U, NativeUInt(LSource));
  if (U >= SizeOf(Cardinal)) then
  begin
    X := PCardinal(LSource)^;
    goto process_standard;
  end;
  U := PWord(LSource)^;
  Inc(LSource);
  if (U < $d800) then goto process_character;
  if (U >= $e000) then goto process_character;
  if (U >= $dc00) then goto unknown;

  PByte(LDest)^ := UNKNOWN_CHARACTER;
  Inc(LDest);

  // result
done:
  Result := NativeInt(LDest) - NativeInt(ATarget);
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
  X, U, LCount: NativeUInt;
  LDest: PWideChar;
  LSource: PAnsiChar;
{$ifdef CPUINTEL}
const
  MASK_80 = MASK_80_SMALL;
{$else .CPUARM}
var
  MASK_80: NativeUInt;
{$endif}
begin
  LCount := ALength;
  LDest := ATarget;
  LSource := ASource;

  if (LCount = 0) then goto done;
  Inc(LCount, NativeUInt(LSource));
  Dec(LCount, MAX_UTF8CHAR_SIZE);

  {$ifdef CPUARM}
    MASK_80 := MASK_80_SMALL;
  {$endif}

  // conversion loop
  if (NativeUInt(LSource) > LCount{TopSource}) then goto small_length;
  X := PCardinal(LSource)^;
  if (X and Integer(MASK_80) = 0) then
  begin
    repeat
    process4:
      Inc(LSource, SizeOf(Cardinal));

      {$ifNdef LARGEINT}
        PCardinal(LDest)^ := (X and $7f) + ((X and $7f00) shl 8);
        X := X shr 16;
        Inc(LDest, 2);
        PCardinal(LDest)^ := (X and $7f) + ((X and $7f00) shl 8);
        Inc(LDest, 2);
      {$else}
        PNativeUInt(LDest)^ := (X and $7f) + ((X and $7f00) shl 8) +
          ((X and $7f0000) shl 16) + ((X and $7f000000) shl 24);
        Inc(NativeUInt(LDest), SizeOf(NativeUInt));
      {$endif}

      if (NativeUInt(LSource) > LCount{TopSource}) then goto small_length;
      X := PCardinal(LSource)^;
    until (X and Integer(MASK_80) <> 0);
    goto look_first;
  end else
  begin
  look_first:
    if (X and $80 = 0) then
    begin
    process1_3:
      {$ifNdef LARGEINT}
        PCardinal(LDest)^ := (X and $7f) + ((X and $7f00) shl 8);
        Inc(LDest, 2);
        PCardinal(LDest)^ := ((X shr 16) and $7f);
      {$else}
        PNativeUInt(LDest)^ := (X and $7f) + ((X and $7f00) shl 8) +
          ((X and $7f0000) shl 16);
      {$endif}

      if (X and $8000 <> 0) then goto ascii_1;
      if (X and $800000 <> 0) then goto ascii_2;
      ascii_3:
        Inc(LSource, 3);
        Inc(LDest, 3{$ifdef SMALLINT}- 2{$endif});
        goto next_iteration;
      ascii_2:
        X := X shr 16;
        Inc(LSource, 2);
        {$ifdef LARGEINT}Inc(LDest, 2);{$endif}
        if (UTF8CHAR_SIZE[Byte(X)] <= 2) then goto process_standard;
        goto next_iteration;
      ascii_1:
        X := X shr 8;
        Inc(LSource);
        Inc(LDest, 1{$ifdef SMALLINT}- 2{$endif});
        if (UTF8CHAR_SIZE[Byte(X)] <= 3) then goto process_standard;
        // goto next_iteration;
    end else
    begin
    process_standard:
      if (X and $C0E0 = $80C0) then
      begin
        X := ((X and $1F) shl 6) + ((X shr 8) and $3F);
        Inc(LSource, 2);

      process_character:
        PWord(LDest)^ := X;
        Inc(LDest);
      end else
      begin
        U := UTF8CHAR_SIZE[Byte(X)];
        case (U) of
          1:
          begin
            X := X and $7f;
            Inc(LSource);
            PWord(LDest)^ := X;
            Inc(LDest);
          end;
          3:
          begin
            if (X and $C0C000 = $808000) then
            begin
              U := (X and $0F) shl 12;
              U := U + (X shr 16) and $3F;
              X := (X and $3F00) shr 2;
              Inc(LSource, 3);
              Inc(X, U);
              if (U shr 11 = $1B) then X := $fffd;
              PWord(LDest)^ := X;
              Inc(LDest);
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

              PCardinal(LDest)^ := X;
              Inc(LDest, 2);
              goto next_iteration;
            end;
            goto unknown;
          end;
        else
        unknown:
          PWord(LDest)^ := UNKNOWN_CHARACTER;
          Inc(LDest);
          Inc(LSource, U);
          Inc(LSource, NativeUInt(U = 0));
        end;
      end;
    end;
  end;

next_iteration:
  if (NativeUInt(LSource) > LCount{TopSource}) then goto small_length;
  X := PCardinal(LSource)^;
  if (X and Integer(MASK_80) = 0) then goto process4;
  if (X and $80 = 0) then goto process1_3;
  goto process_standard;

small_length:
  U := LCount{TopSource} + MAX_UTF8CHAR_SIZE;
  if (U = NativeUInt(LSource)) then goto done;
  X := PByte(LSource)^;
  if (X <= $7f) then
  begin
    PWord(LDest)^ := X;
    Inc(LSource);
    Inc(LDest);
    if (NativeUInt(LSource) <> LCount{TopSource}) then goto small_length;
    goto done;
  end;
  X := UTF8CHAR_SIZE[X];
  Dec(U, NativeUInt(LSource));
  if (X{char size} > U{available source length}) then
  begin
    PWord(LDest)^ := UNKNOWN_CHARACTER;
    Inc(LDest);
    goto done;
  end;

  case X{char size} of
    2:
    begin
      X := PWord(LSource)^;
      goto process_standard;
    end;
    3:
    begin
      Inc(LSource, 2);
      X := Byte(LSource^);
      Dec(LSource, 2);
      X := (X shl 16) or PWord(LSource)^;
      goto process_standard;
    end;
  else
    // 4..5
    goto unknown;
  end;

  // result
done:
  Result := (NativeInt(LDest) - NativeInt(ATarget)) shr 1;
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
  LStore: record
    LTopSource: NativeUInt;
  end;
  {$endif}

  X, U, LCount: NativeUInt;
  LDest: PAnsiChar;
  LSource: PAnsiChar;
  {$ifNdef CPUX86}
  LTopSource: NativeUInt;
  {$endif}
  LUTF8Table: ^TUTF8Table;

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

  LCount := ALength;
  LDest := ATarget;
  LSource := ASource;

  if (LCount = 0) then goto done;
  Inc(LCount, NativeUInt(LSource));
  Dec(LCount, SizeOf(Cardinal));
  {$ifdef CPUX86}LStore.{$endif}LTopSource := LCount;
  LUTF8Table := Pointer(@FUTF8Table);

  {$ifdef CPUARM}
  MASK_80 := MASK_80_SMALL;
  {$endif}

  // conversion loop
  if (NativeUInt(LSource) > {$ifdef CPUX86}LStore.{$endif}LTopSource) then goto small_length;
  X := PCardinal(LSource)^;
  if (X and Integer(MASK_80) = 0) then
  begin
    repeat
    process4:
      Inc(LSource, SizeOf(Cardinal));
      PCardinal(LDest)^ := X;
      Inc(LDest, SizeOf(Cardinal));

      if (NativeUInt(LSource) > {$ifdef CPUX86}LStore.{$endif}LTopSource) then goto small_length;
      X := PCardinal(LSource)^;
    until (X and Integer(MASK_80) <> 0);
    goto look_first;
  end else
  begin
  look_first:
    if (X and $80 = 0) then
    begin
    process1_3:
      PCardinal(LDest)^ := X;
      if (X and $8000 <> 0) then goto ascii_1;
      if (X and $800000 <> 0) then goto ascii_2;
      ascii_3:
        X := X shr 24;
        Inc(LSource, 3);
        Inc(LDest, 3);
        goto small_1;
      ascii_2:
        X := X shr 16;
        Inc(LSource, 2);
        Inc(LDest, 2);
        goto small_2;
      ascii_1:
        X := X shr 8;
        Inc(LSource);
        Inc(LDest);
        goto small_3;
    end else
    begin
    process_not_ascii:
      if (X and $8000 = 0) then goto small_1;
      if (X and $800000 = 0) then goto small_2;
      if (X and $80000000 = 0) then goto small_3;

      small_4:
        U := LUTF8Table[Byte(X)];
        X := X shr 8;
        Inc(LSource);
        PCardinal(LDest)^ := U;
        U := U shr 24;
        Inc(LDest, U);
      small_3:
        U := LUTF8Table[Byte(X)];
        X := X shr 8;
        Inc(LSource);
        PCardinal(LDest)^ := U;
        U := U shr 24;
        Inc(LDest, U);
      small_2:
        U := LUTF8Table[Byte(X)];
        X := X shr 8;
        Inc(LSource);
        PCardinal(LDest)^ := U;
        U := U shr 24;
        Inc(LDest, U);
      small_1:
        U := LUTF8Table[Byte(X)];
        Inc(LSource);
        X := U;
        PWord(LDest)^ := U;
        U := U shr 24;
        Inc(LDest, U);
        if (X >= (3 shl 24)) then
        begin
          Dec(LDest);
          X := X shr 16;
          PByte(LDest)^ := X;
          Inc(LDest);
        end;
    end;
  end;

  if (NativeUInt(LSource) > {$ifdef CPUX86}LStore.{$endif}LTopSource) then goto small_length;
  X := PCardinal(LSource)^;
  if (X and Integer(MASK_80) = 0) then goto process4;
  if (X and $80 = 0) then goto process1_3;
  goto process_not_ascii;

small_length:
  case (NativeUInt(LSource) - {$ifdef CPUX86}LStore.{$endif}LTopSource) of
   3{1}: begin
           X := PByte(LSource)^;
           goto small_1;
         end;
   2{2}: begin
           X := PWord(LSource)^;
           goto small_2;
         end;
   1{3}: begin
           Inc(LSource, 2);
           X := Byte(LSource^);
           Dec(LSource, 2);
           X := (X shl 16) or PWord(LSource)^;
           goto small_3;
         end;
  end;

  // result
done:
  Result := NativeUInt(LDest) - NativeUInt(ATarget);
end;


type
  TUnicodeConvertDesc = record
    CodePage: Word;
    Dest: Pointer;
    Source: Pointer;
    Count: NativeUInt;
  end;

function InternalAnsiFromUtf8(const AConvertDesc: TUnicodeConvertDesc): Integer;
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
  X, U, LCount: NativeUInt;
  LDest: PAnsiChar;
  LSource: PAnsiChar;
  LBufferDest: PWideChar;
  LBufferCount: NativeUInt;
  LBuffer: array[0..BUFFER_SIZE + 4] of WideChar;
  LStored: record
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
  LStored.CodePage := AConvertDesc.CodePage;
  LDest := AConvertDesc.Dest;
  LSource := AConvertDesc.Source;
  LCount := AConvertDesc.Count;
  LStored.Dest := LDest;

  LBufferDest := @LBuffer[0];
  {$ifdef CPUARM}
  MASK_80 := MASK_80_SMALL;
  {$endif}

  repeat
    if (LCount = 0) then Break;
    if (LCount >= SizeOf(Cardinal)) then
    begin
      X := PCardinal(LSource)^;
      if (X and $80 = 0) then
      begin
        if (LBufferDest = @LBuffer[0]) then
        begin
        ascii_write:
          PCardinal(LDest)^ := X;
          if (X and MASK_80 = 0) then
          begin
            Dec(LCount, SizeOf(Cardinal));
            Inc(LSource, SizeOf(Cardinal));
            Inc(LDest, SizeOf(Cardinal));
          end else
          begin
            U := Byte(X and $8080 = 0);
            X := Byte(X and $808080 = 0);
            Inc(X, U);
            Dec(LCount);
            Inc(LSource);
            Inc(LDest);
            Dec(LCount, X);
            Inc(LSource, X);
            Inc(LDest, X);
            if (LCount = 0) then Break;
            X := PByte(LSource)^;
            goto non_ascii;
          end;
        end else
        begin
        unicode_write:
          LStored.X := X;
          begin
            LBufferCount := (NativeUInt(LBufferDest) - NativeUInt(@LBuffer[0])) shr 1;
            Inc(LDest,
            {$ifdef MSWINDOWS}
              WideCharToMultiByte(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
            {$else}
              LocaleCharsFromUnicode(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
            {$endif} );
            LBufferDest := @LBuffer[0];
          end;
          X := LStored.X;
          if (X <> NativeUInt(-1)) then goto ascii_write;
        end;
      end else
      begin
        goto non_ascii;
      end;
    end else
    begin
      X := PByte(LSource)^;
    non_ascii:
      U := UTF8CHAR_SIZE[Byte(X)];
      if (U <= LCount) then
      begin
        case U of
          1:
          begin
            X := PByte(LSource)^;
            Dec(LCount);
            Inc(LSource);
          end;
          2:
          begin
            X := PWord(LSource)^;
            Dec(LCount, 2);
            Inc(LSource, 2);
            if (X and $C0E0 <> $80C0) then goto unknown;

            X := ((X and $1F) shl 6) + ((X shr 8) and $3F);
          end;
          3:
          begin
            Inc(LSource, SizeOf(Word));
            X := PByte(LSource)^;
            Dec(LSource, SizeOf(Word));
            X := X shl 16;
            Inc(X, PWord(LSource)^);
            Dec(LCount, 3);
            Inc(LSource, 3);
            if (X and $C0C000 <> $808000) then goto unknown;

            U := (X and $0F) shl 12;
            U := U + (X shr 16) and $3F;
            X := (X and $3F00) shr 2;
            Inc(X, U);
            if (U shr 11 = $1B) then goto unknown;
          end;
        else
          Inc(U, Byte(U = 0));
          Dec(LCount, U);
          Inc(LSource, U);
        unknown:
          X := UNKNOWN_CHARACTER;
        end;

        PWord(LBufferDest)^ := X;
        Inc(LBufferDest);
        X := NativeUInt(-1);
        if (NativeUInt(LBufferDest) >= NativeUInt(@LBuffer[BUFFER_SIZE])) then goto unicode_write;
      end else
      begin
        if (LBufferDest <> @LBuffer[0]) then
        begin
          PWord(LBufferDest)^ := UNKNOWN_CHARACTER;
          Inc(LBufferDest);
        end else
        begin
          PByte(LDest)^ := UNKNOWN_CHARACTER;
          Inc(LDest);
        end;
        Break;
      end;
    end;
  until (False);

  // last chars
  if (LBufferDest <> @LBuffer[0]) then
  begin
    LBufferCount := (NativeUInt(LBufferDest) - NativeUInt(@LBuffer[0])) shr 1;
    Inc(LDest,
    {$ifdef MSWINDOWS}
      WideCharToMultiByte(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
    {$else}
      LocaleCharsFromUnicode(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
    {$endif} );
  end;

  // result
  Result := NativeInt(LDest) - NativeInt(LStored.Dest);
end;

function TLua.AnsiFromUtf8(ATarget: PAnsiChar; ACodePage: Word; ASource: PAnsiChar; ALength: Integer): Integer;
var
  LConvertDesc: TUnicodeConvertDesc;
begin
  if (ACodePage = 0) then ACodePage := CODEPAGE_DEFAULT;
  LConvertDesc.CodePage := ACodePage;
  LConvertDesc.Dest := ATarget;
  LConvertDesc.Source := ASource;
  LConvertDesc.Count := ALength;

  Result := InternalAnsiFromUtf8(LConvertDesc);
end;

function InternalAnsiFromAnsi(const AConvertDesc: TUnicodeConvertDesc; const AUnicodeTable: Pointer): Integer;
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
  X, U, LCount: NativeUInt;
  LDest: PAnsiChar;
  LSource: PAnsiChar;
  LBufferDest: PWideChar;
  LBufferCount: NativeUInt;
  LBuffer: array[0..BUFFER_SIZE + 4] of WideChar;
  LStored: record
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
  LStored.CodePage := AConvertDesc.CodePage;
  LDest := AConvertDesc.Dest;
  LSource := AConvertDesc.Source;
  LCount := AConvertDesc.Count;
  LStored.Dest := LDest;

  LBufferDest := @LBuffer[0];
  {$ifdef CPUARM}
  MASK_80 := MASK_80_SMALL;
  {$endif}

  repeat
    if (LCount = 0) then Break;
    if (LCount >= SizeOf(Cardinal)) then
    begin
      X := PCardinal(LSource)^;
      if (X and $80 = 0) then
      begin
      ascii:
        if (LBufferDest = @LBuffer[0]) then
        begin
        ascii_write:
          PCardinal(LDest)^ := X;
          if (X and MASK_80 = 0) then
          begin
            Dec(LCount, SizeOf(Cardinal));
            Inc(LSource, SizeOf(Cardinal));
            Inc(LDest, SizeOf(Cardinal));
          end else
          begin
            U := Byte(X and $8080 = 0);
            X := Byte(X and $808080 = 0);
            Inc(X, U);
            Dec(LCount);
            Inc(LSource);
            Inc(LDest);
            Dec(LCount, X);
            Inc(LSource, X);
            Inc(LDest, X);
            if (LCount = 0) then Break;
            X := PByte(LSource)^;
            goto non_ascii;
          end;
        end else
        begin
        unicode_write:
          LStored.X := X;
          begin
            LBufferCount := (NativeUInt(LBufferDest) - NativeUInt(@LBuffer[0])) shr 1;
            Inc(LDest,
            {$ifdef MSWINDOWS}
              WideCharToMultiByte(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
            {$else}
              LocaleCharsFromUnicode(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
            {$endif} );
            LBufferDest := @LBuffer[0];
          end;
          X := LStored.X;
          if (X <> NativeUInt(-1)) then goto ascii_write;
        end;
      end else
      begin
        goto non_ascii;
      end;
    end else
    begin
      X := PByte(LSource)^;
      Inc(X, $ffffff00);
      if (X and $80 = 0) then goto ascii;
    non_ascii:
      PWord(LBufferDest)^ := PUnicodeTable(AUnicodeTable)[Byte(X)];
      Inc(LBufferDest);
      Inc(LSource);
      Dec(LCount);
      X := NativeUInt(-1);
      if (NativeUInt(LBufferDest) >= NativeUInt(@LBuffer[BUFFER_SIZE])) then goto unicode_write;
    end;
  until (False);

  // last chars
  if (LBufferDest <> @LBuffer[0]) then
  begin
    LBufferCount := (NativeUInt(LBufferDest) - NativeUInt(@LBuffer[0])) shr 1;
    Inc(LDest,
    {$ifdef MSWINDOWS}
      WideCharToMultiByte(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
    {$else}
      LocaleCharsFromUnicode(LStored.CodePage, 0, Pointer(@LBuffer[0]), LBufferCount, Pointer(LDest), LBufferCount, nil, nil)
    {$endif} );
  end;

  // result
  Result := NativeInt(LDest) - NativeInt(LStored.Dest);
end;

function TLua.AnsiFromAnsi(ATarget: PAnsiChar; ATargetCodePage: Word; ASource: PAnsiChar;
  ASourceCodePage: Word; ALength: Integer): Integer;
var
  LConvertDesc: TUnicodeConvertDesc;
  LSourceCodePage: Word;
begin
  LConvertDesc.Dest := ATarget;
  LConvertDesc.Source := ASource;
  LConvertDesc.Count := ALength;

  if (ATargetCodePage = 0) then ATargetCodePage := CODEPAGE_DEFAULT;
  LSourceCodePage := ASourceCodePage;
  if (LSourceCodePage = 0) then LSourceCodePage := CODEPAGE_DEFAULT;

  if (ATargetCodePage = LSourceCodePage) then
  begin
    System.Move(LConvertDesc.Source^, LConvertDesc.Dest^, LConvertDesc.Count);
    Result := LConvertDesc.Count;
  end else
  begin
    LConvertDesc.CodePage := ATargetCodePage;
    if (LSourceCodePage <> FCodePage) then SetCodePage(LSourceCodePage);
    Result := InternalAnsiFromAnsi(LConvertDesc, @Self.FUnicodeTable);
  end;
end;

procedure TLua.unpack_lua_string(var AResult: LuaString; const ARttiName: ShortString);
var
  LCount: NativeInt;
  LChars: PLuaChar;
  LBuffer: array[Low(Byte)..High(Byte) + 3] of LuaChar;
begin
  LCount := PByte(@ARttiName)^;
  LChars := Pointer(@ARttiName[1]);

  LCount := UnicodeFromUtf8(Pointer(@LBuffer), Pointer(LChars), LCount);
  LChars := @LBuffer[0];

  SetLength(AResult, LCount);
  System.Move(LChars^, Pointer(AResult)^, LCount * SizeOf(LuaChar));
end;

procedure TLua.unpack_lua_string(var AResult: LuaString; const AChars: __luadata; const ACount: Integer);
var
  LBuffer: ^TLuaBuffer;
  LSize: NativeInt;
begin
  if (Pointer(AResult) <> nil) then AResult := '';
  if (ACount = 0) then Exit;

  LBuffer := @TLuaBuffer(FInternalBuffer);
  LSize := ACount * 2 + 2;
  if (LBuffer.Capacity < LSize) then
  begin
    LBuffer.Size := 0;
    LBuffer.Alloc(LSize);
  end;

  LSize{Count} := UnicodeFromUtf8(Pointer(LBuffer.FBytes), Pointer(AChars), ACount);
  SetLength(AResult, LSize{Count});
  System.Move(Pointer(LBuffer.FBytes)^, Pointer(AResult)^, LSize{Count} * SizeOf(WideChar));
end;

procedure TLua.unpack_lua_string(var AResult: LuaString; const AName: __luaname);
begin
  unpack_lua_string(AResult, Pointer(AName), LStrLen(AName));
end;

procedure TLua.unpack_lua_string(var AResult: LuaString; const ABuffer: __luabuffer);
begin
  unpack_lua_string(AResult, Pointer(ABuffer), Length(ABuffer));
end;

procedure TLua.push_ansi_chars(const S: PAnsiChar; const ACodePage: Word; const ACount: Integer);
var
  LSize: Integer;
  LBuffer: ^TLuaBuffer;
begin
  if (ACount <= 0) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  if (ACodePage = CODEPAGE_UTF8) then
  begin
    push_utf8_chars(S, ACount);
    Exit;
  end;

  LBuffer := @TLuaBuffer(FInternalBuffer);
  LSize := ACount * 6 + 1;
  if (LBuffer.Capacity < LSize) then
  begin
    LBuffer.Size := 0;
    LBuffer.Alloc(LSize);
  end;

  lua_pushlstring(Handle, Pointer(LBuffer.FBytes),
    Utf8FromAnsi(Pointer(LBuffer.FBytes), Pointer(S), ACodePage, ACount) );
end;

procedure TLua.push_utf8_chars(const S: PAnsiChar; const ACount: Integer);
begin
  if (ACount <= 0) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  lua_pushlstring(Handle, Pointer(S), ACount);
end;

procedure TLua.push_wide_chars(const S: PWideChar; const ACount: Integer);
var
  LSize: Integer;
  LBuffer: ^TLuaBuffer;
begin
  if (ACount <= 0) then
  begin
    lua_pushlstring(Handle, Pointer(@NULL_CHAR), 0);
    Exit;
  end;

  LBuffer := @TLuaBuffer(FInternalBuffer);
  LSize := ACount * 3 + 1;
  if (LBuffer.Capacity < LSize) then
  begin
    LBuffer.Size := 0;
    LBuffer.Alloc(LSize);
  end;

  lua_pushlstring(Handle, Pointer(LBuffer.FBytes),
    Utf8FromUnicode(Pointer(LBuffer.FBytes), Pointer(S), ACount) );
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
  {$ifdef UNICODE}
    push_unicode_string(S);
  {$else}
    push_wide_string(S);
  {$endif}
end;
{$else .CPUX86}
asm
  jmp push_wide_string
end;
{$endif}

{$ifNdef NEXTGEN}
procedure TLua.stack_short_string(var S: ShortString; const AStackIndex: Integer; const AMaxLength: Integer);
var
  LChars: __luadata;
  LCount: NativeInt;
  LSize: NativeInt;
  LBuffer: ^TLuaBuffer;
begin
  LChars := lua_tolstring(Handle, AStackIndex, Pointer(@LCount));
  if (LCount <> 0) then
  begin
    LBuffer := @TLuaBuffer(FInternalBuffer);
    LSize := LCount + 3;
    if (LBuffer.Capacity < LSize) then
    begin
      LBuffer.Size := 0;
      LBuffer.Alloc(LSize);
    end;

    FillShortString(S, Pointer(LBuffer.FBytes),
      AnsiFromUtf8(Pointer(LBuffer.FBytes), 0, LChars, LCount),
      AMaxLength);
  end else
  begin
    PByte(@S)^ := 0;
  end;
end;

procedure TLua.stack_ansi_string(var S: AnsiString; const AStackIndex: Integer; const ACodePage: Word);
var
  LChars: __luadata;
  LCount, LSize: NativeInt;
  LBuffer: ^TLuaBuffer;
begin
  if (Pointer(S) <> nil) then S := '';
  LChars := lua_tolstring(Handle, AStackIndex, Pointer(@LCount));
  if (LCount = 0) then Exit;
  LBuffer := @TLuaBuffer(FInternalBuffer);

  if (ACodePage <> CODEPAGE_UTF8) then
  begin
    LSize := LCount + 3;
    if (LBuffer.Capacity < LSize) then
    begin
      LBuffer.Size := 0;
      LBuffer.Alloc(LSize);
    end;

    LCount := AnsiFromUtf8(Pointer(LBuffer.FBytes), ACodePage, LChars, LCount);
    LChars := Pointer(LBuffer.FBytes);
  end;

  if (LCount = 0) then Exit;
  SetLength(S, LCount);
  {$ifdef INTERNALCODEPAGE}
    PWord(NativeInt(S) - ASTR_OFFSET_CODEPAGE)^ := ACodePage;
  {$endif}
  System.Move(LChars^, Pointer(S)^, LCount);
end;

procedure TLua.stack_wide_string(var S: WideString; const AStackIndex: Integer);
var
  LChars: __luadata;
  LCount: NativeInt;
  LSize: NativeInt;
  LBuffer: ^TLuaBuffer;
begin
  if (Pointer(S) <> nil) then S := '';
  LChars := lua_tolstring(Handle, AStackIndex, Pointer(@LCount));
  if (LCount = 0) then Exit;

  LBuffer := @TLuaBuffer(FInternalBuffer);
  LSize := LCount * 2 + 2;
  if (LBuffer.Capacity < LSize) then
  begin
    LBuffer.Size := 0;
    LBuffer.Alloc(LSize);
  end;

  LCount := UnicodeFromUtf8(Pointer(LBuffer.FBytes), LChars, LCount);
  SetLength(S, LCount);
  System.Move(Pointer(LBuffer.FBytes)^, Pointer(S)^, LCount * SizeOf(WideChar));
end;
{$endif}

procedure TLua.stack_unicode_string(var S: UnicodeString; const AStackIndex: Integer);
{$ifdef UNICODE}
var
  LChars: __luadata;
  LCount: NativeInt;
  LSize: NativeInt;
  LBuffer: ^TLuaBuffer;
begin
  if (Pointer(S) <> nil) then S := '';
  LChars := lua_tolstring(Handle, AStackIndex, Pointer(@LCount));
  if (LCount = 0) then Exit;

  LBuffer := @TLuaBuffer(FInternalBuffer);
  LSize := LCount * 2 + 2;
  if (LBuffer.Capacity < LSize) then
  begin
    LBuffer.Size := 0;
    LBuffer.Alloc(LSize);
  end;

  LCount := UnicodeFromUtf8(Pointer(LBuffer.FBytes), LChars, LCount);
  SetLength(S, LCount);
  System.Move(Pointer(LBuffer.FBytes)^, Pointer(S)^, LCount * SizeOf(WideChar));
end;
{$else .CPUX86}
asm
  jmp TLua.stack_wide_string
end;
{$endif}

procedure TLua.stack_lua_string(var S: LuaString; const AStackIndex: Integer);
{$ifdef INLINESUPPORTSIMPLE}
begin
  {$ifdef UNICODE}
    stack_unicode_string(S, AStackIndex);
  {$else}
    stack_wide_string(S, AStackIndex);
  {$endif}
end;
{$else .CPUX86}
asm
  jmp stack_wide_string
end;
{$endif}

procedure TLua.stack_force_unicode_string(var S: UnicodeString; const AStackIndex: Integer;
  const AExtendedMode{ToDo}: Boolean);
const
  HEX_CHARS: array[0..15] of WideChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  TYPE_TEMPLATES: array[LUA_TTABLE..LUA_TUSERDATA] of UnicodeString = (
    'table: 01234567' {$ifdef LARGEINT}+ '89ABCDEF'{$endif},
    'function: 01234567' {$ifdef LARGEINT}+ '89ABCDEF'{$endif},
    'userdata: 01234567' {$ifdef LARGEINT}+ '89ABCDEF'{$endif}
  );
var
  j, LLuaType: Integer;
  X: NativeUInt;
  C: PWideChar;
begin
  LLuaType := lua_type(Handle, AStackIndex);
  case LLuaType of
    LUA_TNIL:
    begin
      S := 'nil';
    end;
    LUA_TBOOLEAN:
    begin
      if (lua_toboolean(Handle, AStackIndex)) then
      begin
        S := 'true';
      end else
      begin
        S := 'false';
      end;
    end;
    LUA_TNUMBER, LUA_TSTRING:
    begin
      stack_unicode_string(S, AStackIndex);
    end;
    LUA_TLIGHTUSERDATA, LUA_TTABLE, LUA_TFUNCTION, LUA_TUSERDATA:
    begin
      if (LLuaType = LUA_TLIGHTUSERDATA) then LLuaType := LUA_TUSERDATA;
      S := TYPE_TEMPLATES[LLuaType];
      UniqueString(S);
      C := @S[Length(S)];
      X := NativeUInt(lua_topointer(Handle, AStackIndex));
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
  LInstance: Pointer;
  LUserData: PLuaUserData;
  LSize: Integer;
begin
  LInstance := @ASource;
  if (AMetaType.F.Kind in [mtClass, mtInterface]) then
  begin
    if (Assigned(LInstance)) then
    begin
      LInstance := PPointer(LInstance)^;
    end;
    goto simple_case;
  end;

  if (not AOwner) then
  begin
  simple_case:
    LUserData := lua_newuserdata(Handle, SizeOf(TLuaUserData));
    LUserData.Instance := LInstance;
    if (Assigned(LInstance)) and (AOwner) then
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
          TObject(LInstance).__ObjAddRef;
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

    if (Assigned(LInstance)) then
    begin
      if (AMetaType.Managed) then
      begin
        case (AMetaType.F.Kind) of
          mtRecord:
          begin
            CopyRecord(LUserData.Instance, LInstance, PLuaRecordInfo(AMetaType).F.TypeInfo);
          end;
          mtArray:
          begin
            if (PLuaArrayInfo(AMetaType).FIsDynamic) then
            begin
              PPointer(LUserData.Instance)^ := PPointer(LInstance)^;
              if (PPointer(LInstance)^ <> nil) then
                DynArrayAddRef(PPointer(LInstance)^);
            end else
            begin
              CopyArray(LUserData.Instance, LInstance, PLuaArrayInfo(AMetaType).FFinalTypeInfo,
                PLuaArrayInfo(AMetaType).FFinalItemsCount);
            end;
          end;
        end;
      end else
      begin
        System.Move(LInstance^, LUserData.Instance^, AMetaType.Size);
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

function TLua.push_complex_property(const APropertyInfo; const AInstance: Pointer): Pointer{PLuaUserData};
begin
  Result := nil{ToDo};
end;

function TLua.push_luaarg(const AValue: TLuaArg): Boolean;
type
  TIntToStr = procedure(const Value: Integer; var Result: string);
begin
  with AValue do
  case (LuaType) of
      ltEmpty: lua_pushnil(Handle);
    ltBoolean: lua_pushboolean(Handle, F.VBoolean);
    ltInteger: lua_pushinteger(Handle, F.VInteger);
     ltDouble: lua_pushnumber(Handle, F.VDouble);
     ltString: push_lua_string(FString);
    ltPointer: lua_pushlightuserdata(Handle, F.VPointer);
  (* ToDo *)
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

function TLua.push_variant(const AValue: Variant): Boolean;
type
  TDateProc = procedure(const DateTime: TDateTime; var Result: string);
  TIntToStr = procedure(const Value: Integer; var Result: string);
var
  LType: Integer;
  LValue: Pointer;
begin
  // TVarType, Data
  LType := TVarData(AValue).VType;
  LValue := @TVarData(AValue).VWords[3];
  if (LType and varByRef <> 0) then
  begin
    LType := LType and (not varByRef);
    LValue := PPointer(LValue)^;
  end;

  // push
  case (LType) of
    varEmpty, varNull, varError{EmptyParam}: lua_pushnil(Handle);
    varSmallint: lua_pushinteger(Handle, PSmallInt(LValue)^);
    varInteger : lua_pushinteger(Handle, PInteger(LValue)^);
    varSingle  : lua_pushnumber(Handle, PSingle(LValue)^);
    varDouble  : lua_pushnumber(Handle, PDouble(LValue)^);
    varCurrency: lua_pushnumber(Handle, PCurrency(LValue)^);
    varDate    :
    begin
      if (Pointer(FStringBuffer.Default) <> nil) then FStringBuffer.Default := '';

      case InspectDateTime(PDateTime(LValue)^) of
        0: TDateProc(@DateTimeToStr)(PDouble(LValue)^, FStringBuffer.Default);
        1: TDateProc(@DateToStr)(PDouble(LValue)^, FStringBuffer.Default);
        2: TDateProc(@TimeToStr)(PDouble(LValue)^, FStringBuffer.Default);
      end;

      {$ifdef UNICODE}push_unicode_string{$else}push_ansi_string{$endif}(FStringBuffer.Default);
    end;
    {$ifNdef NEXTGEN}
    varOleStr  : push_wide_string(PWideString(LValue)^);
    {$endif}
    varBoolean : lua_pushboolean(Handle, PBoolean(LValue)^);
    varShortInt: lua_pushinteger(Handle, PShortInt(LValue)^);
    varByte    : lua_pushinteger(Handle, PByte(LValue)^);
    varWord    : lua_pushinteger(Handle, PWord(LValue)^);
    varLongWord: lua_pushnumber(Handle, PLongWord(LValue)^);
    varInt64   : lua_pushnumber(Handle, PInt64(LValue)^);
    $15{UInt64}: lua_pushnumber(Handle, PInt64(LValue)^);
    {$ifdef UNICODE}
    varUString : push_unicode_string(PUnicodeString(LValue)^);
    {$endif}
    {$ifNdef NEXTGEN}
    varString  : push_ansi_string(PAnsiString(LValue)^);
    {$endif}
  else
    TIntToStr(@IntToStr)(LType, FStringBuffer.Default);
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TLua.push_argument(const AValue: TVarRec): Boolean;
type
  TIntToStr = procedure(const Value: Integer; var Result: string);
begin
  with AValue do
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
    vtPointer: if (VPointer = nil) then lua_pushnil(Handle) else lua_pushlightuserdata(Handle, VPointer);
    (* ToDo *)
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

procedure TLua.stack_pop(const ACount: Integer);
begin
  lua_settop(Handle, not ACount{-ACount - 1}); // lua_pop(Handle, ACount);
end;

function TLua.stack_variant(var AResult: Variant; const AStackIndex: Integer; const AOleMode: Boolean): Boolean;
const
  varDeepData = $BFE8;
var
  LType, LLuaType: Integer;
  LVarData: TVarData absolute AResult;
begin
  LType := LVarData.VType;
  if (LType and varDeepData <> 0) then
  case LType of
    varBoolean, varUnknown+1..$15{varUInt64}: ;
  else
    VarClear(AResult);
  end;

  LLuaType := lua_type(Handle, AStackIndex);
  case (LLuaType) of
        LUA_TNIL: begin
                    LVarData.VType := varEmpty;
                  end;
    LUA_TBOOLEAN: begin
                    LVarData.VType := varBoolean;
                    LVarData.VBoolean := lua_toboolean(Handle, AStackIndex);
                  end;
     LUA_TNUMBER: begin
                    LVarData.VDouble := lua_tonumber(Handle, AStackIndex);
                    if (NumberToInteger(LVarData.VDouble, LVarData.VInteger)) then
                    begin
                      LVarData.VType := varInteger;
                     end else
                    begin
                      LVarData.VType := varDouble;
                    end;
                  end;
     LUA_TSTRING: begin
                    LVarData.VUnknown := nil;

                    {$ifdef MSWINDOWS}
                    if (not AOleMode) then
                    {$endif}
                    begin
                      {$ifdef UNICODE}
                        LVarData.VType := varUString;
                        stack_unicode_string(UnicodeString(LVarData.VString{VUString}), AStackIndex);
                      {$else}
                        LVarData.VType := varOleStr;
                        stack_wide_string(WideString(Pointer(LVarData.VOleStr)), AStackIndex);
                      {$endif}
                    end
                    {$ifdef MSWINDOWS}
                    else
                    begin
                      LVarData.VType := varOleStr;
                      stack_wide_string(WideString(Pointer(LVarData.VOleStr)), AStackIndex);
                    end
                    {$endif}
                    ;
                  end;
  else
    LVarData.VType := varEmpty;
    GetLuaTypeName(FStringBuffer.Default, LLuaType);
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TLua.stack_luaarg(var AResult: TLuaArg; const AStackIndex: integer; const AAllowLuaTable: Boolean): Boolean;
label
  copy_meta_params, fail_user_data, fail_lua_type;
var
  LLuaType: Integer;
  LUserData: ^TLuaUserData;
  LMetaType: ^TLuaMetaType;
begin
  Result := True;
  AResult.F.LuaType := ltEmpty;

  LLuaType := lua_type(Handle, AStackIndex);
  case (LLuaType) of
    LUA_TNIL:    {done};
    LUA_TBOOLEAN:
    begin
      AResult.F.LuaType := ltBoolean;
      AResult.F.VBoolean := lua_toboolean(Handle, AStackIndex);
    end;
    LUA_TNUMBER:
    begin
      AResult.F.VDouble := lua_tonumber(Handle, AStackIndex);
      if (NumberToInteger(AResult.F.VDouble, AResult.F.VInteger)) then
      begin
        AResult.F.LuaType := ltInteger;
      end else
      begin
        AResult.F.LuaType := ltDouble;
      end;
    end;
    LUA_TSTRING:
    begin
      AResult.F.LuaType := ltString;
      stack_lua_string(AResult.FString, AStackIndex);
    end;
    LUA_TLIGHTUSERDATA:
    begin
      AResult.F.LuaType := ltPointer;
      AResult.F.VPointer := lua_touserdata(Handle, AStackIndex);
    end;
    LUA_TFUNCTION:
    begin
      AResult.F.LuaType := ltPointer;
      AResult.F.VPointer := function_topointer(AStackIndex);
    end;
    LUA_TUSERDATA:
    begin
      LUserData := lua_touserdata(Handle, AStackIndex);
      if (LUserData = nil) or (LUserData.Instance = nil) then goto fail_user_data;

      LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LUserData.MetaType);
      // ToDo
      AResult.F.LuaType := ltPointer;
      AResult.F.VPointer := LMetaType;

    fail_user_data:
      LUserData.GetDescription(FStringBuffer.Default, Self);
      Result := False;
    end;
    LUA_TTABLE:
    begin
      // TClass, Info or LuaTable
      LMetaType := nil;
      lua_rawgeti(Handle, AStackIndex, 0);
      if (lua_type(Handle, -1) = LUA_TUSERDATA) then
      begin
        LMetaType := lua_touserdata(Handle, -1);
        if (NativeInt(LMetaType) and 3 <> 0) or (LMetaType.F.Marker <> LUA_METATYPE_MARKER) then
          LMetaType := nil;
      end;
      lua_settop(Handle, -1-1);

      if (Assigned(LMetaType)) then
      begin
        if (LMetaType.F.Kind = mtClass) then
        begin
          AResult.F.LuaType := ltClass;
          AResult.F.VPointer := Pointer(PLuaClassInfo(LMetaType).ClassType);
        end else
        begin
          AResult.F.LuaType := ltPointer;
          AResult.F.VPointer := Pointer(LMetaType);
        end;
      end else
      begin
        goto fail_lua_type;
      end;
    end;
  else
  fail_lua_type:
    GetLuaTypeName(FStringBuffer.Default, LLuaType);
    Result := False;
  end;
end;

procedure __TLuaGarbageCollection(const Self: TLua);
var
  LErrCode: Integer;
begin
  LErrCode := lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0);
  if (LErrCode <> 0) then Self.CheckScriptError(LErrCode);
end;

procedure TLua.GarbageCollection;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaGarbageCollection(Self);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaGarbageCollection
end;
{$endif}

procedure TLua.NativeFrameEnter(var AFrame{: TLuaNativeFrame}; const AName: PShortString; const ACritical: Boolean);
var
  LFrame: PLuaNativeFrame;
begin
  LFrame := Pointer(@AFrame);
  LFrame.Parent := FFrames;
  LFrame.Name := AName;
  LFrame.Critical := ACritical;

  FFrames := LFrame;
end;

procedure TLua.NativeFrameLeave;
var
  LFrame: PLuaNativeFrame;
begin
  LFrame := FFrames;
  FFrames := LFrame.Parent;
end;

procedure TLua.NativeFrameRename(const AName: PShortString);
var
  LFrame: PLuaNativeFrame;
begin
  LFrame := FFrames;
  LFrame.Name := AName;
end;

procedure TLua.NativeFrameBufferedRename(var ANameBuffer: ShortString; const AName: LuaString);
var
  LLength, LSize, LCount: Integer;
  LBuffer: ^TLuaBuffer;
begin
  LLength := System.Length(AName);
  LBuffer := Pointer(@FInternalBuffer);
  LBuffer.Size := 0;

  LCount := 0;
  if (LLength <> 0) then
  begin
    LSize := LLength * 3 + 1;
    if (LBuffer.Capacity < LSize) then LBuffer.Alloc(LSize);
    LCount := Utf8FromUnicode(Pointer(LBuffer.FBytes), Pointer(AName), LLength);
  end;

  FillShortString(ANameBuffer, Pointer(LBuffer.FBytes), LCount, High(Byte));
  NativeFrameRename(Pointer(@ANameBuffer));
end;

procedure TLua.RegisterError(const AText: LuaString);
var
  LCount: Integer;
  LFrame: PLuaNativeFrame;
  LCritical: Boolean;
  LPath: LuaString;
  LMode: Integer;
begin
  LCount := 0;
  LFrame := FFrames;
  LCritical := LFrame.Critical;
  while (Assigned(LFrame)) do
  begin
    if (LCount = 0) then
    begin
      unpack_lua_string(LPath, LFrame.Name^);
    end else
    begin
      unpack_lua_string(FStringBuffer.Lua, LFrame.Name^);
      FStringBuffer.Lua := FStringBuffer.Lua + ' - ';
      FStringBuffer.Lua := FStringBuffer.Lua + LPath;
      LPath := FStringBuffer.Lua;
    end;

    Inc(LCount);
    LFrame := LFrame.Parent;
  end;

  if (Assigned(FOnRegisterError)) then
  begin
    LMode := Ord(not FOnRegisterError(LPath, AText, LCount, LCritical)) shl 1;
  end else
  begin
    LMode := (Ord(LCritical) shl 1) + Ord(FPrintRegisterError and System.IsConsole);
  end;

  if (LMode <> 0) then
  begin
    if (Pointer(LPath) <> nil) then LPath := LPath + ': ';
    LPath := LPath + AText;

    if (LMode and 1 <> 0) then
    begin
      Writeln(LPath);
    end;

    if (LMode and 2 <> 0) then
    begin
      InternalError(LPath);
    end;
  end;
end;

procedure TLua.RegisterErrorFmt(const AFmtStr: LuaString; const AArgs: array of const);
begin
  RegisterError(LuaString({$ifdef UNICODE}Format{$else}WideFormat{$endif}(UnicodeString(AFmtStr), AArgs)));
end;

procedure TLua.RegisterErrorTypeExists(const AName: LuaString);
begin
  RegisterErrorFmt('type "%s" is already registered', [AName]);
end;

{$ifdef FPC}
procedure FmtStr(var AResult: UnicodeString; const AFmt: WideString; const AArgs: array of const);
var
  LBuffer: WideString;
begin
  WideFmtStr(LBuffer, AFmt, AArgs);
  AResult := LBuffer;
end;
{$endif}

procedure TLua.InternalError(const AText: LuaString);
const
  SLN_CONST: Cardinal = Ord('S') + (Ord('l') shl 8) + (Ord('n') shl 16);
var
  LDebugInfo: lua_Debug;
begin
  // debug information
  FillChar(LDebugInfo, SizeOf(LDebugInfo), #0);
  lua_getstack(Handle, 1, @LDebugInfo);
  lua_getinfo(Handle, __luaname(Pointer(@SLN_CONST)), @LDebugInfo);

  // standard exception
  if (LDebugInfo.currentline < 0) then
    raise ELua.Create(string(AText)) at FReturnAddress;

  // script exception, format error to parse in Check
  unpack_lua_string(FStringBuffer.Lua, __luaname(Pointer(@LDebugInfo.short_src[4])));
  {$ifdef UNICODE}
    FmtStr(FStringBuffer.Unicode, '%s:%d: %s', [FStringBuffer.Lua, LDebugInfo.currentline, AText]);
    push_unicode_string(FStringBuffer.Unicode);
  {$else}
    WideFmtStr(FStringBuffer.Wide, '%s:%d: %s', [FStringBuffer.Lua, LDebugInfo.currentline, AText]);
    push_wide_string(FStringBuffer.Wide);
  {$endif}
  lua_error(Handle);
end;

procedure TLua.InternalErrorFmt(const AFmtStr: LuaString; const AArgs: array of const);
{$ifdef CPUX64}
begin
  {
    Attention!
    For some reason, memory leaks here for the x64 compiler
    if you use local variables
  }
  FmtStr(FStringBuffer.Unicode, AFmtStr, AArgs);
  InternalError(FStringBuffer.Unicode);
end;
{$else}
var
  Fmt, Buffer: UnicodeString;
begin
  Fmt := UnicodeString(AFmtStr);
  Buffer := {$ifdef UNICODE}Format{$else}WideFormat{$endif}(Fmt, AArgs);
  InternalError(LuaString(Buffer));
end;
{$endif}

procedure TLua.Error(const AText: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  InternalError(AText);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalError
end;
{$endif}

procedure TLua.ErrorFmt(const AFmtStr: LuaString; const AArgs: array of const);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  InternalErrorFmt(AFmtStr, AArgs);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalErrorFmt
end;
{$endif}

procedure TLua.CheckScriptError(const AErrCode: Integer; AUnit: TLuaUnit);
const
  LINE_MARKS: array[Boolean] of string = ('', '-->> ');
var
  LErr, LUnitName: ^LuaString;
  P1, P2, LCount, i: Integer;
  LUnitLine: Integer;
  LText, LLineFmt: ^string;
  LMinLine, LMaxLine: Integer;
  LLine: TLuaUnitLine;
  S: PChar;

  function CharPos(const C: LuaChar; const AStr: LuaString; const AStartFrom: Integer = 1): Integer; far;
  var
    S: PLuaChar;
    LCount: Integer;
  begin
    S := Pointer(AStr);
    if (S <> nil) then
    begin
      LCount := PInteger(NativeInt(S) - SizeOf(Integer))^ {$ifdef LUA_LENGTH_SHIFT}shr 1{$endif};
      Dec(LCount, AStartFrom - 1);
      Inc(S, AStartFrom - 1);

      if (LCount > 0) then
      repeat
        if (S^ = C) then
        begin
          Result := Integer((NativeUInt(S) - NativeUInt(AStr)) div SizeOf(LuaChar)) + 1;
          Exit;
        end;

        Dec(LCount);
        Inc(S);
      until (LCount = 0);
    end;

    Result := 0;
  end;

  function CharsToInt(S: PLuaChar; ACount: Integer): Integer;
  label
    fail;
  begin
    if (ACount <= 0) then goto fail;

    Result := 0;
    repeat
      case S^ of
        '0'..'9': Result := Result * 10 + Integer(Integer(S^) - Ord('0'));
      else
        goto fail;
      end;

      Dec(ACount);
      Inc(S);
    until (ACount = 0);
    Exit;

  fail:
    Result := 0;
  end;

  function EmptyLine(const AUnit: TLuaUnit; const AUnitLine: Integer): Boolean; far;
  var
    S: __luadata;
    LCount, i: Integer;
  begin
    Result := False;
    S := AUnit.FLines[AUnitLine - 1].Chars;
    LCount := AUnit.FLines[AUnitLine - 1].Count;
    for i := 1 to LCount do
    begin
      if (Byte(S^) > 32) then Exit;
      Inc(S);
    end;

    Result := True;
  end;

begin
  if (AErrCode = 0) then Exit;

  // error text
  LErr := @FStringBuffer.Lua;
  stack_lua_string(LErr^, -1);
  stack_pop;
  if (Pointer(LErr^) = nil) then Exit;

  // unit name buffer
  LUnitName := @FStringBuffer.Unicode;

  // change error text, detect chunk name, line number
  LUnitLine := -1;
  if (LErr^[1] = '[') then
  begin
    P1 := CharPos('"', LErr^);
    P2 := CharPos(']', LErr^);
    if (P1 <> 0) and (P2 <> 0) then
    begin
      // unit name
      LCount := P2 - P1 - 2;
      SetLength(LUnitName^, LCount);
      System.Move(PLuaChar(@PLuaChar(Pointer(LErr^))[P1 {+ 1 - 1}])^, Pointer(LUnitName^)^, LCount * SizeOf(LuaChar));
      Delete(LErr^, 1, P2);

      // unit line
      if (Pointer(LErr^) <> nil) and (LErr^[1] = ':') then
      begin
        P1 := 1;
        P2 := CharPos(':', LErr^, 2);
        if (P2 <> 0) then
        begin
          LUnitLine := CharsToInt(PLuaChar(@PLuaChar(Pointer(LErr^))[P1 {+ 1 - 1}]), P2 - P1 - 1);
          Delete(LErr^, 1, P2);
          if (Pointer(LErr^) <> nil) and (LErr^[1] = #32) then Delete(LErr^, 1, 1);
        end;
      end;
    end;
  end;

  // unit (chunk)
  if (Pointer(LUnitName^) = nil) then
  begin
    LUnitName := @GLOBAL_NAME_SPACE;
  end else
  if (AUnit = nil) then
  begin
    AUnit := GetUnitByName(LUnitName^);
  end;
  if (AUnit <> nil) and (Cardinal(LUnitLine) > Cardinal(AUnit.FLinesCount)) then AUnit := nil;

  // availiable: unit instance, unit name, unit line, error text
  // may be later it will be used in debug mode

  // base output text
  LText := @FStringBuffer.Default;
  FmtStr(LText^, 'unit "%s", line %d.'#10'%s', [LUnitName^, LUnitLine, LErr^]);

  // unit output text lines
  if (AUnit <> nil) then
  begin
    // min/max
    LMinLine := LUnitLine - 2;
    if (LMinLine < 1) then LMinLine := 1;
    while (LMinLine <> LUnitLine) and (EmptyLine(AUnit, LMinLine)) do Inc(LMinLine);
    LMaxLine := LUnitLine + 2;
    if (LMaxLine > AUnit.FLinesCount) then LMaxLine := AUnit.FLinesCount;
    while (LMaxLine <> LUnitLine) and (EmptyLine(AUnit, LMaxLine)) do Dec(LMaxLine);

    // max digits
    i := LMaxLine;
    LCount := 0;
    repeat
      i := i div 10;
      Inc(LCount);
    until (i = 0);

    // line format
    {$ifdef UNICODE}
      LLineFmt := @FStringBuffer.Unicode;
    {$else}
      LLineFmt := @FStringBuffer.Ansi;
    {$endif}
    FmtStr(LLineFmt^, '%%s'#10'%%%dd:  %%s%%s', [LCount]);

    // output text
    LText^ := LText^ + #10#10'Code:';
    for i := LMinLine to LMaxLine do
    begin
      LLine := AUnit.GetLine(i);
      Self.unpack_lua_string(FStringBuffer.Lua, LLine.Chars, LLine.Count);
      FmtStr(LText^, LLineFmt^, [LText^, i, LINE_MARKS[i = LUnitLine], FStringBuffer.Lua]);
    end;
  end;

  // correct: #13 --> #10 (for console cases)
  S := Pointer(LText);
  for i := 0 to Length(LText^) - 1 do
  begin
    if (S[i] = #13) then S[i] := #10;
  end;

  // exception
  raise ELuaScript.Create(LText^) at FReturnAddress;
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
  LCount: NativeInt;
begin
  LCount := TLuaStack(FEmptyRefs).Count;
  if (LCount <> 0) then
  begin
    Dec(LCount);
    TLuaStack(FEmptyRefs).FCount := LCount;
    Result := TLuaStack(FEmptyRefs).Items[LCount];
  end else
  begin
    Result := FRef + 1;
    FRef := Result;
  end;
end;

procedure TLua.global_free_ref(var ARef: Integer);
var
  LValue: Integer;
begin
  LValue := ARef;
  ARef := 0;
  if (LValue > 0) then
  begin
    if (LValue = FRef) then
    begin
      FRef := LValue - 1;
    end else
    begin
      TLuaStack(FEmptyRefs).Push(LValue);
    end;

    lua_pushnil(Handle);
    lua_rawseti(Handle, LUA_REGISTRYINDEX, LValue);
  end;
end;

procedure TLua.global_fill_value(const ARef: Integer);
begin
  if (ARef > 0) then
  begin
    lua_rawseti(Handle, LUA_REGISTRYINDEX, ARef);
  end else
  begin
    stack_pop;
  end;
end;

procedure TLua.global_push_value(const ARef: Integer);
begin
  if (ARef > 0) then
  begin
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, ARef);
  end else
  begin
    lua_pushnil(Handle);
  end;
end;

function TLua.GetGlobalEntity(const AName: LuaString; const AModeCreate: Boolean): Pointer;
label
  not_found;
var
  LEntityItem: ^TLuaDictionaryItem;
  LLuaName: __luaname;
  LCount: Integer;
  LPtr: __luapointer;
begin
  if (AModeCreate) then
  begin
    LLuaName := TLuaNames(FNames).Add(AName);
  end else
  begin
    LLuaName := TLuaNames(FNames).FindValue(AName);

    if (not Assigned(LLuaName)) then
    begin
      // try to find and cache identifier
      LCount := TLuaBuffer(FInternalBuffer).Size;
      if (LCount > 255) then LCount := 255;
      lua_pushlstring(Handle, Pointer(TLuaBuffer(FInternalBuffer).FBytes), LCount);
      if Assigned(TLuaDictionary(FGlobalEntities).InternalFind(lua_tolstring(Handle, -1, nil), False)) then
      begin
        LLuaName := TLuaNames(FNames).InternalFind(True).Value;
      end;
      lua_settop(Handle, -1 -1);
    end;
  end;

  if (Assigned(LLuaName)) then
  begin
    LEntityItem := TLuaDictionary(FGlobalEntities).InternalFind(LLuaName, AModeCreate);
    if (not Assigned(LEntityItem)) then goto not_found;
  end else
  begin
  not_found:
    Result := nil;
    Exit;
  end;

  // retreave or add empty global entity
  LPtr := LEntityItem.Value;
  if (LPtr = LUA_POINTER_INVALID) then
  begin
    LPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaGlobalEntity));
    LEntityItem.Value := LPtr;
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
    PInteger(Result)^ := 0;
    PLuaGlobalEntity(Result).Ptr := LUA_POINTER_INVALID;
  end else
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
  end;
end;

function TLua.GetRecordInfo(const AName: LuaString): PLuaRecordInfo;
var
  LEntity: PLuaGlobalEntity;
begin
  LEntity := GetGlobalEntity(AName, False);
  if (Assigned(LEntity)) and (LEntity.Kind = gkMetaType) then
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
    if (Result.F.Kind = mtRecord) then Exit;
  end;

  Result := nil;
end;

function TLua.GetArrayInfo(const AName: LuaString): PLuaArrayInfo;
var
  LEntity: PLuaGlobalEntity;
begin
  LEntity := GetGlobalEntity(AName, False);
  if (Assigned(LEntity)) and (LEntity.Kind = gkMetaType) then
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
    if (Result.F.Kind = mtArray) then Exit;
  end;

  Result := nil;
end;

function TLua.GetSetInfo(const AName: LuaString): PLuaSetInfo;
var
  LEntity: PLuaGlobalEntity;
begin
  LEntity := GetGlobalEntity(AName, False);
  if (Assigned(LEntity)) and (LEntity.Kind = gkMetaType) then
  begin
    Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
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

procedure __TLuaGetVariable(const Self: TLua; const AName: LuaString; var AResult: Variant);
var
  LModify: TLuaGlobalModify;
  LPtr: NativeUInt;
begin
  LModify.Name := @AName;
  LModify.VariantMode := True;
  LModify.V := @AResult;

  LPtr := NativeUInt(@LModify);
  Self.__global_index(Cardinal(LPtr), {$ifdef LARGEINT}LPtr shr 32{$else}0{$endif});
end;

function TLua.GetVariable(const AName: LuaString): Variant;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaGetVariable(Self, AName, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaGetVariable
end;
{$endif}

procedure __TLuaSetVariable(const Self: TLua; const AName: LuaString; const AValue: Variant);
var
  LModify: TLuaGlobalModify;
  LPtr: NativeUInt;
begin
  LModify.Name := @AName;
  LModify.VariantMode := True;
  LModify.V := @AValue;

  LPtr := NativeUInt(@LModify);
  Self.__global_newindex(Cardinal(LPtr), {$ifdef LARGEINT}LPtr shr 32{$else}0{$endif});
end;

procedure TLua.SetVariable(const AName: LuaString; const AValue: Variant);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaSetVariable(Self, AName, AValue);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaSetVariable
end;
{$endif}

procedure __TLuaGetVariableEx(const Self: TLua; const AName: LuaString; var AResult: TLuaArg);
var
  LModify: TLuaGlobalModify;
  LPtr: NativeUInt;
begin
  LModify.Name := @AName;
  LModify.VariantMode := False;
  LModify.Arg := @AResult;

  LPtr := NativeUInt(@LModify);
  Self.__global_index(Cardinal(LPtr), {$ifdef LARGEINT}LPtr shr 32{$else}0{$endif});
end;

function TLua.GetVariableEx(const AName: LuaString): TLuaArg;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaGetVariableEx(Self, AName, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaGetVariableEx
end;
{$endif}

procedure __TLuaSetVariableEx(const Self: TLua; const AName: LuaString; const AValue: TLuaArg);
var
  LModify: TLuaGlobalModify;
  LPtr: NativeUInt;
begin
  LModify.Name := @AName;
  LModify.VariantMode := False;
  LModify.Arg := @AValue;

  LPtr := NativeUInt(@LModify);
  Self.__global_newindex(Cardinal(LPtr), {$ifdef LARGEINT}LPtr shr 32{$else}0{$endif});
end;

procedure TLua.SetVariableEx(const AName: LuaString; const AValue: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaSetVariableEx(Self, AName, Value);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaSetVariableEx
end;
{$endif}

function TLua.VariableExists(const AName: LuaString): Boolean;
var
  LEntity: PLuaGlobalEntity;
  LLuaType: Integer;
begin
  LEntity := GetGlobalEntity(AName, False);
  if (Assigned(LEntity)) then
  begin
    if (LEntity.Kind in [gkMetaType, gkVariable, gkConst]) then
    begin
      Result := True;
      Exit;
    end else
    if (LEntity.Kind = gkScriptVariable) then
    begin
      global_push_value(LEntity.Ref);
      LLuaType := lua_type(Handle, -1);
      stack_pop;

      Result := (LLuaType <> LUA_TNONE) and (LLuaType <> LUA_TFUNCTION);
      Exit;
    end;
  end;

  Result := False;
end;

function TLua.ProcExists(const AName: LuaString): Boolean;
var
  LEntity: PLuaGlobalEntity;
  LLuaType: Integer;
begin
  LEntity := GetGlobalEntity(AName, False);
  if (Assigned(LEntity)) then
  begin
    if (LEntity.Kind = gkProc) then
    begin
      Result := True;
      Exit;
    end else
    if (LEntity.Kind = gkScriptVariable) then
    begin
      global_push_value(LEntity.Ref);
      LLuaType := lua_type(Handle, -1);
      stack_pop;

      Result := (LLuaType = LUA_TFUNCTION);
      Exit;
    end;
  end;

  Result := False;
end;

function InternalLuaTableVariable(const Self: TLua; const ATable: LuaString; const AName: PLuaString): Boolean;
var
  LEntity: PLuaGlobalEntity;
begin
  LEntity := Self.GetGlobalEntity(ATable, False);
  if (Assigned(LEntity)) and (LEntity.Kind = gkScriptVariable) then
  begin
    Self.global_push_value(LEntity.Ref);
    if (lua_type(Self.Handle, -1) = LUA_TTABLE) then
    begin
      if (Assigned(AName)) then
      begin
        Self.push_lua_string(AName^);
        lua_rawget(Self.Handle, -2);
      end;

      Result := True;
      Exit;
    end;
    lua_settop(Self.Handle, 0);
  end;

  Result := False;
end;

procedure __TLuaGetTableVariable(const ASelfVariantMode: NativeInt;
  const ATable, AName: LuaString; const AResult: Pointer);
var
  LVariantMode: Boolean;
  LSelf: TLua;
  LLuaType: Integer;
  LSupported: Boolean;
begin
  LVariantMode := (ASelfVariantMode and 1 <> 0);
  LSelf := TLua(ASelfVariantMode and -2);

  if (InternalLuaTableVariable(LSelf, ATable, @AName)) then
  begin
    LLuaType := lua_type(LSelf.Handle, -1);
    if (LLuaType > LUA_TNIL) and (LLuaType <> LUA_TFUNCTION) then
    begin
      if (LVariantMode) then
      begin
        LSupported := LSelf.stack_variant(PVariant(AResult)^, -1, False);
      end else
      begin
        LSupported := LSelf.stack_luaarg(PLuaArg(AResult)^, -1, False);
      end;

      lua_settop(LSelf.Handle, 0);
      if (not LSupported) then
      begin
        raise ELua.CreateFmt('Can''t get variable "%s.%s" of type "%s"', [ATable, AName,
          LSelf.FStringBuffer.Default]) at LSelf.FReturnAddress;
      end;
      Exit;
    end else
    begin
      lua_settop(LSelf.Handle, 0);
    end;
  end;

  raise ELua.CreateFmt('Table variable "%s.%s" not found', [ATable, AName]) at LSelf.FReturnAddress;
end;

function TLua.GetTableVariable(const ATable, AName: LuaString): Variant;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaGetTableVariable(NativeInt(Self) or Ord(True), ATable, AName, @Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  or eax, 1
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  or rcx, 1
  {$endif}
  jmp __TLuaGetTableVariable
end;
{$endif}

function TLua.GetTableVariableEx(const ATable, AName: LuaString): TLuaArg;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaGetTableVariable(NativeInt(Self) or Ord(False), ATable, AName, @Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaGetTableVariable
end;
{$endif}

procedure __TLuaSetTableVariable(const ASelfVariantMode: NativeInt;
  const ATable, AName: LuaString; const AValue: Pointer);
var
  LVariantMode: Boolean;
  LSelf: TLua;
  LSupported: Boolean;
begin
  LVariantMode := (ASelfVariantMode and 1 <> 0);
  LSelf := TLua(ASelfVariantMode and -2);

  if (InternalLuaTableVariable(LSelf, ATable, nil)) then
  begin
    LSelf.push_lua_string(AName);

    if (LVariantMode) then
    begin
      LSupported := LSelf.push_variant(PVariant(AValue)^);
    end else
    begin
      LSupported := LSelf.push_luaarg(PLuaArg(AValue)^);
    end;

    if (LSupported) then
    begin
      lua_rawset(LSelf.Handle, -3);
      lua_settop(LSelf.Handle, 0);
    end else
    begin
      lua_settop(LSelf.Handle, 0);
      raise ELua.CreateFmt('Can''t set variable "%s.%s" of type "%s"', [ATable, AName,
        LSelf.FStringBuffer.Default]) at LSelf.FReturnAddress;
    end;
    Exit;
  end;

  raise ELua.CreateFmt('Table "%s" not found', [ATable]) at LSelf.FReturnAddress;
end;

procedure TLua.SetTableVariable(const ATable, AName: LuaString; const AValue: Variant);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaSetTableVariable(NativeInt(Self) or Ord(True), ATable, AName, @AValue);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  or eax, 1
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  or rcx, 1
  {$endif}
  jmp __TLuaSetTableVariable
end;
{$endif}

procedure TLua.SetTableVariableEx(const ATable, AName: LuaString; const AValue: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaSetTableVariable(NativeInt(Self) or Ord(False), ATable, AName, @AValue);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaSetTableVariable
end;
{$endif}

function TLua.VariableExists(const ATable, AName: LuaString): Boolean;
var
  LLuaType: Integer;
begin
  if (InternalLuaTableVariable(Self, ATable, @AName)) then
  begin
    LLuaType := lua_type(Handle, -1);
    lua_settop(Handle, 0);
    Result := (LLuaType > LUA_TNIL) and (LLuaType <> LUA_TFUNCTION);
    Exit;
  end;

  Result := False;
end;

function TLua.ProcExists(const ATable, AName: LuaString): Boolean;
var
  LLuaType: Integer;
begin
  if (InternalLuaTableVariable(Self, ATable, @AName)) then
  begin
    LLuaType := lua_type(Handle, -1);
    lua_settop(Handle, 0);
    Result := (LLuaType = LUA_TFUNCTION);
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

function TLua.Call(const AProcName: LuaString): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaCall_noargs(Self, ProcName, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

function TLua.Call(const AProcName: LuaString; const AArgs: array of TLuaArg): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaCall_luaargs(Self, AProcName, AArgs, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

function TLua.Call(const AProcName: LuaString; const AArgs: array of const): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaCall__arguments(Self, AProcName, AArgs, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

function TLua.Call(const ATable, AProcName: LuaString): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaTableCall_noargs(Self, ATable, AProcName, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

function TLua.Call(const ATable, AProcName: LuaString; const AArgs: array of TLuaArg): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaTableCall_luaargs(Self, ATable, AProcName, AArgs, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

function TLua.Call(const ATable, AProcName: LuaString; const AArgs: array of const): TLuaArgs;
{$ifNdef CPUINTEL}
begin
  Pointer(Self.FTempBuffer.N) := ReturnAddress;
  __TLuaTableCall__arguments(Self, ATable, AProcName, AArgs, Result);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FTempBuffer.N
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FTempBuffer.N
  {$endif}
  jmp __TLuaTableCall__arguments
end;
{$endif}

function TLua.InternalConvertScript(var AData: __luabuffer): Integer;
label
  fix_dest;
var
  LSize, LOffset: Integer;
  S, LDest: PByte;
  LMarker: Cardinal;
  LBOM: TLuaScriptBOM;
  X, Y: NativeUInt;
  LBuffer: __luabuffer;
begin
  // detect BOM
  LSize := Length(AData);
  S := Pointer(AData);
  LBOM := High(TLuaScriptBOM);
  LOffset := BOM_INFO[LBOM].Size;
  repeat
    if (LSize >= LOffset) then
    begin
      LMarker := 0;
      System.Move(S^, LMarker, LOffset);
      if (LMarker = BOM_INFO[LBOM].Data) then Break;
    end;

    Dec(LBOM);
    LOffset := BOM_INFO[LBOM].Size;
  until (LBOM = sbNone);
  Inc(S, LOffset);
  Dec(LSize, LOffset);

  // sbUTF16BE/cbUTF32/cbUTF32BE --> sbUTF16
  if (LBOM in [sbUTF16BE, cbUTF32, cbUTF32BE]) then
  begin
    LDest := Pointer(AData);
    case LBOM of
      sbUTF16BE:
      begin
        while (LSize >= SizeOf(Word)) do
        begin
          X := PWord(S)^;
          Inc(S, SizeOf(Word));
          Dec(LSize, SizeOf(Word));

          X := Swap(X);
          PWord(LDest)^ := X;
          Inc(LDest, SizeOf(Word));
        end;
      end;
      cbUTF32, cbUTF32BE:
      begin
        while (LSize >= SizeOf(Cardinal)) do
        begin
          X := PCardinal(S)^;
          Inc(S, SizeOf(Cardinal));
          Dec(LSize, SizeOf(Cardinal));

          if (LBOM = cbUTF32BE) then
          begin
            X := (Swap(X) shl 16) + Swap(X shr 16);
          end;

          if (X <= $ffff) then
          begin
            if (X shr 11 = $1B) then PWord(LDest)^ := $fffd
            else PWord(LDest)^ := X;

            Inc(LDest, SizeOf(Word));
          end else
          begin
            Y := (X - $10000) shr 10 + $d800;
            X := (X - $10000) and $3ff + $dc00;
            X := (X shl 16) + Y;

            PCardinal(LDest)^ := X;
            Inc(LDest, SizeOf(Cardinal));
          end;
        end;
      end;
    end;

    LOffset := 0;
    LSize := NativeInt(LDest) - NativeInt(AData);
    S := Pointer(AData);
    LBOM := sbUTF16;
  end;

  // final convert
  case LBOM of
    sbUTF8: ;
    sbNone:
    begin
      // ANSI --> UTF8
      SetLength(LBuffer, LSize * 3 + 1);
      LSize := Utf8FromAnsi(Pointer(LBuffer), Pointer(S), 0, LSize);
      AData := LBuffer;
      goto fix_dest;
    end;
    sbUTF16:
    begin
      // UTF16 --> UTF8
      SetLength(LBuffer, (LSize shr 1) * 3 + 1);
      LSize := Utf8FromUnicode(Pointer(LBuffer), Pointer(S), LSize shr 1);
      AData := LBuffer;
      goto fix_dest;
    end;
  end;

  // result
  Result := LOffset;
  Exit;
fix_dest:
  LBuffer := {$ifdef NEXTGEN}nil{$else}''{$endif};
  SetLength(AData, LSize);
  {$ifNdef NEXTGEN}
  if (Pointer(AData) <> nil) then
  begin
    S := Pointer(AData);
    Inc(S, LSize);
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

function SkipScriptStringIdentifier(S, ATop: PByte): PByte;
var
  LFirst, X: NativeUInt;
begin
  LFirst := S^;
  Inc(S);

  repeat
    if (S = ATop) then Break;
    X := CHAR_TABLE[S^];
    Inc(S);
    if (X = 0) then Continue;

    case X of
      CHAR_QUOTE:
      begin
        Dec(S);
        X := S^;
        Inc(S);
        if (X = LFirst) then Break;
      end;
      CHAR_SLASH:
      begin
        if (S = ATop) then Break;
        Inc(S);
      end;
      CHAR_CRLF: Break;
    end;
  until (False);

  Result := S;
end;

procedure TLua.InternalPreprocessScript(var ABuffer: __luabuffer; const AOffset: Integer);
label
  line_comment, clear_chars, replace, next_find;
var
  S, LStart, LTop, LStoredS, LStoredPrint: PByte;
  LUnique: Boolean;
  LSize: Integer;
  X: NativeUInt;
  LPtrOffset: NativeInt;
begin
  // initialize buffer
  LUnique := False;
  LSize := Length(ABuffer) - AOffset;
  NativeInt(S) := NativeInt(ABuffer) + AOffset;
  NativeInt(LTop) := NativeInt(S) + LSize;

  // remove comments
  repeat
    if (S = LTop) then Break;
    X := CHAR_TABLE[S^];
    Inc(S);
    if (X = 0) then Continue;

    case X of
      CHAR_QUOTE:
      begin
        Dec(S);
        S := SkipScriptStringIdentifier(S, LTop);
      end;
      CHAR_SLASH:
      begin
        if (S = LTop) then Break;
        Inc(S);
      end;
      CHAR_MINUS:
      begin
        LStart := S;
        Dec(LStart);
        if (S = LTop) then Break;
        if (S^ <> Ord('-')) then Continue;
        Inc(S);
        if (S = LTop) then goto clear_chars;

        if (S^ = Ord('[')) then
        begin
          // multy-line comment
          Inc(S);
          if (S = LTop) or (S^ <> Ord('[')) then goto line_comment;
          repeat
            if (S = LTop) then Break;
            X := S^;
            Inc(S);
            if (X = Ord(']')) then
            begin
              if (S = LTop) then Break;
              X := S^;
              Inc(S);
              if (X = Ord(']')) then Break;
            end;
          until (False);
        end else
        begin
        line_comment:
          repeat
            if (S = LTop) then Break;
            X := CHAR_TABLE[S^];
            Inc(S);
            if (X = CHAR_CRLF) then Break;
          until (False);
        end;

      clear_chars:
        if (not LUnique) then
        begin
          LPtrOffset := UniqueLuaBuffer(ABuffer);
          Inc(NativeInt(LStart), LPtrOffset);
          Inc(NativeInt(S), LPtrOffset);
          Inc(NativeInt(LTop), LPtrOffset);
          LUnique := True;
        end;

        repeat
          if (CHAR_TABLE[LStart^] <> CHAR_CRLF) then LStart^ := 32;
          Inc(LStart);
        until (LStart = S);
      end;
    end;
  until (False);

  // replace print --> _PNT_
  NativeInt(LStart) := NativeInt(ABuffer) + AOffset;
  S := LStart;
  repeat
    if (S = LTop) then Break;

    // find '(' character
    repeat
      if (S = LTop) then Break;
      X := CHAR_TABLE[S^];
      Inc(S);
      if (X = 0) then Continue;

      case X of
        CHAR_QUOTE:
        begin
          Dec(S);
          S := SkipScriptStringIdentifier(S, LTop);
        end;
        CHAR_SLASH:
        begin
          if (S = LTop) then Exit;
          Inc(S);
        end;
        CHAR_BRACKET:
        begin
          Break;
        end;
      end;
    until (False);
    LStoredS := S;
    Dec(S){'('};

    // skip space chars: func..(
    repeat
      if (S = LStart) then goto next_find;
      Dec(S);
      X := CHAR_TABLE[S^];
    until (X <> CHAR_SPACE);
    if (X <> 0) then goto next_find;

    // detect "print" function
    if (S^ <> Ord('t')) then goto next_find;
    if (S = LStart) then goto next_find;
    Dec(S);
    if (S^ <> Ord('n')) then goto next_find;
    if (S = LStart) then goto next_find;
    Dec(S);
    if (S^ <> Ord('i')) then goto next_find;
    if (S = LStart) then goto next_find;
    Dec(S);
    if (S^ <> Ord('r')) then goto next_find;
    if (S = LStart) then goto next_find;
    Dec(S);
    if (S^ <> Ord('p')) then goto next_find;
    LStoredPrint := S;

    // detect not "."/":" before "print"
    repeat
      if (S = LStart) then goto replace;
      Dec(S);
      X := CHAR_TABLE[S^];
    until (X < CHAR_CRLF);
    if (X = CHAR_POINT) or (X = CHAR_COLON) then goto next_find;

  replace:
    if (not LUnique) then
    begin
      LPtrOffset := UniqueLuaBuffer(ABuffer);
      Inc(NativeInt(LStart), LPtrOffset);
      Inc(NativeInt(LTop), LPtrOffset);
      Inc(NativeInt(LStoredPrint), LPtrOffset);
      Inc(NativeInt(LStoredS), LPtrOffset);
      LUnique := True;
    end;
    S := LStoredPrint;
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
    S := LStoredS;
  until (False);
end;

function TLua.InternalRunScript(var AData: __luabuffer; const AOffset: Integer; const AUnitName: __luaname;
  const AUnit: TLuaUnit; const AMakeResult: Boolean; var AExceptionAddress: Pointer; const AReturnAddress: Pointer): Exception;
var
  LBuffer: __luabuffer;
  LBufferOffset, LErrCode, LSize: Integer;
  LLastReturnAddress: Pointer;
  LMemory: __luadata;
  {$ifdef CPUINTEL}
  CW: Word;
  {$endif}
begin
  // preprocessing, compilation, execution
  Result := nil;
  try
    // preprocess
    LBuffer := AData;
    InternalPreprocessScript(LBuffer, AOffset);

    // user preprocessing
    LBufferOffset := AOffset;
    if (Assigned(FOnPreprocessScript)) then
      FOnPreprocessScript(LBuffer, AData, LBufferOffset, AUnit.Name, UniqueLuaBuffer);

    // compilation, execution
    LLastReturnAddress := EnterScript(AReturnAddress);
    try
      {$ifdef CPUINTEL}
      {$WARN SYMBOL_PLATFORM OFF}
      CW := Get8087CW;
      Set8087CW($037F {default intel C++ mode});
      try
      {$endif}
      begin
        LSize := Length(LBuffer) - LBufferOffset;
        NativeInt(LMemory) := NativeInt(LBuffer) + LBufferOffset;
        LErrCode := luaL_loadbuffer(Handle, LMemory, LSize, AUnitName);
        if (LErrCode = 0) then LErrCode := lua_pcall(Handle, 0, 0, 0);
        if (LErrCode = 0) then LErrCode := lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
        if (LErrCode <> 0) and (AMakeResult) then CheckScriptError(LErrCode, AUnit);
      end;
      {$ifdef CPUINTEL}
      finally
        Set8087CW(CW);
      end;
      {$WARN SYMBOL_PLATFORM ON}
      {$endif}
    finally
      LeaveScriptStack(LLastReturnAddress);
    end;
  except
    on E: Exception do
    if (AMakeResult) then
    begin
      AcquireExceptionObject;
      AExceptionAddress := ExceptAddr;
      Result := E;
    end;
  end;
end;

procedure TLua.InternalLoadScript(var AData: __luabuffer; const AUnitName: LuaString;
  const AFileName: string);
const
  MAX_UNITNAME_LENGTH = 255;
var
  LOffset, LCount: Integer;
  LLuaUnitName: __luaname;
  LLuaUnitNameBuffer: array[0..MAX_UNITNAME_LENGTH * 3] of AnsiChar;
  LUnit, LLastUnit: TLuaUnit;
  E: Exception;
  LReturnAddress, LExceptAddress, LTemp: Pointer;
begin
  // convert script data
  LOffset := InternalConvertScript(AData);

  // unit name
  LCount := Length(AUnitName);
  if (LCount = 0) then
  begin
    LLuaUnitName := Pointer(@NULL_CHAR);
  end else
  begin
    if (Ord(AUnitName[1]) <= 32) or (Ord(AUnitName[LCount]) <= 32) or (LCount > MAX_UNITNAME_LENGTH) then
      raise ELua.Create('Invalid unit name') at FReturnAddress;

    LCount := Utf8FromUnicode(Pointer(@LLuaUnitNameBuffer), Pointer(AUnitName), LCount);
    Byte(LLuaUnitNameBuffer[LCount]) := 0;
    LLuaUnitName := Pointer(@LLuaUnitNameBuffer);
  end;

  // unit, last unit
  if (LCount = 0) then
  begin
    LLastUnit := nil;
  end else
  begin
    LLastUnit := Self.GetUnitByName(AUnitName);
  end;
  LUnit := TLuaUnit.Create;
  LUnit.FLua := Self;
  LUnit.FIndex := -1;
  LUnit.FName := AUnitName;
  LUnit.FNameLength := Length(AUnitName);
  LUnit.FData := AData;
  LUnit.FDataOffset := LOffset;
  LUnit.FFileName := AFileName;
  LUnit.InitializeLines;

  // try run script
  LReturnAddress := FReturnAddress;
  E := InternalRunScript(AData, LOffset, LLuaUnitName, LUnit, True, LExceptAddress, LReturnAddress);

  // add/replace unit or cleanup and compile/run previous instance
  if (E = nil) then
  begin
    if (Assigned(LLastUnit)) then
    begin
      LUnit.FIndex := LLastUnit.FIndex;
      FUnits[LUnit.FIndex] := LUnit;
      LLastUnit.Free;
    end else
    begin
      LUnit.FIndex := FUnitCount;
      Inc(FUnitCount);
      SetLength(FUnits, FUnitCount);
      FUnits[LUnit.FIndex] := LUnit;
    end;
  end else
  begin
    LUnit.Free;

    if (Assigned(LLastUnit)) then
    begin
      InternalRunScript(LLastUnit.FData, LLastUnit.FDataOffset, LLuaUnitName, LLastUnit,
        False, LTemp, LReturnAddress);
    end;

    raise E at LExceptAddress;
  end;
end;

procedure CheckScriptSize(const ASelf: TLua; const ASize: Int64);
begin
  if (ASize < 0) or (ASize > $800000) then
   raise ELuaScript.CreateFmt('Incorrect script size: %d', [ASize])
     at ASelf.FReturnAddress;
end;

procedure __TLuaRunScript(const ASelf: TLua; const AScript: LuaString);
const
  BOM: Cardinal = $0000FEFF;
  BOM_SIZE = 2;
var
  LBufferSize: Integer;
  LData: __luabuffer;
begin
  LBufferSize := Length(AScript) * SizeOf(LuaChar);
  CheckScriptSize(ASelf, LBufferSize);

  SetLength(LData, BOM_SIZE + LBufferSize);
  System.Move(BOM, Pointer(LData)^, BOM_SIZE);
  System.Move(Pointer(AScript)^, Pointer(NativeInt(LData) + BOM_SIZE)^, LBufferSize);

  ASelf.InternalLoadScript(LData, '', '');
end;

procedure TLua.RunScript(const AScript: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRunScript(Self, AScript);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRunScript
end;
{$endif}

procedure __TLuaLoadFileScript(const ASelf: TLua; const AFileName: string);
var
  LHandle: THandle;
  {$ifdef MSWINDOWS}
    P: TPoint;
  {$endif}
  {$ifdef POSIX}
    S: {$ifdef FPC}Stat{$else}_stat{$endif};
  {$endif}
  LSize64: Int64;
  LSize: Integer;
  LData: __luabuffer;
begin
  if (not FileExists(AFileName)) then
  begin
    raise ELua.CreateFmt('File "%s" not found', [AFileName]) at ASelf.FReturnAddress;
  end;

  {$ifdef MSWINDOWS}
    LHandle := {$ifdef UNITSCOPENAMES}Winapi.{$endif}{$ifdef FPC}Windows.CreateFileW{$else}Windows.CreateFile{$endif}
      (PChar(AFileName), $0001{FILE_READ_DATA}, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  {$else}
    Handle := FileOpen(AFileName, fmOpenRead or fmShareDenyNone);
  {$endif}
  if (LHandle = INVALID_HANDLE_VALUE) then
    raise ELua.CreateFmt('Cannot open file "%s"', [AFileName]) at ASelf.FReturnAddress;
  try
    {$ifdef MSWINDOWS}
      P.X := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.GetFileSize(LHandle, Pointer(@P.Y));
      if (P.Y = -1) then P.X := -1;
      LSize64 := PInt64(@P)^;
    {$endif}
    {$ifdef POSIX}
      if ({$ifdef FPC}FpFStat{$else}fstat{$endif}(LHandle, S) = 0) then
        Size64 := S.st_size
      else
        Size64 := -1;
    {$endif}

    CheckScriptSize(ASelf, LSize64);
    LSize := LSize64;

    SetLength(LData, LSize);
    LSize := FileRead(LHandle, Pointer(LData)^, LSize);
    if (LSize < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};
    SetLength(LData, LSize);

    ASelf.InternalLoadScript(LData, LuaString(ExtractFileName(AFileName)), AFileName);
  finally
    FileClose(LHandle);
  end;
end;

procedure TLua.LoadScript(const AFileName: string);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaLoadFileScript(Self, AFileName);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaLoadFileScript
end;
{$endif}

procedure __TLuaLoadBufferScript(const ASelf: TLua; const ABuffer: Pointer;
  const ABufferSize: Integer; const ABOM: TLuaScriptBOM; const AUnitName: LuaString);
var
  LSize: Integer;
  LData: __luabuffer;
begin
  CheckScriptSize(ASelf, ABufferSize);

  LSize := BOM_INFO[ABOM].Size;
  SetLength(LData, LSize + ABufferSize);
  System.Move(BOM_INFO[ABOM].Data, Pointer(LData)^, LSize);
  System.Move(ABuffer^, Pointer(NativeInt(LData) + LSize)^, ABufferSize);

  ASelf.InternalLoadScript(LData, AUnitName, '');
end;

procedure TLua.LoadScript(const ABuffer: Pointer; const ABufferSize: Integer;
  const ABOM: TLuaScriptBOM; const AUnitName: LuaString);
{$ifdef RETURNADDRESS}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaLoadBufferScript(Self, ABuffer, ABufferSize, ABOM, AUnitName);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaLoadBufferScript
end;
{$endif}

{$ifdef FPC}
function FindResource(AModuleHandle: TFPResourceHMODULE; AResourceName, AResourceType: PChar): TFPResourceHandle;
{$ifdef MSWINDOWS}
begin
  Result := Windows.FindResourceW(AModuleHandle, AResourceName, AResourceType);
end;
{$else}
var
  LBufferString: string;
  LBufferName, LBufferType: UTF8String;
  LResourceName, LResourceType: PAnsiChar;
begin
  LResourceName := Pointer(AResourceName);
  if (NativeUInt(AResourceName) <= High(Word)) then
  begin
    LBufferString := AResourceName;
    LBufferName := UTF8String(LBufferString);
    LResourceName := PAnsiChar(LBufferName);
  end;

  LResourceType := Pointer(AResourceType);
  if (NativeUInt(AResourceType) <= High(Word)) then
  begin
    LBufferString := AResourceType;
    LBufferType := UTF8String(LBufferString);
    LResourceType := PAnsiChar(LBufferType);
  end;

  Result := System.FindResource(AModuleHandle, LResourceName, LResourceType);
end;
{$endif}
{$endif}

procedure __TLuaLoadResourceScript(const Self: TLua; const AInstance: THandle;
  const AName, AResType: PChar; const AUnitName: LuaString);

  procedure RaiseNotFound;
  var
    V: NativeUInt;
    N, T: string;
  begin
    V := NativeUInt(AName);
    if (V <= High(Word)) then N := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else N := '"' + string(AName) + '"';

    V := NativeUInt(AResType);
    if (V <= High(Word)) then T := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else T := '"' + string(AResType) + '"';

    raise ELua.CreateFmt('Resource %s (%s) not found', [N, T]);
  end;

var
  HResInfo: THandle;
  HGlobal: THandle;
begin
  HResInfo := FindResource(AInstance, AName, AResType);
  if (HResInfo = 0) then RaiseNotFound;
  HGlobal := LoadResource(AInstance, HResInfo);
  if (HGlobal = 0) then RaiseNotFound;
  try
    __TLuaLoadBufferScript(Self, LockResource(HGlobal), SizeOfResource(AInstance, HResInfo), sbNone, AUnitName);
  finally
    UnlockResource(HGlobal);
    FreeResource(HGlobal);
  end;
end;

procedure TLua.LoadScript(const AInstance: THandle; const AName, AResType: PChar;
  const UnitName: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaLoadResourceScript(Self, AInstance, AName, AResType, AUnitName);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaLoadResourceScript
end;
{$endif}

procedure __TLuaLoadBytesScript(const ASelf: TLua; const ABuffer: TBytes;
  const ABOM: TLuaScriptBOM; const AUnitName: LuaString);
begin
  __TLuaLoadBufferScript(ASelf, Pointer(ABuffer), Length(ABuffer), ABOM, AUnitName);
end;

procedure TLua.LoadScript(const ABuffer: TBytes; const ABOM: TLuaScriptBOM;
  const AUnitName: LuaString);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaLoadBytesScript(Self, ABuffer, ABOM, AUnitName);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaLoadBytesScript
end;
{$endif}


const
  // TLuaGlobalKind = (gkMetaType, gkVariable, gkProc, gkConst, gkScriptVariable);
  NATIVE_GLOBAL_KINDS: set of TLuaGlobalKind = [gkMetaType, gkVariable, gkProc];
  GLOBAL_INDEX_KINDS: set of TLuaGlobalKind = [gkConst, gkScriptVariable];
  CONST_GLOBAL_KINDS: set of TLuaGlobalKind = [gkMetaType, gkProc, gkConst];


{$ifNdef LUA_NATIVEFUNC}
type
  TLuaFunctionParams = packed record
    Lua: Pointer;
    Callback: function(const ALua: Pointer; const P1, P2: __luapointer): Integer;
    P1: __luapointer;
    P2: __luapointer;
  end;
  PLuaFunctionParams = ^TLuaFunctionParams;

function LuaFuntionWrapper(L: Plua_State): Integer; cdecl;
var
  LParams: PLuaFunctionParams;
begin
  LParams := lua_touserdata(L, LUA_UPVALUEINDEX);
  Result := LParams.Callback(LParams.Lua, Params.P1, Params.P2);
end;
{$endif}

function TLua.alloc_luafunction(const ACallback: Pointer; const P1, P2: __luapointer): __luafunction;
{$ifdef LUA_NATIVEFUNC}
begin
  Result := TLuaCFunctionHeap(FCFunctionHeap).Alloc(Self, ACallback, P1, P2);
end;
{$else}
begin
  alloc_push_luafunction(ACallback, P1, P2);
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

procedure TLua.alloc_push_luafunction(const ACallback: Pointer; const P1, P2: __luapointer);
{$ifdef LUA_NATIVEFUNC}
begin
  lua_pushcclosure(Handle, alloc_luafunction(ACallback, P1, P2), 0);
end;
{$else}
var
  LParams: PLuaFunctionParams;
begin
  LParams := lua_newuserdata(Handle, SizeOf(TLuaFunctionParams));
  LParams.Lua := Self;
  LParams.Callback := Callback;
  LParams.P1 := P1;
  LParams.P2 := P2;
  lua_pushcclosure(Handle, LuaFuntionWrapper, 1);
end;
{$endif}

procedure TLua.push_luafunction(const AFunction: __luafunction);
begin
  {$ifdef LUA_NATIVEFUNC}
    lua_pushcclosure(Handle, AFunction, 0);
  {$else}
    // global_push_value(Func);
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, AFunction);
  {$endif}
end;

function TLua.function_topointer(const AStackIndex: Integer): Pointer;
{$ifdef LUA_NATIVEFUNC}
var
  LData: PLuaCFunctionData;
  LOffset: NativeInt;
  {$ifdef LARGEINT}
  LHeap: ^TLuaMemoryHeap;
  {$endif}
  LMetaType: ^TLuaMetaType;
  LMethod: ^TLuaMethod;
begin
  Result := Pointer(@lua_tocfunction(Handle, AStackIndex));

  if (not Assigned(Result)) then Exit;
  with PLuaCFunctionPage(NativeInt(Result) and MEMORY_PAGE_CLEAR)^ do
    if (MarkerLow <> MEMORY_PAGE_MARKER_LOW) or (MarkerHigh <> MEMORY_PAGE_MARKER_HIGH) then Exit;

  // callback address
  LData := Result;
  {$ifdef CPUX86}
    LOffset := PInteger(@LData.Bytes[16])^;
    Result := Pointer(LOffset + (NativeInt(@LData.Bytes[15]) + 5));
  {$endif}
  {$ifdef CPUX64}
    if (LData.Bytes[21] = $E9) then
    begin
      LOffset := PInteger(@LData.Bytes[21])^;
      Result := Pointer(LOffset + (NativeInt(@LData.Bytes[21]) + 5));
    end else
    begin
      Result := PPointer(@LData.Bytes[23])^;
    end;
  {$endif}
  if (Result <> @TLua.UniversalMethodCallback{ToDo}) then Exit;

  // parameters
  {$ifdef CPUX86}
    LMetaType := PPointer(@LData.Bytes[6])^;
    LMethod := PPointer(@LData.Bytes[11])^;
  {$endif}
  {$ifdef CPUX64}
    LHeap := Pointer(@TLua(PPointer(@LData.Bytes[2])^).FMemoryHeap);
    LMetaType := LHeap.Unpack(PInteger(@LData.Bytes[11])^);
    LMethod := LHeap.Unpack(PInteger(@LData.Bytes[17])^);
  {$endif}

  // result (optional virtual)
  Result := LMethod.Address;
  if (NativeUInt(Result) >= PROPSLOT_VIRTUAL) then
    Result := PPointer(NativeUInt(LMetaType.F.ClassType) + NativeUInt(Result) and PROPSLOT_CLEAR)^;
end;
{$else}
var
  LParams: PLuaFunctionParams;
  {$ifdef LARGEINT}
  LHeap: ^TLuaMemoryHeap;
  {$endif}
  LMetaType: ^TLuaMetaType;
  LClosureType: ^TLuaClosureType;
begin
  Result := Pointer(lua_tocfunction(Handle, StackIndex));
  if (not Assigned(Result)) or (Result <> @LuaFuntionWrapper) then Exit;

  // callback address
  lua_getupvalue(Handle, AStackIndex, 1);
  LParams := lua_touserdata(Handle, -1);
  stack_pop;
  Result := @LParams.Callback;
  if (Result <> @TLua.UniversalMethodCallback{ToDo}) then Exit;

  // parameters
  {$ifdef CPUX86}
    LMetaType := Pointer(LParams.P1);
    LClosureType := Pointer(LParams.P2);
  {$endif}
  {$ifdef CPUX64}
    LHeap := Pointer(@LParams.Lua.FMemoryHeap);
    LMetaType := LHeap.Unpack(LParams.P1);
    LClosureType := LHeap.Unpack(LParams.P2);
  {$endif}

  // result (optional virtual)
  Result := Proc.Address;
  if (NativeUInt(Result) >= PROPSLOT_VIRTUAL) then
    Result := PPointer(NativeInt(LMetaType.F.ClassType) + NativeInt(Result) and PROPSLOT_CLEAR)^;
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

procedure TLua.InternalRegisterCallback(const AName: __luaname; const ACallback: Pointer; const P1, P2: __luapointer);
begin
  lua_pushlstring(Handle, Pointer(AName), LStrLen(AName));
  alloc_push_luafunction(ACallback, P1, P2);
  lua_rawset(Handle, -3);
end;

procedure TLua.InternalRegTypeName(const ATypeName: LuaString;
  const ATypeInfo: PTypeInfo; const APointerDepth: Integer);
begin
  TLuaRegisteredTypeNames(FRegisteredTypeNames).Add(ATypeName, ATypeInfo, APointerDepth);
end;

procedure TLua.InternalRegStandardTypeNames;

  procedure Add(const ATypeName: LuaString; const ATypeInfo: PTypeInfo);
  begin
    InternalRegTypeName(ATypeName, ATypeInfo, 0);
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
  Add('PLuaChar', TypeInfoPWideChar);
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

function TLua.InternalRegisterMetaTable(const AMetaType: PLuaMetaType): Integer;
const
  LUA_GLOBALSINDEX = -10002;
  LUA_RIDX_GLOBALS = 2;
begin
  Result := InternalNewMetaTable;

  if (not Assigned(AMetaType)) then
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
    AMetaType.F.Ref := Result;

    global_push_value(Result);
    lua_pushlightuserdata(Handle, AMetaType);
    lua_rawseti(Handle, -2, 0);

    lua_pushvalue(Handle, 1);
    lua_setmetatable(Handle, -2);
    stack_pop;
  end;
end;

function TLua.InternalTableToMetaType(const AStackIndex: Integer): PLuaMetaType;
var
  LPtr: Pointer;
begin
  Result := nil;
  if (lua_type(Handle, AStackIndex) = LUA_TTABLE) then
  begin
    lua_rawgeti(Handle, AStackIndex, 0);
    LPtr := nil;
    if (lua_type(Handle, -1) = LUA_TLIGHTUSERDATA) then LPtr := lua_touserdata(Handle, -1);
    stack_pop;
    if (LPtr <> nil) then
    try
      if (PLuaMetaType(LPtr).F.Marker = LUA_METATYPE_MARKER) then
        Result := LPtr;
    except
    end;
  end;
end;

function TLua.InternalAddGlobal(const AKind: Byte{TLuaGlobalKind}; const AName: __luaname): Pointer{PLuaGlobalEntity};
const
  KIND_NAMES: array[TLuaGlobalKind] of string = ('type', 'variable', 'method', 'enum', '');
var
  LKind: TLuaGlobalKind absolute AKind;
  LItem: PLuaDictionaryItem;
  LValue: __luapointer;
  LEntity: PLuaGlobalEntity;
begin
  LItem := TLuaDictionary(FGlobalEntities).InternalFind(AName, True);
  LValue := LItem.Value;
  if (LValue = LUA_POINTER_INVALID) then
  begin
    LValue := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaGlobalEntity));
    LItem.Value := LValue;
    LEntity := TLuaMemoryHeap(FMemoryHeap).Unpack(LValue);

    if (LKind in NATIVE_GLOBAL_KINDS) then
    begin
      LEntity.Ptr := LUA_POINTER_INVALID;
    end else
    begin
      LEntity.Ref := global_alloc_ref;
    end;
  end else
  begin
    LEntity := TLuaMemoryHeap(FMemoryHeap).Unpack(LValue);
    if (LEntity.Kind = LKind) then
    begin
      Result := LEntity;
      Exit;
    end;

    // solve name space conflict
    if (LEntity.Kind = gkScriptVariable) then
    begin
      if (LKind in GLOBAL_INDEX_KINDS) then
      begin
        // clear variable, but stay Ref
        lua_pushnil(Handle);
        global_fill_value(LEntity.Ref);
      end else
      begin
        // dispose variable and Ref, pointer will be filled later
        global_free_ref(LEntity.Ref);
        LEntity.Ptr := LUA_POINTER_INVALID;
      end;
    end else
    begin
      unpack_lua_string(FStringBuffer.Lua, AName);
      raise ELua.CreateFmt('Global %s "%s" is already registered',
        [KIND_NAMES[LEntity.Kind], FStringBuffer.Lua]) at FReturnAddress;
    end;
  end;

  LEntity.Kind := LKind;
  LEntity.Constant := (LKind in CONST_GLOBAL_KINDS);
  Result := LEntity;
end;

function TLua.InternalAddMetaType(const AKind: TLuaMetaKind; const AName: LuaString;
  const ATypeInfo: Pointer; const AInstanceSize: Integer; const AAdditionalSize: NativeInt): PLuaMetaType;
const
  SIZES: array[TLuaMetaKind] of Integer = (SizeOf(TLuaClassInfo), SizeOf(TLuaInterfaceInfo),
    SizeOf(TLuaRecordInfo), SizeOf(TLuaArrayInfo), SizeOf(TLuaSetInfo));
var
  LLuaName: __luaname;
  LSize: NativeInt;
  LPtr: __luapointer;
  LEntity: PLuaGlobalEntity;

  procedure RegisterOperators(const AValues: array of Integer);
  var
    i, LValue: Integer;
  begin
    for i := Low(AValues) to High(AValues) do
    begin
      LValue := AValues[i];
      InternalRegisterCallback(OPERATOR_NAMES[LValue], @TLua.__operator, LPtr, LValue);
    end;
  end;
begin
  // global entity
  LLuaName := TLuaNames(FNames).Add(AName);
  LEntity := InternalAddGlobal(Ord(gkMetaType), LLuaName);

  // allocation
  LSize := SIZES[AKind] + AAdditionalSize;
  LPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(LSize);
  LEntity.Ptr := LPtr;
  Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr);

  // base properties
  Result.F.Marker := LUA_METATYPE_MARKER;
  PInteger(@Result.F.Kind)^ := Ord(AKind);
  {$ifdef LARGEINT}
  Result.F.Ptr := LPtr;
  {$endif}
  Result.FLua := Self;
  Result.FName := AName;
  Result.F.Size := AInstanceSize;
  Result.F.TypeInfo := ATypeInfo;
  Result.F.Weak := IsWeakTypeInfo(ATypeInfo);
  Result.FillManagedValue;
  Result.FillHFAValue;

  // metatable (Ref)
  {Result.F.Ref := } InternalRegisterMetaTable(Result);

  // metatypes dictionary
  TLuaDictionary(FMetaTypes).Add(LLuaName, LPtr);
  if (Assigned(ATypeInfo)) then TLuaDictionary(FMetaTypes).Add(ATypeInfo, LPtr);
  if (Result.F.Kind = mtClass) then TLuaDictionary(FMetaTypes).Add(Result.F.ClassType.ClassInfo, LPtr);

  // clear instance (specific fields)
  System.FillChar(Pointer(NativeInt(Result) + SizeOf(TLuaMetaType))^,
    LSize - SizeOf(TLuaMetaType), #0);

  // registered type names
  InternalRegTypeName(AName, Pointer(Result), 0);

  // metatable callbacks
  LPtr := Result.Ptr;
  global_push_value(Result.F.Ref);
  begin
    // common
    InternalRegisterCallback(Pointer(@ID_LEN), @TLua.__len, LPtr);
    InternalRegisterCallback(Pointer(@ID_TOSTRING), @TLua.__tostring, LPtr);
    InternalRegisterCallback(Pointer(@ID_CALL), @TLua.__call, LPtr, Ord(False));
    InternalRegisterCallback(Pointer(@ID_GC), @TLua.__gc, LPtr);
    InternalRegisterCallback(Pointer(@ID_INDEX), @TLua.__index, LPtr);
    InternalRegisterCallback(Pointer(@ID_NEWINDEX), @TLua.__newindex, LPtr, Ord(False));

    // operators
    case AKind of
      mtRecord:
      begin
        RegisterOperators([OPERATOR_NEG, OPERATOR_ADD, OPERATOR_SUB, OPERATOR_MUL, OPERATOR_DIV,
          OPERATOR_MOD, OPERATOR_POW, OPERATOR_EQUAL, OPERATOR_LESS, OPERATOR_LESS_EQUAL]);
      end;
      mtArray:
      begin
        if (Assigned(ATypeInfo)) and (PTypeInfo(ATypeInfo).Kind = tkDynArray) then
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
procedure TLua.InternalReplaceChildNameSpace(const AParentMetaItem: Pointer{PLuaDictionaryItem};
  const AName: __luaname; const ALastValue{+inherited}, AValue{+inherited}: __luapointer; const AIsDefaultProp: Boolean);
var
  LCurrentMetaItem, LTopMetaItem: PLuaDictionaryItem;
  LParentPtr, LLastDefaultPtr: __luapointer;
  LMetaType: PLuaClassInfo;
  LChildNameItem: PLuaDictionaryItem;
begin
  LMetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(PLuaDictionaryItem(AParentMetaItem).Value);
  LLastDefaultPtr := LMetaType.DefaultProperty;
  if (AIsDefaultProp) then
  begin
    LMetaType.DefaultProperty := AValue and NAMESPACE_FLAGS_CLEAR;
  end;

  LCurrentMetaItem := AParentMetaItem;
  LParentPtr := LCurrentMetaItem.Value;
  LTopMetaItem := @TLuaDictionary(FMetaTypes).FItems[TLuaDictionary(FMetaTypes).Count];
  repeat
    Inc(LCurrentMetaItem);
    if (LCurrentMetaItem = LTopMetaItem) then Break;
    if (LCurrentMetaItem.Value = LParentPtr) then Continue;

    LMetaType := TLuaMemoryHeap(FMemoryHeap).Unpack(LCurrentMetaItem.Value);
    if (LMetaType.Kind = mtClass) and (LMetaType.Parent = LParentPtr) then
    begin
      LChildNameItem := TLuaDictionary(LMetaType.FNameSpace).InternalFind(AName, True);
      if (LChildNameItem.Value = ALastValue{invalid(new) or last}) then
      begin
        LChildNameItem.Value := AValue;
        InternalReplaceChildNameSpace(LCurrentMetaItem, AName, ALastValue, AValue, AIsDefaultProp);
      end else
      if (AIsDefaultProp) and (LMetaType.DefaultProperty = LLastDefaultPtr) then
      begin
        // replace defaults
        InternalReplaceChildNameSpace(LCurrentMetaItem, AName, ALastValue, AValue, AIsDefaultProp);;
      end;
    end;
  until (False);
end;

function TLua.InternalAddMethod(const AMetaType: PLuaMetaType; const AName: LuaString;
  const AAddress: Pointer; const AMethodKind: TLuaMethodKind; const AInvokable: __luapointer;
  const ACritical: Boolean; {Universal} const AMinArgsCount, AMaxArgsCount: Word): __luapointer;
const
  STR_METHOD: array[0..6] of Byte = (6, Ord('M'), Ord('e'), Ord('t'), Ord('h'), Ord('o'), Ord('d'));
label
  new_or_replace, done;
var
  LAddress: Pointer;
  LVmtOffset: NativeInt;
  LProcName: __luaname;
  LTypeName: ^LuaString;
  LValue: TLuaNamespaceMethod;
  LItem: PLuaDictionaryItem;
  LEntity: PLuaGlobalEntity;
  LLastPtr, LPtr: __luapointer;
  LProc: ^TLuaNamespaceMethod;
  LFrame: TLuaNativeFrame;
  LNameBuffer: ShortString;
begin
  Result := LUA_POINTER_INVALID;
  NativeFrameEnter(LFrame, PShortString(@STR_METHOD), ACritical);
  try
    LAddress := AAddress;
    if (NativeUInt(LAddress) <= $FFFF) then
    begin
      RegisterError('address not defined');
      Exit;
    end;

    if (AInvokable = LUA_POINTER_INVALID) and
      ((AMinArgsCount > AMaxArgsCount) or (AMaxArgsCount > 20)) then
    begin
      RegisterErrorFmt('non-available argument count range: %d..%d', [AMinArgsCount, AMaxArgsCount]);
      Exit;
    end;

    // method name
    if (not IsValidIdent(AName)) then
    begin
      RegisterErrorFmt('non-supported name ("%s")', [AName]);
      Exit;
    end;
    LProcName := TLuaNames(FNames).Add(AName);
    NativeFrameBufferedRename(LNameBuffer, AName);

    if Assigned(AMetaType) and (AMetaType.F.Kind = mtClass) then
    begin
      LVmtOffset := PLuaClassInfo(AMetaType).VmtOffset(LAddress, False);
      if (LVmtOffset >= 0) then
      begin
        LAddress := Pointer(NativeInt(PROPSLOT_VIRTUAL) or LVmtOffset);
      end;
    end;

    // type name
    if (AMetaType = nil) then
    begin
      LTypeName := @GLOBAL_NAME_SPACE;
    end else
    begin
      LTypeName := @AMetaType.FName;
    end;

    // parameters
    LValue.ItemName := LProcName;
    LValue.Address := LAddress;
    LValue.Kind := AMethodKind;
    LValue.Mode := TLuaMethodMode(AInvokable <> LUA_POINTER_INVALID);
    LValue.ScriptInstance := (AMethodKind <> mkStatic);
    LValue.Invokable := AInvokable;
    if (LValue.Mode = mmUniversal) then
    begin
      LValue.MinArgsCount := AMinArgsCount;
      LValue.MaxArgsCount := AMaxArgsCount;
    end;
    LValue.Func := LUA_FUNC_INVALID;

    // find existing/add item
    LEntity := nil;
    LItem := nil;
    if (AMetaType = nil) then
    begin
      LEntity := InternalAddGlobal(Ord(gkProc), LProcName);
      LLastPtr := LEntity.Ptr;
    end else
    begin
      LItem := TLuaDictionary(PLuaRecordInfo(AMetaType).FNameSpace).InternalFind(LProcName, True);
      LLastPtr := LItem.Value;
    end;

    // fill/update routine
    //  not found: fill and add childs (replace invalid)
    //  found inherited: replace to new, replace last value to new
    if (LLastPtr = LUA_POINTER_INVALID) then
    begin
    new_or_replace:
      LPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaNamespaceMethod));
      if (AMetaType = nil) then
      begin
        LEntity.Ptr := LPtr;
      end else
      begin
        LItem.Value := LPtr or NAMESPACE_FLAG_PROC;
      end;
      LProc := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr);
      LProc^ := LValue;
      LProc.Func := alloc_luafunction(LProc);

      if (Assigned(AMetaType) and (AMetaType.F.Kind = mtClass)) then
      begin
        InternalReplaceChildNameSpace(TLuaDictionary(FMetaTypes).Find(Pointer(AMetaType.F.ClassType)),
          LProcName, LLastPtr, LPtr or NAMESPACE_FLAG_INHERITED, False);
      end
    end else
    if (LLastPtr and NAMESPACE_STD_MASK = NAMESPACE_STD_MASK) then
    begin
      // ToDo?
      Exit;
    end else
    if (LLastPtr and NAMESPACE_FLAG_PROC = 0) then
    begin
      RegisterErrorFmt('property "%s" is already contained in %s', [AName, LTypeName^]);
      Exit;
    end else
    begin
      if (LLastPtr and NAMESPACE_FLAG_INHERITED = 0) then
      begin
        // own method
        LPtr := LLastPtr;
        LProc := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr and NAMESPACE_FLAGS_CLEAR);
        LProc^ := LValue;
        LProc.Func := alloc_luafunction(LProc);
      end else
      begin
        // inherited method: done if not changed
        LProc := TLuaMemoryHeap(FMemoryHeap).Unpack(LLastPtr and NAMESPACE_FLAGS_CLEAR);
        if (LValue.Address = LProc.Address) and (LValue.Kind = LProc.Kind) and
          (LValue.Invokable{ArgsCount} = LProc.Invokable{ArgsCount}) then
        begin
          LPtr := LLastPtr;
          goto done;
        end;

        // replace to new allocated method
        goto new_or_replace;
      end;
    end;

  done:
    Result := LPtr and NAMESPACE_FLAGS_CLEAR;
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
    NativeFrameBufferedRename(LNameBuffer, AName);
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

procedure __AddClassRtti(const ALua: TLua; const AClassInfo: PLuaClassInfo;
  const AValue: TClass; const AUseExtendedRtti: Boolean);
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
  LPtr: PByte;
  LCount, i: Integer;
  LFieldTable: PClassFieldTable;
  LField: PClassField;
  LItemName: LuaString;
  LPropInfo: {$ifdef UNITSCOPENAMES}System.{$endif}TypInfo.PPropInfo;
  {$ifdef EXTENDEDRTTI}
  LMemberVisibility: TMemberVisibility;
  LFieldEx: PClassFieldEx;
  LReference: TLuaReference;
  LMethodEx: PExtendedClassMethodEntry;
  LMethodKind: TLuaMethodKind;
  LVmtOffset: NativeInt;
  LChildFrame: TLuaNativeFrame;
  LInvokable: __luapointer;
  {$endif}

  procedure AddPropInfo;
  var
    LOptions: TLuaPropertyOptions;
    LGetter, LSetter: NativeUInt;
  begin
    ALua.unpack_lua_string(LItemName, PShortString(@LPropInfo.Name)^);

    LGetter := NativeUInt(LPropInfo.GetProc);
    LSetter := NativeUInt(LPropInfo.SetProc);
    FillChar(LOptions, SizeOf(LOptions), #0);
    LOptions.MetaType := AClassInfo;
    LOptions.TypeInfo := LPropInfo.PropType{$ifNdef FPC}^{$endif};
    {$ifdef EXTENDEDRTTI}
    LOptions.Getter.Reference := LReference;
    LOptions.Setter.Reference := LReference;
    {$endif}
    LOptions.Index := LPropInfo.Index;

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

    ALua.InternalAddProperty(LItemName, @LOptions, True, False);
  end;

  function AddUniversalMethod(const Method: PClassMethodEntry; const MethodKind: TLuaMethodKind): Boolean;
  var
    LBuffer: ShortString;
    LNameLength: Integer;
  begin
    LNameLength := Method.NameLength;
    Result := (LNameLength > 3) and (Method.NameChars[1] = Byte('l')) and
      (Method.NameChars[2] = Byte('u')) and (Method.NameChars[3] = Byte('a'));

    if (Result) then
    begin
      Dec(LNameLength, 3);
      PByte(@LBuffer)^ := LNameLength;
      System.Move(Method.NameChars[4], LBuffer[1], LNameLength);
      ALua.unpack_lua_string(LItemName, LBuffer);

      ALua.InternalAddMethod(AClassInfo, LItemName, Method.Address, MethodKind, LUA_POINTER_INVALID, False);
    end;
  end;
begin
  // registrators hierarchy
  if (AClassInfo.F.ClassType <> AValue) and (AClassInfo.F.ClassType <> AValue.ClassParent) then
  begin
    __AddClassRtti(ALua, AClassInfo, AValue.ClassParent, AUseExtendedRtti);
  end;

  // published (standard) fields
  LPtr := PPointer(NativeInt(AValue) + vmtFieldTable)^;
  if (Assigned(LPtr)) then
  begin
    LFieldTable := Pointer(LPtr);
    if (LFieldTable.Classes <> nil) then
    for i := 0 to Integer(LFieldTable.Classes.Count) - 1 do
    begin
      ALua.InternalAutoRegister(LFieldTable.Classes.Values[i].ClassInfo, AUseExtendedRtti);
    end;

    LPtr := Pointer(@LFieldTable.Fields[0]);
    for i := 0 to Integer(LFieldTable.Count) - 1 do
    begin
      LField := Pointer(LPtr);
      ALua.unpack_lua_string(LItemName, LField.Name);
      ALua.InternalAddField(AClassInfo, LItemName,
        LField.Offset, LFieldTable.Classes.Values[LField.TypeIndex].ClassInfo,
        lrDefault, False, True, False);

      LPtr := GetTail(LField.Name);
    end;
  end;

  // extended fields
  {$ifdef EXTENDEDRTTI}
  if (Assigned(LPtr)) and (AUseExtendedRtti) then
  begin
    LCount := PWord(LPtr)^;
    Inc(LPtr, SizeOf(Word));

    for i := 1 to LCount do
    begin
      LFieldEx := Pointer(LPtr);
      LPtr := GetTail(LFieldEx.Name);
      LPtr := SkipAttributes(LPtr, LReference);

      LMemberVisibility := TMemberVisibility(LFieldEx.Flags and 3);
      case (LMemberVisibility) of
        mvPublic, mvPublished:
        begin
          ALua.unpack_lua_string(LItemName, LFieldEx.Name);
          ALua.InternalAddField(AClassInfo, LItemName, LFieldEx.Offset,
            GetTypeInfo(LFieldEx.TypeRef), LReference, False, True, False);
        end;
      end;
    end;
  end;
  {$endif}

  // published (standard) properties
  LPtr := GetTail(GetTypeData(AValue.ClassInfo).UnitName);
  LCount := PWord(LPtr)^;
  Inc(LPtr, SizeOf(Word));
  for i := 1 to LCount do
  begin
    LPropInfo := Pointer(LPtr);
    {$ifdef EXTENDEDRTTI}
    LReference := lrDefault;
    {$endif}
    AddPropInfo;
    LPtr := GetTail(LPropInfo.Name);
  end;

  // extended properties
  {$ifdef EXTENDEDRTTI}
  if (AUseExtendedRtti) then
  begin
    LCount := PWord(LPtr)^;
    Inc(LPtr, SizeOf(Word));

    for i := 1 to LCount do
    begin
      LMemberVisibility := TMemberVisibility(LPtr^ and 3);
      Inc(LPtr);

      LPropInfo := PPointer(LPtr)^;
      Inc(LPtr, SizeOf(Pointer));
      LPtr := SkipAttributes(LPtr, LReference);

      case (LMemberVisibility) of
        mvPublic, mvPublished:
        begin
          AddPropInfo;
        end;
      end;
    end;
  end;
  {$endif}

  // published (standard) methods
  LPtr := PPointer(NativeInt(AValue) + vmtMethodTable)^;
  if (Assigned(LPtr)) then
  begin
    LCount := PWord(LPtr)^;
    Inc(LPtr, SizeOf(Word));

    for i := 1 to LCount do
    begin
      AddUniversalMethod(Pointer(LPtr), mkInstance);
      Inc(LPtr, PClassMethodEntry(LPtr).Size);
    end;
  end;

  // extended methods
  {$ifdef EXTENDEDRTTI}
  if (Assigned(LPtr)) and (AUseExtendedRtti) then
  begin
    LCount := PWord(LPtr)^;
    Inc(LPtr, SizeOf(Word));

    for i := 1 to LCount do
    begin
      LMethodEx := PExtendedClassMethodEntry(LPtr);

      LMemberVisibility := LMethodEx.MemberVisibility;
      case (LMemberVisibility) of
        mvPublic, mvPublished:
        begin
          LMethodKind := LMethodEx.DefaultMethodKind;
          if (not AddUniversalMethod(LMethodEx.Entry, LMethodKind)) then
          begin
            LVmtOffset := AClassInfo.VmtOffset(LMethodEx.Entry.Address, True);
            if (LVmtOffset = -1) or (LVmtOffset > {$ifdef FPC}vmtToString{$else .DELPHI}vmtDestroy{$endif}) then
            begin
              ALua.NativeFrameEnter(LChildFrame, PShortString(@LMethodEx.Entry.Name), False);
              try
                LInvokable := TLuaInvokableBuilder(ALua.FInvokableBuilder).BuildClassMethod(
                  AClassInfo, LMethodEx.Entry, LMethodKind, LMethodEx.IsHasSelf);
              finally
                ALua.NativeFrameLeave;
              end;

              if (LInvokable <> LUA_POINTER_INVALID) then
              begin
                ALua.unpack_lua_string(LItemName, LMethodEx.Entry.Name);
                ALua.InternalAddMethod(AClassInfo, LItemName, LMethodEx.Entry.Address, LMethodKind, LInvokable, False);
              end;
            end;
          end;
        end;
      end;

      Inc(LPtr, SizeOf(TExtendedClassMethodEntry));
    end;
  end;
  {$endif}
end;

// add class, if not already exists
// UseRtti means to register all published properties
// in registrator-Class case:
// addition sub-registered class using published registrator-Class methods, fields and properties
function TLua.InternalAddClass(const AClass: TClass; const AUseRtti: Boolean;
  const ACritical: Boolean): PLuaClassInfo;
const
  STR_CLASS: array[0..5] of Byte = (5, Ord('C'), Ord('l'), Ord('a'), Ord('s'), Ord('s'));
var
  i: Integer;
  LClassName: LuaString;
  LLuaClassName: __luaname;
  LClassRegistrator, LClassValue, LClassChild: TClass;
  LClassPtr: __luapointer;
  LClassInfo, LParent: PLuaClassInfo;
  LItem: PLuaDictionaryItem;
  LFrame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(LFrame, PShortString(@STR_CLASS), ACritical);
  try
    if (NativeUInt(AClass) <= $FFFF) then
    begin
      RegisterError('parameter not defined');
      Exit;
    end;

    // find exists
    LClassPtr := TLuaDictionary(FMetaTypes).FindValue(Pointer(AClass));
    if (LClassPtr <> LUA_POINTER_INVALID) and (not AUseRtti) then
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LClassPtr);
      if (Result.F.Kind <> mtClass) then
      begin
        LClassName := Result.Name;
        RegisterErrorFmt('invalid class "%s" parameter', [LClassName]);
      end;
      Exit;
    end;

    // registrator, class, class name
    LClassValue := AClass;
    LClassRegistrator := nil;
    NativeFrameRename(PShortString(PPointer(NativeInt(LClassValue) + vmtClassName)^));
    repeat
      LClassName := LuaString(LClassValue.ClassName);
      if (Length(LClassName) > 3) and (LClassName[1] = 'l') and
        (LClassName[2] = 'u') and (LClassName[3] = 'a') then
      begin
        if (LClassRegistrator = nil) then LClassRegistrator := AClass;
        LClassChild := LClassValue;
        LClassValue := LClassValue.ClassParent;
        if (LClassValue = nil) then
        begin
          RegisterError('ClassRegistrator is defined, but real class not found');
          Exit;
        end;

        if (LClassValue.InstanceSize <> LClassChild.InstanceSize) then
        begin
          LClassName := LuaString(LClassChild.ClassName);
          RegisterErrorFmt('Class registrator "%s" can''t have own fields', [LClassName]);
          Exit;
        end;
      end else
      begin
        Break;
      end;
    until (False);

    // existing or new class
    if (LClassValue <> AClass) then
    begin
      LClassPtr := TLuaDictionary(FMetaTypes).FindValue(Pointer(LClassValue));
    end;
    if (LClassPtr <> LUA_POINTER_INVALID) then
    begin
      LClassInfo := TLuaMemoryHeap(FMemoryHeap).Unpack(LClassPtr);
    end else
    begin
      // check existing name (type)
      LLuaClassName := TLuaNames(FNames).Add(LClassName);
      if (TLuaDictionary(FMetaTypes).Find(LLuaClassName) <> nil) then
      begin
        RegisterErrorTypeExists(LClassName);
        Exit;
      end;

      // globals, metatypes dictionary, metatable
      LClassInfo := Pointer(InternalAddMetaType(mtClass, LClassName, Pointer(LClassValue), SizeOf(Pointer)));

      // register parents, inherits name space
      if (LClassValue.ClassParent = nil) then
      begin
        // TObject: default name space
        TLuaDictionary(LClassInfo.FNameSpace).Assign(TLuaDictionary(FStdObjectNameSpace));
        LClassInfo.Parent := LUA_POINTER_INVALID;
        LClassInfo.DefaultProperty := LUA_POINTER_INVALID;
       end else
      begin
        LParent := InternalAddClass(AClass.ClassParent, AUseRtti, ACritical);
        TLuaDictionary(LClassInfo.FNameSpace).Assign(TLuaDictionary(LParent.FNameSpace));
        LItem := Pointer(TLuaDictionary(LClassInfo.FNameSpace).FItems);
        for i := 1 to TLuaDictionary(LClassInfo.FNameSpace).Count do
        begin
          if (LItem.Value > 0) then
            LItem.Value := LItem.Value or NAMESPACE_FLAG_INHERITED;

          Inc(LItem);
        end;

        LClassInfo.Parent := LParent.Ptr;
        LClassInfo.DefaultProperty := LParent.DefaultProperty;
        LClassInfo.CFunctionCreate := LParent.CFunctionCreate;
        LClassInfo.CFunctionFree := LParent.CFunctionFree;
        LClassInfo.AssignCallback := LParent.AssignCallback;
        LClassInfo.CreateCallback := LParent.CreateCallback;
        LClassInfo.CreateCallbackArgsCount := LParent.CreateCallbackArgsCount;
      end;
    end;

    // class rtti
    if (LClassInfo.Parent <> LUA_POINTER_INVALID) and (AUseRtti) then
    begin
      __AddClassRtti(Self, LClassInfo, LClassValue, True);
    end;

    // class registrators
    if (LClassRegistrator <> nil) then
    begin
      __AddClassRtti(Self, LClassInfo, LClassRegistrator, AUseRtti);
    end;

    // result
    Result := LClassInfo;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalGetClassInfo(const AClass: TClass): PLuaClassInfo;
var
  LItem: PLuaDictionaryItem;
begin
  if (Assigned(AClass)) then
  begin
    LItem := TLuaDictionary(FMetaTypes).InternalFind(Pointer(AClass), False);
    if (Assigned(LItem)) then
    begin
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LItem.Value);
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

function TLua.InternalAddRecord(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaRecordInfo;
const
  STR_RECORD: array[0..6] of Byte = (6, Ord('R'), Ord('e'), Ord('c'), Ord('o'), Ord('r'), Ord('d'));
//var
//  Frame: TLuaNativeFrame;
var
  LName: LuaString;
begin
// NativeFrameEnter(Frame, PShortString(@STR_RECORD));
//  try
    // ToDo
    unpack_lua_string(LName, PShortString(@ATypeInfo.Name)^);
    Result := InternalAddRecordEx(LName, ATypeInfo, True, ACritical);
//  finally
//    NativeFrameLeave;
//  end;
end;

// TypeInfo can be the following:
//  - TypeInfo(record)
//  - TypeInfo(Dynamic array of record type)
//  - Pointer(SizeOf(record))
function TLua.InternalAddRecordEx(const AName: LuaString; const ATypeInfo: Pointer;
  const AUseRtti: Boolean; const ACritical: Boolean): PLuaRecordInfo;
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
  LRecordTypeInfo: PTypeInfo;
  LRecordSize: Integer;
  LTypeData: PTypeData;
  LRecordPtr: __luapointer;
  LLuaName: __luaname;
  {$ifdef EXTENDEDRTTI}
  LPtr: PByte;
  LCount, i, j: Integer;
  LItemName: LuaString;
  LItemTypeInfo: PTypeInfo;
  LField: PRecordTypeField;
  LReference: TLuaReference;
  LMethod: PRecordTypeMethod;
  LMethodKind: TLuaMethodKind;
  LNameBuffer: ShortString;
  LChildFrame: TLuaNativeFrame;
  LInvokable: __luapointer;
  {$endif}
  LFrame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(LFrame, PShortString(@STR_RECORD), ACritical);
  try
    if (not IsValidIdent(AName)) then
    begin
      RegisterErrorFmt('non-supported name ("%s")', [AName]);
      Exit;
    end;

    if (ATypeInfo = nil) then
    begin
      RegisterErrorFmt('TypeInfo of record "%s" not defined', [AName]);
      Exit;
    end;

    LRecordTypeInfo := nil;
    if (NativeUInt(ATypeInfo) <= $FFFF) then
    begin
      LRecordSize := NativeInt(ATypeInfo);
    end else
    begin
      LTypeData := GetTypeData(ATypeInfo);

      // record or dynamic array
      if (PTypeInfo(ATypeInfo).Kind in RECORD_TYPES) then
      begin
        LRecordSize := LTypeData.elSize;
        LRecordTypeInfo := ATypeInfo;
      end else
      if (PTypeInfo(ATypeInfo).Kind = tkDynArray) then
      begin
        LRecordSize := LTypeData.elSize;

        if (LTypeData.elType <> nil) then
        begin
          LRecordTypeInfo := LTypeData.elType{$ifNdef FPC}^{$endif};
          if (LRecordTypeInfo <> nil) and (not (LRecordTypeInfo.Kind in RECORD_TYPES)) then
          begin
            unpack_lua_string(FStringBuffer.Lua, PShortString(@LRecordTypeInfo.Name)^);
            GetTypeKindName(FStringBuffer.Default, LRecordTypeInfo.Kind);
            RegisterErrorFmt('sub dynamic type "%s" is not record type (%s)',
              [FStringBuffer.Lua, FStringBuffer.Default]);
            Exit;
          end;
        end;
      end else
      begin
        GetTypeKindName(FStringBuffer.Default, PTypeInfo(ATypeInfo).Kind);
        RegisterErrorFmt('type "%s" is not record or subdynamic type (%s)',
          [AName, FStringBuffer.Default]);
        Exit;
      end;
    end;

    // find existing, name item
    if (Assigned(LRecordTypeInfo)) then
    begin
      unpack_lua_string(FStringBuffer.Lua, PShortString(@LRecordTypeInfo.Name)^);

      if (FStringBuffer.Lua <> AName) then
      begin
        RegisterErrorFmt('mismatch of names TypeInfo "%s" and "%s" as parameter "Name"',
          [FStringBuffer.Lua, AName]);
        Exit;
      end;

      LRecordPtr := TLuaDictionary(FMetaTypes).FindValue(LRecordTypeInfo);
    end else
    begin
      LRecordPtr := LUA_POINTER_INVALID;
    end;
    LLuaName := TLuaNames(FNames).Add(AName);
    if (LRecordPtr = LUA_POINTER_INVALID) then LRecordPtr := TLuaDictionary(FMetaTypes).FindValue(LLuaName);

    if (LRecordPtr <> LUA_POINTER_INVALID) then
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LRecordPtr);

      if (Result.Kind <> mtRecord) then
      begin
        RegisterErrorTypeExists(AName);
        Exit;
      end;

      if (Result.Size <> LRecordSize) then
      begin
        RegisterErrorFmt('size of %s (%d) differs from the previous value (%d)',
          [AName, LRecordSize, Result.Size]);
        Exit;
      end;

      if (Assigned(LRecordTypeInfo)) then
      begin
        if (Result.F.TypeInfo = nil) then
        begin
          Result.F.TypeInfo := LRecordTypeInfo;
          Result.FillManagedValue;
          Result.FillHFAValue;
          TLuaDictionary(FMetaTypes).Add(LRecordTypeInfo, LRecordPtr);
        end else
        if (Result.F.TypeInfo <> LRecordTypeInfo) then
        begin
          RegisterErrorFmt('TypeInfo of "%s" differs from the previous value', [AName]);
          Exit;
        end;
      end;
    end else
    begin
      Result := Pointer(InternalAddMetaType(mtRecord, AName, LRecordTypeInfo, LRecordSize));
      TLuaDictionary(Result.FNameSpace).Assign(TLuaDictionary(FStdRecordNameSpace));
    end;

    if (not AUseRtti) or (not Assigned(LRecordTypeInfo)) then
      Exit;

    {$ifdef EXTENDEDRTTI}
    // name
    NativeFrameBufferedRename(LNameBuffer, AName);

    // skip managed (anonymous) fields
    LPtr := Pointer(@GetTypeData(LRecordTypeInfo).ManagedFldCount);
    LCount := PInteger(LPtr)^;
    Inc(LPtr, SizeOf(Integer));
    Inc(LPtr, LCount * SizeOf(TAnonymousFieldInfo));

    // skip "ops"
    LCount := LPtr^;
    Inc(LPtr);
    Inc(LPtr, LCount * SizeOf(Pointer));

    // fields
    LCount := PInteger(LPtr)^;
    Inc(LPtr, SizeOf(Integer));
    for i := 1 to LCount do
    begin
      LField := Pointer(LPtr);
      LPtr := GetTail(LField.Name);
      LPtr := SkipAttributes(LPtr, LReference);

      case TMemberVisibility(LField.Flags and 3) of
        mvPublic, mvPublished:
        begin
          LItemTypeInfo := GetTypeInfo(LField.Header.TypeInfo); // ToDo

          if (Assigned(LItemTypeInfo)) then
          begin
            unpack_lua_string(LItemName, LField.Name);
            InternalAddField(Result, LItemName, LField.Header.Offset, LItemTypeInfo,
              LReference, False, True, False);
          end;
        end;
      end;
    end;

    // methods
    LPtr := SkipAttributes(LPtr);
    LCount := PWord(LPtr)^;
    Inc(LPtr, SizeOf(Word));
    for i := 1 to LCount do
    begin
      LMethod := Pointer(LPtr);
      LPtr := GetTail(LMethod.Name);

      case TMemberVisibility((LMethod.Flags shr 2) and 3) of
        mvPublic, mvPublished:
        begin
          LMethodKind := METHOD_KINDS[LMethod.Flags and 3];
          if (LMethodKind <> mkOperator) then
          begin
            NativeFrameEnter(LChildFrame, PShortString(@LMethod.Name), False);
            try
              LInvokable := TLuaInvokableBuilder(FInvokableBuilder).BuildProcedureSignature(Result, Pointer(LPtr), LMethodKind);
            finally
              NativeFrameLeave;
            end;

            if (LInvokable <> LUA_POINTER_INVALID) then
            begin
              unpack_lua_string(LItemName, LMethod.Name);
              InternalAddMethod(Result, LItemName, LMethod.Address, LMethodKind, LInvokable, False);
            end;
          end;
        end;
      end;

      // skip parameters
      LPtr := @PProcedureSignature(LPtr).ParamCount;
      j := LPtr^;
      Inc(LPtr);
      while (j <> 0) do
      begin
        LPtr := GetTail(PProcedureParam(LPtr).Name);
        LPtr := SkipAttributes(LPtr);
        Dec(j);
      end;

      LPtr := SkipAttributes(LPtr);
    end;
    {$endif}
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddInterface(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaInterfaceInfo;
const
  STR_INTERFACE: array[0..9] of Byte = (9, Ord('I'), Ord('n'), Ord('t'), Ord('e'),
    Ord('r'), Ord('f'), Ord('a'), Ord('c'), Ord('e'));
var
  LPtr: PByte;
  LIsFunc: Boolean;
  LCount, i, j: Integer;
  LMetaTypePtr, LInvokablePtr, LMethodPtr: __luapointer;
  LParent: PLuaInterfaceInfo;
  LParentTypeInfo: PTypeInfo;
  LItemName: LuaString;
  LLuaItemName: __luaname;
  LMethodEntry: PIntfMethodEntry;
  LItem: PLuaDictionaryItem;
  LMethod: ^TLuaNamespaceMethod;
  LVmtOffset: NativeInt;
  LFrame: TLuaNativeFrame;
  LChildFrame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(LFrame, PShortString(@STR_INTERFACE), ACritical);
  try
    // check type info
    if (ATypeInfo = nil) then
    begin
      RegisterError('TypeInfo not defined');
      Exit;
    end;
    if (ATypeInfo.Kind <> tkInterface) or (IsReferenceMethodType(ATypeInfo)) then
    begin
      unpack_lua_string(FStringBuffer.Lua, PShortString(@ATypeInfo.Name)^);
      GetTypeKindName(FStringBuffer.Default, ATypeInfo.Kind);
      RegisterErrorFmt('type "%s" is not interface type (%s)',
        [FStringBuffer.Lua, FStringBuffer.Default]);
      Exit;
    end;

    // try to find
    LMetaTypePtr := TLuaDictionary(FMetaTypes).FindValue(ATypeInfo);
    if (LMetaTypePtr <> LUA_POINTER_INVALID) then
    begin
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LMetaTypePtr);
      Exit;
    end;

    // name
    NativeFrameRename(PShortString(@ATypeInfo.Name));

    // parent
    LParent := nil;
    LParentTypeInfo := {$ifNdef FPC}GetTypeInfo{$endif}(GetTypeData(ATypeInfo).IntfParent);
    if (Assigned(LParentTypeInfo)) then
      LParent := InternalAddInterface(LParentTypeInfo, ACritical);

    // name, check existing
    unpack_lua_string(LItemName, PShortString(@ATypeInfo.Name)^);
    LLuaItemName := TLuaNames(FNames).Add(LItemName);
    if (TLuaDictionary(FMetaTypes).Find(LLuaItemName) <> nil) then
    begin
      Result := LParent;
      Exit;
    end;

    // global metatype, inherits name space, base VMT offset
    Result := Pointer(InternalAddMetaType(mtInterface, LItemName, ATypeInfo, SizeOf(Pointer)));
    Result.Parent := LUA_POINTER_INVALID;
    if (Assigned(LParent)) then
    begin
      Result.Count := LParent.Count;
      Result.Parent := LParent.Ptr;
      TLuaDictionary(Result.FNameSpace).Assign(TLuaDictionary(LParent.FNameSpace));
      LVmtOffset := LParent.Count * SizeOf(Pointer);
    end else
    begin
      TLuaDictionary(Result.FNameSpace).Assign(TLuaDictionary(FStdInterfaceNameSpace));
      LVmtOffset := 0;
    end;

    // method table
    LPtr := GetTail(GetTypeData(ATypeInfo).IntfUnit);
    Inc(Result.Count, PWord(LPtr)^);
    Inc(LPtr, SizeOf(Word));
    LCount := PWord(LPtr)^;
    Inc(LPtr, SizeOf(Word));
    if (LCount = 0) or (LCount = $ffff) then
      Exit;
    for i := 1 to LCount do
    begin
      LMethodEntry := Pointer(LPtr);
      NativeFrameEnter(LChildFrame, PShortString(@LMethodEntry.Name), False);
      try
        LInvokablePtr := TLuaInvokableBuilder(FInvokableBuilder).BuildIntfMethod(Result, LMethodEntry);
      finally
        NativeFrameLeave;
      end;

      // InternalAddMethod
      if (LInvokablePtr <> LUA_POINTER_INVALID) then
      begin
        LLuaItemName := TLuaNames(FNames).Add(PShortString(@LMethodEntry.Name)^);
        LItem := TLuaDictionary(Result.FNameSpace).InternalFind(LLuaItemName, True);
        if (LItem.Value <> LUA_POINTER_INVALID) then
        begin
          LMethod := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LItem.Value);
          LMethod.Invokable := LInvokablePtr;
        end else
        begin
          LMethodPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaNamespaceMethod));
          LItem.Value := LMethodPtr or NAMESPACE_FLAG_PROC;
          LMethod := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LMethodPtr);

          LMethod.ItemName := LLuaItemName;
          LMethod.Address := Pointer(NativeInt(PROPSLOT_VIRTUAL) or LVmtOffset);
          LMethod.Kind := mkInstance;
          LMethod.Mode := mmInvokable;
          LMethod.ScriptInstance := True;
          LMethod.Invokable := LInvokablePtr;
          LMethod.Func := alloc_luafunction(LMethod);
        end;
      end;

      // skip method
      Inc(LVmtOffset, SizeOf(Pointer));
      LPtr := GetTail(LMethodEntry.Name);
      LIsFunc := (LPtr^ = 1);
      Inc(LPtr);
      Inc(LPtr);
      j := LPtr^;
      Inc(LPtr);
      while (j <> 0) do
      begin
        Inc(LPtr);
        LPtr := GetTail(LPtr^);
        LPtr := GetTail(LPtr^);
        Inc(LPtr, SizeOf(Pointer));
        LPtr := SkipAttributes(LPtr);

        Dec(j);
      end;
      if (LIsFunc) then
      begin
        LPtr := GetTail(LPtr^);
        Inc(LPtr, SizeOf(Pointer));
      end;
      LPtr := SkipAttributes(LPtr);
    end;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddArray(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaArrayInfo;
const
  STR_ARRAY: array[0..5] of Byte = (5, Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'));
var
  LFrame: TLuaNativeFrame;
begin
  NativeFrameEnter(LFrame, PShortString(@STR_ARRAY), ACritical);
  try
    Result := nil{ToDo};
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddArrayEx(const AIdentifier, AItemTypeInfo: Pointer;
  const ABounds: array of Integer; const ACritical: Boolean): PLuaArrayInfo;
const
  STR_ARRAY: array[0..5] of Byte = (5, Ord('A'), Ord('r'), Ord('r'), Ord('a'), Ord('y'));
  STATIC_DYNAMIC: array[boolean] of string = ('static', 'dynamic');
var
  LArrayTypeInfo, LBufTypeInfo: PTypeInfo;
  LTypeData: PTypeData;
  LArrayPtr: __luapointer;
  LLuaName: __luaname;
  LIsDynamic: Boolean;
  LItemSize, LDimention, i, LCount: Integer;
  LElemType: PPTypeInfo;
  LAdvancedSize: NativeInt;
  LFrame: TLuaNativeFrame;
  LNameBuffer: ShortString;
begin
  Result := nil;
  NativeFrameEnter(LFrame, PShortString(@STR_ARRAY), ACritical);
  try
    // identifier: name or typeinfo
    begin
      if (NativeUInt(AIdentifier) <= $FFFF) then
      begin
        RegisterError('array identifier not defined');
        Exit;
      end;
      try
        if (TTypeKind(AIdentifier^) in [tkArray, tkDynArray]) then
        begin
          LArrayTypeInfo := PTypeInfo(AIdentifier);
          unpack_lua_string(FStringBuffer.Lua, PShortString(@LArrayTypeInfo.Name)^);
        end else
        begin
          FStringBuffer.Lua := PLuaChar(AIdentifier);
          LArrayTypeInfo := nil;
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
      NativeFrameBufferedRename(LNameBuffer, FStringBuffer.Lua);
    end;

    // static/dynamic
    LIsDynamic := Assigned(LArrayTypeInfo) and (LArrayTypeInfo.Kind = tkDynArray);
    if (LIsDynamic <> (Length(ABounds) = 0)) then
    begin
      if (LIsDynamic) then
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
      LItemSize := 100500;
    end;

    // Dimention, Bounds
    if (LIsDynamic) then
    begin
      LBufTypeInfo := LArrayTypeInfo;
      LDimention := 0;
      while (LBufTypeInfo <> nil) do
      begin
        Inc(LDimention);
        LTypeData := GetTypeData(LBufTypeInfo);
        LElemType := {$ifdef FPC}@{$endif}LTypeData.elType;

        if (LElemType = nil) then
        begin
          if (LItemSize = LTypeData.elSize) then Break;
        end else
        begin
          LBufTypeInfo := LElemType^;
          if (LBufTypeInfo = AItemTypeInfo) then Break;

          if (LBufTypeInfo.Kind = tkDynArray) then
          begin
            //ToDo if (PLuaArrayInfo(ItemTypeInfo).FTypeInfo = BufTypeInfo) then Break;
            Continue;
          end else
          if (LBufTypeInfo.Kind = tkArray) then
          begin
            //ToDo if (PLuaArrayInfo(ItemTypeInfo).FTypeInfo = BufTypeInfo) or
            //ToDo    (PLuaArrayInfo(ItemTypeInfo).FSize = GetTypeData(BufTypeInfo).elSize) then Break;
          end else
          if (LBufTypeInfo.Kind in RECORD_TYPES) then
          begin
            //ToDo if (PLuaRecordInfo(ItemTypeInfo).FTypeInfo = BufTypeInfo) then Break;
          end;
        end;

        RegisterErrorFmt('incorrect ItemTypeInfo of dynamic array "%s"', [FStringBuffer.Lua]);
        Exit;
      end;
    end else
    begin
      LDimention := Length(ABounds);
      if (LDimention and 1 = 1) then
      begin
        RegisterErrorFmt('"%s" registering... bounds size should be even. %d is an incorrect size',
          [FStringBuffer.Lua, LDimention]);
        Exit;
      end;
      LDimention := LDimention shr 1;

      for i := 0 to LDimention-1 do
      if (ABounds[i * 2] > ABounds[i * 2 + 1]) then
      begin
        RegisterErrorFmt('"%s" registering... incorrect bounds: "%d..%d"',
          [FStringBuffer.Lua, ABounds[i * 2], ABounds[i * 2 + 1]]);
        Exit;
      end;
    end;

    // find
    LArrayPtr := LUA_POINTER_INVALID;
    if (Assigned(LArrayTypeInfo)) then LArrayPtr := TLuaDictionary(FMetaTypes).FindValue(LArrayTypeInfo);
    if (LArrayPtr = LUA_POINTER_INVALID) then
    begin
      LLuaName := TLuaNames(FNames).Add(FStringBuffer.Lua);
      LArrayPtr := TLuaDictionary(FMetaTypes).FindValue(LLuaName);
      if (LArrayPtr = LUA_POINTER_INVALID) then
      begin
        LAdvancedSize := Length(ABounds) * SizeOf(Integer);
        Inc(LAdvancedSize, (LDimention - 1) * SizeOf(NativeInt));
        Result := Pointer(InternalAddMetaType(mtArray, FStringBuffer.Lua, LArrayTypeInfo, 0{fill later}, LAdvancedSize));
        Result.FNameSpace := FStdRecordNameSpace;

        Result.FIsDynamic := LIsDynamic;
        Result.FDimention := LDimention;
        Result.FItemSize := LItemSize;

        if (LIsDynamic) then
        begin
          LBufTypeInfo := LArrayTypeInfo;
          PTypeInfo(Result.FMultiplies[0]) := LBufTypeInfo;
          for i := 1 to LDimention - 1 do
          begin
            LBufTypeInfo := GetTypeData(LBufTypeInfo).elType{$ifNdef FPC}^{$endif};
            PTypeInfo(Result.FMultiplies[i]) := LBufTypeInfo;
          end;

          Result.F.Size := SizeOf(Pointer);
          Result.FFinalTypeInfo := LArrayTypeInfo;
          Result.FFinalItemsCount := 1;
        end else
        begin
          Result.FMultiplies[LDimention - 1] := LItemSize;
          for i := LDimention - 2 downto 0 do
          begin
            LCount := (ABounds[(i + 1) * 2 + 1] - ABounds[(i + 1) * 2]) + 1;
            Result.FMultiplies[i] := Result.FMultiplies[i + 1] * LCount;
          end;

          Result.FBounds := Pointer(@Result.FMultiplies[LDimention]);
          System.Move(ABounds[0], Result.FBounds^, Length(ABounds) * SizeOf(Integer));

          Result.F.Size := Result.FMultiplies[0];
          Result.FFinalTypeInfo := AItemTypeInfo{ToDo ???};
          Result.FFinalItemsCount := Result.F.Size div LItemSize;
        end;
      end else
      begin
        Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LArrayPtr);
        if (Result.Kind <> mtArray) {ToDo or (Result.Low <> Low) or (Result.High <> High)} then
        begin
          RegisterErrorTypeExists(FStringBuffer.Lua);
          Exit;
        end;

        if (Assigned(LArrayTypeInfo)) then
          TLuaDictionary(FMetaTypes).Add(LArrayTypeInfo, LArrayPtr);
      end;
    end else
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LArrayPtr);
    end;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalAddSet(const ATypeInfo: PTypeInfo; const ACritical: Boolean): PLuaSetInfo;
const
  STR_SET: array[0..3] of Byte = (3, Ord('S'), Ord('e'), Ord('t'));
  MASK_3 = $FF shl 3;
var
  LPtr: __luapointer;
  LItemTypeInfo: PTypeInfo;
  LTypeData: PTypeData;
  LLow, LHigh: Integer;
  LLuaName: __luaname;
  LFrame: TLuaNativeFrame;
begin
  Result := nil;
  NativeFrameEnter(LFrame, PShortString(@STR_SET), ACritical);
  try
    if (NativeUInt(ATypeInfo) <= $FFFF) then
    begin
      RegisterError('TypeInfo of set not defined');
      Exit;
    end;

    if (PTypeInfo(ATypeInfo).Kind <> tkSet) then
    begin
      GetTypeKindName(FStringBuffer.Default, PTypeInfo(ATypeInfo).Kind);
      RegisterErrorFmt('TypeInfo of set is invalid: TypeKind = %s', [FStringBuffer.Default]);
      Exit;
    end;

    LPtr := TLuaDictionary(FMetaTypes).FindValue(ATypeInfo);
    if (LPtr = LUA_POINTER_INVALID) then
    begin
      LItemTypeInfo := Pointer(GetTypeData(ATypeInfo).CompType);
      {$ifNdef FPC}
      if (Assigned(LItemTypeInfo)) then
      begin
        LItemTypeInfo := PPointer(LItemTypeInfo)^;
      end;
      {$endif}
      if (not Assigned(LItemTypeInfo)) then
      begin
        Exit; // ToDo?
      end;

      LTypeData := GetTypeData(LItemTypeInfo);
      LLow := LTypeData.MinValue;
      LHigh := LTypeData.MaxValue;

      unpack_lua_string(FStringBuffer.Lua, PShortString(@PTypeInfo(ATypeInfo).Name)^);
      LLuaName := TLuaNames(FNames).Add(FStringBuffer.Lua);
      LPtr := TLuaDictionary(FMetaTypes).FindValue(LLuaName);
      if (LPtr = LUA_POINTER_INVALID) then
      begin
        NativeFrameRename(PShortString(@PTypeInfo(ATypeInfo).Name));
        Result := Pointer(InternalAddMetaType(mtSet, FStringBuffer.Lua, ATypeInfo, 0{fill later}));
        Result.FNameSpace := FStdSetNameSpace;

        Result.FItemTypeInfo := LItemTypeInfo;
        Result.FLow := LLow;
        Result.FHigh := LHigh;
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

        if (LItemTypeInfo.Kind = tkEnumeration) and (not IsBooleanType(LItemTypeInfo)) then
        begin
          InternalAutoRegister(LItemTypeInfo);
        end;
      end else
      begin
        Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr);
        if (Result.Kind <> mtSet) or (Result.Low <> LLow) or (Result.High <> LHigh) then
        begin
          RegisterErrorTypeExists(FStringBuffer.Lua);
          Exit;
        end;

        TLuaDictionary(FMetaTypes).Add(ATypeInfo, LPtr);
      end;
    end else
    begin
      Result := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr);
    end;
  finally
    NativeFrameLeave;
  end;
end;

procedure TLua.InternalAddEnum(const ATypeInfo: PTypeInfo; const ACritical: Boolean);
const
  STR_ENUMERATION: array[0..11] of Byte = (11, Ord('E'), Ord('n'), Ord('u'), Ord('m'),
    Ord('e'), Ord('r'), Ord('a'), Ord('t'), Ord('i'), Ord('o'), Ord('n'));
var
  i, LMin, LMax, LRef: Integer;
  LPtr: PByte;
  LLuaName: __luaname;
  LTypeData: PTypeData;
  LFrame: TLuaNativeFrame;
begin
  Self.NativeFrameEnter(LFrame, PShortString(@STR_ENUMERATION), ACritical);
  try
    if (NativeUInt(ATypeInfo) <= $FFFF) then
    begin
      Self.RegisterError('TypeInfo not defined');
      Exit;
    end;

    if (ATypeInfo.Kind <> tkEnumeration) or (IsBooleanType(ATypeInfo)) then
    begin
      Self.unpack_lua_string(Self.FStringBuffer.Lua, PShortString(@ATypeInfo.Name)^);
      GetTypeKindName(Self.FStringBuffer.Default, ATypeInfo.Kind);
      Self.RegisterErrorFmt('type "%s" (kind: %s) is not enumeration',
        [Self.FStringBuffer.Lua, Self.FStringBuffer.Default]);
      Exit;
    end;

    // check fake (enumeration) storage
    if (Assigned(TLuaDictionary(Self.FGlobalEntities).InternalFind(ATypeInfo, False))) then
      Exit;

    // frame
    Self.NativeFrameRename(PShortString(@ATypeInfo.Name));

    // each enumeration value
    with GetTypeData(ATypeInfo)^ do
    begin
      LMin := MinValue;
      LMax := MaxValue;
      LTypeData := GetTypeData(BaseType{$ifNdef FPC}^{$endif});
      LPtr := Pointer(@LTypeData.NameList);

      for i := LMin to LMax do
      begin
        LLuaName := TLuaNames(Self.FNames).Add(PShortString(LPtr)^);
        LRef := PLuaGlobalEntity(Self.InternalAddGlobal(Ord(gkConst), LLuaName)).Ref;
        lua_pushinteger(Self.Handle, i);
        Self.global_fill_value(LRef);

        LPtr := GetTail(LPtr^);
      end;
    end;

    // fake (enumeration) storage
    TLuaDictionary(Self.FGlobalEntities).Add(ATypeInfo, LUA_POINTER_INVALID);
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

function TLua.InternalAutoRegister(const ATypeInfo: PTypeInfo; const AUseRtti: Boolean;
  const ACritical: Boolean): Pointer{PLuaMetaType/PLuaClosureType};
label
  invokable;
var
  LPtr: __luapointer;
  LClass: TClass;
  LDictionary: ^TLuaDictionary;
  LClosure: ^TLuaClosureType;
  LInvokableBuilder: __TLuaInvokableBuilder;
//  Frame: TLuaNativeFrame;
begin
//  NativeFrameEnter(Frame, PShortString(@));
//  try
    LPtr := LUA_POINTER_INVALID;

    case ATypeInfo.Kind of
      tkEnumeration:
      begin
        if (not IsBooleanType(ATypeInfo)) then
          InternalAddEnum(ATypeInfo, ACritical);
      end;
      //? tkVariant:
      tkMethod{$ifdef EXTENDEDRTTI}, tkProcedure{$endif}:
      begin
      invokable:
        LDictionary := @TLuaDictionary(FClosureTypes);
        LPtr := LDictionary.FindValue(ATypeInfo);
        if (LPtr = LUA_POINTER_INVALID) then
        begin
          LPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaClosureType));
          LClosure := TLuaMemoryHeap(FMemoryHeap).Unpack(LPtr);
          LClosure.Name := PShortString(@ATypeInfo.Name);
          LInvokableBuilder := InternalInvokableBuilderEnter;
          try
            LClosure.Invokable := InternalBuildInvokable(nil, ATypeInfo, TLuaMethodKind($ff), ACritical);
          finally
            InternalInvokableBuilderLeave(LInvokableBuilder);
          end;
          LDictionary.Add(ATypeInfo, LPtr);
        end;
      end;
      tkSet, tkClass, tkArray, tkRecord{$ifdef FPC}, tkObject{$endif},
      tkInterface, tkDynArray{$ifdef EXTENDEDRTTI}, tkClassRef{$endif}:
      begin
        LDictionary := @TLuaDictionary(FMetaTypes);
        LPtr := LDictionary.FindValue(ATypeInfo);

        if (LPtr = LUA_POINTER_INVALID) then
        begin
          if (ATypeInfo.Kind = tkInterface) and (IsReferenceMethodType(ATypeInfo)) then
            goto invokable;

          LInvokableBuilder := InternalInvokableBuilderEnter;
          try
            case ATypeInfo.Kind of
              tkClass:
              begin
                LClass := GetTypeData(ATypeInfo).ClassType;
                LPtr := LDictionary.FindValue(Pointer(LClass));
                if (LPtr = LUA_POINTER_INVALID) then
                begin
                  LPtr := InternalAddClass(LClass, AUseRtti, ACritical).Ptr;
                end else
                begin
                  LDictionary.Add(ATypeInfo, LPtr);
                end;
              end;
              {$ifdef EXTENDEDRTTI}
              tkClassRef:
              begin
                LClass := GetTypeData(GetTypeInfo(GetTypeData(ATypeInfo).InstanceType)).ClassType;
                LPtr := LDictionary.FindValue(Pointer(LClass));
                if (LPtr = LUA_POINTER_INVALID) then
                  LPtr := InternalAddClass(LClass, AUseRtti, ACritical).Ptr;

                LDictionary.Add(ATypeInfo, LPtr);
              end;
              {$endif}
              tkSet:
              begin
                // todo
                LPtr := InternalAddSet(ATypeInfo, ACritical).Ptr;
              end;
              tkArray:
              begin
                // todo
                LPtr := InternalAddArray(ATypeInfo, ACritical).Ptr;
              end;
              tkDynArray:
              begin
                // todo
                LPtr := InternalAddArray(ATypeInfo, ACritical).Ptr;
              end;
              tkRecord{$ifdef FPC}, tkObject{$endif}:
              begin
                // todo
                LPtr := InternalAddRecord(ATypeInfo, ACritical).Ptr;
              end;
              tkInterface:
              begin
                // todo
                LPtr := InternalAddInterface(ATypeInfo, ACritical).Ptr;
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
    if (LPtr <> LUA_POINTER_INVALID) then
      Result := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
//  finally
//    NativeFrameLeave;
//  end;
end;

function TLua.InternalBuildInvokable(const AMetaType: PLuaMetaType; const ATypeInfo: PTypeInfo;
  const AMethodKind: TLuaMethodKind; const ACritical: Boolean): __luapointer;
var
  LBuilder: ^TLuaInvokableBuilder;
  LFrame: TLuaNativeFrame;
begin
  NativeFrameEnter(LFrame, PShortString(@ATypeInfo.Name), ACritical);
  try
    Result := LUA_POINTER_INVALID;
    LBuilder := @TLuaInvokableBuilder(FInvokableBuilder);

    if (Assigned(ATypeInfo)) then
    case ATypeInfo.Kind of
      tkMethod:
      begin
        Result := LBuilder.BuildMethod(AMetaType, ATypeInfo, AMethodKind);
      end;
      {$ifdef EXTENDEDRTTI}
      tkProcedure:
      begin
        Result := LBuilder.BuildProcedure(AMetaType, ATypeInfo, AMethodKind);
      end;
      tkInterface:
      begin
        if (IsReferenceMethodType(ATypeInfo)) then
          Result := LBuilder.BuildReferenceMethod(AMetaType, ATypeInfo, AMethodKind);
      end;
      {$endif}
    end;
  finally
    NativeFrameLeave;
  end;
end;

function TLua.InternalBuildInvokable(const AMetaType: PLuaMetaType; const AName: LuaString;
  const AParams: array of TLuaProcParam; const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean;
  const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv; const ACritical: Boolean): __luapointer;
var
  LBuilder: ^TLuaInvokableBuilder;
  LFrame: TLuaNativeFrame;
  LNameBuffer: ShortString;
begin
  NativeFrameEnter(LFrame, nil, ACritical);
  try
    NativeFrameBufferedRename(LNameBuffer, AName);
    LBuilder := @TLuaInvokableBuilder(FInvokableBuilder);
    Result := LBuilder.BuildCustom(AMetaType, AParams, AResultType, AIsResultUnsafe, AMethodKind, ACallConv);
  finally
    NativeFrameLeave;
  end;
end;

function TLua.__print: Integer;
var
  LCount, i: Integer;
{$ifNdef UNICODE}
  LOutput: THandle;
  LWritten: Cardinal;
const
  CRLF_CHAR: WideChar = #10;
{$endif}
begin
  LCount := lua_gettop(Handle);

  if (LCount = 0) then
  begin
    Writeln;
  end else
  for i := 1 to LCount do
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, i, False);

    if (i <> 1) then Write(#9);

    {$ifdef UNICODE}
      if (i = LCount) then
      begin
        Writeln(FStringBuffer.Unicode);
      end else
      begin
        Write(FStringBuffer.Unicode);
      end;
    {$else .MSWINDOWS}
      LOutput := GetStdHandle(STD_OUTPUT_HANDLE);
      WriteConsoleW(LOutput, Pointer(FStringBuffer.Unicode), Length(FStringBuffer.Unicode), LWritten, nil);
      if (i = LCount) then WriteConsoleW(LOutput, @CRLF_CHAR, 1, LWritten, nil);
    {$endif}
  end;

  Result := 0;
end;

function TLua.__printf: Integer;
const
  SLN_CONST: Cardinal = Ord('S') + (Ord('l') shl 8) + (Ord('n') shl 16);
var
  LCount, LErrCode, P: Integer;
  LErr: ^string;
begin
  LCount := lua_gettop(Handle);

  global_push_value(FFormatWrapper);
  lua_insert(Handle, 1);
  LErrCode := lua_pcall(Self.Handle, LCount, LUA_MULTRET, 0);
  if (LErrCode = 0) then LErrCode := lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0);
  if (LErrCode <> 0) then
  begin
    LErr := @FStringBuffer.Default;
    {$ifdef UNICODE}
      stack_unicode_string(LErr^, -1);
    {$else}
      stack_ansi_string(LErr^, -1, 0);
    {$endif}
    stack_pop;
    P := Pos(']', LErr^);
    Delete(LErr^, 1, P);

    repeat
      P := Pos('format', LErr^);
      if (P = 0) then Break;

      LErr^[P + 0] := 'p';
      LErr^[P + 1] := 'r';
      LErr^[P + 2] := 'i';
      LErr^[P + 3] := 'n';
      LErr^[P + 4] := 't';
      LErr^[P + 5] := 'f';
    until (False);

    Self.Error(LErr^);
  end;

  Result := __print;
end;

function TLua.__GetUserData(const AMetaType: __luapointer; const ACheckAlreadyDestroyed: Boolean): Pointer;
label
  invalid_instance;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
begin
  LUserData := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: ;
 LUA_TUSERDATA: begin
                  LUserData := lua_touserdata(Handle, 1);
                  if (not Assigned(LUserData)) then goto invalid_instance;
                  if (not Assigned(LUserData.Instance)) and (ACheckAlreadyDestroyed) then
                  begin
                    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
                    Self.InternalErrorFmt('%s instance is already destroyed', [LMetaType.FName]);
                  end;
                end;
  else
  invalid_instance:
    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    Self.InternalErrorFmt('%s: invalid self argument', [LMetaType.FName]);
  end;

  Result := LUserData;
end;

function TLua.__GetSelfInstance(const AMethodName: __luaname): Pointer;
label
  invalid_instance;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
  LName: ^LuaString;
begin
  LUserData := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: ;
 LUA_TUSERDATA: begin
                  LUserData := lua_touserdata(Handle, 1);
                  if (not Assigned(LUserData)) then goto invalid_instance;
                  if (not Assigned(LUserData.Instance)) then
                  begin
                    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LUserData.MetaType);
                    Self.InternalErrorFmt('%s instance is already destroyed', [LMetaType.FName]);
                  end;
                end;
  else
  invalid_instance:
    LName := @Self.FStringBuffer.Lua;
    Self.unpack_lua_string(LName^, AMethodName);
    Self.InternalErrorFmt('%s: invalid self instance argument', [LName]);
  end;

  Result := LUserData.Instance;
end;

function TLua.__GetSelfClass(const AMethodName: __luaname; const AAllowInstance: Boolean): TClass;
label
  invalid_instance;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
  LName: ^LuaString;
begin
  Result := nil;
  case lua_type(Handle, 1) of
    LUA_TTABLE: begin
                  LMetaType := InternalTableToMetaType(1);
                  if (not Assigned(LMetaType)) or (LMetaType.Kind <> mtClass) then goto invalid_instance;
                  Result := LMetaType.F.ClassType;
                end;
 LUA_TUSERDATA: begin
                  if (not AAllowInstance) then goto invalid_instance;
                  LUserData := lua_touserdata(Handle, 1);
                  if (not Assigned(LUserData)) then goto invalid_instance;
                  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LUserData.MetaType);
                  if (not Assigned(LUserData.Instance)) then
                  begin
                    Self.InternalErrorFmt('%s instance is already destroyed', [LMetaType.FName]);
                  end;
                  if (LMetaType.Kind <> mtClass) then goto invalid_instance;
                  Result := TClass(PPointer(LUserData.Instance)^);
                end;
  else
  invalid_instance:
    LName := @Self.FStringBuffer.Lua;
    Self.unpack_lua_string(LName^, AMethodName);
    Self.InternalErrorFmt('%s: invalid self class argument', [LName]);
  end;
end;

function TLua.__InitTableValues(const AUserData: Pointer{PLuaUserData}; const AStackIndex: Integer): Integer;
label
  done;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
  LItem: PLuaDictionaryItem;
  LPtr: __luapointer;
  LProp: PLuaProperty;
  LLuaType: Integer;
  LTempUserData: TLuaUserData;

  procedure ErrorKeyValue(const AMetaType: PLuaMetaType; const ADescription: string);
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, -2);
    stack_force_unicode_string(FStringBuffer.UnicodeReserved, -1);

    ErrorFmt('Failure changing %s.%s to "%s": %s', [AMetaType.Name,
      FStringBuffer.Unicode, FStringBuffer.UnicodeReserved, ADescription]);
  end;

  procedure ErrorProperty(const AMetaType: PLuaMetaType; const AProp: PLuaProperty; const ADescription: string);
  const
    PROPERTY_KINDS: array[0..3] of string = ('property', 'class property', 'field', 'class field');
  var
    LKind: Integer;
  begin
    LKind := Ord(AProp.IsField) * 2 + Ord(AProp.IsStatic);
    ErrorKeyValue(AMetaType, PROPERTY_KINDS[LKind] + ' ' + ADescription);
  end;

begin
  LUserData := AUserData;
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LUserData.MetaType);
  if (not (LMetaType.F.Kind in [mtClass, mtRecord])) then
  begin
    ErrorFmt('%s instance can not be initialized by a table', [LMetaType.Name]);
    goto done;
  end;
  if (LUserData.Constant) then
  begin
    ErrorFmt('"%s" instance is a constant', [LMetaType.Name]);
  end;

  // for each
  lua_pushnil(Handle);
  while (lua_next(Handle, AStackIndex) <> 0) do
  begin
    if (lua_type(Handle, -2) <> LUA_TSTRING) then
    begin
      ErrorKeyValue(LMetaType, 'invalid key');
      goto done;
    end;

    LItem := TLuaDictionary(LMetaType.FNameSpace).InternalFind(lua_tolstring(Handle, -2, nil), False);
    if (Assigned(LItem)) then
    begin
      LPtr := LItem.Value;
      if (LPtr >= 0) then
      begin
        if (LPtr and NAMESPACE_FLAG_PROC <> 0) then
        begin
          ErrorKeyValue(LMetaType, 'method can not be changed');
          goto done;
        end;

        LPtr := LPtr and NAMESPACE_FLAGS_CLEAR;
        LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
        if (LProp.F.Options and PROP_SLOTSETTER_MASK = 0{Prop.SetterMode = pmNone}) then
        begin
          ErrorProperty(LMetaType, LProp, 'is a constant');
          goto done;
        end;

        if (LProp.F.Kind <> vkObject) and (LProp.F.Options and PROP_SLOTSETTER_MASK = 0{Prop.SetterMode = pmNone}) then
        begin
          ErrorProperty(LMetaType, LProp, 'is read only');
          goto done;
        end;

        if (LProp.Options and PROP_COMPLEX_MODE <> 0{Prop.IsComplex}) then
        begin
          ErrorProperty(LMetaType, LProp, 'is complicated to initialize');
          goto done;
        end;

        LLuaType := lua_type(Handle, -1);
        if (LLuaType = LUA_TTABLE) and (InternalTableToMetaType(-1) = nil) then
        begin
          // sub table initialization
          if (not (LProp.F.Kind in [vkObject, vkRecord])) then
          begin
            ErrorProperty(LMetaType, LProp, 'can not be initialized by a table');
            goto done;
          end;

          begin
            if (LProp.F.Options and PROP_SLOTGETTER_MASK = 0{Prop.GetterMode = pmNone}) then
            begin
              ErrorProperty(LMetaType, LProp, 'is write only');
              goto done;
            end;

            if (LProp.F.Options and PROP_SLOTGETTER_MASK <> Cardinal(pmField){Prop.GetterMode <> pmField}) or
              (not {Prop.SetterMode in [pmNone, pmField]}
               (TLuaPropertyMode((LProp.F.Options shr PROP_SLOTSETTER_SHIFT) and PROP_SLOT_MASK) in [pmNone, pmField])
              ) then
            begin
              ErrorProperty(LMetaType, LProp, 'is not a field');
              goto done;
            end;
          end;

          LTempUserData.Instance := Pointer(NativeUInt(LUserData.Instance) + LProp.Getter);
          PInteger(@LTempUserData.Kind)^ := 0;
          LTempUserData.MetaType := LProp.MetaType.Ptr;
          if (LProp.F.Kind = vkObject) then
          begin
            LTempUserData.Instance := PPointer(LTempUserData.Instance)^;
            if (not Assigned(LTempUserData.Instance)) then
            begin
              ErrorProperty(LMetaType, LProp, 'is nil');
              goto done;
            end;
          end;
          __InitTableValues(@LTempUserData, lua_gettop(Handle));
        end else
        begin
          // set property value
          FParamBuffer.UserData := LUserData;
          FParamBuffer.Prop := LProp;
          __newindex(LUserData.MetaType, True);
        end;
      end else
      begin
        // standard item
        stack_lua_string(FStringBuffer.Lua, -2);
        ErrorKeyValue(LMetaType, 'standard item can not be changed');
        goto done;
      end;
    end else
    begin
      ErrorKeyValue(LMetaType, 'item not found');
      goto done;
    end;

    // next iteration
    lua_settop(Handle, - 1 - 1);
  end;

done:
  Result := 0;
end;

function _SetContainsWrapper(ADest, ALeft, ARight: PByte; ASize: Integer): Boolean;
begin
  Result := _SetLe(ARight, ALeft, ASize);
end;

function TLua.__ExecuteSetMethod(const AUserData: Pointer{PLuaUserData}; const AFirstArgIndex, AArgsCount, AMethod: Integer): Integer;
label
  invalid_argument;
const
  METHOD_NAMES: array[0..3] of string = ('.Include() method', '.Exclude() method',
    '.Contains() method', ' constructor');
var
  i, LLuaType: Integer;
  LResult: Boolean;
  LInstance: Pointer;
  LSetInfo: PLuaSetInfo;
  LBitProc: function(Value: PByte; Bit: Integer): Boolean;
  LSetProc: function(Dest, Left, Right: PByte; ASize: Integer): Boolean;
  LValueUserData: PLuaUserData;
begin
  LSetInfo := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(PLuaUserData(AUserData).MetaType);
  case AMethod of
    1:
    begin
      // exclude
      LBitProc := Pointer(@SetBitExclude);
      LSetProc := Pointer(@SetsDifference);
    end;
    2:
    begin
      // contains
      LBitProc := SetBitContains;
      LSetProc := _SetContainsWrapper;
    end;
  else
    // include, constructor
    LBitProc := Pointer(@SetBitInclude);
    LSetProc := Pointer(@SetsUnion);
  end;

  LResult := False;
  LInstance := PLuaUserData(AUserData).Instance;
  for i := AFirstArgIndex to (AFirstArgIndex + AArgsCount - 1) do
  begin
    LLuaType := lua_type(Handle, i);
    case LLuaType of
      LUA_TNUMBER:
      begin
        FTempBuffer.D := lua_tonumber(Handle, i);
        if (NumberToInteger(FTempBuffer.D, FTempBuffer.I)) and
          (FTempBuffer.I >= LSetInfo.FLow) and (FTempBuffer.I <= LSetInfo.FHigh) then
        begin
          LResult := LBitProc(LInstance, FTempBuffer.I - LSetInfo.FCorrection);
        end else
        begin
          goto invalid_argument;
        end;
      end;
      LUA_TUSERDATA:
      begin
        LValueUserData := lua_touserdata(Handle, i);
        if (Assigned(LValueUserData)) and (Assigned(LValueUserData.Instance)) and
          (LValueUserData.MetaType = LSetInfo.Ptr) then
        begin
          LResult := LSetProc(LInstance, LInstance, LValueUserData.Instance, LSetInfo.Size);
        end else
        begin
          goto invalid_argument;
        end;
      end;
    else
    invalid_argument:
      stack_force_unicode_string(FStringBuffer.Unicode, i);
      ErrorFmt('Failure %s%s: invalid argument %d (%s)', [LSetInfo.Name, METHOD_NAMES[AMethod],
        i, FStringBuffer.Unicode]);
      LResult := False;
    end;

    if (AMethod = 2) and (not LResult) then Break;
  end;

  if (AMethod = 2) then
  begin
    lua_pushboolean(Handle, LResult);
    Result := 1;
  end else
  begin
    Result := 0;
  end;
end;

function TLua.__len(const AMetaType: __luapointer): Integer;
var
  LMetaType: PLuaMetaType;
begin
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  lua_pushinteger(Handle, LMetaType.F.Size);
  Result := 1;
end;

function TLua.__tostring(const AMetaType: __luapointer): Integer;
label
  push_type_name, invalid_user_data;
type
  TObjectToString = procedure(Instance: Pointer; var Result: string);
  TSetDescription = procedure(MetaType, Instance: Pointer; var Result: LuaString);
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
begin
  LUserData := __GetUserData(AMetaType, False);
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);

  if (LUserData = nil) then
  begin
    goto push_type_name;
  end else
  if (LUserData.Instance = nil) then
  begin
    goto invalid_user_data;
  end else
  case Byte(LUserData.Kind) of
    Ord(mtComplexProperty):
    begin
      push_lua_string('ikComplexProperty: ToDo');
    end;
    Ord(mtClass):
    begin
      {$ifdef EXTENDEDRTTI}
      TObjectToString(PPointer(PNativeUInt(LUserData.Instance)^ + NativeUInt(vmtToString))^)(LUserData.Instance, FStringBuffer.Default);
      push_unicode_string(FStringBuffer.Default);
      {$else}
      push_lua_string(InternalGetClassInfo(TClass(PPointer(LUserData.Instance)^)).FName);
      {$endif}
    end;
    Ord(mtRecord), Ord(mtInterface), Ord(mtArray):
    begin
    push_type_name:
      push_lua_string(LMetaType.FName);
    end;
    Ord(mtSet):
    begin
      TSetDescription(@TLuaSetInfo.Description)(LMetaType, LUserData.Instance, FStringBuffer.Lua);
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
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
begin
  LUserData := __GetUserData(AMetaType, False);

  if (Assigned(LUserData)) and (LUserData.Owner) and
    (Assigned(LUserData.Instance)) then
  begin
    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
    if (LMetaType.F.Kind = mtClass) or (LMetaType.F.Managed) then
      LMetaType.Dispose(LUserData.Instance);
  end;

  Result := 0;
end;

function TLua.__call(const AMetaType: __luapointer; const AParamMode: Boolean): Integer;
label
  construct, class_record_construct, done;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
  LArgsCount: Integer;
  LFirstArgIndex: Integer;
begin
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);
  LArgsCount := lua_gettop(Handle) - 1;
  if (AParamMode) then
  begin
    Inc(LArgsCount);
    goto construct;
  end;

  LUserData := __GetUserData(AMetaType);
  if (Assigned(LUserData)) then
  begin
    // initialization by a table
    if (LArgsCount = 1) and (lua_type(Handle, -1) = LUA_TTABLE) and
      (InternalTableToMetaType(-1) = nil) then
    begin
      if (LMetaType.F.Kind in [mtClass, mtRecord]) then
      begin
        Result := __InitTableValues(LUserData, -1);
      end else
      begin
        ErrorFmt('%s instance can not be initialized by a table.', [LMetaType.Name]);
        Result := 0;
      end;
    end else
    begin
      ErrorFmt('Incorrect usage of %s constructor.', [LMetaType.Name]);
      Result := 0;
    end;
    Exit;
  end;

  // constructor (default gc-destroyed instance)
construct:
  LUserData := push_metatype_instance(LMetaType, nil^, not AParamMode, False);
  LFirstArgIndex := 2 - Ord(AParamMode);
  case LMetaType.F.Kind of
    mtClass:
    begin
      LUserData.Instance := Pointer(PLuaClassInfo(LMetaType).F.ClassType.NewInstance);
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
      if (LArgsCount = 1) and (lua_type(Handle, -1) = LUA_TTABLE) and
        (InternalTableToMetaType(-1) = nil) then
      begin
        __InitTableValues(LUserData, -1);
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
      __ExecuteSetMethod(LUserData, LFirstArgIndex, LArgsCount, 3{constructor});
    end;
  end;

done:
  Result := 1;
end;

function TLua.__operator(const AMetaType: __luapointer; const AKind: Cardinal{Byte}): Integer;
label
  instance_destroyed, failure_operation, push_compare, done;
const
  RECORD_OPERATORS: array[OPERATOR_NEG..OPERATOR_LESS_EQUAL] of TLuaOperator = (
    loNeg, loAdd, loSub, loMul, loDiv, loMod, loPow, loCompare, loCompare, loCompare);
  OPERATION_NAMES: array[TLuaOperator] of string = ('neg', '+', '-', '*', '/', '%', '^', 'compare');
var
  LMetaType: PLuaMetaType;
  LInverted: Boolean;
  LCompare: Integer;
  LLeftLuaType, LRightLuaType, LTempLuaType: Integer;
  LLeftUserData, LRightUserData, LTempUserData, LTargetUserData: PLuaUserData;
  LRecordOperator: TLuaOperator;
  LRecordOperatorCallback: TLuaOperatorCallback;
begin
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(AMetaType);

  // check operator arguments: user data
  LLeftLuaType := lua_type(Handle, 1);
  LRightLuaType := LUA_TNONE;
  if (AKind <> OPERATOR_NEG) then LRightLuaType := lua_type(Handle, 2);
  LLeftUserData := nil;
  LRightUserData := nil;
  if (LLeftLuaType = LUA_TUSERDATA) then
  begin
    LLeftUserData := lua_touserdata(Handle, 1);
    if (not Assigned(LLeftUserData)) or (not Assigned(LLeftUserData.Instance)) then
    begin
      LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LLeftUserData.MetaType);
    instance_destroyed:
      // __GetUserData like error
      InternalErrorFmt('%s instance is already destroyed', [LMetaType.FName]);
      goto done;
    end else
    begin
      if (LLeftUserData.MetaType <> LMetaType.Ptr) then
      begin
      failure_operation:
        stack_force_unicode_string(FStringBuffer.Unicode, 1);
        if (LRightLuaType = LUA_TNONE) then
        begin
          InternalErrorFmt('Failure operation "%s" with "%s" operand', [
            OPERATION_NAMES[RECORD_OPERATORS[AKind]], FStringBuffer.Unicode]);
        end else
        begin
          stack_force_unicode_string(FStringBuffer.UnicodeReserved, 2);
          InternalErrorFmt('Failure operation "%s" with "%s" and "%s" operands', [
            OPERATION_NAMES[RECORD_OPERATORS[AKind]], FStringBuffer.Unicode, FStringBuffer.UnicodeReserved]);
        end;
        goto done;
      end;
    end;
  end;
  if (LRightLuaType = LUA_TUSERDATA) then
  begin
    LRightUserData := lua_touserdata(Handle, 2);
    if (not Assigned(LRightUserData)) or (not Assigned(LRightUserData.Instance)) then
    begin
      LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LRightUserData.MetaType);
      goto instance_destroyed;
    end else
    begin
      if (LRightUserData.MetaType <> LMetaType.Ptr) then
        goto failure_operation;
    end;
  end;

  // check operator arguments: compatibility
  LInverted := (not Assigned(LLeftUserData));
  if (LInverted) then
  begin
    LTempLuaType := LLeftLuaType;
    {.$HINTS OFF LeftLuaType := RightLuaType; }
    LRightLuaType := LTempLuaType;
    LTempUserData := LLeftUserData;
    LLeftUserData := LRightUserData;
    LRightUserData := LTempUserData;

    if (not (AKind in [OPERATOR_DIV..OPERATOR_POW])) then
      goto failure_operation;
  end;

  case LMetaType.F.Kind of
    mtRecord:
    begin
      LRecordOperator := RECORD_OPERATORS[AKind];
      LRecordOperatorCallback := PLuaRecordInfo(LMetaType).FOperatorCallback;
      if (LRecordOperator in PLuaRecordInfo(LMetaType).FOperators) and Assigned(LRecordOperatorCallback) then
      begin
        if (AKind = OPERATOR_NEG) then
        begin
          LTargetUserData := push_metatype_instance(LMetaType, nil^, True, False);
          LRecordOperatorCallback(LTargetUserData.Instance^, LLeftUserData.Instance^,
            LLeftUserData.Instance^, loNeg);
        end else
        case LRightLuaType of
          LUA_TUSERDATA:
          begin
            case (AKind) of
              OPERATOR_ADD, OPERATOR_SUB:
              begin
                LTargetUserData := push_metatype_instance(LMetaType, nil^, True, False);
                LRecordOperatorCallback(LTargetUserData.Instance^, LLeftUserData.Instance^,
                  LRightUserData.Instance^, LRecordOperator);
              end;
              OPERATOR_EQUAL..OPERATOR_LESS_EQUAL:
              begin
                //compare
                LRecordOperatorCallback(LCompare, LLeftUserData.Instance^,
                  LRightUserData.Instance^, LRecordOperator);
                goto push_compare;
              end;
            else
              goto failure_operation;
            end;
          end;
          LUA_TNUMBER:
          begin
            if (not (AKind in [OPERATOR_MUL..OPERATOR_POW])) then goto failure_operation;
            FTempBuffer.D := lua_tonumber(Handle, 2 - Ord(LInverted));
            LTargetUserData := push_metatype_instance(LMetaType, nil^, True, False);
            LRecordOperatorCallback(LTargetUserData.Instance^, LLeftUserData.Instance^,
              FTempBuffer.D, LRecordOperator);
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
      if (AKind = OPERATOR_NEG) then
      begin
        LTargetUserData := push_metatype_instance(LMetaType, nil^, True, False);
        SetInvert(LTargetUserData.Instance, LLeftUserData.Instance, PLuaSetInfo(LMetaType).FAndMasks, PLuaSetInfo(LMetaType).FRealSize);
      end else
      case LRightLuaType of
        LUA_TUSERDATA:
        begin
          if (RECORD_OPERATORS[AKind] = loCompare) then
          begin
            LCompare := SetsCompare(LLeftUserData.Instance, LRightUserData.Instance, PLuaSetInfo(LMetaType).Size, AKind = OPERATOR_LESS_EQUAL);
          push_compare:
            if (LInverted) then LCompare := -LCompare;
            case (AKind) of
              OPERATOR_EQUAL: lua_pushboolean(Handle, (LCompare = 0));
              OPERATOR_LESS: lua_pushboolean(Handle, (LCompare < 0));
              OPERATOR_LESS_EQUAL: lua_pushboolean(Handle, (LCompare <= 0));
            end;
          end else
          begin
            LTargetUserData := push_metatype_instance(LMetaType, nil^, True, False);
            case (AKind) of
              OPERATOR_ADD: SetsUnion(LTargetUserData.Instance, LLeftUserData.Instance, LRightUserData.Instance, LMetaType.Size);
              OPERATOR_SUB: SetsDifference(LTargetUserData.Instance, LLeftUserData.Instance, LRightUserData.Instance, LMetaType.Size);
              OPERATOR_MUL: SetsIntersection(LTargetUserData.Instance, LLeftUserData.Instance, LRightUserData.Instance, LMetaType.Size);
            end;
          end;
        end;
        LUA_TNUMBER:
        begin
          FTempBuffer.D := lua_tonumber(Handle, 2 - Ord(LInverted));
          if (not NumberToInteger(FTempBuffer.D, FTempBuffer.I)) or
            (FTempBuffer.I < PLuaSetInfo(LMetaType).FLow) or (FTempBuffer.I > PLuaSetInfo(LMetaType).FHigh) then
            goto failure_operation;

          Dec(FTempBuffer.I, PLuaSetInfo(LMetaType).FCorrection);
          LTargetUserData := push_metatype_instance(LMetaType, nil^, True, False);
          case (AKind) of
            OPERATOR_ADD: SetsUnion(LTargetUserData.Instance, LLeftUserData.Instance, FTempBuffer.I, LMetaType.Size);
            OPERATOR_SUB: SetsDifference(LTargetUserData.Instance, LLeftUserData.Instance, FTempBuffer.I, LMetaType.Size, LInverted);
            OPERATOR_MUL: SetsIntersection(LTargetUserData.Instance, LLeftUserData.Instance, FTempBuffer.I, LMetaType.Size);
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

function TLua.push_standard(const AMetaType: PLuaMetaType; const AStdIndex: Cardinal): Boolean;
label
  ret_false, is_filled_zero, done;
var
  LPtr: __luapointer;
  LArrayIndex, LCount: Integer;
  LUserData: PLuaUserData;

  procedure ErrorConst;
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    ErrorFmt('%s() method can''t be called, because %s instance is a constant', [FStringBuffer.Lua, AMetaType.Name]);
  end;
begin
  LUserData := __GetUserData(AMetaType.Ptr, Cardinal(AStdIndex - STD_IS_EMPTY) > 1{STD_IS_EMPTY/STD_FREE});

  case (AStdIndex) of
    STD_TYPE:
    begin
      global_push_value(AMetaType.Ref);
      goto done;
    end;
    STD_TYPE_NAME:
    begin
      push_lua_string(AMetaType.FName);
      goto done;
    end;
    STD_TYPE_PARENT:
    begin
      LPtr := LUA_POINTER_INVALID;
      case AMetaType.F.Kind of
        mtClass:
        begin
          LPtr := PLuaClassInfo(AMetaType).Parent;
        end;
        mtInterface:
        begin
          LPtr := PLuaInterfaceInfo(AMetaType).Parent;
        end;
      end;

      if (LPtr = LUA_POINTER_INVALID) then
      begin
        lua_pushnil(Handle);
      end else
      begin
        LPtr := PLuaMetaType({$ifdef LARGEINT}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr)).Ptr;
        global_push_value(LPtr);
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
      if (Assigned(LUserData)) then
      begin
        if (AMetaType.F.Kind <> mtClass) and (LUserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_ASSIGN), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__Assign, 1 + (1 shl 16));
        goto done;
      end;
    end;
    STD_IS_REF:
    begin
      lua_pushboolean(Handle, (Assigned(LUserData)) and
        (LUserData.Kind <> mtClass) and (not LUserData.Owner));
      goto done;
    end;
    STD_IS_CONST:
    begin
      lua_pushboolean(Handle, (Assigned(LUserData)) and (LUserData.Constant));
      goto done;
    end;
    STD_IS_CLASS:
    begin
      lua_pushboolean(Handle, AMetaType.F.Kind = mtClass);
      goto done;
    end;
    STD_IS_INTERFACE:
    begin
      lua_pushboolean(Handle, AMetaType.F.Kind = mtInterface);
      goto done;
    end;
    STD_IS_RECORD:
    begin
      lua_pushboolean(Handle, AMetaType.F.Kind = mtRecord);
      goto done;
    end;
    STD_IS_SET:
    begin
      lua_pushboolean(Handle, AMetaType.F.Kind = mtSet);
      goto done;
    end;
    STD_IS_EMPTY:
    begin
      if (not Assigned(LUserData)) or (LUserData.Instance = nil) then
      begin
        lua_pushboolean(Handle, True);
      end else
      case AMetaType.F.Kind of
        mtClass, mtInterface:
        begin
        ret_false:
          lua_pushboolean(Handle, False);
        end;
        mtArray:
        begin
          if (not PLuaArrayInfo(AMetaType).IsDynamic) then goto is_filled_zero;
          lua_pushboolean(Handle, PPointer(LUserData.Instance)^ = nil);
        end
      else
      is_filled_zero:
        lua_pushboolean(Handle, IsMemoryFilledZero(LUserData.Instance, AMetaType.F.Size));
      end;

      goto done;
    end;
    STD_FREE:
    begin
      if (Assigned(LUserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_FREE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__Free, 0 + (0 shl 16));
        goto done;
      end;
    end;
    STD_CREATE:
    begin
      if (not Assigned(LUserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_CREATE), Ord(ckClassInstance), Ord(cmInternal),
        //  MetaType, @TLua.__ClassCreate, {???} 0 + ($ffff shl 16));
        goto done;
      end;
    end;
    STD_ADDREF:
    begin
      if (Assigned(LUserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_ADDREF), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__IntfAddRef, 0 + (0 shl 16));
        goto done;
      end;
    end;
    STD_RELEASE:
    begin
      if (Assigned(LUserData)) then
      begin
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_RELEASE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__IntfRelease, 0 + (0 shl 16));
        goto done;
      end;
    end;
    STD_LENGTH:
    begin
      with PLuaArrayInfo(AMetaType)^ do
      if (IsDynamic) then
      begin
        LCount := 0;
        if (Assigned(LUserData)) then LCount := DynArrayLength(PPointer(LUserData.Instance)^);
      end else
      begin
        LArrayIndex := 0;
        if (Assigned(LUserData)) then LArrayIndex := (LUserData.Counts and $F) * 2;
        LCount := Integer(PMemoryItems(FBounds).Cardinals[LArrayIndex + 1]) -
          Integer(PMemoryItems(FBounds).Cardinals[LArrayIndex]) + 1;
      end;

      lua_pushinteger(Handle, LCount);
      goto done;
    end;
    STD_RESIZE:
    begin
      if (Assigned(LUserData)) and (PLuaArrayInfo(AMetaType).IsDynamic) then
      begin
        if (LUserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_RESIZE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__DynArrayResize, 1 + (PLuaArrayInfo(MetaType).Dimention shl 16));
        goto done;
      end;
    end;
    STD_LOW:
    begin
      if (AMetaType.F.Kind = mtArray) then
      begin
        with PLuaArrayInfo(AMetaType)^ do
        begin
          LCount := 0;
          if (not IsDynamic) then
          begin
            if (not Assigned(LUserData)) then
            begin
              LCount := FBounds^;
            end else
            begin
              LArrayIndex := (LUserData.Counts and $F) * 2;
              LCount := Integer(PMemoryItems(FBounds).Cardinals[LArrayIndex]);
            end;
          end;

          lua_pushinteger(Handle, LCount);
          goto done;
        end;
      end else
      begin
        // mtSet
        lua_pushinteger(Handle, PLuaSetInfo(AMetaType).FLow);
        goto done;
      end;
    end;
    STD_HIGH:
    begin
      if (AMetaType.F.Kind = mtArray) then
      begin
        with PLuaArrayInfo(AMetaType)^ do
        begin
          if (IsDynamic) then
          begin
            LCount := -1;
            if (Assigned(LUserData)) then LCount := DynArrayLength(PPointer(LUserData.Instance)^) - 1;
          end else
          begin
            LArrayIndex := 0;
            if (Assigned(LUserData)) then
            begin
              LArrayIndex := (LUserData.Counts and $F) * 2;
            end;

            Inc(LArrayIndex);
            LCount := Integer(PMemoryItems(FBounds).Cardinals[LArrayIndex]);
          end;

          lua_pushinteger(Handle, LCount);
          goto done;
        end;
      end else
      begin
        // mtSet
        lua_pushinteger(Handle, PLuaSetInfo(AMetaType).FHigh);
        goto done;
      end;
    end;
    STD_INCLUDE:
    begin
      if (Assigned(LUserData)) then
      begin
        if (AMetaType.F.Kind = mtArray) then
        begin
          if (LUserData.Counts and $F + 1 = LUserData.Counts shr 4) then
          begin
            if (LUserData.Constant) then ErrorConst;
            {ToDo} lua_pushnil(FHandle);
            //push_closure(Pointer(@ID_INCLUDE), Ord(ckInstance), Ord(cmInternal),
            //  UserData, @TLua.__DynArrayInclude, 1 + ($ffff shl 16));
            goto done;
          end;
        end else
        begin
          // mtSet
          if (LUserData.Constant) then ErrorConst;
          {ToDo} lua_pushnil(FHandle);
          //push_closure(Pointer(@ID_INCLUDE), Ord(ckInstance), Ord(cmInternal),
          //  UserData, @TLua.__SetInclude, 1 + ($ffff shl 16));
          goto done;
        end;
      end;
    end;
    STD_EXCLUDE:
    begin
      if (Assigned(LUserData)) then
      begin
        if (LUserData.Constant) then ErrorConst;
        {ToDo} lua_pushnil(FHandle);
        //push_closure(Pointer(@ID_EXCLUDE), Ord(ckInstance), Ord(cmInternal),
        //  UserData, @TLua.__SetExclude, 1 + ($ffff shl 16));
        goto done;
      end;
    end;
    STD_CONTAINS:
    begin
      if (Assigned(LUserData)) then
      begin
        if (LUserData.Constant) then ErrorConst;
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

function TLua.set_standard(const AMetaType: PLuaMetaType; const AStdIndex: Cardinal): Boolean;
label
  invalid_array_length, done;
var
  LUserData: PLuaUserData;

  procedure ErrorConst;
  begin
    stack_lua_string(FStringBuffer.Lua, 2);
    ErrorFmt('%s can''t be called, because %s instance is a constant', [FStringBuffer.Lua, AMetaType.Name]);
  end;
begin
  case (AStdIndex) of
    STD_LENGTH:
    begin
      if (AMetaType.F.Kind = mtArray) and (PLuaArrayInfo(AMetaType).FIsDynamic) then
      begin
        LUserData := __GetUserData(AMetaType.Ptr);
        if (Assigned(LUserData)) then
        begin
          if (not LUserData.Constant) then
          begin
            if (lua_type(Handle, 3) = LUA_TNUMBER) then
            begin
              FTempBuffer.D := lua_tonumber(Handle, 3);
              if (not NumberToInteger(FTempBuffer.D, FTempBuffer.I)) or
               (FTempBuffer.I < 0) then goto invalid_array_length;

              {$ifdef LARGEINT}
              TPoint(FTempBuffer.I64).Y := 0;
              {$endif}
              DynArraySetLength(PPointer(LUserData.Instance)^,
                PTypeInfo(PLuaArrayInfo(AMetaType).FMultiplies[LUserData.Counts and $F]),
                1, @FTempBuffer.N);
              goto done;
            end else
            begin
            invalid_array_length:
              stack_force_unicode_string(FStringBuffer.Unicode, 3);
              ErrorFmt('Invalid %s.Length value "%s"', [AMetaType.Name, FStringBuffer.Unicode]);
            end;
          end else
          begin
            ErrorFmt('Length can''t be changed, because %s instance is a constant', [AMetaType.Name]);
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
          VarClear(FTempBuffer.V);
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
              Self.ErrorFmt('%s.%s method is not a class method', [LMetaType.FName, FStringBuffer.Lua]);
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
    ErrorFmt('Can not change %s.%s %s to %s ("%s")', [LMetaType.FName,
      FStringBuffer.Lua, PROPERTY_MODES[Ord(LMetaType.F.Kind = mtRecord)],
      FStringBuffer.Default, FStringBuffer.Unicode]);
  end;

done:
  Result := 0;
end;

function TLua.__global_index(const AModifyLow, AModifyHigh: Cardinal): Integer;
label
  unsupported;
var
  LNativeModify: ^TLuaGlobalModify;
  LEntity: PLuaGlobalEntity;
  LLuaType: Integer;
  LLuaName: __luaname;
  LEntityItem: ^TLuaDictionaryItem;
  LName: PLuaString;
  LMetaType: ^TLuaMetaType;
  LProp: ^TLuaProperty;
  LProc: ^TLuaNamespaceMethod;
begin
  NativeUInt(LNativeModify) := {$ifdef LARGEINT}(NativeUInt(AModifyHigh) shl 32) +{$endif} AModifyLow;

  // entity               0
  if (Assigned(LNativeModify)) then
  begin
    LEntity := GetGlobalEntity(LNativeModify.Name^, False);
  end else
  begin
    LLuaType := lua_type(Handle, 2);
    if (LLuaType <> LUA_TSTRING) then
    begin
      GetLuaTypeName(FStringBuffer.Default, LLuaType);
      ErrorFmt('Global key should be a string. Type %s is not available as a global key', [FStringBuffer.Default]);
    end;
    LLuaName := lua_tolstring(Handle, 2, nil);
    LEntityItem := TLuaDictionary(FGlobalEntities).InternalFind(LLuaName, False);

    LEntity := nil;
    if (Assigned(LEntityItem)) then
    begin
      LEntity := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntityItem.Value);
    end;
  end;
  if (not Assigned(LEntity)) then
  begin
    if (Assigned(LNativeModify)) then
    begin
      LName := LNativeModify.Name;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      LName := @FStringBuffer.Lua;
    end;

    InternalErrorFmt('Global variable "%s" not found', [LName^]);
  end;

  // push entity value
  case LEntity.Kind of
    gkMetaType:
    begin
      LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
      global_push_value(LMetaType.Ref);
    end;
    gkVariable:
    begin
      LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
      LProp.GetValue(Pointer(LProp.Getter)^, Self);
    end;
    gkProc:
    begin
      LProc := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
      push_luafunction(LProc.Func);
    end;
  else
    // gkConst, gkScriptVariable
    global_push_value(LEntity.Ref);
  end;

  if (Assigned(LNativeModify)) then
  begin
    if (LNativeModify.VariantMode) then
    begin
      if (not stack_variant(LNativeModify.V^, -1, False)) then goto unsupported;
    end else
    begin
      if (not stack_luaarg(LNativeModify.Arg^, -1, False)) then
      begin
      unsupported:
        stack_pop;
        raise ELua.CreateFmt('Can''t get global variable "%s" of type "%s"', [LNativeModify.Name^,
          FStringBuffer.Default]) at FReturnAddress;
      end;
    end;
    stack_pop;
  end;

  Result := 1;
end;

function TLua.__global_newindex(const AModifyLow, AModifyHigh: Cardinal): Integer;
label
  script_variable, unsupported;
var
  LNativeModify: ^TLuaGlobalModify;
  LEntity: PLuaGlobalEntity;
  LLuaType: Integer;
  LLuaName: __luaname;
  LEntityItem: ^TLuaDictionaryItem;
  LPtr: __luapointer;
  LName: PLuaString;
  LProp: ^TLuaProperty;
  LDone: Boolean;
begin
  NativeUInt(LNativeModify) := {$ifdef LARGEINT}(NativeUInt(AModifyHigh) shl 32) +{$endif} AModifyLow;

  // entity
  if (Assigned(LNativeModify)) then
  begin
    LEntity := GetGlobalEntity(LNativeModify.Name^, True);
  end else
  begin
    LLuaType := lua_type(Handle, 2);
    if (LLuaType <> LUA_TSTRING) then
    begin
      GetLuaTypeName(FStringBuffer.Default, LLuaType);
      ErrorFmt('Global key should be a string. Type %s is not available as a global key', [FStringBuffer.Default]);
    end;
    LLuaName := lua_tolstring(Handle, 2, nil);
    LEntityItem := TLuaDictionary(FGlobalEntities).InternalFind(LLuaName, True);

    LPtr := LEntityItem.Value;
    if (LPtr = LUA_POINTER_INVALID) then
    begin
      lua_pushvalue(Handle, 2);
      global_fill_value(global_alloc_ref);
      LPtr := TLuaMemoryHeap(FMemoryHeap).Alloc(SizeOf(TLuaGlobalEntity));
      LEntityItem.Value := LPtr;
      LEntity := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
      PInteger(LEntity)^ := 0;
      PLuaGlobalEntity(LEntity).Ptr := LUA_POINTER_INVALID;
    end else
    begin
      LEntity := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LPtr);
    end;
  end;

  if (LEntity.Ptr = LUA_POINTER_INVALID) then
  begin
    // new item
    LEntity.Kind := gkScriptVariable;
    LEntity.Ref := global_alloc_ref;
    goto script_variable;
  end else
  if (LEntity.Constant) then
  begin
    if (Assigned(LNativeModify)) then
    begin
      LName := LNativeModify.Name;
    end else
    begin
      stack_lua_string(FStringBuffer.Lua, 2);
      LName := @FStringBuffer.Lua;
    end;
    InternalErrorFmt('Global const "%s" can not be changed', [LName^]);
  end else
  if (LEntity.Kind = gkScriptVariable) then
  begin
  script_variable:
    if (LNativeModify = nil) then
    begin
      lua_pushvalue(Handle, 3);
    end else
    if (LNativeModify.VariantMode) then
    begin
      if (not push_variant(LNativeModify.V^)) then goto unsupported;
    end else
    begin
      if (not push_luaarg(LNativeModify.Arg^)) then
      begin
      unsupported:
        raise ELua.CreateFmt('Can''t set global variable "%s" of type "%s"', [LNativeModify.Name^,
          FStringBuffer.Default]) at FReturnAddress;
      end;
    end;
    global_fill_value(LEntity.Ref);
  end else
  begin
    // gkVariable
    // optional push native value
    if (Assigned(LNativeModify)) then
    begin
      if (LNativeModify.VariantMode) then
      begin
        if (not push_variant(LNativeModify.V^)) then goto unsupported;
      end else
      begin
        if (not push_luaarg(LNativeModify.Arg^)) then goto unsupported;
      end;
    end;

    // set property value, optional pop native value
    LProp := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LEntity.Ptr);
    LDone := LProp.SetValue(Pointer(LProp.Setter)^, Self, -1);
    if (Assigned(LNativeModify)) then stack_pop;

    // check error
    if (not LDone) then
    begin
      if (Assigned(LNativeModify)) then
      begin
        LName := LNativeModify.Name;
      end else
      begin
        stack_lua_string(FStringBuffer.Lua, 2);
        LName := @FStringBuffer.Lua;
      end;
      InternalErrorFmt('Can not change global variable "%s" to %s ("%s")',
        [LName^, FStringBuffer.Default, FStringBuffer.Unicode]);
    end;
  end;

  Result := 0;
end;

function TLua.__InheritsFrom(const AMetaType: PLuaMetaType; const AArgsCount: Integer{ = 1}): Integer;
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
    ErrorFmt('Invalid meta type value "%s"', [FStringBuffer.Unicode]);
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

function TLua.__Assign(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer{ = 1}): Integer;
label
  invalid_argument, done;
var
  LLuaType: Integer;
  LUserData, LValueUserData: PLuaUserData;
  LMetaType, LValueMetaType: PLuaMetaType;

  procedure ErrorParameter(const AMetaType: PLuaMetaType; const ADescription: string);
  begin
    stack_force_unicode_string(FStringBuffer.Unicode, 1);
    ErrorFmt('Failure %s.Assign(%s): %s.', [AMetaType.Name, FStringBuffer.Unicode, ADescription]);
  end;
begin
  LLuaType := lua_type(Handle, 1);
  LUserData := AUserData;
  LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LUserData.MetaType);

  if (LLuaType <> LUA_TUSERDATA) then
  begin
    if (LLuaType = LUA_TTABLE) and (InternalTableToMetaType(1) = nil) then
    begin
      __InitTableValues(LUserData, 1);
      goto done;
    end else
    begin
      goto invalid_argument;
    end;
  end else
  begin
    LValueUserData := lua_touserdata(Handle, 1);
    if (LValueUserData.Kind = mtComplexProperty) then goto invalid_argument;
    if (not Assigned(LValueUserData.Instance)) then
    begin
      ErrorParameter(LMetaType, 'instance is already destroyed');
      goto done;
    end;

    LValueMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LValueUserData.MetaType);
    if (LMetaType.F.Kind <> LValueMetaType.F.Kind) then goto invalid_argument;
  end;

  // check compatibility
  if (LMetaType <> LValueMetaType) then
  case LMetaType.F.Kind of
    mtClass:
    begin
      if (not LMetaType.F.ClassType.InheritsFrom(LValueMetaType.F.ClassType)) and
        (not LValueMetaType.F.ClassType.InheritsFrom(LMetaType.F.ClassType)) then
      begin
        goto invalid_argument;
      end;
    end;
    mtInterface:
    begin
      if (not PLuaInterfaceInfo(LValueMetaType).InheritsFrom(PLuaInterfaceInfo(LMetaType))) then
        goto invalid_argument;
    end;
  else
  invalid_argument:
    ErrorParameter(LMetaType, 'invalid argument');
    goto done;
  end;

  // assign
  LMetaType.Assign(LUserData.Instance^, LValueUserData.Instance);

done:
  Result := 0;
end;

function TLua.__Free(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
var
  LUserData: PLuaUserData;
  LMetaType: PLuaMetaType;
  LInstance: Pointer;
begin
  LUserData := AUserData;
  LInstance := LUserData.Instance;
  LUserData.Instance := nil;
  if (Assigned(LInstance)) then
  begin
    LMetaType := {$ifdef SMALLINT}Pointer{$else}TLuaMemoryHeap(FMemoryHeap).Unpack{$endif}(LUserData.MetaType);
    LMetaType.Dispose(LInstance);
  end;

  Result := 0;
end;

function TLua.__ClassCreate(const AMetaType: PLuaMetaType; const AArgsCount: Integer): Integer;
begin
  Result := __call(AMetaType.Ptr, True);
end;

function TLua.__IntfAddRef(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
var
  LUserData: PLuaUserData;
  LCount: Integer;
begin
  LUserData := AUserData;
  LCount := IInterface(LUserData.Instance)._AddRef;
  lua_pushinteger(Handle, LCount);
  Result := 1;
end;

function TLua.__IntfRelease(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
var
  LUserData: PLuaUserData;
  LCount: Integer;
begin
  LUserData := AUserData;
  LCount := IInterface(LUserData.Instance)._Release;
  if (LCount = 0) then
  begin
    LUserData.Instance := nil;
  end;
  lua_pushinteger(Handle, LCount);
  Result := 1;
end;

function TLua.__DynArrayInclude(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
begin
  Result := 0{ToDo};
end;

function TLua.__DynArrayResize(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
begin
  Result := 0{ToDo};
end;

function TLua.__SetInclude(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
begin
  Result := __ExecuteSetMethod(AUserData, 1, AArgsCount, 0);
end;

function TLua.__SetExclude(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
begin
  Result := __ExecuteSetMethod(AUserData, 1, AArgsCount, 1);
end;

function TLua.__SetContains(const AUserData: Pointer{PLuaUserData}; const AArgsCount: Integer): Integer;
begin
  Result := __ExecuteSetMethod(AUserData, 1, AArgsCount, 2);
end;

function TLua.UniversalMethodCallback(const AMethod: Pointer{PLuaMethod}; const AArgsCount: Integer): Integer;
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
          {FStringBuffer.Lua :=} LMethod.UnpackName(Self);
          unpack_lua_string(FStringBuffer.LuaReserved, LParam.Name^);
          Self.ErrorFmt('Invalid %s.%s argument value: "%s" (%s)',
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
  Self.ErrorFmt('Invoked %s "%s" signature not found',
    [
      METHOD_KINDS[LMethod.Kind], LName^
    ]);
  goto invalid_result;
invalid_args_count:
  LName := LMethod.UnpackName(Self);
  if (LMinArgsCount = LMaxArgsCount) then
  begin
    Self.ErrorFmt('Invalid arguments count (%d) of %s "%s", reqired: %d',
      [
        LArgsCount, METHOD_KINDS[LMethod.Kind], LName^, {Min/}LMaxArgsCount
      ]);
  end else
  if (LMaxArgsCount = High(Word)) then
  begin
    Self.ErrorFmt('Invalid arguments count (%d) of %s "%s", reqired: %d..inf',
      [
        LArgsCount, METHOD_KINDS[LMethod.Kind], LName^, LMinArgsCount
      ]);
  end else
  begin
    Self.ErrorFmt('Invalid arguments count (%d) of %s "%s", reqired: %d..%d',
      [
        LArgsCount, METHOD_KINDS[LMethod.Kind], LName^, LMinArgsCount, LMaxArgsCount
      ]);
  end;

invalid_result:
  Result := 0;
end;

procedure TLua.RegTypeName(const ATypeName: LuaString; const ATypeInfo: PTypeInfo;
  const APointerDepth: Integer);
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Self.InternalRegTypeName(ATypeName, ATypeInfo, APointerDepth);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalRegTypeName
end;
{$endif}

procedure TLua.RegClass(const AClass: TClass; const AUseRtti: Boolean);
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Self.InternalAddClass(AClass, AUseRtti, True);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  push [esp]
  mov [esp + 4], 1
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  mov r9d, 1
  {$endif}
  jmp TLua.InternalAddClass
end;
{$endif}

procedure __TLuaRegClasses(const ASelf: TLua; const AClasses: array of TClass; const AUseRtti: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(AClasses) do
  ASelf.InternalAddClass(AClasses[i], AUseRtti, True);
end;

procedure TLua.RegClasses(const AClasses: array of TClass; const AUseRtti: Boolean);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegClasses(Self, AClasses, AUseRtti);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegClasses
end;
{$endif}

function TLua.RegRecord(const ATypeInfo: PTypeInfo): PLuaRecordInfo;
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Result := InternalAddRecord(ATypeInfo);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddRecord
end;
{$endif}

function TLua.RegRecord(const AName: LuaString; const ATypeInfo: PTypeInfo; const AUseRtti: Boolean): PLuaRecordInfo;
{$ifNdef CPUINTEL}
begin
  Self.FReturnAddress := ReturnAddress;
  Self.FFrames := nil;
  Result := InternalAddRecordEx(AName, ATypeInfo, AUseRtti);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddRecordEx
end;
{$endif}

function TLua.RegArray(const ATypeInfo: PTypeInfo): PLuaArrayInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  Result := InternalAddArray(ATypeInfo);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddArray
end;
{$endif}

function TLua.RegArray(const AIdentifier: Pointer; const AItemTypeInfo: PTypeInfo; const ABounds: array of Integer): PLuaArrayInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  Result := InternalAddArrayEx(AIdentifier, AItemTypeInfo, ABounds);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp TLua.InternalAddArrayEx
end;
{$endif}

function TLua.RegSet(const ATypeInfo: PTypeInfo): PLuaSetInfo;
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  Result := InternalAddSet(ATypeInfo, True);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  mov ecx, 1
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  mov r8d, 1
  {$endif}
  jmp TLua.InternalAddSet
end;
{$endif}

procedure __TLuaRegUniversalProc(const ASelf: TLua; const AProcName: LuaString;
  const AProc: TLuaProcCallback; const AMinArgsCount, AMaxArgsCount: Word);
begin
  ASelf.InternalAddMethod(nil, AProcName, @AProc, mkStatic, LUA_POINTER_INVALID, True, AMinArgsCount, AMaxArgsCount);
end;

procedure TLua.RegProc(const AProcName: LuaString; const ACallback: TLuaProcCallback;
  const AMinArgsCount, AMaxArgsCount: Word);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegUniversalProc(Self, AProcName, ACallback, AMinArgsCount, AMaxArgsCount);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegUniversalProc
end;
{$endif}

procedure __TLuaRegRttiProc(const ASelf: TLua; const AProcName: LuaString;
  const AAddress: Pointer; const ATypeInfo: PTypeInfo);
var
  LInvokable: __luapointer;
begin
  LInvokable := ASelf.InternalBuildInvokable(nil, ATypeInfo, mkStatic, True);
  if (LInvokable <> LUA_POINTER_INVALID) then
  begin
    ASelf.InternalAddMethod(nil, AProcName, AAddress, mkStatic, LInvokable, True);
  end;
end;

procedure TLua.RegProc(const AProcName: LuaString; const AAddress: Pointer; const ATypeInfo: PTypeInfo);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegRttiProc(Self, AProcName, AAddress, ATypeInfo);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegRttiProc
end;
{$endif}

procedure __TLuaRegCustomProc(const ASelf: TLua; const AProcName: LuaString;
  const AAddress: Pointer; const AParams: array of TLuaProcParam;
  const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean; const ACallConv: TCallConv);
var
  LInvokable: __luapointer;
begin
  LInvokable := ASelf.InternalBuildInvokable(nil, AProcName, AParams,
    AResultType, AIsResultUnsafe, mkStatic, ACallConv, True);
  if (LInvokable <> LUA_POINTER_INVALID) then
  begin
    ASelf.InternalAddMethod(nil, AProcName, AAddress, mkStatic, LInvokable, True);
  end;
end;

procedure TLua.RegProc(const AProcName: LuaString;
  const AAddress: Pointer; const AParams: array of TLuaProcParam;
  const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean; const ACallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegCustomProc(Self, AProcName, AAddress, AParams, AResultType, AIsResultUnsafe, ACallConv);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegCustomProc
end;
{$endif}

procedure __TLuaRegUniversalClassMethod(const ASelf: TLua; const AClass: TClass;
  const AMethodName: LuaString; const AMethod: TLuaMethodCallback;
  const AMinArgsCount, AMaxArgsCount: Word; const AMethodKind: TLuaMethodKind);
begin
  ASelf.InternalAddMethod(ASelf.InternalAddClass(AClass, False, True), AMethodName, @AMethod,
    AMethodKind, LUA_POINTER_INVALID, True, AMinArgsCount, AMaxArgsCount);
end;

procedure TLua.RegMethod(const AClass: TClass; const AMethodName: LuaString; const ACallback: TLuaMethodCallback;
  const AMinArgsCount, AMaxArgsCount: Word; const AMethodKind: TLuaMethodKind);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegUniversalClassMethod(Self, AClass, AMethodName, ACallback, AMinArgsCount, AMaxArgsCount, AMethodKind);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegUniversalClassMethod
end;
{$endif}

procedure __TLuaRegRttiClassMethod(const ASelf: TLua; const AClass: TClass;
  const AMethodName: LuaString; const AAddress: Pointer; const ATypeInfo: PTypeInfo;
  const AMethodKind: TLuaMethodKind);
var
  LClassInfo: PLuaClassInfo;
  LInvokable: __luapointer;
begin
  LClassInfo := ASelf.InternalAddClass(AClass, False, True);
  LInvokable := ASelf.InternalBuildInvokable(LClassInfo, ATypeInfo, AMethodKind, True);
  if (LInvokable <> LUA_POINTER_INVALID) then
  begin
    ASelf.InternalAddMethod(LClassInfo, AMethodName, AAddress, AMethodKind, LInvokable, True);
  end;
end;

procedure TLua.RegMethod(const AClass: TClass; const AMethodName: LuaString; const AAddress: Pointer;
  const ATypeInfo: PTypeInfo; const AMethodKind: TLuaMethodKind);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegRttiClassMethod(Self, AClass, AMethodName, AAddress, ATypeInfo, AMethodKind);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegRttiClassMethod
end;
{$endif}

procedure __TLuaRegCustomClassMethod(const ASelf: TLua; const AClass: TClass; const AMethodName: LuaString;
  const AAddress: Pointer; const AParams: array of TLuaProcParam;
  const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean;
  const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv);
var
  LClassInfo: PLuaClassInfo;
  LInvokable: __luapointer;
begin
  LClassInfo := ASelf.InternalAddClass(AClass, False, True);
  LInvokable := ASelf.InternalBuildInvokable(LClassInfo, AMethodName, AParams,
    AResultType, AIsResultUnsafe, mkStatic, ACallConv, True);
  if (LInvokable <> LUA_POINTER_INVALID) then
  begin
    ASelf.InternalAddMethod(LClassInfo, AMethodName, AAddress, AMethodKind, LInvokable, True);
  end;
end;

procedure TLua.RegMethod(const AClass: TClass; const AMethodName: LuaString;
  const AAddress: Pointer; const AParams: array of TLuaProcParam;
  const AResultType: PTypeInfo; const AIsResultUnsafe: Boolean;
  const AMethodKind: TLuaMethodKind; const ACallConv: TCallConv);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegCustomClassMethod(Self, AClass, AMethodName, AAddress, AParams, AResultType, AIsResultUnsafe, AMethodKind, ACallConv);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

procedure TLua.RegVariable(const AName: LuaString; const AInstance; const AClass: TClass;
  const AIsConst: Boolean; const AReference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegVariableClass(Self, AName, AInstance, AClass, AIsConst, AReference);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
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

procedure TLua.RegVariable(const AName: LuaString; const AInstance; const ATypeInfo: PTypeInfo;
  const AIsConst: Boolean; const AReference: TLuaReference);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegVariableTypeInfo(Self, AName, AInstance, ATypeInfo, AIsConst, AReference);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  pop ebp
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegVariableTypeInfo
end;
{$endif}

procedure __TLuaRegConstVariant(const ASelf: TLua; const AName: LuaString;
  const AValue: Variant);
var
  LRef: integer;
  LLuaName: __luaname;
begin
  if (not IsValidIdent(AName)) then
    raise ELua.CreateFmt('Invalid constant name "%s"', [AName]) at ASelf.FReturnAddress;

  LLuaName := TLuaNames(ASelf.FNames).Add(AName);
  LRef := PLuaGlobalEntity(ASelf.InternalAddGlobal(Ord(gkConst), LLuaName)).Ref;
  if (not ASelf.push_variant(AValue)) then
  begin
    ASelf.stack_pop;
    raise ELua.CreateFmt('Not supported variant value (%s)', [ASelf.FStringBuffer.Default]) at ASelf.FReturnAddress;
  end else
  begin
    ASelf.global_fill_value(LRef);
  end;
end;

procedure TLua.RegConst(const AName: LuaString; const AValue: Variant);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegConstVariant(Self, AName, AValue);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegConstVariant
end;
{$endif}

procedure __TLuaRegConstLuaArg(const ASelf: TLua; const AName: LuaString;
  const AValue: TLuaArg);
var
  ARef: integer;
  ALuaName: __luaname;
begin
  if (not IsValidIdent(AName)) then
    raise ELua.CreateFmt('Invalid constant name "%s"', [AName]) at ASelf.FReturnAddress;

  ALuaName := TLuaNames(ASelf.FNames).Add(AName);
  ARef := PLuaGlobalEntity(ASelf.InternalAddGlobal(Ord(gkConst), ALuaName)).Ref;
  if (not ASelf.push_luaarg(AValue)) then
  begin
    ASelf.stack_pop;
    raise ELua.CreateFmt('Not supported argument value (%s)', [ASelf.FStringBuffer.Default]) at ASelf.FReturnAddress;
  end else
  begin
    ASelf.global_fill_value(ARef);
  end;
end;

procedure TLua.RegConst(const AName: LuaString; const AValue: TLuaArg);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  __TLuaRegConstLuaArg(Self, AName, AValue);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  {$endif}
  jmp __TLuaRegConstLuaArg
end;
{$endif}

procedure TLua.RegEnum(const ATypeInfo: PTypeInfo);
{$ifNdef CPUINTEL}
begin
  FReturnAddress := ReturnAddress;
  FFrames := nil;
  InternalAddEnum(ATypeInfo, True);
end;
{$else} {$ifdef FPC}assembler;nostackframe;{$endif}
asm
  {$ifdef CPUX86}
  push [esp]
  pop [EAX].TLua.FReturnAddress
  mov [EAX].TLua.FFrames, 0
  mov ecx, 1
  {$else .CPUX64} {$ifNdef FPC}.NOFRAME{$endif}
  push qword ptr [rsp]
  pop [RCX].TLua.FReturnAddress
  mov [RCX].TLua.FFrames, 0
  mov r8d, 1
  {$endif}
  jmp TLua.InternalAddEnum
end;
{$endif}

function TLua.GetUnit(const AIndex: Integer): TLuaUnit;
begin
  if (Cardinal(AIndex) >= Cardinal(FUnitCount)) then
    raise ELua.CreateFmt('Invalid unit index %d, unit count: %d', [AIndex, FUnitCount]);

  Result := FUnits[AIndex];
end;

function TLua.GetUnitByName(const AName: LuaString): TLuaUnit;
var
  i, LCount: Integer;
begin
  if (Pointer(AName) <> nil) then
  begin
    LCount := PInteger(NativeInt(AName) - SizeOf(Integer))^ {$ifdef LUA_LENGTH_SHIFT}shr 1{$endif};

    for i := 0 to FUnitCount - 1 do
    begin
      Result := FUnits[i];
      if (Result.FNameLength = LCount) and
        (CompareMem(Pointer(Result.FName), Pointer(AName), LCount * SizeOf(LuaChar))) then Exit;
    end;
  end;

  Result := nil;
end;


initialization
  InitUnicodeLookups;
  InitCharacterTable;

finalization
  FreeLuaLibrary;


end.
