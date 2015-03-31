unit CrystalLUA;

{******************************************************************************}
{ Copyright (c) 2010-2015 Dmitry Mozulyov                                      }
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

{ *********************************************************************** }
{                                                                         }
{ CrystalLUA is a small part of large casual game engine CrystalEngine,   }
{ that simplifies the interaction of Lua language and Delphi/FPC code.    }
{                                                                         }
{ Lua version: 5.1                                                        }
{                                                                         }
{ Copyright: Dmitry Mozulyov (aka Devil)                                  }
{ email: softforyou@inbox.ru                                              }
{ icq: 250481638                                                          }
{ *********************************************************************** }


// you can define LUA_INITIALIZE to create and destroy Lua:TLua instance automatically
//{$define LUA_INITIALIZE}

// you can choose encoding by define LUA_UNICODE or LUA_ANSI directly
// but if you ignore - it will be defined automatically by UNICODE directive case
//{$define LUA_UNICODE}
//{$define LUA_ANSI}

// you can disable Classes unit using if you want.
// it may minimize exe size for simple applications, such as a console
//{$define NO_CLASSES}


// compiler directives
{$ifdef FPC}
  {$mode Delphi}
  {$asmmode Intel}
{$endif}
{$if CompilerVersion >= 24}
  {$LEGACYIFEND ON}
{$ifend}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$if CompilerVersion >= 15}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ifend}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$if (CompilerVersion < 23) and (not Defined(FPC))}
  {$define CPUX86}
{$ifend}
{$if (Defined(FPC)) or (CompilerVersion >= 17)}
  {$define INLINESUPPORT}
{$ifend}
{$if Defined(CPUX86) or Defined(CPUX64)}
   {$define CPUINTEL}
{$ifend}
{$if SizeOf(Pointer) = 8}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$if CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ifend}
{$if (not Defined(FPC)) and (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
  {$define INTERNALCODEPAGE}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}


interface
  uses Types,
       {$ifdef MSWINDOWS}Windows,{$endif}
       {$ifdef KOL}
         KOL, err
       {$else}
         SysUtils, Classes
       {$endif},
       TypInfo;

type
  // standard types
  {$if CompilerVersion < 19}
  NativeInt = Integer;
  PNativeInt = PInteger;
  NativeUInt = Cardinal;
  PNativeUInt = PCardinal;
  {$ifend}
  {$if (not Defined(FPC)) and (CompilerVersion < 15)}
  UInt64 = Int64;
  PUInt64 = ^UInt64;
  {$ifend}
  {$if CompilerVersion < 23}
  TExtended80Rec = Extended;
  PExtended80Rec = ^TExtended80Rec;
  {$ifend}
  TBytes = array of Byte;

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
  {$ifdef LUA_UNICODE}
    {$ifdef UNICODE}
      LuaString = UnicodeString;
      PLuaString = PUnicodeString;
    {$else}
      LuaString = WideString;
      PLuaString = PWideString;
    {$endif}
    LuaChar = WideChar;
    PLuaChar = PWideChar;
  {$else}
    LuaString = AnsiString;
    PLuaString = PAnsiString;
    LuaChar = AnsiChar;
    PLuaChar = PAnsiChar;
  {$endif}

  // internal string identifier: utf8 or ansi
  __luaname = type PAnsiChar;
  // internal character pointer: utf8 or ansi
  __luadata = type PAnsiChar;
  // internal character storage: utf8 or ansi
  __luabuffer = type AnsiString;
  // internal memory offset
  __luapointer = type Integer;
  

type
  TLua = class;
  PLuaArg = ^TLuaArg;
  PLuaTable = ^TLuaTable;
  PLuaModule = ^TLuaModule;

  {$ifdef NO_CRYSTAL}
  TExcept = class(Exception)
  public
    class procedure Assert(const Message: string; const CodeAddr: pointer = nil); overload;
    class procedure Assert(const FmtStr: string; const Args: array of const; const CodeAddr: pointer = nil); overload;
  end;
  {$endif}

  // incorrect script use exception
  ELuaScript = class(ELua);

  // Lua types, that is used between script and native side
  TLuaArgType = (ltEmpty, ltBoolean, ltInteger, ltDouble, ltString, ltPointer, // <-- simple types
                 ltClass, ltObject, ltRecord, ltArray, ltSet, ltTable {<-- difficult types} );
                                              // todo ltInterface
                                              // todo Method ?

  // todo
  // базовая структура для хранения ссылок на структуры, массивы и множества (Set)
  // используется внутри TLuaArg
  __lua_difficult_type__ = object
  protected
    {$hints off}align: array[0..1] of byte; {FLuaType = ?, align[0]} {$hints on}
    FIsRef: boolean;
    FIsConst: boolean;
  public
    Data: pointer;
    { info: information type }
    property IsRef: boolean read FIsRef write FIsRef;
    property IsConst: boolean read FIsConst write FIsConst;
  end;

  // internal types information
  __TLuaType = record
    kind: integer;
    name: __luaname; 
    namespace: array[0..27] of byte; {__TLuaHashArray(__PLuaIdentifier)}
    metatable: integer; {ref}
    Lua: TLua;

  // TODO подумать!!!!!!  
  (*     // персональные функции-конструкторы/деструкторы
     __Create, __Free: pointer; //lua_CFunction
       // альтернативный (дополняющий) конструктор
       constructor_address: pointer;
       constructor_args_count: integer;
       // указатель на метод assign(arg: tluaarg)
       assign_address: pointer; *)
  end;
  __PLuaType = ^__TLuaType;


  // Record instance information: Pointer/IsRef/IsConst/RecordInfo
  PLuaRecordInfo = ^TLuaRecordInfo;
  TLuaRecord = object(__lua_difficult_type__)
  public
    Info: PLuaRecordInfo;
  end;
  PLuaRecord = ^TLuaRecord;

  // Array instance information: Pointer/IsRef/IsConst/ArrayInfo
  PLuaArrayInfo = ^TLuaArrayInfo; 
  TLuaArray = object(__lua_difficult_type__)
  public
    Info: PLuaArrayInfo;
  end;
  PLuaArray = ^TLuaArray;

  // Set instance information: Pointer/IsRef/IsConst/SetInfo
  PLuaSetInfo = ^TLuaSetInfo; 
  TLuaSet = object(__lua_difficult_type__)
  public
    Info: PLuaSetInfo;
  end;
  PLuaSet = ^TLuaSet;

  // universal CrystalLUA argument
  TLuaArg = object
  private
    str_data: string;
    FLuaType: TLuaArgType;
    {$hints off} align: array[0..2] of byte;{ <-- дополнительные данные. нужны например для TLuaTable}{$hints on} 
    Data: array[0..1] of integer;
    procedure Assert(const NeededType: TLuaArgType; const CodeAddr: pointer);

    function  GetLuaTypeName: string;    
    function  GetEmpty: boolean;
    procedure SetEmpty(const Value: boolean);
    function  GetBoolean: boolean;
    procedure SetBoolean(const Value: boolean);
    function  GetInteger: integer;
    procedure SetInteger(const Value: integer);
    function  GetDouble: double;
    procedure SetDouble(Value: double);
    function  GetString: string;
    procedure SetString(const Value: string);
    function  GetPointer: pointer;
    procedure SetPointer(const Value: pointer);
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
    property LuaType: TLuaArgType read FLuaType;
    property LuaTypeName: string read GetLuaTypeName;
    property Empty: boolean read GetEmpty write SetEmpty;
    property AsBoolean: boolean read GetBoolean write SetBoolean;
    property AsInteger: integer read GetInteger write SetInteger;
    property AsDouble: double read GetDouble write SetDouble;
    property AsString: string read GetString write SetString;
    property AsPointer: pointer read GetPointer write SetPointer;
    property AsVariant: Variant read GetVariant write SetVariant;
    property AsClass: TClass read GetClass write SetClass;
    property AsObject: TObject read GetObject write SetObject;
    property AsRecord: TLuaRecord read GetRecord write SetRecord;
    property AsArray: TLuaArray read GetArray write SetArray;
    property AsSet: TLuaSet read GetSet write SetSet;
    property AsTable: PLuaTable read GetTable;

    function ForceBoolean: boolean;
    function ForceInteger: integer;
    function ForceDouble: double;
    function ForceString: string;
    function ForcePointer: pointer;
    function ForceVariant: Variant;
    function ForceClass: TClass;
    function ForceObject: TObject;
    function ForceRecord: TLuaRecord;
    function ForceArray: TLuaArray;
    function ForceSet: TLuaSet;
    function ForceTable: PLuaTable;
  end;
  TLuaArgs = array of TLuaArg;

  // internal argument(s), stored in TLua instance
  TLuaArgsEx = object(TLuaArg)
  private

  public
    // todo

  end;
  PLuaArgsEx = ^TLuaArgsEx;

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
  {$hints on}
  
  // highlevel interface to read and modify Lua-tables
  TLuaTable = object
  private
    {$hints off}none: byte; {TLuaArgType = ltTable} {$hints on}
    {$hints off}
    {здесь 3 служебных байта} align: array[0..2] of byte;  
    Lua: TLua;
    Index_: integer;
    function  GetLength: integer;
    function  GetCount: integer;

    procedure ThrowValueType(const CodeAddr: pointer; const pop: boolean=false);
    function  GetValue(const AIndex: integer): Variant;
    procedure SetValue(const AIndex: integer; const NewValue: Variant);
    function  GetValueEx(const AIndex: integer): TLuaArg;
    procedure SetValueEx(const AIndex: integer; const NewValue: TLuaArg);
    function  GetKeyValue(const Key: string): Variant;
    procedure SetKeyValue(const Key: string; const NewValue: Variant);
    function  GetKeyValueEx(const Key: Variant): TLuaArg;
    procedure SetKeyValueEx(const Key: Variant; const NewValue: TLuaArg);
  public
    // перебор элементов
    function Pairs(var Pair: TLuaPair): boolean; overload;
    function Pairs(var Pair: TLuaPair; const FromKey: Variant): boolean; overload;

    // длинна
    property Length: integer read GetLength; // длинна (для массивов)
    property Count: integer read GetCount; // общее количество элементов

    // значения
    property Value[const Index: integer]: Variant read GetValue write SetValue;
    property KeyValue[const Key: string]: Variant read GetKeyValue write SetKeyValue;
    property ValueEx[const Index: integer]: TLuaArg read GetValueEx write SetValueEx;
    property KeyValueEx[const Key: Variant]: TLuaArg read GetKeyValueEx write SetKeyValueEx;
  end;
  {$hints on}

  // lua reference is the fastest way to operate some script variable from native side
  TLuaReference = class
  private
  {$hints off}
    Index: integer;
    Data: TLuaTable;
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
    property ValueEx: TLuaArg read GetValueEx write SetValueEx;
  end;
  TLuaReferenceDynArray = array of TLuaReference;
  {$hints on}

  // operators
  TLuaOperator = (loNeg, loAdd, loSub, loMul, loDiv, loMod, loPow, loCompare);
  TLuaOperators = set of TLuaOperator;
  TLuaOperatorCallback = procedure(var _Result, _X1, _X2; const Kind: TLuaOperator);


  // all information (such as name, field, methods)
  // you should use it to operate records between native and script
  // todo Переместить выше ?
  TLuaRecordInfo = object
  private
  {$hints off}
    FType: __TLuaType;

    // todo
    FClassIndex: integer;
    FTypeInfo: ptypeinfo;
//    FName: string;
    FSize: integer;
    FOperators: TLuaOperators;
    FOperatorCallback: TLuaOperatorCallback;

    function GetName: LuaString;
  //  function  GetFieldsCount: integer;
//    procedure InternalRegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer);
    procedure SetOperators(const Value: TLuaOperators);
    procedure SetOperatorCallback(const Value: TLuaOperatorCallback);
  public
    procedure RegField(const FieldName: LuaString; const FieldPointer: pointer; const tpinfo: pointer);
    procedure RegProperty(const PropertyName: LuaString; const tpinfo: pointer; const PGet, PSet: pointer);
    procedure RegParameterProperty(const AClass: TClass; const PropertyName: LuaString; const Parameters: PLuaRecordInfo;{todo LuaString-->autobinding} const tpinfo: pointer; const PGet, PSet: pointer; const is_default: boolean=false);
    procedure RegProc(const AClass: TClass; const ProcName: LuaString; const Address: pointer{TLuaClassProc});
    // todo overload autobinding

(*    procedure RegClassProc(const AClass: TClass; const ProcName: LuaString; const Address: pointer{TLuaClassProc}; const is_class: boolean=false);
    // todo overload autobinding
    procedure RegProperty(const AClass: TClass; const PropertyName: LuaString; const tpinfo: pointer; const PGet, PSet: pointer; const is_class: boolean=false);
    procedure RegParameterProperty(const AClass: TClass; const PropertyName: LuaString; const Parameters: PLuaRecordInfo;{todo LuaString-->autobinding} const tpinfo: pointer; const PGet, PSet: pointer; const is_class: boolean=false; const is_default: boolean=false);
*)
  //  procedure RegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer); overload;
  //  procedure RegField(const FieldName: string; const FieldPointer: pointer; const tpinfo: pointer; const pRecord: pointer = nil); overload;
  //  procedure RegProc(const ProcName: string; const Proc: TLuaClassProc; const ArgsCount: integer=-1);

    property Lua: TLua read FType.Lua;
    property Name: LuaString read GetName;
    property Size: integer read FSize;
    //property FieldsCount: integer read GetFieldsCount;
    property Operators: TLuaOperators read FOperators write SetOperators;
    property OperatorCallback: TLuaOperatorCallback read FOperatorCallback write SetOperatorCallback;
  end;
  {$hints on}

  // information needed to use arrays between native and script side
  // todo Переместить выше ?
  TLuaArrayInfo = object
  private
  {$hints off}
    FType: __TLuaType;

    // todo
    // основные
   // FName: string;
    FClassIndex: integer;
    FIsDynamic: boolean;
    ItemInfo: array[0..31] of byte; // TLuaPropertyInfo;

    // размерность
    FBoundsData: TIntegerDynArray; // заполняется только для статических
    FBounds: pinteger;
    FDimention: integer;
    FItemSize: integer; // размер элемента. нужен для расчёта смещений
    FMultiplies: TIntegerDynArray; // множители (стат) и typeinfo (дин)

    // для финализации
    FTypeInfo: ptypeinfo; // конечный элемент финализации (или nil). для массивов c typeinfo - сам дин массив
    FItemsCount: integer; // количество элементов. для массивов c typeinfo - 1
    FSize: integer;       // размер такого массива целиком. для динамических - 4
    function GetName: LuaString;
  public
    property Lua: TLua read FType.Lua;
    property Name: LuaString read GetName;
    property IsDynamic: boolean read FIsDynamic;
    property Bounds: pinteger read FBounds;
    property Dimention: integer read FDimention;
  end;

  // information needed to use set between native and script side
  // todo Переместить выше ?
  TLuaSetInfo = object
  private
    FType: __TLuaType;

    // todo
   // FName: string;
    FClassIndex: integer;
    FTypeInfo: ptypeinfo;
    FSize: integer;
    FLow: integer;
    FHigh: integer;
    FCorrection: integer;
    FRealSize: integer; // это поле нужно только для того чтобы грамотно инвертировать 3х байтные (а sizeof = 4) множества
    FAndMasks: integer; // для коррекции при инфертировании (конечный байт) or (начальный байт shl 8)

    //function  EnumName(const Value: integer): string;
    function  Description(const X: pointer): string;
    function GetName: LuaString;
  public
    property Lua: TLua read FType.Lua;
    property Name: LuaString read GetName;
    property Size: integer read FSize;
    property Low: integer read FLow;
    property High: integer read FHigh;
  end;
  {$hints on}

  // temporary buffer to store difficult type instances
  // which is used as Result in universal recall functions
  TLuaResultBuffer = object
  private
    Memory: pointer;
    Size: integer;
    items_count: integer;
    tpinfo: ptypeinfo;

    procedure Finalize(const free_mem: boolean=false);
  public
    function AllocRecord(const RecordInfo: PLuaRecordInfo): pointer;
    function AllocArray(const ArrayInfo: PLuaArrayInfo): pointer;
    function AllocSet(const SetInfo: PLuaSetInfo): pointer;
  end;

  // universal recall functions
  TGlobalRecallProc = procedure(const Args: TLuaArgsEx; var Result: TLuaArg);
  TClassRecallProc = procedure(const Args: TLuaArgsEx; var Result: TLuaArg) of object;



  

  // -------------------- internal CrystalLUA routine ------------------------

  // один кусок памяти
  __PLuaMemoryPoolItem = ^__TLuaMemoryPoolItem;
  __TLuaMemoryPoolAllocator = procedure(PoolItem: __PLuaMemoryPoolItem; ItemSize: dword; var LastItemsCount: dword);
  __TLuaMemoryPoolCleaner = procedure(PoolItem: __PLuaMemoryPoolItem);
  __TLuaMemoryPoolItem = record
    next: __PLuaMemoryPoolItem; // для организации списка
    userdata: pointer; // если нужна информация для организации буфера

    // кусок памяти
    memory: pointer;
    size: dword; 
    // элементы
    data_items: pointer;
  end;

  // универсальный тип, позволяющий производить простое и быстрое
  // выделение/очищение однородной памяти
  __TLuaMemoryPool = object
  private
    // элементы
    FItemSize: dword;
    FItems: array of __TLuaMemoryPoolItem;
    FAllocableList: __PLuaMemoryPoolItem; // список доступных для аллока элементов

    // калбеки
    FLastItemsCount: dword;
    FAllocator: __TLuaMemoryPoolAllocator;
    FCleaner: __TLuaMemoryPoolCleaner;
  public
    // количество выделенных элементов.
    // чисто для сверки
    Count: integer;

    // инициализация, финализация
    procedure Initialize(ItemSize: dword; Allocator: __TLuaMemoryPoolAllocator=nil; Cleaner: __TLuaMemoryPoolCleaner=nil);
    procedure Finalize();

    // рабочие методы
    function alloc(): pointer;
    procedure release(P: pointer);
  end;
  __PLuaMemoryPool = ^__TLuaMemoryPool;


  // элемент простого хеш-массива для поиска
  __PLuaHashItem = ^__TLuaHashItem;
  __TLuaHashItem = record
    next: __PLuaHashItem; // внутренняя часть

    // рабочая часть
    Key: integer;
    Value: pointer;
  end;

  // простой хеш массив, рассчитанный только на поиск и добавление
  // хеш-функция практически отсутствует
  __TLuaHashArray = object
  private
    FLua: TLua; // в частности для FHashItems: __TLuaMemoryPool
    FValues: __PLuaMemoryPool;
    FArray: array of __PLuaHashItem;

    FArraySize: integer;
    FAndMask: integer; // arraysize-1
    FAllocatedMax: integer;

    FAllocated: integer;
    procedure Grow();
    function AllocAddingItem(const Key: integer; const alloc_value: boolean=true): __PLuaHashItem;
    procedure ForceAdd(const Key: integer; const TryFind: boolean; const Value: pointer);
  public
    procedure Initialize(const Lua: TLua; const Values: __PLuaMemoryPool);
    procedure Finalize();

    // стандартные методы: поиск/добавление
    function find(const Key: integer): pointer;
    function add(const Key: integer; var added: boolean): pointer; overload;
    // особый метод добавления конкретного значения
    procedure add(const Key: integer; const TryFind: boolean; const Value: pointer); overload;

    // дублирование методов для указателей (PtrKey shr 2)
    function find_ptr(const PtrKey: pointer): pointer;
    function add_ptr(const PtrKey: pointer; var added: boolean): pointer; overload;
    procedure add_ptr(const PtrKey: pointer; const TryFind: boolean; const Value: pointer); overload;
  end;
  __PLuaHashArray = ^__TLuaHashArray;

  // элемент, необходимы для более экономного использования глобальных "ссылок" ref
  __PLuaEmptyRef = ^__TLuaEmptyRef;
  __TLuaEmptyRef = record
    next: __PLuaEmptyRef;
    ref: integer;
  end;
  // очередь свободных "ссылок" ref
  __TLuaEmptyRefQueue = record
    Pool: __TLuaMemoryPool;
    Items: __PLuaEmptyRef;
  end;


  // структура для простого выделения памяти
  // чтобы не мучать менеджер памяти простыми временными выделениями
  __TLuaDataBuffer = object
  private
    procedure Grow();
  public
    Memory: pansichar; // чтобы проще было брать смещение
    Size: integer; // полный размер буфера
    MemoryOffset: __luapointer; // текущее смещение в буфере

    procedure Initialize();
    procedure Finalize();

    // простой способ выделить память
    function alloc(const Bytes: integer): pointer;
  end;


  // некий "адрес" скриптовой ошибки
  // модуль (на самом деле вычисляется по индексу),
  // номер апдейта модуля (может отличаться от текущего)
  // и номер строки в модуле (с 0)
  __TLuaScriptErrorAddress = record
    Module: PLuaModule;
    UpdateNumber: integer;
    Line: integer; 
  end;

  // полная структура, информации которой хватит для "возбуждения" скриптовой ошибки
  // т.е. "адрес" ошибки в скриптовом модуле и текст пояснения ошибки! + "слепок" кода
  __TLuaScriptError = record
    Address: __TLuaScriptErrorAddress;
    Text: LuaString;
    Code: string;
  end;


  // хранилище Lua-имён
  // нужно в первую очередь потому, что строковые идентификаторы в Lua
  // можно сделать константными. и тогда поиск идентификатора можно свести к хеш-поиску
  __TLuaNames = object
  private
    FLua: TLua;
    FItems: __TLuaHashArray;
    FAutoAdd: boolean;

    // регистрируем строку в Lua, пишем соответствующий указатель в массив
    function internalAdd(const Name: __luaname; const CRC_len: integer): __luaname;
    // принимает строку во внутреннем формате, возвращает зарегистрированный идентификатор
    function internalIdentifier(const Name: __luaname; const Len: integer): __luaname;
  public
    // инициализация/финализация
    procedure Initialize(const Lua: TLua);
    procedure Finalize();

    // случай ручной регистрации
    function Identifier(const Name: LuaString): __luaname; overload;
    // случай RTTI регистрации
    function Identifier(const RTTIName: PShortString): __luaname; overload;

    // если не был найден - добавляем. в случае false возвращаем nil
    property AutoAdd: boolean read FAutoAdd write FAutoAdd;
  end;

  // внутренние параметры для перевызова нативного кода
  __TLuaRecall = record
    CodeAddr: pointer;
    Reg: dword;
    Back: dword;
  end;

  // режим работы скрипта. очень важно!
  __TLuaScriptCallMode = (smNone, smScript, smLoading, smTestLoading);

  // общее хранилище разных структур, памяти
  __TLuaMemoryStorage = record
  {самые присамые базовые параметры}
    // произошла ли инициализация
    Initialized: boolean;

    // пул сгенерированных "калбеков"
    // единственные куски памяти, которые выделяются особым ОС способом
    Dumps: __TLuaMemoryPool;
  {базовые параметры}
    // пул для всех хеш-массивов (простых)
    HashItems: __TLuaMemoryPool;

    // очередь свободных "ссылок" Ref
    RefQueue: __TLuaEmptyRefQueue;

    // буфер накопления данных
    DataBuffer: __TLuaDataBuffer;
  {пулы структур, участвующие в регистрации}
    // пул для используемых типов.
    // __PLuaUniversalType
    Types: __TLuaMemoryPool;

    // пул для используемых идентификаторов:
    // __PLuaIdentifier
    Identifiers: __TLuaMemoryPool;

(*  // FClasses ?

    // пул информации по зарегистрированным структурам
    // PLuaRecordInfo
    FRecords: __TLuaMemoryPool;

    // пул для зранения информации по зарегистрированным массивам
    // PLuaArrayInfo
    FArrays: __TLuaMemoryPool;

    // пул для хранения информации по зарегистрированным множествам
    // PLuaSetInfo
    FSets: __TLuaMemoryPool;

    // пул для хранения зарегистрированных событий
    // а надо ли их регистрировать ?
    // PLuaEventInfo
    FEvents: __TLuaMemoryPool;

    // ?FBindings: __TLuaMemoryPool;
  {}
    // как-то нужно будет разграничить
    // сложные свойства и простые свойства
    // TODO
    FSimpleProps: __TLuaMemoryPool;
    FDifficultProps: __TLuaMemoryPool;

    // пул для информации по методам (proc)
    // PLuaProcInfo = ^__TLuaProcInfo
    FProcs: __TLuaMemoryPool;

    // пул для хранения объектов глобального пространство
    // PLuaGlobalItem = ^__TLuaGlobalItem
    FGlobalItems: __TLuaMemoryPool;  *)
  {}
    // основной поисковый массив
    // на входе какой-то указатель, на выходе - информация по типу
    RegisteredTypes: __TLuaHashArray;

    // хранилище строковых идентификаторов
    Names: __TLuaNames;

  {}

  end;

  // неафишируемая информация по Классу
  // по факту регистрация происходит в __TLuaRegClass
  // todo
  __PLuaClassInfo = ^__TLuaClassInfo;
  __TLuaClassInfo = record
    FType: __TLuaType;

    AClass: TClass;
    Parent: __PLuaClassInfo;

    // todo
  end;


  // todo
  __TLuaInterfaceInfo = record
    FType: __TLuaType;

    // todo
  end;
  __PLuaInterfaceInfo = ^__TLuaInterfaceInfo;


  // todo
  //
  //
  __TLuaMethodInfo = record

  end;
  __PLuaMethodInfo = ^__TLuaMethodInfo;


  // все разновидности "типов" которые могут существовать (в терминологии CrystalLUA)
  __TLuaFieldKind = (fkUnknown, fkBoolean, fkInteger, fkInt64, fkFloat, fkPointer,
                     fkString, fkVariant,
                     fkObject, fkClass, fkRecord, fkArray, fkSet, fkInterface,
                     fkUniversal, fkMethod);

  // режим идентификатора (в namespace)
  // указывает, какой именно идентификатор хранится в __PLuaIdentifier
  // от режима зависит:
  // - можно ли взять значение идентификатора
  // - можно ли изменить значение идентификатора
  // - по какому алгоритму взять значение идентификатора
  // - по какому алгоритму изменить значение идентификатора
  //
  // - дополнительная логика изменения состояния Lua (например для глобальных идентификаторов)
  // - особая логика отлова ошибок!
  //
  __TLuaIdentifierMode = (imGlobal,        (* __PLuaGlobalIdentifierInfo *)
                          imProcedure,     (* __PLuaProcInfo. *)
                          imProperty       (* __PLuaFieldInfo или __PLuaPropertyInfo *)
                          );

                                
  // разновидности типов булеанов
  __TLuaFieldBoolType = (btBoolean, btByteBool, btWordBool, btLongBool);

  // разновидности "строк"
  __TLuaFieldStringType = (stAnsiChar, stWideChar, stShortString,
                          {todo !!!!!} stPAnsiChar, stPWideChar,
                           stAnsiString, stWideString {$ifdef UNICODE},stUnicodeString{$endif});

  // базовая информация по свойству, полю, элементу массива и т.д.
  // содержит имя, местоположение, а так же аналог typeinfo в удобном универсальном виде
  __TLuaFieldBaseInfo = packed record
    Name: __luaname;                      // имя (или nil для массивов)
    case boolean of
      false: (Values: Int64);
       true: (Mode: __TLuaIdentifierMode; // где хранится
              Kind: __TLuaFieldKind;      // какой тип
              // InstanceMode      
              case __TLuaFieldKind of     // нужная информация
                fkBoolean: (BoolType: __TLuaFieldBoolType);
                fkInteger: (OrdType: TypInfo.TOrdType; MinMax: pinteger);
                  fkInt64: (IsUnsigned64: boolean; Int64MinMax: pint64);
                  fkFloat: (FloatType: TypInfo.TFloatType);
                fkPointer: ({none});
                 fkString: (StringType: __TLuaFieldStringType;
                              case __TLuaFieldStringType of
                                stShortString: (ShortStrMaxLen: dword);
                                 stAnsiString: (AnsiCodePage: dword);
                            );
                fkVariant: (IsOleVariant: boolean);
                 fkObject,
                  fkClass: (ClassInfo: __PLuaClassInfo);
                 fkRecord: (RecordInfo: PLuaRecordInfo);
                  fkArray: (ArrayInfo: PLuaArrayInfo);
                    fkSet: (SetInfo: PLuaSetInfo);
              fkInterface: (InterfaceInfo: __PLuaInterfaceInfo);
              fkUniversal: ({none});
                 fkMethod: (MethodInfo: __PLuaMethodInfo);)
  end;
  __PLuaFieldBaseInfo = ^__TLuaFieldBaseInfo;

  // информация по полю: класса, структуры, массива или глобального пространства
  __TLuaFieldInfo = record
    Base: __TLuaFieldBaseInfo;
    read_mode: integer;
    write_mode: integer;
  end;
  __PLuaFieldInfo = ^__TLuaFieldInfo;

  // информация по свойству. Применяется к классам и структурам - когда есть сеттеры или геттеры
  // виртуальные методы применительны только к классам. Сложные(параметризированные) доступны и классам, и структурам
  __TLuaPropertyInfo = record //(__TLuaFieldInfo)
    Base: __TLuaFieldBaseInfo;
    read_mode: integer;
    write_mode: integer;

    read_getter: pointer;
    write_setter: pointer;
    parameters: PLuaRecordInfo; // nil если свойство обычное
  end;
  __PLuaPropertyInfo = ^__TLuaPropertyInfo;


  // разновидность глобального идентификатора (не метода. метод имеет стандартное строение __TLuaProcInfo)
  // разнородность нужна для грамотной реакции на добавление объекта ?
 // {$MESSAGE 'здесь'} 
  // TODO подумать
  __TLuaGlobalKind = (gkType, gkVariable, gkProc, gkConst, gkLuaData);

  // информация по переменной глобального пространства:
  // переменная, константная переменная, Lua-объекты(в том числе типы)
  // если глобальный идентификатор - глобальная процедура, то идентификатор представляет собой __TLuaProcInfo (Mode = imProc)
  //
  // IsNative - это глобальная переменная, которая в конечном счёте берётся из нативного пространства.
  // для сложных типов (TObject,структура,массив,множество,интерфейс) userdata или nil(TObject) кэшируется.
  // переменная может быть константной(неизменяемой). В этом случае write_ptr=PROP_NONE_USE, IsConst=true.
  __TLuaGlobalIdentifierInfo = record //(__TLuaFieldInfo)
    Base: __TLuaFieldBaseInfo;
    read_ptr: pointer; // если не используется, то PROP_NONE_USE
    write_ptr: pointer;

    IsConst: boolean;
    IsNative: boolean;
    CachedObject: TObject; // для глобальных Object-ов/IInterface создаётся кешируемое значение в Lua (Ret) - чтобы увеличить скорость доступа
    Ref: integer; // соответствующее объекту значение в Lua (или -1)
  end;
  __PLuaGlobalIdentifierInfo = ^__TLuaGlobalIdentifierInfo;

(*
  {|}   __TLuaGlobalKind = (gkType, gkVariable, gkProc, gkConst, gkLuaData);
  {|}   TLuaGlobalVariable  = packed record
  {|}     _Name: string;
  {|}     _Kind: __TLuaGlobalKind;
  {|}     IsConst: boolean; // если константа, то нельзя менять из кода луа или через Variables[]
  {|}     case boolean of
  {|}       false: (Ref: integer); // индекс в таблице LUA_GLOBALSINDEX
  {|}        true: (Index: integer); // нативный индекс. Положительный для методов и отрицательный для свойств (глобальных переменных)
  {|}   end;
*)

  // структура, в которой хранится необходимая информация по параметру метода
  __TLuaParameterInfo = packed record //(__TLuaFieldInfo)
    Base: __TLuaFieldBaseInfo;
    read_offset: integer;
    write_offset: integer;

    links_count: byte; // если 0 - в стеке само значение, иначе - указатель на указатель...
  end;
  __PLuaParameterInfo = ^__TLuaParameterInfo;


  // информация по методу
  __TLuaProcInfo = packed record
    Name: __luaname; 
    lua_CFunction: pointer; // непосредственно "callback" который используется в lua. он переадресует вызов в TLua.CallbackProc

    Mode: __TLuaIdentifierMode; // imProc ?
    with_class: boolean; // class function ProcName(...)
    align: word;

    ArgsCount: integer;
    Address: pointer;
    // Args ?
  end;
  __PLuaProcInfo = ^__TLuaProcInfo;

  // обощённое описание идентификатора (которое ищется в namespace):
  // глобальная функция, переменная, поле, свойство,
  __TLuaIdentifier = record
    case Integer of
      0: (FieldProp: __TLuaPropertyInfo);
      1: (Parameter: __TLuaParameterInfo);
      2: (Proc: __TLuaProcInfo);
      3: (Item: __TLuaGlobalIdentifierInfo);
      4: (Name: __luaname; Mode: __TLuaIdentifierMode);
  end;
  __PLuaIdentifier = ^__TLuaIdentifier;

  // предназначение этого типа только одно
  // иметь некую обощённую структуру внутренних типов, чтобы не париться с max(sizeof())
  // а просто передать sizeof(__TLuaUniversalType) пулу типов
  __TLuaUniversalType = record
    case Integer of
     -1: (FType: __TLuaType);
      // global namespace ?
      1: (FClass: array[1..sizeof(__TLuaClassInfo)] of byte);
      2: (FRecord: array[1..sizeof(TLuaRecordInfo)] of byte);
      3: (FArray: array[1..sizeof(TLuaArrayInfo)] of byte);
      4: (FSet: array[1..sizeof(TLuaSetInfo)] of byte);
      5: (FInterface: array[1..sizeof(__TLuaInterfaceInfo)] of byte);
  end;






  // внутренняя рутина для калбеков ------------------------------------------
  {|}   { информация по процедуре }
(*  {|}   TLuaProcInfo = record
  {|}     ProcName: string;
  {|}
  {|}     ArgsCount: integer;
  {|}     Address: pointer;
  {|}     with_class: boolean; // class function ProcName(...)
  {|}     lua_CFunction: pointer; // непосредственно "callback" который используется в lua. он переадресует вызов в TLua.CallbackProc
  {|}   end;
  {|}   PLuaProcInfo = ^TLuaProcInfo;
  {|}   TLuaProcInfoDynArray = array of TLuaProcInfo;
  {|}
  {|}
  {|}   // все разновидности свойств которые могут существовать (в терминологии CrystalLUA)
//  {|}   __TLuaFieldKind = (pkUnknown, pkBoolean, pkInteger, pkInt64, pkFloat,
//  {|}                       pkObject, pkString, pkVariant, pkInterface,
//  {|}                       pkPointer, pkClass, pkRecord, pkArray, pkSet, pkUniversal);
  {|}
  {|}   // разновидности типов булеанов
//  {|}   __TLuaFieldBoolType = (btBoolean, btByteBool, btWordBool, btLongBool);
  {|}
  {|}   // разновидности "строк"
  {|}  // __TLuaFieldStringType = (stAnsiChar, stWideChar, stShortString, stAnsiString, stWideString {$ifdef UNICODE},stUnicodeString{$endif});
  {|}
  {|}   // базовая информация
  {|}   TLuaPropertyInfoBase = packed record
  {|}     Information: pointer; // typeinfo или вспомогательная информация: по структурами, массивами и множествами
  {|}     Kind: __TLuaFieldKind; // тип свойства
  {|}     case Integer of
  {|}       0: (OrdType: TypInfo.TOrdType);
  {|}       1: (FloatType: TypInfo.TFloatType);
  {|}       2: (StringType: __TLuaFieldStringType; ShortStrMaxLen: byte {для shortstring-ов});
  {|}       3: (BoolType: __TLuaFieldBoolType);
  {|}   end;
  {|}
  {|}   // минимально необходимые данные для функционирования свойства
  {|}   TLuaPropertyInfoCompact = object
  {|}     // базовая информация: тип, разновидность, "typeinfo"
  {|}     Base: TLuaPropertyInfoBase;
  {|}
  {|}     // режимы чтения и записи; нет, функция или смещение
  {|}     read_mode: integer;
  {|}     write_mode: integer;
  {|}   end;
        PLuaPropertyInfoCompact = ^TLuaPropertyInfoCompact;
  {|}
  {|}   { полные данные по свойству }
  {|}   { отличается от компактного именем и возможностью вызова по калбеку (+ инициализация по RTTI) }
  {|}   TLuaPropertyInfo = object(TLuaPropertyInfoCompact)
  {|}     PropertyName: string;
  {|}
  {|}     // информация о свойстве
  {|}     IsRTTI: boolean;
  {|}     PropInfo: PPropInfo;
  {|}
  {|}     // для параметризированных свойств (INDEXED_PROPERTY, NAMED_PROPERTY или PLuaRecordInfo)
  {|}     Parameters: pointer;
  {|}
  {|}     // методы
  {|}     procedure Cleanup();
  {|}     procedure Fill(const RTTIPropInfo: PPropInfo; const PropBase: TLuaPropertyInfoBase); overload;
  {|}     procedure Fill(const class_info; const PropBase: TLuaPropertyInfoBase; const PGet, PSet: pointer; const AParameters: PLuaRecordInfo); overload;
  {|}
  {|}     // описание (тип, индексы, r/w)
  {|}     function Description(): string;
  {|}   end;
  {|}   PLuaPropertyInfo = ^TLuaPropertyInfo;
  {|}   TLuaPropertyInfoDynArray = array of TLuaPropertyInfo;
  {|}
  {|}   { небольшая структура, которая используется для push/pop свойств вне стандартного __index/__newindex }
  {|}   TLuaPropertyStruct = packed record
  {|}     PropertyInfo: PLuaPropertyInfo;
  {|}     Instance: pointer;
  {|}     Index: pointer; // указатель на структуру для вызова сложных свойств
  {|}     ReturnAddr: pointer; // если не nil, от вызывается на нативной стороне
  {|}
  {|}     case boolean of
  {|}       false: (IsConst: boolean); // имеет значение при __index_prop_push
  {|}        true: (StackIndex: integer); // имеет значение при __newindex_prop_set
  {|}   end;
  {|}   PLuaPropertyStruct = ^TLuaPropertyStruct;
  {|}
  {|}   { информация о глобальной переменной: внутри Lua или нативной }
  {|}   TLuaGlobalVariable  = packed record
  {|}     _Name: string;
  {|}     _Kind: __TLuaGlobalKind;
  {|}     IsConst: boolean; // если константа, то нельзя менять из кода луа или через Variables[]
  {|}     case boolean of
  {|}       false: (Ref: integer); // индекс в таблице LUA_GLOBALSINDEX
  {|}        true: (Index: integer); // нативный индекс. Положительный для методов и отрицательный для свойств (глобальных переменных)
  {|}   end;
  {|}   PLuaGlobalVariable = ^TLuaGlobalVariable;
  {|}   TLuaGlobalVariableDynArray = array of TLuaGlobalVariable;
  {|}
  {|}   { имя - процедуры или какого-то свойства}
  {|}   { так же содержит индекс . а вообще хранится Hash }
  {|}   TLuaHashIndex = record
  {|}     Hash: integer;
  {|}     Index: integer;
  {|}   end;
  {|}   TLuaHashIndexDynArray = array of TLuaHashIndex;
  {|}
  {|}   { информация по классу . его процедуры и свойства }
  {|}   { а так же список имён }
  {|}   TLuaClassKind = (ckClass, ckRecord, ckArray, ckSet);
  {|}   TLuaClassInfo = object
  {|}   private
  {|}     // список доступных имён. нужен для того чтобы исключить коллизию и дублирование имён
  {|}     Names: TLuaHashIndexDynArray;
  {|}     // возвращает индекс. отрицательный (для свойств) или положительный (для методов)
  {|}     function  InternalAddName(const Name: string; const AsProc: boolean; {var Initialized: boolean; }const CodeAddr: pointer): integer;
  {|}   private
  {|}     // персональные функции-конструкторы/деструкторы
  {|}     __Create, __Free: pointer; //lua_CFunction
  {|}     // альтернативный (дополняющий) конструктор
  {|}     constructor_address: pointer;
  {|}     constructor_args_count: integer;
  {|}     // указатель на метод assign(arg: tluaarg)
  {|}     assign_address: pointer;
  {|}
  {|}     // полный список имён включая предков. Index = {1: is_proc, 15: class_index, 16: index}
  {|}     // в global native используется как список по всем глобальным переменным (свой алгоритм)
  {|}     NameSpace: TLuaHashIndexDynArray;
  {|}     function NameSpacePlace(const Lua: TLua; const Name: pchar; const NameLength: integer; var ProcInfo, PropertyInfo: pointer): integer;
  {|}   public  
  {|}     _Class: pointer;  // TClass, PLuaRecordInfo, PLuaArrayInfo, PLuaSetInfo
  {|}     _ClassKind: TLuaClassKind;
  {|}     _ClassSimple: boolean; // для убыстренного поиска в простых классах и структурах
  {|}     _ClassName: string; // имя типа: класса или структуры
  {|}     _ClassIndex: integer;
  {|}     _DefaultProperty: integer; // свойство по умолчанию: (AX: ClassIndex, AY: PropertyIndex)
  {|}     ParentIndex: integer;
  {|}     Ref: integer;
  {|}
  {|}     Procs: TLuaProcInfoDynArray; // методы, функции
  {|}     Properties: TLuaPropertyInfoDynArray; // свойства
  {|}
  {|}     function  PropertyIdentifier(const Name: string = ''): string;
  {|}     procedure Cleanup();
  {|}   end;
  {|}   PLuaClassInfo = ^TLuaClassInfo;
  {|}   TLuaClassInfoDynArray = array of TLuaClassInfo;
  {|}
  {|}   TLuaClassIndex = record
  {|}     _Class: pointer; // TClass или PLuaRecordInfo или PLuaArrayInfo или typeinfo(Record) или typeinfo(DynamicArray)
  {|}     Index: integer;
  {|}   end;
  {|}   TLuaClassIndexDynArray = array of TLuaClassIndex;
  {|}
  {|}   TLuaGlobalModifyInfo = packed record
  {|}     Name: string;
  {|}     CodeAddr: pointer;
  {|}     IsVariant: boolean;
  {|}     case boolean of
  {|}       false: (Arg: PLuaArg);
  {|}        true: (V: PVariant);
  {|}   end;     *)
  {|}
  {|}   // TObject, указатель на структуру, массив, статический массив,
  {|}   // элемент сложного свойства или множество. Самый ходовой объект
  {|}   TLuaUserData = packed record
  {|}     instance: pointer; // объект, над которым производятся действия
  {|}
  {|}     // флаги
  {|}     kind: (ukInstance, ukArray, ukProperty, ukSet);
  {|}     array_params: byte; // количество(4bits)/наполнение(4bits) параметров по массивам и сложным свойствам
  {|}     is_const: boolean; // для неизменяемых структур и массивов
  {|}     gc_destroy: boolean; // автоматический деструктор внутри lua
  {|}
  {|}     // Info
  {|}     case integer of
  {|}       0: (ClassIndex: integer); // integer, потому что наиболее неустойчивый user data
  {|}       1: (ArrayInfo: PLuaArrayInfo); // всегда указатель
  {|}       2: (SetInfo: PLuaSetInfo); // всегда указатель
  {|}       3: (PropertyInfo: __PLuaPropertyInfo); // временный. поэтому используется указатель
  {|}   end;
  {|}   PLuaUserData = ^TLuaUserData;
  {|}

  // <<-- внутренняя рутина для калбеков -------------------------------------



  // one module line in internal CrystalLUA format: UTF8(LUA_UNICODE) or ANSI(LUA_ANSI)
  TLuaModuleLineInfo = record
    Chars: __luadata;
    Length: integer;
  end;

  // script module information
  TLuaModule = object
  private
    FLua: TLua;
    FName: LuaString;
    FFileName: string;
    FIndex: integer;

    FBuffer: __luabuffer;
    FBufferOffset: integer;
    FLinesCount: integer;
    FLinesInfo: array of TLuaModuleLineInfo;

    FUpdateCounter: integer;
    FUpdate: integer;

    function InitializeLines(): boolean;
    function Compile(): integer;
    function GetLine(AIndex: integer): LuaString;
    function GetLineInfo(AIndex: integer): TLuaModuleLineInfo;
  private
    function GetCodeString(const Line: integer; const AsUpdate: integer): string;  
  public
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const AFileName: string); overload;
    procedure SaveToFile(); overload;

    property Lua: TLua read FLua;
    property Name: LuaString read FName;
    property FileName: string read FFileName;
    property Index: integer read FIndex;

    property LinesCount: integer read FLinesCount;
    property Lines[AIndex: integer]: LuaString read GetLine;
    property LinesInfo[AIndex: integer]: TLuaModuleLineInfo read GetLineInfo;
  end;


  // Main class
  TLua = class(TObject)
  private
    // error check block
    FRecall: __TLuaRecall;
    FNameSpaceInitalized: boolean;
    FScriptMode: __TLuaScriptCallMode;
    FExceptionThreadId: cardinal;
    FExceptionRecaller: pointer;

    procedure InternalRecall;
    procedure InternalRegisterRecall;
    procedure InspectScriptError(var Error: __TLuaScriptError);
    function  ScriptErrorShowInIDE(const Error: __TLuaScriptError): boolean;
    procedure ScriptThrow(const Error: __TLuaScriptError);
    procedure ScriptExceptionHandler(const E: Exception);
    function  CheckIdentifier(const Identifier: LuaString): __luaname;
  private
    // low-level block
    FHandle: pointer;
//    FBufferArg: TLuaArg;
    FResultBuffer: TLuaResultBuffer;
    FReferences: TLuaReferenceDynArray; // список ссылок

    function ScriptCall(const ArgsCount: integer=-1; const auto_throw: boolean=true; const AScriptMode: __TLuaScriptCallMode=smScript): integer;
    function Callback(const T: __TLuaType; const Mode: integer): integer;
    function StdIdentifierCallback(const userdata: PLuaUserData; const T: __TLuaType; const stdindex: integer; const getter: boolean): integer;

   // function  InternalCheckArgsCount(PArgs: pinteger; ArgsCount: integer; const ProcName: string; const AClass: TClass): integer;
   // function  StackArgument(const Index: integer): string;

    // сложные пуши
   { function  push_userdata(const ClassInfo: TLuaClassInfo; const gc_destroy: boolean; const Data: pointer): PLuaUserData;
    function  push_difficult_property(const Instance: pointer; const PropertyInfo: TLuaPropertyInfo): PLuaUserData;
    function  push_variant(const Value: Variant): boolean;
    function  push_luaarg(const LuaArg: TLuaArg): boolean;
    function  push_argument(const Value: TVarRec): boolean;
    }
    // взаимодействие со стеком
    procedure stack_clear;
    procedure stack_pop(const count: integer=1);
    //function  stack_variant(var Ret: Variant; const StackIndex: integer): boolean;
    //function  stack_luaarg(var Ret: TLuaArg; const StackIndex: integer; const lua_table_available: boolean): boolean;
  public  //private
    // data storage and temopary data
    FStorage: __TLuaMemoryStorage;

    FTemporary: __luapointer;
    procedure temporary_clear();
    function temporary_alloc(const Info: __PLuaFieldBaseInfo): __luapointer;
  private
    // modules routine
    FModulesCount: integer;
    FModules: array of TLuaModule;
    FTestModule: TLuaModule;
    function  GetModule(const Index: integer): PLuaModule;
    function  GetModuleIndex(const Name: LuaString): integer;
    function  GetModuleByName(const Name: LuaString): PLuaModule;

    procedure InternalLoadScript(var unique_buffer: __luabuffer; const ModuleName: LuaString; const FileName: string; const TestMode: boolean);
  private
    // глобальное пространство
    // глобальные процедуры, переменные, калбеки глобальных переменных из Lua
    FRef: integer;
    FGlobalType: __PLuaType;
    FDifficultProperties: __PLuaType;
    //GlobalNative: TLuaClassInfo; // нативные: методы и перменные
    //GlobalVariables: TLuaGlobalVariableDynArray; // полный список включая Lua-переменные
    //property  NameSpaceHash: TLuaHashIndexDynArray read GlobalNative.NameSpace; // Hash по всем глобальным переменным и функциям

    // работа с глобальной луа-таблицей
    procedure global_alloc_ref(var ref: integer);
    procedure global_free_ref(var ref: integer);
    procedure global_fill_value(const ref: integer);
    procedure global_push_value(const ref: integer);

    // найти глобальную переменную. если false, то Index - place в hash списке глобальных имён
    //function  GlobalVariablePos(const Name: pchar; const NameLength: integer; var Index: integer; const auto_create: boolean=false): boolean;
  private
    // инициализация, информация по классам
 {   ClassesInfo: TLuaClassInfoDynArray;
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
 }
    procedure INITIALIZE_NAME_SPACE();
{    function  internal_class_index(AClass: pointer; const look_class_parents: boolean = false): integer;
    function  internal_class_index_by_name(const AName: string): integer;
    function  internal_add_class_info(const is_global_space: boolean = false): integer;
    function  internal_add_class_index(const AClass: pointer; const AIndex: integer): integer;
    function  internal_add_class_index_by_name(const AName: string; const AIndex: integer): integer;
    function  internal_register_global(const Name: string; const Kind: __TLuaGlobalKind; const CodeAddr: pointer): PLuaGlobalVariable;
    function  internal_register_metatable(const CodeAddr: pointer; const GlobalName: string=''; const ClassIndex: integer = -1; const is_global_space: boolean = false): integer;
}   // function  InternalAddClass(AClass: TClass; UsePublished: boolean): integer;
//    function  InternalAddRecord(const Name: string; tpinfo: pointer): integer;
//    function  InternalAddArray(Identifier, itemtypeinfo: pointer; const ABounds: array of integer): integer;
//    function  InternalAddSet(tpinfo: pointer): integer;
 {   function  InternalAddProc(const IsClass: boolean; AClass: pointer; const ProcName: string; ArgsCount: integer; const with_class: boolean; Address: pointer): integer;
    function  InternalAddProperty(const IsClass: boolean; AClass: pointer; const PropertyName: string; tpinfo: ptypeinfo; const IsConst, IsDefault: boolean; const PGet, PSet, Parameters: pointer): integer;

    function __tostring(): integer;
    function __inherits_from(): integer;
    function __assign(): integer;
    function __initialize_by_table(const userdata: PLuaUserData; const stack_index: integer): integer;
    function __tmethod_call(const Method: TMethod): integer;
    function __index_prop_push(const ClassInfo: TLuaClassInfo; const prop_struct: PLuaPropertyStruct): integer;
    function __newindex_prop_set(const ClassInfo: TLuaClassInfo; const prop_struct: PLuaPropertyStruct): integer;
    function __len(const ClassInfo: TLuaClassInfo): integer;
    function __operator(const ClassInfo: TLuaClassInfo; const Kind: integer): integer;
    function __constructor(const ClassInfo: TLuaClassInfo; const __create: boolean): integer;
    function __destructor(const ClassInfo: TLuaClassInfo; const __free: boolean): integer;
    function __call(const ClassInfo: TLuaClassInfo): integer;
    function __global_index(const native: boolean; const info: TLuaGlobalModifyInfo): integer;
    function __global_newindex(const native: boolean; const info: TLuaGlobalModifyInfo): integer;
    function __array_index(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
    function __array_newindex(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
    function __array_dynamic_resize(): integer; }
//    function __array_include(const mode: integer{constructor, include, concat}): integer;
//    function __set_method(const is_construct: boolean; const method: integer{0..2}): integer;
//    function  ProcCallback(const ClassInfo: TLuaClassInfo; const ProcInfo: TLuaProcInfo): integer;
  private
//    FArgs: TLuaArgs;
//    FArgsCount: integer;

    function  GetRecordInfo(const Name: LuaString): PLuaRecordInfo;
    function  GetArrayInfo(const Name: LuaString): PLuaArrayInfo;
    function  GetSetInfo(const Name: LuaString): PLuaSetInfo;
  {  function  GetVariable(const Name: string): Variant;
    procedure SetVariable(const Name: string; const Value: Variant);
    function  GetVariableEx(const Name: string): TLuaArg;
    procedure SetVariableEx(const Name: string; const Value: TLuaArg); }
  public
    constructor Create;
    destructor Destroy; override;

    // exceptions
    procedure Assert(const FmtStr: LuaString; const Args: array of const); overload;
    procedure Assert(const Text: LuaString); overload;

    // script loading and testing
    procedure LoadScript(const FileName: string); overload;
    procedure LoadScript(const Stream: TStream; const ModuleName: LuaString); overload;
    procedure LoadScript(const Buffer: pointer; const BufferSize: integer; const ModuleName: LuaString); overload;
    procedure TestScript(const Script: LuaString);


    // ?
   // procedure GarbageCollection();
   // procedure SaveNameSpace(const FileName: string); dynamic;
   // function CreateReference(const global_name: string=''): TLuaReference;




    // проверка при калбеке
{    function  CheckArgsCount(const ArgsCount: array of integer; const ProcName: string=''; const AClass: TClass=nil): integer; overload;
    function  CheckArgsCount(const ArgsCount: TIntegerDynArray; const ProcName: string=''; const AClass: TClass=nil): integer; overload;
    procedure CheckArgsCount(const ArgsCount: integer; const ProcName: string=''; const AClass: TClass=nil); overload;
 }
    // вызовы
 {   function VariableExists(const Name: string): boolean;
    function ProcExists(const ProcName: string): boolean;
    function Call(const ProcName: string; const Args: TLuaArgs): TLuaArg; overload;
    function Call(const ProcName: string; const Args: array of const): TLuaArg;  overload;
 }
    // registrations
    procedure RegClass(const AClass: TClass; const use_published: boolean = true);
    procedure RegClasses(const AClasses: array of TClass; const use_published: boolean = true);
    function  RegRecord(const Name: LuaString; const tpinfo: ptypeinfo): PLuaRecordInfo;
    function  RegArray(const Identifier: pointer; const itemtypeinfo: pointer; const Bounds: array of integer): PLuaArrayInfo;
    function  RegSet(const SetTypeInfo: ptypeinfo): PLuaSetInfo;
    procedure RegGlobalProc(const Signature: LuaString; const Address: pointer);
    procedure RegGlobalRecallProc(const ProcName: LuaString; const RecallProc: TGlobalRecallProc);
    procedure RegClassProc(const AClass: TClass; const Signature: LuaString; const Address: pointer);
    procedure RegClassRecallProc(const AClass: TClass; const ProcName: LuaString; const RecallProc: TClassRecallProc; const is_class: boolean=false);
    procedure RegProperty(const AClass: TClass; const PropertyName: LuaString; const tpinfo: pointer; const PGet, PSet: pointer; const is_class: boolean=false; const parameters: LuaString='');

{   procedure RegProperty(const AClass: TClass; const PropertyName: string; const tpinfo: pointer; const PGet, PSet: pointer; const parameters: PLuaRecordInfo=nil; const default: boolean=false);
    procedure RegVariable(const VariableName: string; const X; const tpinfo: pointer; const IsConst: boolean = false);
    procedure RegConst(const ConstName: string; const Value: Variant); overload;
    procedure RegConst(const ConstName: string; const Value: TLuaArg); overload;
 }  procedure RegEnum(const EnumTypeInfo: ptypeinfo);

    // вспомогательные свойства
    property ResultBuffer: TLuaResultBuffer read FResultBuffer;
  {  property Variable[const Name: string]: Variant read GetVariable write SetVariable;
    property VariableEx[const Name: string]: TLuaArg read GetVariableEx write SetVariableEx; }
    property RecordInfo[const Name: LuaString]: PLuaRecordInfo read GetRecordInfo;
    property ArrayInfo[const Name: LuaString]: PLuaArrayInfo read GetArrayInfo;
    property SetInfo[const Name: LuaString]: PLuaSetInfo read GetSetInfo;
    // todo InterfaceInfo ?

    // основные свойства
    property Handle: pointer read FHandle;
//    property Args: TLuaArgs read FArgs;
//    property ArgsCount: integer read FArgsCount;

    // modules
    property ModulesCount: integer read FModulesCount;
    property Modules[const Index: integer]: PLuaModule read GetModule;
    property ModuleByName[const Name: LuaString]: PLuaModule read GetModuleByName;
  end;

const
  // чтобы не напутать при задании конструктора и assign()
  LUA_CONSTRUCTOR = 'constructor';
  LUA_ASSIGN = 'assign';

  // параметры typeinfo
  typeinfoTClass  = ptypeinfo($7FFF0000);
  typeinfoPointer = ptypeinfo($7EEE0000);
  typeinfoUniversal = ptypeinfo($7DDD0000);


  // параметры сложных свойств
  INDEXED_PROPERTY = PLuaRecordInfo($7EEEEEEE);
  NAMED_PROPERTY   = PLuaRecordInfo($7AAAAAAA);

  // фильтр всех операторов
  ALL_OPERATORS: TLuaOperators = [low(TLuaOperator)..high(TLuaOperator)];


// вспомогательные функции
function CreateLua(): TLua;
function LuaArgs(const Count: integer): TLuaArgs;
function LuaArg(const Value: boolean): TLuaArg; overload;
function LuaArg(const Value: integer): TLuaArg; overload;
function LuaArg(const Value: double): TLuaArg; overload;
function LuaArg(const Value: string): TLuaArg; overload;
function LuaArg(const Value: pointer): TLuaArg; overload;
function LuaArg(const Value: TClass): TLuaArg; overload;
function LuaArg(const Value: TObject): TLuaArg; overload;
function LuaArg(const Value: TLuaRecord): TLuaArg; overload;
function LuaArg(const Value: TLuaArray): TLuaArg; overload;
function LuaArg(const Value: TLuaSet): TLuaArg; overload;
function LuaArg(const Value: Variant): TLuaArg; overload;
function LuaRecord(const Data: pointer; const Info: PLuaRecordInfo; const IsRef: boolean=true; const IsConst: boolean=false): TLuaRecord;
function LuaArray(const Data: pointer; const Info: PLuaArrayInfo; const IsRef: boolean=true; const IsConst: boolean=false): TLuaArray;
function LuaSet(const Data: pointer; const Info: PLuaSetInfo; const IsRef: boolean=true; const IsConst: boolean=false): TLuaSet;

// todo delete
{function LuaProc(const Proc: TLuaProc0): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc1): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc2): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc3): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc4): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc5): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc6): TLuaProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc0): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc1): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc2): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc3): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc4): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc5): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc6): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc7): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc8): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc9): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc10): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc11): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc12): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc13): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc14): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc15): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc16): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc17): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc18): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc19): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc20): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc21): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc22): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc23): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc24): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc25): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc26): TTClassRecallProc; overload;
function TClassRecallProc(const Proc: TTClassRecallProc27): TTClassRecallProc; overload;
function TClassRecallProcPtr(const Proc: pointer): TTClassRecallProc;  }



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
    Buffer: PKOLChar;
    BufSize: Integer;
    nChars: Integer;
  end;

function EnumStringModules(Instance: NativeInt; Data: Pointer): Boolean;
begin
  with PStrData(Data)^ do
  begin
    nChars := LoadString(Instance, Ident, Buffer, BufSize);
    Result := nChars = 0;
  end;
end;

function FindStringResource(Ident: Integer; Buffer: PKOLChar; BufSize: Integer): Integer;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Buffer := Buffer;
  StrData.BufSize := BufSize;
  StrData.nChars := 0;
  EnumResourceModules(EnumStringModules, @StrData);
  Result := StrData.nChars;
end;

function LoadStr(Ident: Integer): string;
var
  Buffer: array[0..1023] of KOLChar;
begin
  SetString(Result, Buffer, FindStringResource(Ident, Buffer, SizeOf(Buffer)));
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




{$ifdef NO_CRYSTAL}
// ------------ Unicode рутина модуля SysUtilsEx -----------------------------

var
  DEFAULT_CODEPAGE: integer = 0;

procedure InitializeDefaultCodePage();
begin
{$IFDEF MSWINDOWS}
   DEFAULT_CODEPAGE := Windows.GetACP;
{$ELSE}
   {$MESSAGE ERROR 'Default codepage not defined'}
{$ENDIF}
end;


const
  BOM_UTF8    =  {3} $00BFBBEF;
  BOM_UTF16   =  {2} $0000FEFF;
  BOM_UTF16BE =  {2} $0000FFFE;

var
  // таблица для преобразования Ansi в WideChar
  map_ansi_to_wide: array[0..127 {128..255}] of WideChar;

  // таблица для преобразования Ansi в Utf8
  // byte(value) = size(utf8_ansichar)
  {$ifdef LUA_UNICODE}
    map_ansi_to_utf8: array[0..127 {128..255}] of dword;
  {$endif}  

  // hash-таблица для преобразования WideChar в ansi
  map_utf16_to_ansi: record
    // хэш-массив
    hash_array: array[0..127] of byte;

    // ссылки на элементы
    item_nexts: array[0..127] of byte;
  end;


{$ifdef LUA_UNICODE}

// ansi-таблицы
var LowerChars : array[ansichar] of ansichar;
var UpperChars : array [ansichar] of ansichar;

// таблица для нахождения LowerChar(WideChar)
const wine_casemap_lower: array[0..3802-1] of word =
(
   (* index *)
   $01bf, $02bf, $03bf, $044f, $054f, $064f, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $06af, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $07af, $08ae,
   $0100, $09ab, $0100, $0100, $0a2f, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0b2f, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0c1d, $0cfb,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0dda,
   (* defaults *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $0041 .. $00ff *)
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0000, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $0100 .. $01ff *)
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $ff39, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0000, $0001, $0000, $0001, $0000, $0001, $0000, $0001,
   $0000, $0001, $0000, $0001, $0000, $0001, $0000, $0001,
   $0000, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $ff87, $0001, $0000, $0001, $0000, $0001, $0000, $0000,
   $0000, $00d2, $0001, $0000, $0001, $0000, $00ce, $0001,
   $0000, $00cd, $00cd, $0001, $0000, $0000, $004f, $00ca,
   $00cb, $0001, $0000, $00cd, $00cf, $0000, $00d3, $00d1,
   $0001, $0000, $0000, $0000, $00d3, $00d5, $0000, $00d6,
   $0001, $0000, $0001, $0000, $0001, $0000, $00da, $0001,
   $0000, $00da, $0000, $0000, $0001, $0000, $00da, $0001,
   $0000, $00d9, $00d9, $0001, $0000, $0001, $0000, $00db,
   $0001, $0000, $0000, $0000, $0001, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0002, $0001, $0000, $0002,
   $0001, $0000, $0002, $0001, $0000, $0001, $0000, $0001,
   $0000, $0001, $0000, $0001, $0000, $0001, $0000, $0001,
   $0000, $0001, $0000, $0001, $0000, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0000, $0002, $0001, $0000, $0001, $0000, $ff9f, $ffc8,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   (* $0200 .. $02ff *)
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $ff7e, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $2a2b, $0001, $0000, $ff5d, $2a28, $0000,
   $0000, $0001, $0000, $ff3d, $0045, $0047, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $0370 .. $03ff *)
   $0001, $0000, $0001, $0000, $0000, $0000, $0001, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0026, $0000,
   $0025, $0025, $0025, $0000, $0040, $0000, $003f, $003f,
   $0000, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0000, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0000, $0000, $0000, $0000, $ffc4, $0000, $0000, $0001,
   $0000, $fff9, $0001, $0000, $0000, $ff7e, $ff7e, $ff7e,
   (* $0400 .. $04ff *)
   $0050, $0050, $0050, $0050, $0050, $0050, $0050, $0050,
   $0050, $0050, $0050, $0050, $0050, $0050, $0050, $0050,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $000f, $0001, $0000, $0001, $0000, $0001, $0000, $0001,
   $0000, $0001, $0000, $0001, $0000, $0001, $0000, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   (* $0500 .. $05ff *)
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $10a0 .. $10ff *)
   $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
   $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
   $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
   $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $1c60,
   $1c60, $1c60, $1c60, $1c60, $1c60, $1c60, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $1e00 .. $1eff *)
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $e241, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   (* $1f01 .. $1fff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $fff8, $0000, $fff8, $0000, $fff8, $0000, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $fff8, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $ffb6, $ffb6, $fff7, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $ffaa,
   $ffaa, $ffaa, $ffaa, $fff7, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $ff9c, $ff9c, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $fff8,
   $fff8, $ff90, $ff90, $fff9, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $ff80,
   $ff80, $ff82, $ff82, $fff7, $0000, $0000, $0000,
   (* $2103 .. $21ff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $e2a3, $0000, $0000, $0000, $df41,
   $dfba, $0000, $0000, $0000, $0000, $0000, $0000, $001c,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0010, $0010, $0010,
   $0010, $0010, $0010, $0010, $0010, $0010, $0010, $0010,
   $0010, $0010, $0010, $0010, $0010, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0001, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000,
   (* $247c .. $24ff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $001a, $001a, $001a, $001a, $001a, $001a,
   $001a, $001a, $001a, $001a, $001a, $001a, $001a, $001a,
   $001a, $001a, $001a, $001a, $001a, $001a, $001a, $001a,
   $001a, $001a, $001a, $001a, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000,
   (* $2c00 .. $2cff *)
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0030,
   $0030, $0030, $0030, $0030, $0030, $0030, $0030, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0001, $0000, $d609, $f11a, $d619, $0000, $0000, $0001,
   $0000, $0001, $0000, $0001, $0000, $d5e4, $d603, $d5e1,
   $d5e2, $0000, $0001, $0000, $0000, $0001, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $d5c1, $d5c1,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0001, $0000, $0001, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $a612 .. $a6ff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000,
   (* $a722 .. $a7ff *)
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0000, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0001,
   $0000, $0001, $0000, $75fc, $0001, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0000, $0000,
   $0000, $0001, $0000, $5ad8, $0000, $0000, $0001, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0001, $0000,
   $0001, $0000, $0001, $0000, $0001, $0000, $0001, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000,
   (* $ff21 .. $ffff *)
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0020, $0020, $0020, $0020, $0020, $0020,
   $0020, $0020, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000
);

// таблица для нахождения UpperChar(WideChar)
const wine_casemap_upper: array[0..3994-1] of word = 
(
   (* index *)
   $019f, $029f, $039f, $045a, $0556, $0656, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $06dd, $07dc, $08dc,
   $0100, $09d0, $0100, $0100, $0a55, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0b3f, $0c3f, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0cfe, $0ddb,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0100,
   $0100, $0100, $0100, $0100, $0100, $0100, $0100, $0e9a,
   (* defaults *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $0061 .. $00ff *)
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $02e7, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $0000, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $0079,
   (* $0100 .. $01ff *)
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ff18, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $0000, $ffff, $0000, $ffff, $0000, $ffff, $fed4,
   $00c3, $0000, $0000, $ffff, $0000, $ffff, $0000, $0000,
   $ffff, $0000, $0000, $0000, $ffff, $0000, $0000, $0000,
   $0000, $0000, $ffff, $0000, $0000, $0061, $0000, $0000,
   $0000, $ffff, $00a3, $0000, $0000, $0000, $0082, $0000,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000,
   $ffff, $0000, $0000, $0000, $0000, $ffff, $0000, $0000,
   $ffff, $0000, $0000, $0000, $ffff, $0000, $ffff, $0000,
   $0000, $ffff, $0000, $0000, $0000, $ffff, $0000, $0038,
   $0000, $0000, $0000, $0000, $0000, $ffff, $fffe, $0000,
   $ffff, $fffe, $0000, $ffff, $fffe, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $ffb1, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $0000, $ffff, $fffe, $0000, $ffff, $0000, $0000,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   (* $0200 .. $02ff *)
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $0000, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $ffff, $0000, $0000, $2a3f,
   $2a3f, $0000, $ffff, $0000, $0000, $0000, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $2a1f, $2a1c, $2a1e, $ff2e, $ff32, $0000, $ff33, $ff33,
   $0000, $ff36, $0000, $ff35, $0000, $0000, $0000, $0000,
   $ff33, $0000, $0000, $ff31, $0000, $a528, $0000, $0000,
   $ff2f, $ff2d, $0000, $29f7, $0000, $0000, $0000, $ff2d,
   $0000, $29fd, $ff2b, $0000, $0000, $ff2a, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $29e7, $0000, $0000,
   $ff26, $0000, $0000, $ff26, $0000, $0000, $0000, $0000,
   $ff26, $ffbb, $ff27, $ff27, $ffb9, $0000, $0000, $0000,
   $0000, $0000, $ff25, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $0345 .. $03ff *)
   $0054, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $ffff, $0000, $ffff, $0000,
   $0000, $0000, $ffff, $0000, $0000, $0000, $0082, $0082,
   $0082, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $ffda,
   $ffdb, $ffdb, $ffdb, $0000, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe1, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffc0,
   $ffc1, $ffc1, $0000, $ffc2, $ffc7, $0000, $0000, $0000,
   $ffd1, $ffca, $fff8, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $ffaa, $ffb0, $0007, $0000, $0000,
   $ffa0, $0000, $0000, $ffff, $0000, $0000, $ffff, $0000,
   $0000, $0000, $0000,
   (* $0404 .. $04ff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
   $ffe0, $ffe0, $ffe0, $ffe0, $ffb0, $ffb0, $ffb0, $ffb0,
   $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0, $ffb0,
   $ffb0, $ffb0, $ffb0, $ffb0, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $fff1, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff,
   (* $0500 .. $05ff *)
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $1d79 .. $1dff *)
   $8a04, $0000, $0000, $0000, $0ee6, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $1e01 .. $1eff *)
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000, $0000,
   $0000, $0000, $ffc5, $0000, $0000, $0000, $0000, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   (* $1f00 .. $1fff *)
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0008, $0000, $0008, $0000, $0008, $0000, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $004a, $004a, $0056, $0056, $0056, $0056, $0064, $0064,
   $0080, $0080, $0070, $0070, $007e, $007e, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0008, $0008, $0008, $0008, $0008, $0008,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0000, $0009, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $e3db, $0000,
   $0000, $0000, $0000, $0009, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0008, $0008, $0000, $0000, $0000, $0007, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0009, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $210c .. $21ff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $ffe4, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $fff0, $fff0, $fff0, $fff0,
   $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0, $fff0,
   $fff0, $fff0, $fff0, $fff0, $0000, $0000, $0000, $0000,
   $ffff, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000,
   (* $247b .. $24ff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $ffe6, $ffe6, $ffe6,
   $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6,
   $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6,
   $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $ffe6, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000,
   (* $2c16 .. $2cff *)
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0, $ffd0,
   $ffd0, $0000, $0000, $ffff, $0000, $0000, $0000, $d5d5,
   $d5d8, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $0000, $0000, $0000, $0000, $0000, $ffff, $0000, $0000,
   $ffff, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff,
   $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $ffff, $0000,
   $ffff, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000,
   (* $2d00 .. $2dff *)
   $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
   $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
   $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
   $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0,
   $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $e3a0, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $a641 .. $a6ff *)
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   (* $a723 .. $a7ff *)
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $ffff,
   $0000, $ffff, $0000, $0000, $ffff, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $0000, $0000,
   $0000, $ffff, $0000, $0000, $0000, $0000, $ffff, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $ffff, $0000,
   $ffff, $0000, $ffff, $0000, $ffff, $0000, $ffff, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
   $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000,
    (* $ff41 .. $ffff *)
    $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
    $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
    $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0, $ffe0,
    $ffe0, $ffe0, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000
);


function Utf8Length(const Utf8Chars: pansichar; const Utf8Length: integer): integer; overload;
asm
  test eax, eax
  jz @exit0

  test edx, edx
  jle @exit0
jmp @prefix
@exit0:
  xor eax, eax
  jmp @exit
@prefix:
  { push/initialization }
  push ebx

  xor ecx, ecx
@begin:

  @loop:
      movzx ebx, byte ptr [eax]
      dec edx
      inc eax
      test ebx, $80
      jz @loop_continue

      test ebx, $20
      jz @look_next
        inc eax
        dec edx
        jl @end

        movzx ebx, byte ptr [eax]
        and ebx, $C0
        cmp ebx, $80
        jne @end
      @look_next:

      movzx ebx, byte ptr [eax]
      inc eax
      and ebx, $C0
      dec edx
      jl @end
      cmp ebx, $80
      jne @end
  @loop_continue:
  inc ecx
  test edx, edx
  jnz @loop


@end:
  mov eax, ecx

@postfix:
  pop ebx
@exit:
end;
         
function Utf8Length(const S: Utf8String): integer; overload;
asm
  test eax, eax
  jz @exit
  mov edx, [eax-4]
  jmp Utf8Length
@exit:
end;

procedure Utf8UpperCase(const Destination, Source: pansichar; const Utf8Length: integer);
asm
  test eax, eax
  jz @exit
  test edx, edx
  jz @exit
  test ecx, ecx
  jle @exit

@prefix:
  push esi
  push edi
  push ebx

  mov esi, edx
  mov edi, eax
  //lea edx, [ecx+1]
  mov edx, ecx
@begin:

  dec edi
  @loop:
    movzx ebx, byte ptr [esi]
    inc edi
    test ebx, $80
    jz @one
    and ebx, $3F
    inc esi
    test ebx, $20
    jz @two
    jmp @three
  @one:
    // один символ - всё просто
    mov ebx, dword ptr [UpperChars + ebx]
    inc esi
    mov [edi], bl
  dec edx
  jnz @loop
  jmp @loop_end
  @two:
    // два символа
    dec edx
    jz @loop_end

    // wc := (wc shl 6) or (c and $3F);
    movzx eax, byte ptr [esi]
    shl ebx, 6
    mov ecx, eax
    and eax, $C0
    and ecx, $3F
    cmp eax, $80
    jne @loop_end
    or ebx, ecx // wide
    inc edi

    // Destination[i] := WideChar(W + wine_casemap_upper[wine_casemap_upper[W shr 8] + byte(W)]);
    mov eax, ebx
    mov ecx, ebx
    shr eax, 8
    and ecx, $FF
    movzx eax, word ptr [wine_casemap_upper + eax*2]
    add ecx, ecx
    inc esi
    add bx, word ptr [wine_casemap_upper + eax*2 + ecx]

    // Dest[count] := ansichar($C0 or (c shr 6));
    // Dest[count+1] := ansichar($80 or (c and $3F));
    mov eax, ebx
    shl eax, 8
    shr ebx, 6
    and eax, $3F00
    or ebx, $80C0
    or ebx, eax

  dec edx
  mov [edi-1], bx
  jnz @loop
  jmp @loop_end  
  @three:
    // три символа
    sub edx, 2
    jle @loop_end

    // wc := (((wc shl 6) or (c[0] and $3F)) shl 6) or (c[1] and $3F);
    movzx eax, word ptr [esi]
    shl ebx, 12
    mov ecx, eax
    and ebx, $FFFF
    and eax, $C0C0
    and ecx, $3F3F
    cmp eax, $8080
    jne @loop_end
    mov eax, ecx
    and ecx, $FF
    shr eax, 8
    shl ecx, 6
    //lea ebx, [ebx + eax + ecx] : ecx |= eax
    or ecx, eax
    add edi, 2
    or ebx, ecx

    // Destination[i] := WideChar(W + wine_casemap_upper[wine_casemap_upper[W shr 8] + byte(W)]);
    mov eax, ebx
    and ecx, $FF
    shr eax, 8
    add esi, 2
    movzx eax, word ptr [wine_casemap_upper + eax*2]
    add ecx, ecx
    add bx, word ptr [wine_casemap_upper + eax*2 + ecx]

    //Dest[count] := ansichar($E0 or (c shr 12));
    //Dest[count+1] := ansichar($80 or ((c shr 6) and $3F));
    //Dest[count+2] := ansichar($80 or (c and $3F));
    mov eax, ebx
    shr eax, 12
    or  eax, $E0
    mov [edi-2], al
    mov eax, ebx

    shr ebx, 6
    mov bh, al
    and ebx, $3F3F
    or ebx, $8080

  dec edx
  mov [edi-1], bx
  jnz @loop
  @loop_end:


@end:
@postfix:
  pop ebx
  pop edi
  pop esi
@exit:
end;

procedure Utf8LowerCase(const Destination, Source: pansichar; const Utf8Length: integer);
asm
  test eax, eax
  jz @exit
  test edx, edx
  jz @exit
  test ecx, ecx
  jle @exit

@prefix:
  push esi
  push edi
  push ebx

  mov esi, edx
  mov edi, eax
  //lea edx, [ecx+1]
  mov edx, ecx
@begin:

  dec edi
  @loop:
    movzx ebx, byte ptr [esi]
    inc edi
    test ebx, $80
    jz @one
    and ebx, $3F
    inc esi
    test ebx, $20
    jz @two
    jmp @three
  @one:
    // один символ - всё просто
    mov ebx, dword ptr [LowerChars + ebx]
    inc esi
    mov [edi], bl
  dec edx
  jnz @loop
  jmp @loop_end
  @two:
    // два символа
    dec edx
    jz @loop_end

    // wc := (wc shl 6) or (c and $3F);
    movzx eax, byte ptr [esi]
    shl ebx, 6
    mov ecx, eax
    and eax, $C0
    and ecx, $3F
    cmp eax, $80
    jne @loop_end
    or ebx, ecx // wide
    inc edi

    // Destination[i] := WideChar(W + wine_casemap_lower[wine_casemap_lower[W shr 8] + byte(W)]);
    mov eax, ebx
    mov ecx, ebx
    shr eax, 8
    and ecx, $FF
    movzx eax, word ptr [wine_casemap_Lower + eax*2]
    add ecx, ecx
    inc esi
    add bx, word ptr [wine_casemap_Lower + eax*2 + ecx]

    // Dest[count] := ansichar($C0 or (c shr 6));
    // Dest[count+1] := ansichar($80 or (c and $3F));
    mov eax, ebx
    shl eax, 8
    shr ebx, 6
    and eax, $3F00
    or ebx, $80C0
    or ebx, eax

  dec edx
  mov [edi-1], bx
  jnz @loop
  jmp @loop_end  
  @three:
    // три символа
    sub edx, 2
    jle @loop_end

    // wc := (((wc shl 6) or (c[0] and $3F)) shl 6) or (c[1] and $3F);
    movzx eax, word ptr [esi]
    shl ebx, 12
    mov ecx, eax
    and ebx, $FFFF
    and eax, $C0C0
    and ecx, $3F3F
    cmp eax, $8080
    jne @loop_end
    mov eax, ecx
    and ecx, $FF
    shr eax, 8
    shl ecx, 6
    //lea ebx, [ebx + eax + ecx] : ecx |= eax
    or ecx, eax
    add edi, 2
    or ebx, ecx

    // Destination[i] := WideChar(W + wine_casemap_lower[wine_casemap_lower[W shr 8] + byte(W)]);
    mov eax, ebx
    and ecx, $FF
    shr eax, 8
    add esi, 2
    movzx eax, word ptr [wine_casemap_lower + eax*2]
    add ecx, ecx
    add bx, word ptr [wine_casemap_lower + eax*2 + ecx]

    //Dest[count] := ansichar($E0 or (c shr 12));
    //Dest[count+1] := ansichar($80 or ((c shr 6) and $3F));
    //Dest[count+2] := ansichar($80 or (c and $3F));
    mov eax, ebx
    shr eax, 12
    or  eax, $E0
    mov [edi-2], al
    mov eax, ebx

    shr ebx, 6
    mov bh, al
    and ebx, $3F3F
    or ebx, $8080

  dec edx
  mov [edi-1], bx
  jnz @loop
  @loop_end:


@end:
@postfix:
  pop ebx
  pop edi
  pop esi
@exit:
end;

function Utf8FromUtf16(const Utf8Chars: pansichar; const Utf8Length: integer; const WideChars: PWideChar; const WideLength: integer): integer;
const
  NEED_ONE = not $7F;
  NEED_TWO = not $7FF;
var
  Utf8Buffer: pointer;
asm
  test eax, eax
  jz @exit0
  test edx, edx
  jle @exit0
  test ecx, ecx
  jz @exit0
  cmp WideLength, 0
  jle @exit0

jmp @prefix
@exit0:
  xor eax, eax
  jmp @exit
@prefix:
  { push/initialization }
  push ebx
  push edi
  push esi
  mov Utf8Buffer, eax // временно сохраняем буфер (нужен для вычесления длинны)
@begin:
  mov edi, eax // куда копируем (Utf8Chars)
  mov esi, ecx // откуда берём символы (WideChars)
  inc edx // edx - сколько символов можем юзать в выходном буфере + 1
  mov ecx, WideLength // сколько символов надо раскодировать


  @loop:
    movzx ebx, word ptr [esi]
    dec edx
    jz @loop_end

    add esi, 2

    test ebx, NEED_ONE
    jz @need_one
    test ebx, NEED_TWO
    jz @need_two
    @need_three:
      sub edx, 2
      mov eax, ebx
      jle @loop_end

      //Dest[count] := ansichar($E0 or (c shr 12));
      //Dest[count+1] := ansichar($80 or ((c shr 6) and $3F));
      //Dest[count+2] := ansichar($80 or (c and $3F));

      shr eax, 12
      or  eax, $E0
      mov [edi], al
      mov eax, ebx

      shr ebx, 6
      mov bh, al
      add edi, 3
      and ebx, $3F3F
      or ebx, $8080

  dec ecx
  mov [edi-2], bx
  jnz @loop
  jmp @loop_end
    @need_two:
      dec edx
      mov eax, ebx
      jz @loop_end

      //Dest[count] := ansichar($C0 or (c shr 6));
      //Dest[count+1] := ansichar($80 or (c and $3F));
      shl eax, 8
      shr ebx, 6
      and eax, $3F00
      or ebx, $80C0
      add edi, 2
      or ebx, eax

  dec ecx
  mov [edi-2], bx
  jnz @loop
  jmp @loop_end
    @need_one:
      mov [edi], bl
      inc edi
  dec ecx
  jnz @loop
  @loop_end:

@end:
  mov eax, edi
  sub eax, Utf8Buffer

@postfix:
  pop esi
  pop edi
  pop ebx

@exit:
end;


function Utf8FromAnsi(const Utf8Chars: pansichar; const Utf8Length: integer; const AnsiChars: pansichar; const AnsiLength: integer): integer;
const
  UTF8MAP_OFFSET = 128*4;
asm
  test eax, eax
  jz @exit0
  test edx, edx
  jle @exit0
  test ecx, ecx
  jz @exit0
  cmp AnsiLength, 0
  jle @exit0

jmp @prefix
@exit0:
  xor eax, eax
  jmp @exit
@prefix:
  push esi
  push edi
  push ebx
  // push esp

@begin:
  mov esi, ecx
  mov edi, eax
  mov ecx, AnsiLength
  mov esp, edx
  mov AnsiLength, eax // save source pointer
  {
     edi - буфер utf8 символов
     esi - буфер ansi символов
     ecx - счётчик ansi символов которые нужно преобразовать
     esp - счётчик конечного буфера

     ebx - буфер для хранения 4х ansi символов
     eax, edx - вспомогательные буферы
  }

  sub ecx, 4
  jl @loop_end
  @loop:
    mov ebx, [esi]
    test ebx, $80808080
    jz @simple

    mov edx, 4
    @not_simple_loop:
        test ebx, $80
        jz @one
        movzx eax, bl
        shr ebx, 8
        mov eax, [offset map_ansi_to_utf8 + eax*4 - UTF8MAP_OFFSET]
        test eax, 1
        jz @two

      @three:
        sub esp, 3
        jl @end
        shr eax, 8
        mov [edi], ax
        shr eax, 16
        mov [edi+2], al
        add edi, 3

      jmp @not_simple_loop_continue
      @two:
        sub esp, 2
        jl @end
        shr eax, 8
        mov [edi], ax
        add edi, 2

      jmp @not_simple_loop_continue
      @one:
        dec esp
        jl @end
        mov [edi], bl
        inc edi
        shr ebx, 8
    @not_simple_loop_continue:
    dec edx
    jnz @not_simple_loop

  jmp @loop_continue
  @simple:
    sub esp, 4
    jge @s_fill_4
    lea edx, [esp + 4]
    jmp [offset @s_fill_jumps + edx*4]
    @s_fill_jumps: DD @end, @s_fill_1, @s_fill_2, @s_fill_3

    @s_fill_1:
      mov [edi], bl
      inc edi
      jmp @end
    @s_fill_2:
      mov [edi], bx
      add edi, 2
      jmp @end
    @s_fill_3:
      mov [edi], bx
      shr ebx, 16
      add edi, 3
      mov [edi-1], bl
      jmp @end
    @s_fill_4:
      mov [edi], ebx
      add edi, 4
  @loop_continue:
    add esi, 4
    sub ecx, 4
    jge @loop
  @loop_end:


add ecx, 4
jle @end
jmp [offset @last_corrections + ecx*4 -4]
@last_corrections: DD @last1, @last2, @last3

@last1:
  movzx ebx, byte ptr [esi]
  xor ecx, ecx
  mov edx, 1
  jmp @not_simple_loop
@last2:
  movzx ebx, word ptr [esi]
  xor ecx, ecx
  mov edx, 2
  jmp @not_simple_loop
@last3:
  movzx ebx, byte ptr [esi+2]
  shl ebx, 16
  xor ecx, ecx
  mov bx, [esi]
  mov edx, 3
  jmp @not_simple_loop

@end:
  mov eax, edi
  sub eax, AnsiLength
@postfix:
  lea esp, [ebp-12]
  pop ebx
  pop edi
  pop esi
@exit:
end;

{$endif .LUA_UNICODE}


// Ansi <-- Utf8
function AnsiFromUtf8(const AnsiChars: pansichar; const AnsiLength: integer; const Utf8Chars: pansichar; const Utf8Length: integer): integer;
asm
  test eax, eax
  jz @exit0
  test edx, edx
  jle @exit0
  test ecx, ecx
  jz @exit0
  cmp Utf8Length, 0
  jle @exit0

jmp @prefix
@exit0:
  xor eax, eax
  jmp @exit
@prefix:
  push esi
  push edi
  push ebx
  // push esp

@begin:
  mov esi, ecx
  mov edi, eax
  mov ecx, Utf8Length
  mov esp, edx
  mov Utf8Length, eax // save source pointer
  {
     edi - буфер ansi символов
     esi - буфер utf8 символов
     ecx - счётчик utf8 символов которые нужно преобразовать
     esp - счётчик конечного буфера

     ebx - буфер для хранения 4х байт utf8
     eax, edx - вспомогательные буферы
  }

  // 7 (0..127) - 0xxxxxxx(7)
  // 11 (128..$7FF) - 110xxxxx(5) 10xxxxxx(6)
  // 16 ($800..$FFFF) - 1110xxxx(4) 10xxxxxx(6) 10xxxxxx(6)
  //
  // 4 бит ($0F), 5 бит($1F), 6 бит($3F)

  sub ecx, 4
  jl @loop_end
  @loop:
    mov ebx, [esi]
    test ebx, $80808080
    jz @simple

    add ecx, 4
    test ebx, $80
    jz @fill_char

    //  utf8(ebx) --> utf16(ebx)
    test ebx, $20
    jz @two
    @three:
      mov eax, ebx
      and ebx, $0F
      mov edx, eax
      and eax, $3F00
      shl ebx, 12
      and edx, $3F0000
      shr eax, 2
      shr edx, 16
      or ebx, eax
      add esi, 2
      or ebx, edx
      sub ecx, 2
    jmp @utf16_decode
    @two:
      mov edx, ebx
      and ebx, $1F
      and edx, $3F00
      shl ebx, 6
      shr edx, 8
      inc esi
      or ebx, edx
      dec ecx 

    // utf16(ebx) --> ansi(bl)
    @utf16_decode:
      mov edx, ebx
      and edx, 127
      movzx edx, byte ptr [map_utf16_to_ansi.hash_array + edx]
      @hash_loop:
        cmp edx, 255
        jne @hash_loop_continue
           mov ebx, '?'
           jmp @fill_char
        @hash_loop_continue:
        cmp word ptr [offset map_ansi_to_wide + edx*2], bx
        je @hash_loop_end
      movzx edx, byte ptr [map_utf16_to_ansi.item_nexts + edx]
      jmp @hash_loop

      @hash_loop_end:
      lea ebx, [edx+128]

  @fill_char:
    dec esp
    jl @end

    mov [edi], bl
    inc esi
    dec ecx
    inc edi
  jmp @loop_continue
  @simple:
    add esi, 4
    sub esp, 4
    jge @s_fill_4
    lea edx, [esp + 4]
    jmp [offset @s_fill_jumps + edx*4]
    @s_fill_jumps: DD @end, @s_fill_1, @s_fill_2, @s_fill_3

    @s_fill_1:
      mov [edi], bl
      inc edi
      jmp @end
    @s_fill_2:
      mov [edi], bx
      add edi, 2
      jmp @end
    @s_fill_3:
      mov [edi], bx
      shr ebx, 16
      add edi, 3
      mov [edi-1], bl
      jmp @end
    @s_fill_4:
      mov [edi], ebx
      add edi, 4

  @loop_continue:
    sub ecx, 4
    jge @loop
  @loop_end:

add ecx, 4
jle @end
jmp [offset @last_corrections + ecx*4 -4]
@last_corrections: DD @last1, @last2, @last3

@last1:
  movzx ebx, byte ptr [esi]
  test ebx, $80
  jz @fill_char
jmp @end
@last2:
  movzx ebx, word ptr [esi]
  test ebx, $80
  jz @fill_char
  test ebx, $20
  jz @two

jmp @end  
@last3:
  movzx ebx, byte ptr [esi+2]
  shl ebx, 16
  mov bx, [esi]

  test ebx, $80
  jz @fill_char
  test ebx, $20
  jz @two
  test ebx, $10
  jz @three

@end:
  mov eax, edi
  sub eax, Utf8Length
@postfix:
  lea esp, [ebp-12]
  pop ebx
  pop edi
  pop esi
@exit:
end;


// Ansi <-- Wide
procedure AnsiFromUtf16(const AnsiChars: pansichar; const WideChars: PWideChar; const Length: integer);
asm
  test eax, eax
  jz @exit
  test edx, edx
  jz @exit
  test ecx, ecx
  jle @exit

@prefix:
  push ebx
  push esi
  push edi
@begin:

  mov esi, edx
  mov edi, eax
  sub ecx, 2
  jl @loop_pairs_end
  @loop_pairs:
    mov eax, [esi]
    mov ebx, eax
    test eax, $FF80FF80
    jz @simple

    and eax, $0000FFFF
    shr ebx, 16
  @1:
    cmp eax, 127
    mov edx, eax
    jna @1_fill

    and edx, 127
    movzx edx, byte ptr [map_utf16_to_ansi.hash_array + edx]
    @hash_loop_1:
      cmp edx, 255
      jne @hash_loop_1_continue
         mov byte ptr [edi+00], '?'
         jmp @2
      @hash_loop_1_continue:
      cmp word ptr [offset map_ansi_to_wide + edx*2], ax
      je @hash_loop_1_end
    movzx edx, byte ptr [map_utf16_to_ansi.item_nexts + edx]
    jmp @hash_loop_1

    @hash_loop_1_end:
    lea eax, [edx+128]
  @1_fill: mov [edi+00], al
  @2:
    cmp ebx, 127
    mov edx, ebx
    jna @2_fill

    and edx, 127
    movzx edx, byte ptr [map_utf16_to_ansi.hash_array + edx]
    @hash_loop_2:
      cmp edx, 255
      jne @hash_loop_2_continue
         mov byte ptr [edi+01], '?'
         jmp @loop_pairs_continue
      @hash_loop_2_continue:
      cmp word ptr [offset map_ansi_to_wide + edx*2], bx
      je @hash_loop_2_end
    movzx edx, byte ptr [map_utf16_to_ansi.item_nexts + edx]
    jmp @hash_loop_2

    @hash_loop_2_end:
    lea ebx, [edx+128]
  @2_fill: mov [edi+01], bl
  @loop_pairs_continue:
    add esi, 4
    add edi, 2
    sub ecx, 2
    jge @loop_pairs
  jmp @loop_pairs_end
  @simple:
    shr ebx, 8
    add edi, 2
    or eax, ebx
    add esi, 4
    mov [edi-2], ax
    sub ecx, 2
  jge @loop_pairs
  @loop_pairs_end:

add ecx, 2
jle @postfix
   movzx ebx, word ptr [esi]
   xor ecx, ecx
   dec edi
   jmp @2

@postfix:
  pop edi
  pop esi
  pop ebx
@exit:
end;

function Utf16FromUtf8(const WideChars: PWideChar; const WideLength: integer; const Utf8Chars: pansichar; const Utf8Length: integer): integer;
var
  WideBufferLength: integer;
asm
  test eax, eax
  jz @exit0
  test edx, edx
  jle @exit0
  test ecx, ecx
  jz @exit0
  cmp Utf8Length, 0
  jle @exit0

jmp @prefix
@exit0:
  xor eax, eax
  jmp @exit
@prefix:
  { push/initialization }
  push ebx
  push edi
  push esi
  mov WideBufferLength, edx
@begin:

  mov esi, ecx // откуда копируем - Utf8Chars
  mov edi, eax // куда копируем - WideChars
  xor ecx, ecx // количество записанных символов
  mov edx, Utf8Length // количество оставшихся utf8 символов
  // ebx - wc
  // eax - c

  dec esi
  inc edx
  @loop:
    inc esi
    dec edx
    jz @loop_end

    movzx ebx, byte ptr [esi] // cw
    test ebx, $80
    jz @loop_continue

    and ebx, $3F

    // if (wc and $20) <> 0 then
    test ebx, $20
    jz @look_next
      inc esi
      dec edx
      jz @end

      movzx eax, byte ptr [esi] // c
      shl ebx, 6
      test eax, $80
      jz @end
      and eax, $7F
      test eax, $40
      jne @end

      or ebx, eax
    @look_next:

    inc esi
    dec edx
    jz @loop_end
    movzx eax, byte ptr [esi] // c
    shl ebx, 6
    test eax, $80
    jz @end
    and eax, $7F
    test eax, $40
    jne @end
    or ebx, eax

  @loop_continue:
    mov [edi], bx
    inc ecx
    add edi, 2
    cmp ecx, WideBufferLength
    jl @loop
  @loop_end:


@end:
  mov eax, ecx

@postfix:
  pop esi
  pop edi
  pop ebx

@exit:
end;


procedure Utf16FromAnsi(const WideChars: PWideChar; const AnsiChars: pansichar; const Length: integer);
const
  SUB_OFFSET = 128*sizeof(WideChar);
asm
  test eax, eax
  jz @exit
  test edx, edx
  jz @exit
  test ecx, ecx
  jle @exit

@prefix:
  push ebx
  push esi
  push edi
  push ebp
@begin:

  lea esi, [edx-4]
  lea edi, [eax-8]
  jmp @loop_fourth_continue
  @loop_fourth:
    mov eax, [esi]
    test eax, $80808080
    jz @simple
  @1:
    movzx ebx, al
    shr eax, 8
    cmp ebx, 127
    jna @1_fill
      mov bx, word ptr [offset map_ansi_to_wide + ebx*2 - SUB_OFFSET]
  @1_fill: mov [edi+00], bx
  @2:
    movzx edx, al
    shr eax, 8
    cmp edx, 127
    jna @2_fill
      mov dx, word ptr [offset map_ansi_to_wide + edx*2 - SUB_OFFSET]
  @2_fill: mov [edi+02], dx
  @3:
    movzx ebx, al
    shr eax, 8
    cmp ebx, 127
    jna @3_fill
      mov bx, word ptr [offset map_ansi_to_wide + ebx*2 - SUB_OFFSET]
  @3_fill: mov [edi+04], bx
  @4:
    cmp eax, 127
    jna @4_fill
      mov ax, word ptr [offset map_ansi_to_wide + eax*2 - SUB_OFFSET]
  @4_fill: mov [edi+06], ax

  //jmp @loop_fourth_continue
    add esi, 4
    add edi, 8
    sub ecx, 4
  jge @loop_fourth
  jmp @loop_fourth_end  
  @simple:
    mov edx, eax
    and eax, $FF
    mov ebx, edx
    shl edx, 8
    mov ebp, ebx
    and edx, $00FF0000
    and ebx, $FF000000
    or  eax, edx
    and ebp, $00FF0000
    shr ebx, 8
    shr ebp, 16
    mov [edi], eax
    or  ebx, ebp
    mov [edi+4], ebx
  @loop_fourth_continue:
    add esi, 4
    add edi, 8
    sub ecx, 4
  jge @loop_fourth
  @loop_fourth_end:

add ecx, 4
jle @postfix

xor eax, eax
jmp [offset @last_corrections + ecx*4 -4]
@last_corrections: DD @last1, @last2, @last3
@last3:
  mov al, [esi+2]
  xor ecx, ecx
  shl eax, 16
  sub edi, 2
  mov ax, [esi]
  jmp @2
@last2:
  mov ax, [esi]
  sub edi, 4
  xor ecx, ecx
  jmp @3
@last1:
  mov al, [esi]
  sub edi, 6
  xor ecx, ecx
  jmp @4

@postfix:
  pop ebp
  pop edi
  pop esi
  pop ebx
@exit:
end;




procedure WideFromAnsi(CodePage: integer; Dest: PWideChar; Source: pansichar; Length: Integer);
begin
{$IFDEF MSWINDOWS}
   Windows.MultiByteToWideChar(CodePage, 0, Source, Length, Dest, Length);
{$ELSE}
   {$MESSAGE ERROR 'Unicode decoder not defined'}
{$ENDIF}
end;


procedure InitializeLocaleCodePage(const Value: integer=0);
const
  ENG_OFFSET = ord(ansichar('A'))-ord(ansichar('a'));
var
  i: integer;
  buffer: dword;
  next: byte;

  {$ifdef LUA_UNICODE}
  w_value: WideChar;
  W: WideChar;
  A: ansichar;
  {$endif}

  ansichars: array[0..255] of ansichar;
begin
  // заполнение всех ansi символов
  for i := 0 to 255 do ansichars[i] := ansichar(i);

  // Unicode
  WideFromAnsi(0, @map_ansi_to_wide[0], @ansichars[128], 128);

  // Utf8
  {$ifdef LUA_UNICODE}
  for i := 0 to 127 do
  begin
    buffer := 0;
    pbyte(@buffer)^ := Utf8FromUtf16(pointer(integer(@buffer)+1), 3, @map_ansi_to_wide[i], 1);

    map_ansi_to_utf8[i] := buffer;
  end;
  {$endif}

  // utf16 --> ansi
  FillChar(map_utf16_to_ansi, sizeof(map_utf16_to_ansi), 255);
  for i := 0 to 127 do
  begin
    // hash функция
    buffer := ord(map_ansi_to_wide[i]) and 127;

    // указатель на текущий элемент в массиве
    next := map_utf16_to_ansi.hash_array[buffer];

    // занести указательна next в "себя"
    map_utf16_to_ansi.item_nexts[i] := next;

    // занести указатель на "себя" в hash массив
    map_utf16_to_ansi.hash_array[buffer] := i;
  end;

  // Lower/Upper
  {$ifdef LUA_UNICODE}
  for i := 0 to 255 do
  begin
    A := ansichar(i);
    
    pansichar(@LowerChars[A])^ := A;
    pansichar(@UpperChars[A])^ := A;
    case A of
      'a'..'z': inc(pansichar(@UpperChars[A])^, ENG_OFFSET);
      'A'..'Z': dec(pansichar(@LowerChars[A])^, ENG_OFFSET);
    else
    if (A > #127) then
    begin
      W := map_ansi_to_wide[ord(A)-128];

      // upper_case: Destination[i] := WideChar(W + wine_casemap_upper[wine_casemap_upper[W shr 8] + byte(W)]);
      w_value := WideChar(word(W) + wine_casemap_upper[wine_casemap_upper[word(W) shr 8] + byte(W)]);
      if (w_value <> W) then
      begin
        AnsiFromUtf16(@UpperChars[A], @w_value, 1);
        if (UpperChars[A] = '?') then pansichar(@UpperChars[A])^ := A;
      end;

      // lower_case: Destination[i] := WideChar(W + wine_casemap_lower[wine_casemap_lower[W shr 8] + byte(W)]);
      w_value := WideChar(word(W) + wine_casemap_lower[wine_casemap_lower[word(W) shr 8] + byte(W)]);
      if (w_value <> W) then
      begin
        AnsiFromUtf16(@LowerChars[A], @w_value, 1);
        if (LowerChars[A] = '?') then pansichar(@LowerChars[A])^ := A;
      end;
    end;  
    end;
  end;
  {$endif}    
end;







// --<<<------- Unicode рутина модуля SysUtilsEx -----------------------------
{$endif}




{$ifdef NO_CRYSTAL}
// ************************************************************************* //
// ------------   SysUtilsEx-рутина  ------------------------------------------
type
  // дублирование из System. за исключением "X"
  PFieldInfo = ^TFieldInfo;
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    {X: Word; убирается для того чтобы FieldTable поровнялся с GetTypeData}
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;


function InstancePath(): string;
var
  PATH: array[0..MAX_PATH] of char;
begin
  if (System.IsLibrary) then
  begin
  {$ifdef MSWINDOWS}
    GetModuleFileName(hInstance, PATH, MAX_PATH);
    Result := PATH;
  {$else}
    {$MESSAGE ERROR 'InstancePath() for libraries not defined'}
  {$endif}
  end else
  begin
    Result := paramstr(0);
  end;
end;

function IncludePathDelimiter(const S: string): string;
asm
  jmp SysUtils.IncludeTrailingPathDelimiter
end;

function SharedFileStream(const FileName: string): TFileStream;
begin
  Result := TFileStream.Create(FileName, fmShareDenyNone);
end;

function  EnumName(tpinfo: PTypeInfo; Value: byte): string;
begin
  if (tpinfo = nil) then
    Result := IntToStr(Value)
  else
    Result := TypInfo.GetEnumName(tpinfo, integer(Value));
end;


function IsMemoryZeroed(const Memory: pointer; const Size: integer): boolean;
asm
  push edi
  mov ecx, edx
  mov edi, eax
  xor eax, eax

  test edi, edi
  jz @fail
  test ecx, ecx
  jle @fail

// проверить цепочку dword-ов
  shr ecx, 2
  jz @bytes

  REPE SCASD
  jne @fail
  
// проверить оставшуюся цепочку байт 0..3 (edx)
@bytes:
  test edx, 2
  jz @_1_byte
  cmp [edi], ax
  jnz @fail
  add edi, 2

@_1_byte:
  test edx, 1
  jz @ret_true
  cmp [edi], al
  jnz @fail

@ret_true:
  mov eax, 1
  pop edi
  ret
@fail:
  xor eax, eax
  pop edi
end;

// todo удалить ------------->>>>>>>>>>>>>>>>>>>>
function CharPos(const C: char; const S: string): integer;
asm
  push ebx
  push edi
  //if StrLenght > 0 then
  test edx,edx
  jz   @Else1Begin
  //StrLenght := Length(Str);
  mov  edi,[edx-$04]
  //I := 0;
  xor  ecx,ecx
  dec  edx
@RepeatBegin :
  //Inc(I);
  inc  ecx
  //until((Str[I] = Chr) or (I > StrLenght));
  movzx ebx, byte ptr [edx+ecx]
  //cmp  al,[edx+ecx]
  cmp  al, bl
  jz   @Exit
  cmp  edi,ecx
  jne  @RepeatBegin
@Else1Begin :
  //Result := 0;
  xor  eax,eax
  pop  edi
  pop  ebx
  ret
@Exit :
  mov  eax,ecx
  pop  edi
  pop  ebx
end;

function CharPosEx(const C: char; const S: string; const StartFrom: dword): integer;
asm
  push ebx
  push edi

  // if (S <> '') and (StartFrom <> 0)
  test edx,edx
  jz   @ZeroResult
  test ecx,ecx
  jz   @ZeroResult

  mov  edi, [edx-$04] // edi := Length(S)
  dec  edx // сместить указатель, чтобы потом S[+Position] = символ

  // проверка: выход за границы | StartFrom > Length
  cmp  ecx, edi
  jg @ZeroResult

dec ecx // временно декрементировать стартовую позицию
@RepeatBegin :
  //Inc(I);
  inc  ecx
  //until((Str[I] = Chr) or (I > StrLenght));
  movzx ebx, byte ptr [edx+ecx]
  //cmp  al,[edx+ecx]
  cmp  al, bl
  jz   @Exit
  cmp  edi,ecx
  jne  @RepeatBegin
@ZeroResult:
  //Result := 0;
  xor  eax,eax
  pop  edi
  pop  ebx
  ret
@Exit :
  mov  eax,ecx
  pop  edi
  pop  ebx
end;

function  StringHash(const S: string): integer; overload;
asm
  test eax, eax
  jz @exit
  mov edx, [eax-4]
  test edx, edx
  jz @exit

  push ebx
  push esi
  mov ebx, edx
  xor ecx, ecx
  shl ebx, 12

  test edx, edx
  jz @no_loop

  @loop:
    inc ecx
    movzx esi, byte ptr [eax+edx-1]
    and ecx, $f
   // movzx esi, byte ptr [LowerChars + esi]
    ror ebx, cl
    lea esi, [esi + edx*8]
    xor ebx, esi

  dec edx
  jnz @loop

  @no_loop:
  mov eax, ebx
  and eax, $7FFFFFFF
  pop esi
  pop ebx

  @exit:
end;

function  StringHash(const S: pchar; const SLength: integer): integer; overload;
asm
  test eax, eax
  jz @exit
  test edx, edx
  jg @1
  xor eax, eax
  ret

@1:
  push ebx
  push esi
  mov ebx, edx
  xor ecx, ecx
  shl ebx, 12

  test edx, edx
  jz @no_loop

  @loop:
    inc ecx
    movzx esi, byte ptr [eax+edx-1]
    and ecx, $f
    //movzx esi, byte ptr [LowerChars + esi]
    ror ebx, cl
    lea esi, [esi + edx*8]
    xor ebx, esi

  dec edx
  jnz @loop

  @no_loop:
  mov eax, ebx
  and eax, $7FFFFFFF
  pop esi
  pop ebx

  @exit:
end;


function SameStrings (const S1, S2 : string) : boolean; overload;
asm
   cmp eax, edx
   je @exit_true
   test eax, eax
   jz @exit_false
   test edx, edx
   jz @exit_false
   mov ecx, [eax-4]
   cmp ecx, [edx-4]
   jne @exit_false

   push ebx
   jmp @1

@loop_3:
   dec ecx
   mov bl, [eax+ecx]
   cmp bl, [edx+ecx]
   jne @exit_false_pop
@1:test ecx, 3
   jnz @loop_3

   shr ecx, 2
   jz @exit_true_pop
   mov ebx, [eax + ecx*4 - 4]
   cmp ebx, [edx + ecx*4 - 4]
   jne @exit_false_pop
   dec ecx
   jz @exit_true_pop

   cmp ecx, 7
   jle @loop_dword

   push esi
   push edi
     mov esi, eax
     mov edi, edx
     xor eax, eax
     REPE CMPSD
     sete al
   pop edi
   pop esi
   pop ebx
   ret

@loop_dword:
   mov ebx, [eax + ecx*4 - 4]
   cmp ebx, [edx + ecx*4 - 4]
   jne @exit_false_pop
   dec ecx
   jnz @loop_dword

@exit_true_pop:
   pop ebx
   mov eax, 1
   ret
@exit_false_pop:
   pop ebx
@exit_false:
   xor eax, eax
   ret
@exit_true:
   mov eax, 1
end;

function SameStrings (const S1: string; const S2: pchar; const S2Length: integer) : boolean; overload;
asm
   cmp eax, edx
   je @exit_true
   test eax, eax
   jz @exit_false
   test edx, edx
   jz @exit_false
   test ecx, ecx
   jle @exit_false
   cmp ecx, [eax-4]
   jne @exit_false

   push ebx
   jmp @1

@loop_3:
   dec ecx
   mov bl, [eax+ecx]
   cmp bl, [edx+ecx]
   jne @exit_false_pop
@1:test ecx, 3
   jnz @loop_3

   shr ecx, 2
   jz @exit_true_pop
   mov ebx, [eax + ecx*4 - 4]
   cmp ebx, [edx + ecx*4 - 4]
   jne @exit_false_pop
   dec ecx
   jz @exit_true_pop

   cmp ecx, 7
   jle @loop_dword

   push esi
   push edi
     mov esi, eax
     mov edi, edx
     xor eax, eax
     REPE CMPSD
     sete al
   pop edi
   pop esi
   pop ebx
   ret

@loop_dword:
   mov ebx, [eax + ecx*4 - 4]
   cmp ebx, [edx + ecx*4 - 4]
   jne @exit_false_pop
   dec ecx
   jnz @loop_dword

@exit_true_pop:
   pop ebx
   mov eax, 1
   ret
@exit_false_pop:
   pop ebx
@exit_false:
   xor eax, eax
   ret
@exit_true:
   mov eax, 1
end;

// <<<<<<<<<<<<------------- удалить



function  IntPos(Value: integer; Arr: pinteger; ArrLength: integer): integer;
asm
   test ecx, ecx
   jnz @find // учитывать знак ?
   mov eax, -1
   ret

@find:
   push edi

   mov edi, edx
   repne scasd
   je @calc_result
      mov eax, -1
      pop edi
      ret
@calc_result:

   {mov eax, edi
   sub eax, edx
   shr eax, 2
   dec eax   }

   neg edx
   lea eax, [edi + edx - 4]
   shr eax, 2

   pop edi
end;


function InsortedPlace8(Value: integer; Arr: pointer; ArrLength: integer): integer;
//const
//  SIZE = 8;
asm
  test ecx, ecx
  jg @1
  xor eax, eax
  ret
@1:
  push ebx
  push ecx
  push edi

  mov ebx, eax
  dec ecx
  xor eax, eax

  // Arr = edx
  // Value = ebx
  // L = eax
  // R = ecx
  // C = edi

  lea edi, [eax+ecx]
  shr edi, 1

  // while (L <> C)
  cmp eax, edi
  je @endloop

  // cmp Value, Arr[C]
@loop:
  //cmp ebx, [edx+edi*SIZE]
  cmp ebx, [edx+edi*8]
  jle @4
    // L := C; C := (L+R) shr 1;
    mov eax, edi
    add edi, ecx
    shr edi, 1
    cmp eax, edi
    jne @loop
    jmp @endloop
  @4:
    // R := C; C := (L+R) shr 1;
    mov ecx, edi
    add edi, eax
    shr edi, 1
    cmp eax, edi
  jne @loop

@endloop:
  pop edi
  pop ecx
  // while (L < ArrLength) and ( Value > pinteger(integer(Arr)+L*4)^ ) do Inc(L);
  // Result := L;
  @while:
    //cmp ebx, [edx+eax*SIZE]
    cmp ebx, [edx+eax*8]
    jle @exit
    inc eax
    cmp eax, ecx
  jl @while

@exit:
  pop ebx
end;


procedure QuickSort4(Arr: pointer; L, R: integer);
//const
//  SIZE = 4;
var
  R_: integer;
asm
  push ebx // = Value
  push esi // = I
  push edi // = J
           // eax - Arr
           // edx - L
           // ecx - Buf

     // R_ := R; I := L;
     mov R_, ecx
     mov esi, edx

     @repeat_1:
       // J := R;
       mov edi, R_
       // Value := pinteger(integer(Arr)+((L + J) shr 1)*SIZE)^;
       lea ebx, [edx + edi]
       shr ebx, 1
       //mov ebx, [eax+ebx*SIZE]
       mov ebx, [eax+ebx*4]

       push edx // edx - дополнительный буфер
       @repeat_2:

          jmp @1
          @while_1: // while pinteger(integer(Arr)+I*SIZE)^ < Value do Inc(I);
             inc esi
          @1://cmp ebx, [eax+esi*SIZE]
             cmp ebx, [eax+esi*4]
          jg @while_1

          jmp @2
          @while_2: // while pinteger(integer(Arr)+J*SIZE)^ > Value do Dec(J);
             dec edi
          @2://cmp ebx, [eax+edi*SIZE]
             cmp ebx, [eax+edi*4]
          jl @while_2

          // if I <= J then Arr[I] <--> Arr[J]
          cmp esi, edi
          jg @end_repeat_2
          // swap + inc/dec + jmp @repeat_2
  
            mov ecx, [eax+esi*4]
            mov edx, [eax+edi*4]
            mov [eax+edi*4], ecx
            mov [eax+esi*4], edx

            inc esi
            dec edi

            // until I > J
            cmp esi, edi
            jle @repeat_2
          //  
       @end_repeat_2:
       pop edx

       // рекурсия: if L < J then QuickSort4(Arr, L, J);
       cmp edx, edi
       jnl @after_recursy
         mov ecx, edi
         call QuickSort4
       @after_recursy:
   // L := I;
     mov edx, esi
   // until I >= R;
     cmp esi, R_
     jl @repeat_1

  pop edi
  pop esi
  pop ebx
end;

function InsortedPlace4(Value: integer; Arr: pointer; ArrLength: integer): integer;
//const
//  SIZE = 4;
asm
  test ecx, ecx
  jg @1
  xor eax, eax
  ret
@1:
  push ebx
  push ecx
  push edi

  mov ebx, eax
  dec ecx
  xor eax, eax

  // Arr = edx
  // Value = ebx
  // L = eax
  // R = ecx
  // C = edi

  lea edi, [eax+ecx]
  shr edi, 1

  // while (L <> C)
  cmp eax, edi
  je @endloop

  // cmp Value, Arr[C]
@loop:
  //cmp ebx, [edx+edi*SIZE]
  cmp ebx, [edx+edi*4]
  jle @4
    // L := C; C := (L+R) shr 1;
    mov eax, edi
    add edi, ecx
    shr edi, 1
    cmp eax, edi
    jne @loop
    jmp @endloop
  @4:
    // R := C; C := (L+R) shr 1;
    mov ecx, edi
    add edi, eax
    shr edi, 1
    cmp eax, edi
  jne @loop

@endloop:
  pop edi
  pop ecx
  // while (L < ArrLength) and ( Value > pinteger(integer(Arr)+L*4)^ ) do Inc(L);
  // Result := L;
  @while:
    //cmp ebx, [edx+eax*SIZE]
    cmp ebx, [edx+eax*4]
    jle @exit
    inc eax
    cmp eax, ecx
  jl @while

@exit:
  pop ebx
end;

function InsortedPos4(Value: integer; const DynArray): integer;
asm
  mov edx, [edx]
  test edx, edx
  jz @fail_2

    mov ecx, [edx-4] {!}
    {$ifdef fpc} inc ecx {$endif}
    push eax

    call InsortedPlace4
    cmp eax, ecx  // if (Result >= ArrLength)
    jge @fail_1

    // if (pinteger( integer(Arr)+Result*4 )^ <> Value)
    mov ecx, [edx + eax*4]
    pop edx // value
    cmp ecx, edx
    jne @fail_2
    ret

@fail_1:
  pop eax
@fail_2:
  mov eax, -1
end;


procedure FillDword(var Dest; count: Integer; Value: dword);
asm
  test edx, edx
  jnle @1
  ret
@1:
  push edi
  mov edi, eax
  mov eax, ecx
  mov ecx, edx

  REP  STOSD 

  pop edi
end;    

// <<<<<<<<<<<<------------- удалить


function TypeKindName(const Kind: TTypeKind): string; forward;

function WrongDynType(tpinfo: PTypeInfo): string;
begin
  Result := Format('Указанный TypeInfo(%s) не является динамическим массивом',
                      [TypeKindName(tpinfo.Kind)]);
end;

function DynArrayLength(var DynArray): integer;
asm
  mov eax, [eax]
  test eax, eax
  jz @exit
    mov eax, [eax-4]
    {$ifdef fpc} {FreePascal!} inc eax {$endif}
  @exit:
end;

// ToDO удалить !

function DynArrayInsert(var DynamicArray; const tpinfo: PTypeInfo; Start: integer; const Count: integer = 1): pointer;
const
  Length_Start_Count = 'Length = %d, Start = %d, Count = %d';
var
  Len, NewLen, elSize: integer;
  ArrInfo: PTypeData;//PDynArrayTypeInfo absolute tpinfo;
  DestCpyPtr: pointer;
  ArrIsStr: boolean;
begin
  ArrIsStr := (tpinfo.Kind in [tkLString{$ifdef fpc},tkAString{$endif}, tkWString]);
  Len := DynArrayLength(DynamicArray);
  {$ifdef fpc} if (ArrIsStr) and (Len <> 0) then dec(Len); {$endif}
  if (tpinfo.Kind = tkWString) then Len := Len shr 1;
  if (ArrIsStr) then dec(Start);

  if (Start < 0) or (Start > Len) or (Count <= 0) then
    TExcept.Assert(Length_Start_Count, [Len, Start+ord(ArrIsStr), Count]);

  NewLen := Len + Count;

  case (tpinfo.Kind) of
     tkDynArray: {стандартные действия, обработка внизу} ;
      tkLString{$ifdef fpc},tkAString{$endif}: begin
                   // обычная строка
                   // внимание, здесь используется другой индекс!!!
                   SetLength(string(DynamicArray), NewLen);
                   Result := pointer(integer(DynamicArray)+Start);

                   if (Start<>Len) then
                   //CopyMemory(pointer(integer(Result)+Count), Result, Len-Start);
                   Move(Result^, pointer(integer(Result)+Count)^, Len-Start);

                   exit;
                 end;

      tkWString: begin
                   // WideString
                   // внимание, здесь используется другой индекс!!!
                   SetLength(WideString(DynamicArray), NewLen);
                   Result := pointer(integer(DynamicArray)+Start*sizeof(wchar));

                   if (Start<>Len) then
                   //CopyMemory(pointer(integer(Result)+Count*sizeof(wchar)), Result, (Len-Start)*sizeof(wchar));
                   Move(Result^, pointer(integer(Result)+Count*sizeof(wchar))^, (Len-Start)*sizeof(wchar));

                   exit;
                 end;
     else
       TExcept.Assert(WrongDynType(tpinfo));
  end;

  DynArraySetLength(pointer(DynamicArray), tpinfo, 1{!}, @NewLen);

  ArrInfo := GetTypeData(tpinfo);
  elSize := ArrInfo.elSize;
  Result := pointer(integer(DynamicArray)+Start*elSize);

  if (Start <> Len) then
  begin
    DestCpyPtr := pointer(integer(Result)+ Count*elSize);
    //CopyMemory(DestCpyPtr, Result, (Len-Start)*elSize);
    Move(Result^, DestCpyPtr^, (Len-Start)*elSize);

    if (ArrInfo.elType <> nil) then
      //ZeroMemory(Result, Count*elSize);
      FillChar(Result^, Count*elSize, #0);
  end;
end;

// <<<<<<<---------- ToDO удалить 


{$ifdef fpc}
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];
procedure CopyRecord(const dest, source, typeinfo: ptypeinfo);
asm
  {внимание! очень плохо работает в FPC !!!}
  xchg eax, edx
  jmp fpc_Copy_internal
end;
{$else}
procedure CopyRecord(dest, source, typeinfo: pointer);
asm
  jmp System.@CopyRecord
end;
{$endif}

procedure CopyObject(const Dest, Src: TObject);
var
  InitTable: pointer;
  BaseSize, DestSize: integer;
  BaseClass, DestClass, SrcClass: TClass;
begin
  if (Dest = nil) or (Src = nil) then exit; {по идее эксепшн}


  DestClass := TClass(pointer(Dest)^);
  SrcClass := TClass(pointer(Src)^);

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

    if (BaseClass = nil) then exit; {но такого не должно быть}
  end;


  // копирование
  DestSize := BaseClass.InstanceSize;   
  while (BaseClass <> TObject) do
  begin
    InitTable := PPointer(Integer(BaseClass) + vmtInitTable)^;
    if (InitTable <> nil) then
    begin
      CopyRecord(pointer(Dest), pointer(Src), InitTable);
      break;
    end;
    BaseClass := BaseClass.ClassParent;
  end;

  BaseSize := BaseClass.InstanceSize;
  if (BaseSize <> DestSize) then Move(pointer(integer(Src)+BaseSize)^, pointer(integer(Dest)+BaseSize)^, DestSize-BaseSize);
end;


{$ifdef fpc}
procedure CopyArray(const dest, source: pointer; const typeinfo: ptypeinfo; const count: integer);
var
  item_dest, item_src: pointer;
  item_size, i: integer;
begin
  item_dest := dest;
  item_src := source;
  
  case typeinfo.Kind of
    tkVariant: item_size := sizeof(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: item_size := sizeof(pointer);
      tkArray, tkRecord, tkObject: item_size := PFieldTable(GetTypeData(typeinfo)).Size;
  else
    item_size := 0; // exception
  end;

  if (item_size <> 0) then
  for i := 0 to count-1 do
  begin
    fpc_Copy_internal(item_src, item_dest, typeinfo);

    inc(Integer(item_dest), item_size);
    inc(Integer(item_src), item_size);
  end;
end;
{$else}
procedure CopyArray(const dest, source: pointer; const typeinfo: ptypeinfo; const count: integer);
asm
  // если указан статический массив, то домножить count на общее количество элементов в массиве
  // а в typeinfo - базовый элемент
  // сделано это для того чтобы не вызывался Exception
  // по каким то причинам System.CopyArray не приспособлен под аргумент tkArray
  // наверное потому что System.CopyArray внутренняя функция и на этапе прекомпиляции всё грамотно просчитывается
  cmp byte ptr [ecx], tkArray
  jne @1
  push eax
  push edx
    movzx edx, [ecx + TTypeInfo.Name]
    mov eax, [ecx + edx + 6]
    mov ecx, [ecx + edx + 10]
    mul count
    mov ecx, [ecx]
    mov count, eax    
  pop edx
  pop eax
  @1:

  push dword ptr [ebp+8]
  call System.@CopyArray
end;
{$endif}


{$ifdef fpc}
Procedure int_Finalize(Data, TypeInfo: Pointer); [external name 'FPC_FINALIZE'];
procedure Finalize(const Item: pointer; const tpinfo: ptypeinfo; const count: integer=1);
var
  item_ptr: pointer;
  item_size, i: integer;
begin
  item_ptr := Item;
  case tpinfo.Kind of
    tkVariant: item_size := sizeof(Variant);
    tkLString, tkWString, tkInterface, tkDynArray, tkAString: item_size := sizeof(pointer);
      tkArray, tkRecord, tkObject: item_size := PFieldTable(GetTypeData(tpinfo)).Size;
  else
    item_size := 0;
  end;

  if (item_size <> 0) then
  for i := 0 to count-1 do
  begin
    int_Finalize(item_ptr, tpinfo);
    inc(Integer(item_ptr), item_size);
  end;
end;
{$else}
procedure Finalize(const Item: pointer; const tpinfo: ptypeinfo; const count: integer=1);
asm
  jmp System.@FinalizeArray
end;
{$endif}


{$ifdef fpc}
procedure fpc_dynarray_incr_ref(p : pointer); [external name 'FPC_DYNARRAY_INCR_REF'];
procedure DynArrayAddRef(const DynArray);
asm
  mov eax, [eax]
  jmp fpc_dynarray_incr_ref
end;
{$else}
procedure DynArrayAddRef(const DynArray);
asm
  mov eax, [eax]
  jmp System.@DynArrayAddRef;
end;
{$endif}


{$ifdef fpc}
Procedure fpc_dynarray_clear (var p : pointer;ti : pointer);external name 'FPC_DYNARRAY_CLEAR';
procedure DynArrayClear(var a: pointer; typeinfo: pointer);
asm
  jmp fpc_dynarray_clear
end;
{$endif}


{$ifdef fpc}
function fpc_dynarray_copy(psrc : pointer;ti : pointer; lowidx,count:tdynarrayindex): pointer;external name 'FPC_DYNARR_COPY';
procedure DynArrayCopy(var DestArray, SrcArray; tpinfo: ptypeinfo);
var
  Dest: pointer absolute DestArray;
  Src: pointer absolute SrcArray;
begin
  if (Dest = Src) then exit;
  if (Dest <> nil) then fpc_dynarray_clear(Dest, tpinfo);
  if (Src <> nil) then Dest := fpc_dynarray_copy(Src, tpinfo, 0, DynArrayLength(SrcArray));
end;
{$else}
procedure DynArrayCopy(var DestArray, SrcArray; tpinfo: ptypeinfo);
asm
  cmp eax, edx
  jne @1
  ret
  @1:

  push ecx
  mov ecx, eax
  mov eax, [edx]
  pop edx

  jmp System.@DynArrayCopy  
end;
{$endif}


// ?
function IntegerDynArray(const Args: array of Integer): TIntegerDynArray;
var
  Len: integer;
begin
  Len := Length(Args);
  if (Len <> 0) then
  begin
    SetLength(Result, Len);
    Move(pointer(@Args)^, pointer(Result)^, Len*sizeof(Args[0]));
  end;
end;


// <<<---------   SysUtilsEx-рутина  ------------------------------------------
// ************************************************************************* //
{$endif}


// ------------   LUA-рутина  -------------------------------------------------
var
  LuaHandle: THandle;
  LuaPath: string;
  LuaInitialized: boolean;

type
  lua_State = pointer;
  lua_CFunction = function(L: lua_State): integer; cdecl;

  lua_Debug = record           (* activation record *)
    event: integer;
    name: __luaname;           (* (n) *)
    namewhat: __luaname;       (* (n) `global', `local', `field', `method' *)
    what: __luaname;           (* (S) `Lua', `C', `main', `tail'*)
    source: __luaname;         (* (S) *)
    currentline: integer;      (* (l) *)
    nups: integer;             (* (u) number of upvalues *)
    linedefined: integer;      (* (S) *)
    short_src: array[0..60{LUA_IDSIZE} - 1] of AnsiChar; (* (S) *)
    i_ci: integer;             (* active function *)
    // это сделано во избежание. lua_getstack портит первый байт MANY_FIELDS точно!
    MANY_FIELDS: array[0..3] of byte;
  end;


const
  LUA_GLOBALSINDEX = -10002;
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

var
  lua_open: function(): lua_State;
  luaL_openlibs: procedure(L: lua_State); cdecl;
  lua_close: procedure(L: lua_State); cdecl;
  lua_gc: function(L: lua_State; what: integer; data: integer): integer; cdecl;
  luaL_loadbuffer: function(L: lua_State; buff: __luadata; size: integer; name: __luaname): integer; cdecl;
  lua_pcall: function(L: lua_State; nargs, nresults, errf: integer): integer; cdecl;
  lua_error: function(L: lua_State): integer; cdecl;
  lua_next: function(L: lua_State; idx: integer): integer; cdecl;
  lua_getstack: function(L: lua_State; level: integer; var ar: lua_Debug): integer; cdecl;
  lua_getinfo: function(L: lua_State; const what: __luaname; var ar: lua_Debug): integer; cdecl;

  lua_type: function(L: lua_State; idx: integer): integer; cdecl;
  lua_gettop: function(L: lua_State): integer; cdecl;
  lua_settop: procedure(L: lua_State; idx: integer); cdecl;
  lua_remove: procedure(L: lua_State; idx: integer); cdecl;
  lua_insert: procedure(L: lua_State; idx: integer); cdecl;

  lua_pushnil: procedure(L: lua_State); cdecl;
  lua_pushboolean: procedure(L: lua_State; b: LongBool); cdecl;
  lua_pushinteger: procedure(L: lua_State; n: integer); cdecl;
  lua_pushnumber: procedure(L: lua_State; n: double); cdecl;
  lua_pushlstring: procedure(L: lua_State; s: __luaname; len: integer); cdecl;
  lua_pushcclosure: procedure(L: lua_State; fn: lua_CFunction; n: integer); cdecl;
  lua_pushlightuserdata: procedure(L: lua_State; p: pointer); cdecl;
  lua_newuserdata: function(L: lua_State; sz: integer): pointer; cdecl;
  lua_pushvalue: procedure(L: lua_State; idx: Integer); cdecl;
  lua_toboolean: function(L: lua_State; idx: integer): LongBool; cdecl;
  lua_tonumber: function(L: lua_State; idx: integer): double; cdecl;
  lua_tolstring: function(L: lua_State; idx: integer; len: pinteger): __luaname; cdecl;
  lua_tocfunction: function(L: lua_State; idx: integer): lua_CFunction; cdecl;
  lua_touserdata: function(L: lua_State; idx: integer): pointer; cdecl;
  lua_objlen: function(L: lua_State; idx: integer): integer; cdecl;

  lua_rawgeti: procedure(L: lua_State; idx, n: integer); cdecl;
  lua_rawseti: procedure(L: lua_State; idx, n: integer); cdecl;
  lua_rawget: procedure(L: lua_State; idx: integer); cdecl;
  lua_rawset: procedure(L: lua_State; idx: integer); cdecl;
  lua_createtable: procedure(L: lua_State; narr: integer; nrec: integer); cdecl; (* old newtable *)
  lua_setmetatable: function(L: lua_State; objindex: integer): integer; cdecl;


// todo удалить ---------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// коррекция пустой строки
const NULL_CHAR: char = #0;

procedure lua_push_pchar(const L: lua_State; const S: PChar; const any4bytes: integer=0); cdecl;
asm
  pop ebp
  mov eax, [esp+8]
  test eax, eax
  jnz @1
  mov [esp+8], OFFSET NULL_CHAR
  jmp @2

// подсчёт длинны
@1:xor ecx, ecx
   mov edx, eax
   @loop:
     cmp cl, [eax+0]
     je @_0
     cmp cl, [eax+1]
     je @_1
     cmp cl, [eax+2]
     je @_2
     cmp cl, [eax+3]
     je @_3
     add eax, 4
   jmp @loop
   @_3: inc eax
   @_2: inc eax
   @_1: inc eax
   @_0: sub eax, edx

// вызов
@2:mov [esp+12], eax
   jmp lua_pushlstring
end;


procedure lua_push_pascalstring(const L: lua_State; const S: string; const any4bytes: integer=0); cdecl;
asm
  pop ebp
  mov eax, [esp+8]
  test eax, eax
  jnz @1
  mov [esp+8], OFFSET NULL_CHAR
  jmp @2

// подсчёт длинны
@1:mov eax, [eax-4]

// вызов
@2:mov [esp+12], eax
   jmp lua_pushlstring
end;


procedure AnsiFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer);
{$ifdef fpc}
begin
  if (Dest <> '') then Dest := '';
  if (Length > 0) then
  begin
    SetLength(Dest, Length);
    Move(Source^, pointer(Dest)^, Length);
  end;
end;
{$else}
asm
  cmp [eax], 0
  jz @1
  push eax
  push edx
  push ecx
  call System.@LStrClr
  pop ecx
  pop edx
  pop eax
@1:
  test ecx, ecx
  jz @exit
  jmp System.@LStrFromPCharLen
@exit:
end;
{$endif}


procedure lua_to_pascalstring(var Dest: AnsiString; L: lua_State; const Index: integer);
{$ifdef FPC} // todo оптимизировать для асм ?
var
  Len: integer;
  S: pchar;
begin
  if (Dest <> '') then Dest := '';
  S := lua_tolstring(L, Index, @Len);

  if (Len <> 0) then
  begin
    SetLength(Dest, Len);
    Move(S^, pointer(Dest)^, Len);
  end;
end;
{$else}
var
  __Dest, __Len: integer;
asm
  mov __Dest, eax

  cmp [eax], 0
  jz @1
  push edx
  push ecx
  call System.@LStrClr
  pop ecx
  pop edx

@1:
  // lua_tolstring: function(L: lua_State; idx: Integer; len: pinteger=nil): PChar; cdecl;
  lea eax, __Len
  push eax
  push ecx
  push edx
  call lua_tolstring
  add esp, $0c
  cmp __Len, 0
  jz @exit

  // LStrFromPCharLen
  mov edx, eax
  mov ecx, __Len
  mov eax, __Dest
  //call System.@LStrFromPCharLen
  mov esp, ebp
  pop ebp
  jmp System.@LStrFromPCharLen

@exit:
end;
{$endif}


function lua_toint64(L: lua_State; idx: integer): int64; register;
asm
  push edx
  push eax
  call lua_tonumber

  // значение
  fistp qword ptr [esp]
  pop eax
  pop edx
end;
// <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<---------- todo удалить 



// загрузить библиотеку
function LoadLuaHandle(): THandle;
const
  lib_name = 'lua.dll';
begin
  if (LuaPath = '') then
  begin
    // Path
    if (FileExists(lib_name)) then LuaPath := ExpandFileName(lib_name)
    else LuaPath := IncludePathDelimiter(ExtractFilePath(InstancePath)) + lib_name;

    // загрузить
  {$ifdef MSWINDOWS}
    LuaHandle := LoadLibrary(pchar(LuaPath));
  {$else}
    {$MESSAGE ERROR 'LoadLibrary() not defined'}
  {$endif}
  end;

  // результат
  Result := LuaHandle;  
end;

// освободить библиотеку (делается при finalization модуля)
procedure FreeLuaHandle();
begin
  if (LuaHandle <> 0) then
  begin
    FreeLibrary(LuaHandle);
    LuaHandle := 0;
  end;
end;

// проинициализировать библиотеку Lua и все необходимые функции
function InitializeLUA(): boolean;

  function FailLoad(var Proc; const ProcName: pansichar): boolean;
  begin
    {$ifdef MSWINDOWS}
       pointer(Proc) := Windows.GetProcAddress(LuaHandle, ProcName);
    {$else}
       {$MESSAGE ERROR 'GetProcAddress() not defined'}
    {$endif}

    Result := (pointer(Proc) = nil);
  end;

begin
  Result := false;
  if (not LuaInitialized) then
  begin
    {$ifdef NO_CRYSTAL}
    InitializeLocaleCodePage(0);
    {$endif}
    if (LoadLuaHandle() = 0) then exit;

    if FailLoad(@lua_open, 'luaL_newstate') then exit;
    if FailLoad(@luaL_openlibs, 'luaL_openlibs') then exit;
    if FailLoad(@lua_close, 'lua_close') then exit;
    if FailLoad(@lua_gc, 'lua_gc') then exit;
    if FailLoad(@luaL_loadbuffer, 'luaL_loadbuffer') then exit;
    if FailLoad(@lua_pcall, 'lua_pcall') then exit;
    if FailLoad(@lua_error, 'lua_error') then exit;
    if FailLoad(@lua_next, 'lua_next') then exit;
    if FailLoad(@lua_getstack, 'lua_getstack') then exit;
    if FailLoad(@lua_getinfo, 'lua_getinfo') then exit;

    if FailLoad(@lua_type, 'lua_type') then exit;
    if FailLoad(@lua_gettop, 'lua_gettop') then exit;
    if FailLoad(@lua_settop, 'lua_settop') then exit;
    if FailLoad(@lua_remove, 'lua_remove') then exit;
    if FailLoad(@lua_insert, 'lua_insert') then exit;

    if FailLoad(@lua_pushnil, 'lua_pushnil') then exit;
    if FailLoad(@lua_pushboolean, 'lua_pushboolean') then exit;
    if FailLoad(@lua_pushinteger, 'lua_pushinteger') then exit;
    if FailLoad(@lua_pushnumber, 'lua_pushnumber') then exit;
    if FailLoad(@lua_pushlstring, 'lua_pushlstring') then exit;
    if FailLoad(@lua_pushcclosure, 'lua_pushcclosure') then exit;
    if FailLoad(@lua_pushlightuserdata, 'lua_pushlightuserdata') then exit;
    if FailLoad(@lua_newuserdata, 'lua_newuserdata') then exit;
    if FailLoad(@lua_pushvalue, 'lua_pushvalue') then exit;
    if FailLoad(@lua_toboolean, 'lua_toboolean') then exit;
    if FailLoad(@lua_tonumber, 'lua_tonumber') then exit;
    if FailLoad(@lua_tolstring, 'lua_tolstring') then exit;
    if FailLoad(@lua_tocfunction, 'lua_tocfunction') then exit;
    if FailLoad(@lua_touserdata, 'lua_touserdata') then exit;
    if FailLoad(@lua_objlen, 'lua_objlen') then exit;

    if FailLoad(@lua_rawgeti, 'lua_rawgeti') then exit;
    if FailLoad(@lua_rawseti, 'lua_rawseti') then exit;
    if FailLoad(@lua_rawget, 'lua_rawget') then exit;
    if FailLoad(@lua_rawset, 'lua_rawset') then exit;
    if FailLoad(@lua_createtable, 'lua_createtable') then exit;
    if FailLoad(@lua_setmetatable, 'lua_setmetatable') then exit;

    LuaInitialized := true;
  end;
  Result := true;
end;

// todo посмотреть !!!  ---------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

// расшифровка имени луа-типа
// pchar, это чтобы функции работали быстрее, не создавали разных LStrClr и HandleFinally
function LuaTypeName(const luatype: integer): pchar;
begin
  case luatype of
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
    Result := 'UNKNOWN';
  end;
end;

function TypeKindName(const Kind: TTypeKind): string;
begin
  Result := EnumName(typeinfo(TTypeKind), byte(Kind));
end;

// получить "описание" типа userdata
// вызов этой функции происходит тогда, когда произошла внештатная ситуация ! (ошибка)
(*procedure GetUserDataType(var Result: string; const Lua: TLua; const userdata: PLuaUserData);
begin
  if (userdata = nil) then
  begin
    Result := 'nil userdata';
    exit;
  end;
  if (byte(userdata.kind) > byte(ukSet)) then
  begin
    Result := 'unknown userdata';
    exit;
  end;
  if (userdata.instance = nil) then
  begin
    Result := 'already destroyed';
    exit;
  end;

  case userdata.kind of
    ukInstance: Result := Lua.ClassesInfo[userdata.ClassIndex]._ClassName;
       ukArray: Result := userdata.ArrayInfo.Name;
         ukSet: Result := userdata.SetInfo.Name;
    ukProperty: Result := Format('difficult property ''%s''', [userdata.PropertyInfo.PropertyName]);
  end;
end;  *)

// <<<<-------------  todo посмотреть !!!  


// создать дамп перевызова TLua.CallbackProc с параметрами P1 и P2
function CreateCFunctionDump(const Lua: TLua; const P1, P2, CallbackProc: pointer): pointer; overload;
var
  Dump: pchar absolute Result;
begin
  Result := Lua.FStorage.Dumps.alloc(); // CFunctionDumpManager.Alloc();

  // mov eax, Lua
  byte(Dump[0]) := $B8;
  pinteger(@Dump[1])^ := integer(Lua);
  // mov edx, P1
  byte(Dump[5]) := $BA;
  ppointer(@Dump[6])^ := P1;
  // mov ecx, P2
  byte(Dump[10]) := $B9;
  ppointer(@Dump[11])^ := P2;
  // jmp CallbackProc
  byte(Dump[15]) := $E9;
  pinteger(@Dump[16])^ := integer(CallbackProc)-(integer(@Dump[15])+5);
end;

// дамп на основной универсальный калбек
function CreateCFunctionDump(const Lua: TLua; const T: __PLuaType; const Mode: integer): pointer; overload;
begin
  Result := CreateCFunctionDump(Lua, T, pointer(Mode), @TLua.Callback);
end;


// todo а надо ли оно вообще ?
// найти указатель на конечную калбек-функцию, имея исходную CFunction
// нужно при анализе луа-аргумента.
//
// причём если фунцкия зарегистрирована, то возвращается конечная функция
// а если функция внутри lua, то возвращается она сама
function CFunctionPtr(CFunction: Lua_CFunction): pointer;
//var
//  ProcInfo: ^TLuaProcInfo;
begin
  Result := @CFunction;

  // TODO

{  if (InsortedPos4(integer(@CFunction), CFunctionDumps) >= 0) then
  begin
    ProcInfo := ppointer(integer(@CFunction) + 11)^;
    if (ProcInfo <> nil) and (ProcInfo.Address <> nil) then Result := ProcInfo.Address;
  end; }
end;

type
  GLOBAL_NAME_SPACE = class(TObject); // для упрощения некоторых вещей
  TForceString = procedure(const Arg: TLuaArg; var Ret: string);
  TForceVariant = procedure(const Arg: TLuaArg; var Ret: Variant);
  TStackArgument = procedure(const ALua: TLua; const Index: integer; var Ret: string);


// безопасный конструктор Lua. без Exception-ов
function CreateLua(): TLua;
begin
  if (not InitializeLUA) then Result := nil
  else Result := TLua.Create;
end;

procedure __LuaArgs(const Count: integer; var Result: TLuaArgs; const ReturnAddr: pointer);
begin
  if (Count < 0) then
  ELua.Assert('Can''t create an array lenght of %d arguments', [Count], ReturnAddr);

  SetLength(Result, Count);
  
  if (Count <> 0) then
  ZeroMemory(pointer(Result), Count*sizeof(TLuaArg));
end;

function LuaArgs(const Count: integer): TLuaArgs;
asm
  mov ecx, [esp]
  jmp __LuaArgs
end;

function LuaArg(const Value: boolean): TLuaArg;
begin
  Result.AsBoolean := Value;
end;

function LuaArg(const Value: integer): TLuaArg;
begin
  Result.AsInteger := Value;
end;

function LuaArg(const Value: double): TLuaArg;
begin
  Result.AsDouble := Value;
end;

function LuaArg(const Value: string): TLuaArg;
begin
  Result.AsString := Value;
end;

function LuaArg(const Value: pointer): TLuaArg;
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
end;

{function NumberToInteger(var Number: double; var IntValue: integer): boolean;
begin
  Result := (frac(Number) = 0) and (abs(Number) <= MAXINT);
  if (Result) then IntValue := trunc(Number);
end;}

function NumberToInteger(var Number: double; var IntValue: integer): boolean; overload;
asm
  sub esp, 16 {4, Int64, single}
  mov ecx, edx {результат}
  fld qword ptr [eax]
  fld st(0)

  // st(0) -> Int64
  FNSTCW word ptr[esp]
  FNSTCW word ptr[esp+2]
  OR word ptr[esp+2], $0F00  // trunc toward zero, full precision
  FLDCW word ptr[esp+2]
  FISTP qword ptr [esp+4]
  FLDCW word ptr[esp]

  // Frac
  fild qword ptr [esp+4]
  fsubp st(1), st(0)
  fstp dword ptr [esp+12]

  // Frac 0
  cmp [esp+12], 0
  jne @fail

  // Int64 -> integer
  mov eax, [esp+4]
  mov edx, [esp+8]
  sar eax, $1f
  cmp eax, edx
  jnz @fail

  mov edx, [esp+4]
  add esp, 16
  mov [ecx], edx
  mov eax, 1
  ret

@fail:
  xor eax, eax
  add esp, 16
  mov [ecx], eax
end;

function NumberToInteger(var Number; const Handle: pointer; const Index: integer): boolean; overload;
asm
  push eax
  push ecx
  push edx
  call [lua_tonumber]
  add esp, 8
  pop ecx

  { luanumber в st(0), ссылка на результат - в eax }
  sub esp, 16 {4, Int64, single}
  fld st(0) // копия

  // st(0) -> Int64
  FNSTCW word ptr[esp]
  FNSTCW word ptr[esp+2]
  OR word ptr[esp+2], $0F00  // trunc toward zero, full precision
  FLDCW word ptr[esp+2]
  FISTP qword ptr [esp+4]
  FLDCW word ptr[esp]

  // Frac
  fild qword ptr [esp+4]
  fsub st(0), st(1)
  fstp dword ptr [esp+12]

  // Frac 0
  cmp [esp+12], 0
  jne @ret_double

  // Int64 -> integer
  mov eax, [esp+4]
  mov edx, [esp+8]
  sar eax, $1f
  cmp eax, edx
  jnz @ret_double

  // return integer
  ffree st(0)
  mov edx, [esp+4]
  add esp, 16
  mov [ecx], edx
  mov eax, 1
  ret

@ret_double:
  fstp qword ptr [ecx]
  add esp, 16
  xor eax, eax
end;


// таблица может быть TClass или PLuaRecordInfo или TLuaTable
// функция возвращает ClassIndex в случае TLuaClassInfo или -1 в случае TLuaTable
function LuaTableToClass(const Handle: pointer; const Index: integer): integer;
var
  Number: double;
  IntValue: integer absolute Number;
begin
  Result := -1;

  lua_rawgeti(Handle, Index, 0);
  if (lua_type(Handle, -1) = LUA_TNUMBER) then
  begin
    if (NumberToInteger(Number, Handle, -1)) and (IntValue and integer($FFFF0000) = integer(typeinfoTClass))
    then Result := IntValue and $0000FFFF;
  end;
  lua_settop(Handle, -1-1);  
end;


// <<<---------   LUA-рутина  -------------------------------------------------


// ------------   управляющие структуры   -------------------------------------


{ __TLuaMemoryPool }


procedure __std_pool_allocator(PoolItem: __PLuaMemoryPoolItem; ItemSize: dword; var LastItemsCount: dword);
begin
  if (LastItemsCount = 0) then LastItemsCount := 32
  else LastItemsCount := LastItemsCount * 2;

  PoolItem.size := ItemSize*LastItemsCount;
  GetMem(PoolItem.memory, PoolItem.size);
end;

procedure __std_pool_cleaner(PoolItem: __PLuaMemoryPoolItem);
begin
  FreeMem(PoolItem.memory);
end;


procedure __TLuaMemoryPool.Initialize(ItemSize: dword;
                                      Allocator: __TLuaMemoryPoolAllocator;
                                      Cleaner: __TLuaMemoryPoolCleaner);
begin
  FItems := nil; // хотя он уже должен быть пустой
  FAllocableList := nil;
  FLastItemsCount := 0;
  Count := 0;

  FItemSize := (ItemSize + 3) and (not 3);
  FAllocator := Allocator;
  if (not Assigned(FAllocator)) then FAllocator := __std_pool_allocator;
  FCleaner := Cleaner;
  if (not Assigned(FCleaner)) then FCleaner := __std_pool_cleaner;
end;

procedure __TLuaMemoryPool.Finalize;
var
  i: integer;
begin
  // удаляем выделенную память
  for i := 0 to Length(FItems)-1 do
  begin
    FCleaner(@FItems[i]);
  end;

  // массив
  FItems := nil;
end;


function __TLuaMemoryPool.alloc: pointer;
var
  buf: pointer;
  L: integer;

  procedure PoolItemInitialize(PoolItem: __PLuaMemoryPoolItem);
  var
    i: integer;
    item: pointer;
  begin
    PoolItem.next := nil;
    PoolItem.userdata := nil;
    PoolItem.memory := nil;
    PoolItem.size := 0;
    PoolItem.data_items := nil;
    FAllocator(PoolItem, FItemSize, FLastItemsCount);

    // проставить указатели
    PoolItem.data_items := PoolItem.memory;
    item := PoolItem.memory;
    for i := 0 to FLastItemsCount-2 do
    begin
      ppointer(item)^ := pointer(dword(item)+FItemSize);
      inc(integer(item), FItemSize);
    end;
    ppointer(item)^ := nil;
  end;
begin
  // если свободных кусков памяти нет - выделем новый элемент и заполняем
  if (FAllocableList = nil) then
  begin
    L := Length(FItems);
    SetLength(FItems, L+1);
    FAllocableList := @FItems[L];
    PoolItemInitialize(FAllocableList);
  end;

  // результат, следующая итерация
  Result := FAllocableList.data_items;
  inc(Count);
  buf := ppointer(Result)^;
  FAllocableList.data_items := buf;

  // если закончилось - удаляем из списка свободных пулов 
  if (buf = nil) then
  begin
    buf := FAllocableList.next;
    FAllocableList.next := nil;
    FAllocableList := __PLuaMemoryPoolItem(buf);
  end;
end;


// release тоже очень простой. случай, когда
// остаётся пустой кусок памяти (с удалением его из списков) - не рассматривается
procedure __TLuaMemoryPool.release(P: pointer);
var
  i: integer;
  item, buf: __PLuaMemoryPoolItem;
begin
  // поиск места
  item := nil;
  for i := 0 to Length(FItems)-1 do
  with FItems[i] do
  if (dword(P) >= dword(memory)) and (dword(P)-dword(memory) < size) then
  begin
    item := @FItems[i];
    break;
  end;

  // удаление
  ppointer(P)^ := item.data_items;
  item.data_items := P;
  dec(Count);

  // если раньше был полностью заполнен - то сейчас добавить в FAllocableList
  // (если уже не добавлен)
  if (item.next = nil) then
  begin
    // проверяем
    buf := FAllocableList;
    while (buf <> nil) do
    begin
      if (buf = item) then exit;
      buf := buf.next;
    end;

    // добавляем
    item.next := FAllocableList;
    FAllocableList := item;
  end;
end;


// выделение/удаление памяти для дампов кода
procedure __codedump_pool_allocator(PoolItem: __PLuaMemoryPoolItem; ItemSize: dword; var LastItemsCount: dword);
{$ifdef MSWINDOWS}
const
  DUMPS_BLOCK_SIZE = 1024*4; // PageSize
{$endif}  

begin
  if (LastItemsCount = 0) then LastItemsCount := 32
  else LastItemsCount := LastItemsCount * 2;

  PoolItem.size := ItemSize*LastItemsCount;
  GetMem(PoolItem.memory, PoolItem.size);

  {$ifdef MSWINDOWS}
    THandle(PoolItem.userdata) := HeapCreate($00040000{HEAP_CREATE_ENABLE_EXECUTE}, 0, 0);
    PoolItem.memory := HeapAlloc(THandle(PoolItem.userdata), 0, DUMPS_BLOCK_SIZE);
    PoolItem.size := DUMPS_BLOCK_SIZE;
    LastItemsCount := DUMPS_BLOCK_SIZE div ItemSize;
  {$else}
    {$MESSAGE ERROR 'Memory allocation not defined'}
  {$endif}
end;

procedure __codedump_pool_cleaner(PoolItem: __PLuaMemoryPoolItem);
begin
  {$ifdef MSWINDOWS}
    HeapFree(THandle(PoolItem.userdata), 0, PoolItem.memory);
    HeapDestroy(THandle(PoolItem.userdata));
  {$else}
    {$MESSAGE ERROR 'Memory disposing not defined'}
  {$endif}
end;



{ __TLuaHashArray }


procedure __TLuaHashArray.Initialize(const Lua: TLua; const Values: __PLuaMemoryPool);
begin
  FLua := Lua;
  FValues := Values;

  FArraySize := 0;
  FAndMask := 0;
  FAllocatedMax := 0;
  FAllocated := 0;

  Grow();
end;

procedure __TLuaHashArray.Finalize();
begin
  FArray := nil;
end;

function __TLuaHashArray.find_ptr(const PtrKey: pointer): pointer;
asm
  shr edx, 2
  jmp __TLuaHashArray.find
end;

// найти
function __TLuaHashArray.find(const Key: integer): pointer;
begin
  if (FAndMask = 0) then Result := nil
  else Result := FArray[Key and FAndMask];

  while (Result <> nil) do
  begin
    if (__PLuaHashItem(Result).Key = Key) then
    begin
      Result := __PLuaHashItem(Result).Value;
      exit;
    end;

    Result := __PLuaHashItem(Result).next;
  end;
end;

// увеличение размерности массива
procedure __TLuaHashArray.Grow();
var
  i, NewIndex: integer;
  Item, NewItem: __PLuaHashItem;

  NewArray: array of __PLuaHashItem;
  NewArraySize, NewAndMask: integer;
  buf: pointer;
begin
  // новый размер
  if (FArraySize = 0) then NewArraySize := 16
  else NewArraySize := FArraySize * 2;

  // инициализация нового
  NewAndMask := NewArraySize-1;
  SetLength(NewArray, NewArraySize);
  FillChar(pointer(NewArray)^, NewArraySize*sizeof(pointer), 0);

  // копирование элементов
  for i := 0 to FArraySize-1 do
  begin
    Item := FArray[i];

    // копирование всей линии на новое место
    while (Item <> nil) do
    begin
      NewItem := Item;
      Item := Item.next;

      NewIndex := NewItem.Key and NewAndMask;
      NewItem.next := NewArray[NewIndex];
      NewArray[NewIndex] := NewItem;
    end;
  end;

  // подменяем массивы
  buf := pointer(FArray);
  pointer(FArray) := pointer(NewArray);
  pointer(NewArray) := buf;

  // FAndMask, FAllocatedMax
  FArraySize := NewArraySize;
  FAndMask := NewAndMask;
  FAllocatedMax := NewArraySize*3 div 4; // 3/4 * FArraySize
end;

function __TLuaHashArray.AllocAddingItem(const Key: integer; const alloc_value: boolean=true): __PLuaHashItem;
begin
  // отслеживаем ситуацию, когда массив должен "вырасти"
  inc(FAllocated);
  if (FAllocated > FAllocatedMax) then Grow();

  // создаём новый элемент, заполняем базовыми полями
  Result := FLua.FStorage.HashItems.alloc();
  Result.next := nil;
  Result.Key := Key;

  // скорее всего нужно будет выделить новое значение
  if (alloc_value) then
  Result.Value := FValues.alloc();
end;

function __TLuaHashArray.add_ptr(const PtrKey: pointer; var added: boolean): pointer;
asm
  shr edx, 2
  jmp __TLuaHashArray.add
end;

function __TLuaHashArray.add(const Key: integer; var added: boolean): pointer;
var
  Item: __PLuaHashItem;
  Index: integer;
begin
  if (FAndMask = 0) then Item := nil
  else Item := FArray[Key and FAndMask];

  while (Item <> nil) do
  begin
    if (Item.Key = Key) then
    begin
      added := false;
      Result := Item.Value;
      exit;
    end;

    Item := Item.next;
  end;

  // not found
  added := true;
  Item := AllocAddingItem(Key);
  Index := Key and FAndMask;
  Item.next := FArray[Index];
  FArray[Index] := Item;
  Result := Item.Value;
end;

procedure __TLuaHashArray.add(const Key: integer; const TryFind: boolean; const Value: pointer);
asm
  pop ebp
  jmp __TLuaHashArray.ForceAdd
end;

procedure __TLuaHashArray.add_ptr(const PtrKey: pointer; const TryFind: boolean; const Value: pointer);
asm
  pop ebp
  shr edx, 2
  jmp __TLuaHashArray.ForceAdd
end;

// этот метод отличается от своего "старшего брата" тем,
// что не вызывает аллокатора Value, а задаёт явное значение
// используется для результирующих namespace конкретного класса (оставленного из нескольких)
// и ещё паре мест
procedure __TLuaHashArray.ForceAdd(const Key: integer; const TryFind: boolean; const Value: pointer);
var
  Item: __PLuaHashItem;
  Index: integer;
begin
  if (TryFind) then
  begin
    if (FAndMask = 0) then Item := nil
    else Item := FArray[Key and FAndMask];

    while (Item <> nil) do
    begin
      if (Item.Key = Key) then
      begin
        Item.Value := Value;
        exit;
      end;

      Item := Item.next;
    end;
  end;

  // not found
  Item := AllocAddingItem(Key, false);
  Index := Key and FAndMask;
  Item.next := FArray[Index];
  FArray[Index] := Item;
  Item.Value := Value;
end;



{ __TLuaDataBuffer }

const
  LUA_DATABUFFER_DELTA = 1024;

procedure __TLuaDataBuffer.Initialize;
begin
  Memory := nil;
  Size  := 0;
  MemoryOffset := 0;
end;

procedure __TLuaDataBuffer.Finalize;
begin
  if (Memory <> nil) then FreeMem(Memory);
  Size  := 0;
  MemoryOffset := 0;
end;

procedure __TLuaDataBuffer.Grow();
begin
  Size := (MemoryOffset + (LUA_DATABUFFER_DELTA-1)) and (-LUA_DATABUFFER_DELTA);

  if (Memory = nil) then GetMem(pointer(Memory), Size)
  else ReallocMem(pointer(Memory), Size);
end;

function __TLuaDataBuffer.alloc(const Bytes: integer): pointer;
begin
  Result := @Memory[MemoryOffset];
  inc(MemoryOffset, Bytes);

  if (MemoryOffset > Size) then
  begin
    Grow();
    Result := @Memory[MemoryOffset-Bytes];
  end;
end;






{ __TLuaNames }

procedure __TLuaNames.Initialize(const Lua: TLua);
begin
  FLua := Lua;
  FAutoAdd := true;
  FItems.Initialize(Lua, nil);
end;

procedure __TLuaNames.Finalize;
begin
  FItems.Finalize();
end;

function __TLuaNames.Identifier(const Name: LuaString): __luaname;
asm
  test edx, edx
  jnz @1
@zero:
  xor eax, eax
  ret
@1:
  // Length
  mov ecx, [edx-4]

  {$ifdef LUA_ANSI}
    // Name: AnsiString
    jmp __TLuaNames.internalIdentifier
  {$else .LUA_UNICODE}
    // Name: WideString or UnicodeString
    {$ifndef UNICODE}
       // корректировка длинны, если строка WideString
       shr ecx, 1
       jz @zero
    {$endif}

    // push variables
    push eax
    push edx
    push ecx

    // выделить ecx*3 памяти в FLua.FStorage.DataBuffer
    // занести результат в eax
    lea ecx, [ecx + ecx*2]
    mov edx, [EAX].__TLuaNames.FLua
    cmp ecx, [EDX].TLua.FStorage.DataBuffer.Size
    mov eax, [EDX].TLua.FStorage.DataBuffer.Memory
    jbe @memory_allocated
       lea eax, [EDX].TLua.FStorage.DataBuffer
       mov edx, ecx
       push eax
       call __TLuaDataBuffer.alloc
       pop edx
       mov [EDX].__TLuaDataBuffer.MemoryOffset, 0
    @memory_allocated:

    //function Utf8FromUtf16(const Utf8Chars: pansichar; const Utf8Length: integer;
    //                       const WideChars: PWideChar; const WideLength: integer): integer;
    // и подмена буферов. Utf8FromUtf16 захватывает ecx(WideLength) из стека
    mov ecx, [esp+4] // WideChars
    mov edx, [esp]   // WideLength
    mov [esp+4], eax // замена буфера символов
    lea edx, [edx + edx*2]// WideLength --> Utf8Length
    call Utf8FromUtf16

    // стандартный вызов, берём новые параметры
    mov ecx, eax
    pop edx
    pop eax
    jmp __TLuaNames.internalIdentifier    
  {$endif}
end;

function __TLuaNames.Identifier(const RTTIName: PShortString): __luaname;
asm
  test edx, edx
  jnz @1
@zero:
  xor eax, eax
  ret
@1:
  movzx ecx, byte ptr [edx]
  inc edx
  test ecx, ecx
  jz @zero

  {$ifndef UNICODE}
    // версия Delphi не поддерживает Unicode-идентификаторы
    // следовательно идентификатор уже в режиме англ-ansi-utf8
    // и не имеет значения флаг LUA_UNICODE - сразу перенаправоляем в стандартный обработчик
    jmp __TLuaNames.internalIdentifier
  {$else .!!!UNICODE}
    // версия поддерживает Unicode-идентификаторы
    // значит идентификатор уже в кодировке utf8
    {$ifdef LUA_UNICODE}
      // соответственно если
      jmp __TLuaNames.internalIdentifier
    {$else .LUA_ANSI}
      // нужно перевести utf8-символы в Ansi

      // push variables
      push eax
      push edx
      push ecx

      // выделить ecx памяти в FLua.FStorage.DataBuffer
      // занести результат в eax
      mov edx, [EAX].__TLuaNames.FLua
      cmp ecx, [EDX].TLua.FStorage.DataBuffer.Size
      mov eax, [EDX].TLua.FStorage.DataBuffer.Memory
      jbe @memory_allocated
         lea eax, [EDX].TLua.FStorage.DataBuffer
         mov edx, ecx
         push eax
         call __TLuaDataBuffer.alloc
         pop edx
         mov [EDX].__TLuaDataBuffer.MemoryOffset, 0
      @memory_allocated:

      // function AnsiFromUtf8(const AnsiChars: pansichar; const AnsiLength: integer;
      //                       const Utf8Chars: pansichar; const Utf8Length: integer): integer;
      // и подмена буферов. AnsiFromUtf8 захватывает ecx(Utf8Length) из стека
      mov ecx, [esp+4] // Utf8Chars
      mov edx, [esp]   // AnsiLength ( = Utf8Length)
      mov [esp+4], eax // замена буфера
      call AnsiFromUtf8

      // стандартный вызов, берём новые параметры
      mov ecx, eax
      pop edx
      pop eax
      jmp __TLuaNames.internalIdentifier
    {$endif}
  {$endif}
end;

procedure __TLuaNamesThrowLongIdentifier(const Self: __TLuaNames);
begin
  Self.FLua.Assert('Too long identifier');
end;

// принимает строку во внутреннем формате, возвращает зарегистрированный идентификатор
// utf8(LUA_UNICODE) или ansi(LUA_ANSI)
function __TLuaNames.internalIdentifier(const Name: __luaname; const Len: integer): __luaname;
asm
  cmp ecx, 255
  ja __TLuaNamesThrowLongIdentifier
@prefix:
  push esi
  push edi
  push ebx
  push ebp
@begin:
  // нахождение CRC

  mov edi, ecx
  xor ebx, ebx
  mov esi, edx
  jmp @loop_continue
  @loop:
    add ebx, [esi]
    add esi, 4
    rol ebx, 3
  @loop_continue:
    sub edi, 4
    jge @loop
  @loop_end:

  // осталось <= 3 байт
  add edi, 4
  jz @crc_finish
  jmp [OFFSET @jumps_crc_last + edi*4 - 4]
@jumps_crc_last: DD @crc_1, @crc_2, @crc_3
@crc_3:
  movzx edi, byte ptr [esi]
  add ebx, edi
  inc esi
  rol ebx, 1
@crc_2:
  movzx edi, byte ptr [esi]
  add ebx, edi
  inc esi
  rol ebx, 1
@crc_1:
  movzx edi, byte ptr [esi]
  add ebx, edi
  inc esi
  rol ebx, 1
@crc_finish:
  // финальное нахождение CRC (с длинной) идентификатора
  shl ebx, 8
  or ebx, ecx

  // поиск идентификатора с CRC ebx
  mov edi, [EAX].__TLuaNames.FItems.FAndMask
  mov ecx, ebx
  mov esi, [EAX].__TLuaNames.FItems.FArray
  and ecx, edi
  mov esi, [esi + ecx*4]
  test esi, esi
  jz @not_found
  @find_loop:
    cmp ebx, [ESI].__TLuaHashItem.Key
    jne @find_loop_continue

    // CRC равны. длины равны. надо сравнить
    push esi
    movzx ecx, bl
    mov esi, [ESI].__TLuaHashItem.Value
    lea edi, [edx-4]
    sub esi, 4
    jmp @cmp_loop_4_continue
    @cmp_loop_4:
      mov ebp, [esi]
      cmp ebp, [edi]
      jne @find_loop_continue_after_cmp
    @cmp_loop_4_continue:
      add esi, 4
      add edi, 4
      sub ecx, 4
      jge @cmp_loop_4
    @cmp_loop_4_end:
    add ecx, 4 // 0..3
    jmp [OFFSET @jumps_cmp_last + ecx*4]
    @jumps_cmp_last: DD @found, @cmp_1, @cmp_2, @cmp_3
    @cmp_3:
      mov cx, [esi]
      cmp cx, [edi]
      jne @find_loop_continue_after_cmp
      mov cl, [esi+2]
      cmp cl, [edi+2]
      jne @find_loop_continue_after_cmp
    jmp @found
    @cmp_2:
      mov cx, [esi]
      cmp cx, [edi]
      jne @find_loop_continue_after_cmp
    jmp @found
    @cmp_1:
      mov cl, [esi]
      cmp cl, [edi]
      jne @find_loop_continue_after_cmp
    @found:
    pop esi
    mov eax, [ESI].__TLuaHashItem.Value
    jmp @postfix

  @find_loop_continue_after_cmp:
    pop esi
  @find_loop_continue:
    mov esi, [ESI].__TLuaHashItem.next
    test esi, esi
    jnz @find_loop
@not_found:
  cmp [EAX].__TLuaNames.FAutoAdd, 0
  jz @ret_nil
@add_new:
  mov ecx, ebx
  push dword ptr offset @postfix
  jmp __TLuaNames.internalAdd
@ret_nil:
  xor eax, eax
@postfix:
  pop ebp
  pop ebx
  pop edi
  pop esi
@exit:
end;

// регистрируем строку в Lua, пишем соответствующий указатель в массив
function __TLuaNames.internalAdd(const Name: __luaname; const CRC_len: integer): __luaname;
var
  ref: integer;
begin
  // добавление в Lua
  FLua.global_alloc_ref(ref);
  lua_pushlstring(FLua.Handle, Name, CRC_len and $ff);
  Result := lua_tolstring(FLua.Handle, -1, nil);
  FLua.global_fill_value(ref);

  // добавление в поисковый хеш-массив
  FItems.add(CRC_len, false, Result);
end;



// <<<---------   управляющие структуры   -------------------------------------



const
  GLOBAL_INDEX_KINDS: set of __TLuaGlobalKind = [gkType, gkConst, gkLuaData];
  CONST_GLOBAL_KINDS: set of __TLuaGlobalKind = [gkType, gkProc, gkConst];
  NATIVE_GLOBAL_KINDS: set of __TLuaGlobalKind = [gkVariable, gkProc];

  RECORD_TYPES: set of TTypeKind = [tkRecord{$ifdef fpc},tkObject{$endif}];
  VARIANT_SUPPORT = [varEmpty, varNull, varSmallint, varInteger, varSingle,
                     varDouble, varCurrency, varDate, varOleStr, varBoolean, varError{as Empty},
                     varShortInt, varByte, varWord, varLongWord, varInt64{, почему-то не умещается varString}];
  VARIANT_SIMPLE = VARIANT_SUPPORT - [varOleStr];

  MASK_VARIANTS_DIFFICULT = (1 shl varOleStr)or(1 shl varDispatch)or(1 shl varUnknown);


(*function LuaProc(const Proc: TLuaProc0): TLuaProc;
begin Result := TLuaProc(Proc); end;
function LuaProc(const Proc: TLuaProc1): TLuaProc;
begin Result := TLuaProc(Proc); end;
function LuaProc(const Proc: TLuaProc2): TLuaProc;
begin Result := TLuaProc(Proc); end;
function LuaProc(const Proc: TLuaProc3): TLuaProc;
begin Result := TLuaProc(Proc); end;
function LuaProc(const Proc: TLuaProc4): TLuaProc;
begin Result := TLuaProc(Proc); end;
function LuaProc(const Proc: TLuaProc5): TLuaProc;
begin Result := TLuaProc(Proc); end;
function LuaProc(const Proc: TLuaProc6): TLuaProc;
begin Result := TLuaProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc0): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc1): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc2): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc3): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc4): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc5): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc6): TTClassRecallProc;
begin Result := TTClassRecallProc(Proc); end;
function TClassRecallProc(const Proc: TTClassRecallProc7): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc8): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc9): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc10): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc11): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc12): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc13): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc14): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc15): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc16): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc17): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc18): TTClassRecallProc;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc19): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc20): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc21): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc22): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc23): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc24): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc25): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc26): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProc(const Proc: TTClassRecallProc27): TTClassRecallProc; overload;
begin TMethod(Result).Code := @Proc; end;
function TClassRecallProcPtr(const Proc: pointer): TTClassRecallProc;
begin TMethod(Result).Code := Proc; end; *)


// -----------   операции с множествами   -------------
type
  _set1 = set of 0..7;
  _set2 = set of 0..15;
  _set4 = set of 0..31;

procedure IncludeSetBit(const _Set: pointer; const Bit: integer);
asm
  bts [eax], edx
end;

procedure ExcludeSetBit(const _Set: pointer; const Bit: integer);
asm
  btr [eax], edx
end;

function SetBitContains(const _Set: pointer; const Bit: integer): boolean;
asm
  bt [eax], edx
  setc al
end;


// спёрто из System и модифицировано
function  _SetLe(const X1, X2: pointer; const Size: integer): boolean;
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
  and  eax, $FF
end;

// по аналогии с System, только чуть более оптимизировано
function _SetEq(const X1, X2: pointer; const Size: integer): boolean;
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

@@bytes: add ecx, 4
@@loop:
    mov bl, [eax+ecx-1]
    cmp bl, [edx+ecx-1]
    jne @@exit
    dec ecx
    jnz @@loop

@@exit:
  pop ebx
  sete al
  and  eax, $FF
end;


// сравнение множеств
// флаг "subset"  означает "является ли X1 подмножеством X2"
// в остальных случаях надо сравнить на равенство
// Result = 0 если True и 1 если False (условие = subset)
function SetsCompare(const X1, X2: pointer; const Size: integer; const subset: boolean): integer; overload;
var
  Ret: boolean;
begin
  if (subset) then
  begin
    case (Size) of
      1: Ret := (_set1(X1^) <= _set1(X2^));
      2: Ret := (_set2(X1^) <= _set2(X2^));
      4: Ret := (_set4(X1^) <= _set4(X2^));
    else
      Ret := _SetLe(X1, X2, Size);
    end;
  end else
  begin
    case (Size) of
      1: Ret := (_set1(X1^) = _set1(X2^));
      2: Ret := (_set2(X1^) = _set2(X2^));
      4: Ret := (_set4(X1^) = _set4(X2^));
    else
      Ret := _SetEq(X1, X2, Size);
    end;
  end;

  // результат
  Result := ord(not Ret);  
end;

// промежуточная функция
// заполнить 32-байтный буфер нулями и выставить i-й флаг
function __bit_include(var X2; const Bit: integer): pointer;
asm
  xor ecx, ecx
  mov [eax +  0], ecx
  mov [eax +  4], ecx
  mov [eax +  8], ecx
  mov [eax + 12], ecx
  mov [eax + 16], ecx
  mov [eax + 20], ecx
  mov [eax + 24], ecx
  mov [eax + 28], ecx

  bts [eax], edx
  {push ebx
  mov ecx, edx
  mov ebx, 1
  and ecx, 7
  shr edx, 3
  shl ebx, cl
  mov [eax + edx], bl
  pop ebx }
end;


function SetsCompare(const X1: pointer; const Value, Size: integer; const subset: boolean): integer; overload;
var
  X2: array[0..31] of byte;
  Ret: boolean;
begin
  if (subset) then
  begin
    case (Size) of
      {$ifndef fpc}
      1: Ret := (_set1(X1^) <= _set1(byte(1 shl Value)));
      2: Ret := (_set2(X1^) <= _set2(word(1 shl Value)));
      {$endif}
      4: Ret := (_set4(X1^) <= _set4(integer(1 shl Value)));
    else
      // сложный вариант
      Ret := _SetLe(X1, __bit_include(X2, Value), Size);
    end;
  end else
  begin
    case (Size) of
      {$ifndef fpc}
      1: Ret := (_set1(X1^) = _set1(byte(1 shl Value)));
      2: Ret := (_set2(X1^) = _set2(word(1 shl Value)));
      {$endif}
      4: Ret := (_set4(X1^) = _set4(integer(1 shl Value)));
    else
      Ret := _SetEq(X1, __bit_include(X2, Value), Size);
    end;
  end;

  // результат
  Result := ord(not Ret);  
end;

// объединений множеств
procedure SetsUnion(const Dest, X1, X2: pointer; const Size: integer); overload;
asm
  push ebx
  jmp  @@start

@@dwords:
  mov ebx, [edx]
  add edx, 4
  or  ebx, [ecx]
  add ecx, 4
  mov [eax], ebx
  add eax, 4
@@start:
  sub Size, 4
  jg @@dwords
  je @@4

@@bytes:
  inc Size
  jz @@3
  inc Size
  jz @@2

@@1:
  mov bl, [edx]
  or  bl, [ecx]
  mov [eax], bl
  jmp @@exit
@@2:
  mov bx, [edx]
  or  bx, [ecx]
  mov [eax], bx
  jmp @@exit
@@3:
  mov bx, [edx]
  or  bx, [ecx]
  mov [eax], bx
  mov bl, [edx+2]
  or  bl, [ecx+2]
  mov [eax+2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  or  ebx, [ecx]
  mov [eax], ebx
@@exit:
  pop ebx
end;

procedure SetsUnion(const Dest, X1: pointer; const Value, Size: integer); overload;
var
  X2: array[0..31] of byte;
begin
  case Size of
    {$ifndef fpc}
    1: _set1(Dest^) := _set1(X1^) + _set1(byte(1 shl Value));
    2: _set2(Dest^) := _set2(X1^) + _set2(word(1 shl Value));
    {$endif}
    4: _set4(Dest^) := _set4(X1^) + _set4(integer(1 shl Value));
  else
    SetsUnion(Dest, X1, __bit_include(X2, Value), Size);
  end;                             
end;

// различия множеств
procedure SetsDifference(const Dest, X1, X2: pointer; const Size: integer); overload;
asm
  push ebx
  jmp  @@start

@@dwords:
  mov ebx, [edx]
  add edx, 4
  xor ebx, [ecx]
  add ecx, 4
  mov [eax], ebx
  add eax, 4
@@start:
  sub Size, 4
  jg @@dwords
  je @@4

@@bytes:
  inc Size
  jz @@3
  inc Size
  jz @@2

@@1:
  mov bl, [edx]
  xor bl, [ecx]
  mov [eax], bl
  jmp @@exit
@@2:
  mov bx, [edx]
  xor bx, [ecx]
  mov [eax], bx
  jmp @@exit
@@3:
  mov bx, [edx]
  xor bx, [ecx]
  mov [eax], bx
  mov bl, [edx+2]
  xor bl, [ecx+2]
  mov [eax+2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  xor ebx, [ecx]
  mov [eax], ebx
@@exit:
  pop ebx
end;

procedure SetsDifference(const Dest, X1: pointer; const Value, Size: integer; const Exchenge: boolean); overload;
var
  X2: array[0..31] of byte;
begin
  if (not Exchenge) then
  begin
    case Size of
      {$ifndef fpc}
      1: _set1(Dest^) := _set1(X1^) - _set1(byte(1 shl Value));
      2: _set2(Dest^) := _set2(X1^) - _set2(word(1 shl Value));
      {$endif}
      4: _set4(Dest^) := _set4(X1^) - _set4(integer(1 shl Value));
    else
      SetsDifference(Dest, X1, __bit_include(X2, Value), Size);
    end;
  end else
  begin
  case Size of
      {$ifndef fpc}
      1: _set1(Dest^) := _set1(byte(1 shl Value)) - _set1(X1^);
      2: _set2(Dest^) := _set2(word(1 shl Value)) - _set2(X1^);
      {$endif}
      4: _set4(Dest^) := _set4(integer(1 shl Value))- _set4(X1^);
    else
      SetsDifference(Dest, __bit_include(X2, Value), X1, Size);
    end;
  end;
end;

// пересечение множеств
procedure SetsIntersection(const Dest, X1, X2: pointer; const Size: integer); overload;
asm
  push ebx
  jmp  @@start

@@dwords:
  mov ebx, [edx]
  add edx, 4
  and ebx, [ecx]
  add ecx, 4
  mov [eax], ebx
  add eax, 4
@@start:
  sub Size, 4
  jg @@dwords
  je @@4

@@bytes:
  inc Size
  jz @@3
  inc Size
  jz @@2

@@1:
  mov bl, [edx]
  and bl, [ecx]
  mov [eax], bl
  jmp @@exit
@@2:
  mov bx, [edx]
  and bx, [ecx]
  mov [eax], bx
  jmp @@exit
@@3:
  mov bx, [edx]
  and bx, [ecx]
  mov [eax], bx
  mov bl, [edx+2]
  and bl, [ecx+2]
  mov [eax+2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  and ebx, [ecx]
  mov [eax], ebx
@@exit:
  pop ebx
end;

procedure SetsIntersection(const Dest, X1: pointer; const Value, Size: integer); overload;
var
  X2: array[0..31] of byte;
begin
  case Size of
    {$ifndef fpc}
    1: _set1(Dest^) := _set1(X1^) * _set1(byte(1 shl Value));
    2: _set2(Dest^) := _set2(X1^) * _set2(word(1 shl Value));
    {$endif}
    4: _set4(Dest^) := _set4(X1^) * _set4(integer(1 shl Value));
  else
    SetsIntersection(Dest, X1, __bit_include(X2, Value), Size);
  end;                             
end;

procedure SetInvert(const Dest, X1: pointer; const AndMasks, Size: integer);
asm
  push eax
  push ebx
  jmp  @@start

@@dwords:
  mov ebx, [edx]
  add eax, 4
  not ebx
  add edx, 4
  mov [eax-4], ebx
@@start:
  sub Size, 4
  jg @@dwords
  je @@4

@@bytes:
  inc Size
  jz @@3
  inc Size
  jz @@2

@@1:
  mov bl, [edx]
  not ebx
  and ebx, ecx // and bl, cl
  mov [eax], bl
  jmp @@exit
@@2:
  mov bx, [edx]
  not ebx
  mov [eax], bx
  and [eax+1], cl
  jmp @@exit
@@3:
  mov bx, [edx]
  not ebx
  mov [eax], bx
  mov bl, [edx+2]
  not ebx
  and ebx, ecx // and bl, cl
  mov [eax+2], bl
  jmp @@exit
@@4:
  mov ebx, [edx]
  not ebx
  mov [eax], ebx
  and [eax+3], cl
@@exit:
  pop ebx
  pop eax
  and [eax], ch
end;


// -<<--------   операции с множествами   -------------







{ TLuaArg }

procedure TLuaArg.Assert(const NeededType: TLuaArgType; const CodeAddr: pointer);

  function TypeToString(T: TLuaArgType; const DelPrefics:boolean = false): string;
  begin
    Result := EnumName(typeinfo(TLuaArgType), byte(T));
    if (DelPrefics) then Delete(Result, 1, 2);
  end;
begin
  if (LuaType <> NeededType) then
  ELua.Assert('Argument can''t be getted as %s because current type is "%s"',
             [TypeToString(NeededType), TypeToString(LuaType, true)], CodeAddr);
end;

function TLuaArg.GetLuaTypeName: string;
begin
  Result := EnumName(typeinfo(TLuaArgType), byte(FLuaType));
end;

function TLuaArg.GetEmpty: boolean;
begin
  GetEmpty := (FLuaType = ltEmpty);
end;

procedure TLuaArg.SetEmpty(const Value: boolean);
begin
  if (Value) then
  begin
    str_data := '';
    pinteger(@FLuaType)^ := 0;
    Data[0] := 0;
    Data[1] := 0;
  end;
end;

function __TLuaArgGetBoolean(const Self: TLuaArg; const ReturnAddr: pointer): boolean;
begin
  if (Self.LuaType <> ltBoolean) then Self.Assert(ltBoolean, ReturnAddr);
  __TLuaArgGetBoolean := (Self.Data[0] <> 0);
end;

function TLuaArg.GetBoolean(): boolean;
asm
  mov edx, [esp]
  jmp __TLuaArgGetBoolean
end;

procedure TLuaArg.SetBoolean(const Value: boolean);
begin
  FLuaType := ltBoolean;
  Data[0] := ord(Value);
end;

function __TLuaArgGetInteger(const Self: TLuaArg; const ReturnAddr: pointer): integer;
begin
  if (Self.LuaType <> ltInteger) then Self.Assert(ltInteger, ReturnAddr);
  __TLuaArgGetInteger := Self.Data[0];
end;

function TLuaArg.GetInteger(): integer;
asm
  mov edx, [esp]
  jmp __TLuaArgGetInteger
end;

procedure TLuaArg.SetInteger(const Value: integer);
begin
  FLuaType := ltInteger;
  Data[0] := Value;
end;

function __TLuaArgGetDouble(const Self: TLuaArg; const ReturnAddr: pointer): double;
begin
  if (Self.LuaType = ltInteger) then __TLuaArgGetDouble := Self.Data[0]
  else
  begin
    if (Self.LuaType <> ltDouble) then Self.Assert(ltDouble, ReturnAddr);
    __TLuaArgGetDouble := pdouble(@Self.Data)^;
  end;
end;

function TLuaArg.GetDouble(): double;
asm
  mov edx, [esp]
  jmp __TLuaArgGetDouble
end;

procedure TLuaArg.SetDouble(Value: double);
begin
  if (NumberToInteger(Value, Data[0])) then
  begin
    FLuaType := ltInteger;
  end else
  begin
    FLuaType := ltDouble;
    pdouble(@Data)^ := Value;
  end;
end;

procedure __TLuaArgGetString(const Self: TLuaArg; var Result: string; const ReturnAddr: pointer);
begin
  if (Self.LuaType <> ltString) then Self.Assert(ltString, ReturnAddr);
  Result := Self.str_data;
end;

function TLuaArg.GetString(): string;
asm
  mov ecx, [esp]
  jmp __TLuaArgGetString
end;

procedure TLuaArg.SetString(const Value: string);
begin
  str_data := Value;
  FLuaType := ltString;
end;

function __TLuaArgGetPointer(const Self: TLuaArg; const ReturnAddr: pointer): pointer;
begin
  if (Self.LuaType = ltEmpty) then __TLuaArgGetPointer := nil
  else
  begin
    if (Self.LuaType <> ltPointer) then Self.Assert(ltPointer, ReturnAddr);
    __TLuaArgGetPointer := pointer(Self.Data[0]);
  end;
end;

function TLuaArg.GetPointer(): pointer;
asm
  mov edx, [esp]
  jmp __TLuaArgGetPointer
end;

procedure TLuaArg.SetPointer(const Value: pointer);
begin
  pointer(Data[0]) := Value;

  if (Value <> nil) then FLuaType := ltPointer
  else FLuaType := ltEmpty;
end;

procedure __TLuaArgGetVariant(var Self: TLuaArg; var Result: Variant; const ReturnAddr: pointer);
begin
  if (Self.FLuaType in [ltEmpty, ltBoolean, ltInteger, ltDouble, ltString]) then Result := Self.ForceVariant
  else
  begin
    Self.str_data := EnumName(typeinfo(TLuaArgType), byte(Self.FLuaType));
    ELua.Assert('Argument can''t be getted as Variant because current type is "%s"', [Self.str_data], ReturnAddr);
  end;
end;

function TLuaArg.GetVariant(): Variant;
asm
  mov ecx, [esp]
  jmp __TLuaArgGetVariant
end;

// 0: TDateTime, 1: TDate, 2: TTime
function InspectDateTime(const Value: PDateTime): integer;
var
  TR: integer;
  FR: integer;
asm
  fld qword ptr [eax]
  fld st(0)

  sub esp, 4
  FNSTCW word ptr[esp]
  FNSTCW word ptr[esp+2]
  OR word ptr[esp+2], $0F00  // trunc toward zero, full precision
  FLDCW word ptr[esp+2]
  FISTP TR
  FLDCW word ptr[esp]
  add esp, 4
  mov edx, TR

  fisub TR
  fstp FR
  mov ecx, FR

  // результат
  xor  eax, eax
  test edx, edx
  jnz @1
  add eax, 2
  @1:
  test ecx, ecx
  jnz @2
  inc eax
  @2:
  cmp eax, 3
  jne @3
  xor eax, eax
  @3:
end;

procedure __TLuaArgsetVariant(var Self: TLuaArg; const Value: Variant; const ReturnAddr: pointer);
type
  TDateProc = procedure(const DateTime: TDateTime; var Ret: string);

begin
  with TVarData(Value) do
  case (VType) of
    varEmpty, varNull: Self.FLuaType := ltEmpty;
    varSmallint: Self.SetInteger(VSmallInt);
    varInteger : Self.SetInteger(VInteger);
    varSingle  : Self.SetDouble(VSingle);
    varDouble  : Self.SetDouble(VDouble);
    varCurrency: Self.SetDouble(VCurrency);
    varDate    : begin
                   // SetString(DateTimeToStr(VDate));
                   Self.FLuaType := ltString;
                   if (Self.str_data <> '') then Self.str_data := '';
                   case InspectDateTime(@VDate) of
                     0: TDateProc(@DateTimeToStr)(VDate, Self.str_data);
                     1: TDateProc(@DateToStr)(VDate, Self.str_data);
                     2: TDateProc(@TimeToStr)(VDate, Self.str_data);
                   end;
                 end;
    varOleStr  : begin
                   // SetString(ansistring(VOleStr));
                   Self.FLuaType := ltString;
                   Self.str_data := VOleStr;
                 end;
    varBoolean : Self.SetBoolean(VBoolean);
    varShortInt: Self.SetInteger(VShortInt);
    varByte    : Self.SetInteger(VByte);
    varWord    : Self.SetInteger(VWord);
    varLongWord: Self.SetInteger(VLongWord);
    varInt64   : Self.SetDouble(VInt64);
    varString  : Self.SetString(string(VString));
  else
    ELua.Assert('Unknown variant type %d in "TLuaArg.SetVariant" procedure', [VType], ReturnAddr);
  end;
end;

procedure TLuaArg.SetVariant(const Value: Variant);
asm
  mov ecx, [esp]
  jmp __TLuaArgsetVariant
end;

function __TLuaArgGetClass(const Self: TLuaArg; const ReturnAddr: pointer): TClass;
begin
  if (Self.LuaType = ltEmpty) then __TLuaArgGetClass := nil
  else
  begin
    if (Self.LuaType <> ltClass) then Self.Assert(ltClass, ReturnAddr);
    __TLuaArgGetClass := TClass(Self.Data[0]);
  end;
end;

function TLuaArg.GetClass(): TClass;
asm
  mov edx, [esp]
  jmp __TLuaArgGetClass
end;


procedure TLuaArg.SetClass(const Value: TClass);
begin
  TClass(Data[0]) := Value;

  if (Value <> nil) then FLuaType := ltClass
  else FLuaType := ltEmpty;
end;

function __TLuaArgGetObject(const Self: TLuaArg; const ReturnAddr: pointer): TObject;
begin
  if (Self.LuaType = ltEmpty) then __TLuaArgGetObject := nil
  else begin
    if (Self.LuaType <> ltObject) then Self.Assert(ltObject, ReturnAddr);
    __TLuaArgGetObject := TObject(Self.Data[0]);
  end;
end;

function TLuaArg.GetObject(): TObject;
asm
  mov edx, [esp]
  jmp __TLuaArgGetObject
end;


procedure TLuaArg.SetObject(const Value: TObject);
begin
  TObject(Data[0]) := Value;

  if (Value <> nil) then FLuaType := ltObject
  else FLuaType := ltEmpty;
end;

procedure __TLuaArgGetRecord(const Self: TLuaArg; var Result: TLuaRecord; const ReturnAddr: pointer);
begin
  if (Self.LuaType <> ltRecord) then Self.Assert(ltRecord, ReturnAddr);
  Result := PLuaRecord(@Self.FLuaType)^;
end;

function TLuaArg.GetRecord(): TLuaRecord;
asm
  mov ecx, [esp]
  jmp __TLuaArgGetRecord
end;


procedure __TLuaArgsetRecord(var Self: TLuaArg; const Value: TLuaRecord; const ReturnAddr: pointer);
begin
  if (Value.Data = nil) then
  ELua.Assert('LuaRecord.Data = nil. LuaRecord should point to a record', ReturnAddr);

  if (Value.Info = nil) then
  ELua.Assert('LuaRecord.Info is not defined', ReturnAddr);

  if (byte(Value.FIsRef) > 1) then
  ELua.Assert('LuaRecord.IsRef is not defined', ReturnAddr);

  PLuaRecord(@Self.FLuaType)^ := Value;
  Self.FLuaType := ltRecord;
end;

procedure TLuaArg.SetRecord(const Value: TLuaRecord);
asm
  mov ecx, [esp]
  jmp __TLuaArgsetRecord
end;

procedure __TLuaArgGetArray(const Self: TLuaArg; var Result: TLuaArray; const ReturnAddr: pointer);
begin
  if (Self.LuaType <> ltArray) then Self.Assert(ltArray, ReturnAddr);
  Result := PLuaArray(@Self.FLuaType)^;
end;

function  TLuaArg.GetArray: TLuaArray;
asm
  mov ecx, [esp]
  jmp __TLuaArgGetArray
end;

procedure __TLuaArgsetArray(var Self: TLuaArg; const Value: TLuaArray; const ReturnAddr: pointer);
begin
  if (Value.Data = nil) then
  ELua.Assert('LuaArray.Data = nil. LuaArray should point to an array', ReturnAddr);

  if (Value.Info = nil) then
  ELua.Assert('LuaArray.Info is not defined', ReturnAddr);

  if (byte(Value.FIsRef) > 1) then
  ELua.Assert('LuaArray.IsRef is not defined', ReturnAddr);

  PLuaArray(@Self.FLuaType)^ := Value;
  Self.FLuaType := ltArray;
end;

procedure TLuaArg.SetArray(const Value: TLuaArray);
asm
  mov ecx, [esp]
  jmp __TLuaArgsetArray
end;

procedure __TLuaArgGetSet(const Self: TLuaArg; var Result: TLuaSet; const ReturnAddr: pointer);
begin
  if (Self.LuaType <> ltSet) then Self.Assert(ltSet, ReturnAddr);
  Result := PLuaSet(@Self.FLuaType)^;
end;

function  TLuaArg.GetSet: TLuaSet;
asm
  mov ecx, [esp]
  jmp __TLuaArgGetSet
end;

procedure __TLuaArgsetSet(var Self: TLuaArg; const Value: TLuaSet; const ReturnAddr: pointer);
begin
  if (Value.Data = nil) then
  ELua.Assert('LuaSet.Data = nil. LuaSet should point to an array', ReturnAddr);

  if (Value.Info = nil) then
  ELua.Assert('LuaSet.Info is not defined', ReturnAddr);

  if (byte(Value.FIsRef) > 1) then
  ELua.Assert('LuaSet.IsRef is not defined', ReturnAddr);

  PLuaSet(@Self.FLuaType)^ := Value;
  Self.FLuaType := ltSet;
end;

procedure TLuaArg.SetSet(const Value: TLuaSet);
asm
  mov ecx, [esp]
  jmp __TLuaArgsetSet
end;

function __TLuaArgGetTable(const Self: TLuaArg; const ReturnAddr: pointer): PLuaTable;
begin
  if (Self.LuaType <> ltTable) then Self.Assert(ltTable, ReturnAddr);
  __TLuaArgGetTable := PLuaTable(@Self.FLuaType);
end;

function TLuaArg.GetTable(): PLuaTable;
asm
  mov edx, [esp]
  jmp __TLuaArgGetTable
end;
                                          

// 0 - false
// 1 - true
// <0 - fail
function __cast_string_as_boolean(const S: string): integer;
// generated by "Static Serializer"
type
  __TAnsiPointerData = packed record
  case integer of
    0: (chars: array[0..high(integer)-1] of ansichar);
    1: (words: array[0..high(integer)div 2-1] of word);
    2: (dwords: array[0..high(integer)div 4-1] of dword);
  end;
begin
  Result := low(integer); {fail = not defined}

  { not case sensitive, ansi }
  with __TAnsiPointerData(pointer(S)^) do
  case (Length(S)) of
   0: Result := 0; // empty string
   1: case (chars[0]) of
        '0': Result := 0; // "0"
        '1': Result := 1; // "1"
      end;
   2: case (words[0]) of
        $6F6E,$6F4E,$4F6E,$4F4E: Result := 0; // "no"
        $6B6F,$6B4F,$4B6F,$4B4F: Result := 1; // "ok"
      end;
   3: if (chars[0]in['y','Y'])and(chars[1]in['e','E'])and(chars[2]in['s','S']) then Result := 1; // "yes"
   4: case (words[0]) of
        $6F6E,$6F4E,$4F6E,$4F4E: if (chars[2]in['n','N'])and(chars[3]in['e','E']) then Result := 0; // "none"
        $7274,$7254,$5274,$5254: if (chars[2]in['u','U'])and(chars[3]in['e','E']) then Result := 1; // "true"
      end;
   5: if ((dwords[0]=$736C6166)or((chars[0]in['f','F'])and(chars[1]in['a','A'])and
          (chars[2]in['l','L'])and(chars[3]in['s','S'])))
      and(chars[4]in['e','E']) then Result := 0; // "false"

   6: if ((dwords[0]=$636E6163)or((chars[0]in['c','C'])and(chars[1]in['a','A'])and
          (chars[2]in['n','N'])and(chars[3]in['c','C'])))
      and(chars[4]in['e','E'])and(chars[5]in['l','L']) then Result := 0; // "cancel"
  end;
end;


function TLuaArg.ForceBoolean: boolean;
begin
  case LuaType of
      ltEmpty: ForceBoolean := false;
    ltBoolean: ForceBoolean := (Data[0] <> 0);
    ltInteger: ForceBoolean := (Data[0] > 0);
     ltDouble: ForceBoolean := (pdouble(@Data)^ > 0);
     ltString: ForceBoolean := (__cast_string_as_boolean(str_data) > 0);
  else
    ForceBoolean := (Data[0] <> 0);;
  end;
end;

function TLuaArg.ForceInteger: integer;
begin
  case LuaType of
      ltEmpty: ForceInteger := 0;
     ltDouble: ForceInteger := Trunc(pdouble(@Data)^);
     ltString: ForceInteger := StrToIntDef(str_data, 0);
     ltObject: ForceInteger := TObject(Data[0]).InstanceSize;
     ltRecord: ForceInteger := integer(PLuaRecord(@FLuaType).Info.Size);
   ltTable: ForceInteger := PLuaTable(@FLuaType).Length;
  else
    ForceInteger := Data[0];
  end;
end;

function TLuaArg.ForceDouble: double;
begin
  case LuaType of
      ltEmpty: ForceDouble := 0;
    ltBoolean: ForceDouble := Data[0];
    ltInteger: ForceDouble := Data[0];
     ltDouble: ForceDouble := pdouble(@Data)^;
     ltString: ForceDouble := StrToFloatDef(str_data, 0);
  else
    ForceDouble := 0;
  end;
end;

function TLuaArg.ForceString: string;
const
  BOOLEANS: array[boolean] of string = ('false', 'true');
begin
  case LuaType of
    ltBoolean: ForceString := BOOLEANS[Data[0] <> 0];
    ltInteger: ForceString := IntToStr(Data[0]);
     ltDouble: ForceString := Format('%0.2f', [pdouble(@Data)^]);
     ltString: ForceString := str_data;
    ltPointer: ForceString := IntToHex(Data[0], 8);
      ltClass: ForceString := TClass(Data[0]).ClassName;
     ltObject: begin
                if (TObject(Data[0]) is TComponent) then ForceString := TComponent(Data[0]).Name
                else ForceString := TObject(Data[0]).ClassName;
               end;
     ltRecord: ForceString := PLuaRecord(@FLuaType).Info.Name;
      ltArray: ForceString := PLuaArray(@FLuaType).Info.Name;
        ltSet: with PLuaSet(@FLuaType)^ do ForceString := Info.Description(Data);
      ltTable: ForceString := 'LuaTable';
  else
    {ltEmpty:}
    ForceString := 'nil';
  end;
end;

function TLuaArg.ForcePointer: pointer;
begin
  case LuaType of
    ltPointer, ltClass, ltObject, ltRecord, ltArray, ltSet: ForcePointer := pointer(Data[0]);
    ltString: ForcePointer := pointer(str_data);
     ltTable: ForcePointer := PLuaTable(@FLuaType);
  else
    ForcePointer := nil;
  end;
end;

function TLuaArg.ForceVariant: Variant;
var
  VarData: TVarData absolute Result;
begin
  // очистка если был занят
  if (VarData.VType = varString) or (not (VarData.VType in VARIANT_SIMPLE)) then VarClear(Result);

  // присвоить значение
  case (LuaType) of
    ltBoolean: begin
                 VarData.VType := varBoolean;
                 VarData.VBoolean := Data[0] <> 0;
               end;
    ltInteger: begin
                 VarData.VType := varInteger;
                 VarData.VInteger := Data[0];
               end;
     ltDouble: begin
                 VarData.VType := varDouble;
                 VarData.VDouble := double(Data);
               end;
     ltString: begin
                 { Unicode ? todo}
                 VarData.VType := varString;
                 VarData.VInteger := 0;

                 if (str_data <> '') then
                 string(VarData.VString) := str_data;
               end;
   else
     {ltEmpty и др:}
     VarData.VType := varEmpty;
  end;
end;

function TLuaArg.ForceClass: TClass;
begin
  case LuaType of
    ltClass: ForceClass := TClass(Data[0]);
   ltObject: ForceClass := TClass(pointer(Data[0])^);
  else
    ForceClass := nil;
  end;
end;

function TLuaArg.ForceObject: TObject;
begin
  case LuaType of
    ltObject: ForceObject := TObject(Data[0]);
  else
    ForceObject := nil;
  end;  
end;

function TLuaArg.ForceRecord: TLuaRecord;
begin
  case LuaType of
    ltRecord: Result := PLuaRecord(@FLuaType)^;
  else
    ZeroMemory(@Result, sizeof(Result));
  end;
end;

function TLuaArg.ForceArray: TLuaArray;
begin
  case LuaType of
    ltArray: Result := PLuaArray(@FLuaType)^;
  else
    ZeroMemory(@Result, sizeof(Result));
  end;
end;

function TLuaArg.ForceSet: TLuaSet;
begin
  case LuaType of
    ltSet: Result := PLuaSet(@FLuaType)^;
  else
    ZeroMemory(@Result, sizeof(Result));
  end;
end;

function TLuaArg.ForceTable: PLuaTable;
begin
  case LuaType of
    ltTable: ForceTable := PLuaTable(@FLuaType);
  else
    ForceTable := nil;
  end;
end;


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
(*  if (pop) then Lua.stack_pop();
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
*)
end;

procedure TLuaPair.ThrowBroken(const CodeAddr: pointer; const Action: string);
begin
(*  ELua.Assert('Can''t %s, because the Item is broken', [Action], CodeAddr); *)
end;

function TLuaPair.Initialize(const ALua: TLua; const AIndex: integer; const UseKey: boolean): boolean;
const
  MODES: array[boolean] of integer = (PAIRS_BROKEN, PAIRS_ITERATING);
begin
  Result := false;
(*  Lua := ALua;
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
  Mode := MODES[Result]; *)
end;

function __TLuaPairGetBroken(const Self: TLuaPair; const ReturnAddr: pointer): boolean;
begin
  Result := false;
(*  case (Self.Mode) of
    PAIRS_ITERATING: Result := false;
    PAIRS_BROKEN: Result := true;
  else
    Result := false;
    Self.ThrowNotInitialized(ReturnAddr);
  end; *)
end;

function TLuaPair.GetBroken: boolean;
asm
  mov edx, [esp]
  jmp __TLuaPairGetBroken
end;

procedure __TLuaPairGetKey(const Self: TLuaPair; var Result: string; const ReturnAddr: pointer);
begin
(*  with Self do
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
  end; *)
end;

function TLuaPair.GetKey: string;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetKey
end;


procedure __TLuaPairGetKeyEx(const Self: TLuaPair; var Result: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get key');
    PAIRS_ITERATING: Lua.stack_variant(Result, KeyIndex);
  else
    ThrowNotInitialized(ReturnAddr);
  end; *)
end;

function TLuaPair.GetKeyEx: Variant;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetKeyEx
end;

procedure __TLuaPairGetValue(const Self: TLuaPair; var Result: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get value');
    PAIRS_ITERATING: Lua.stack_variant(Result, ValueIndex);
  else
    ThrowNotInitialized(ReturnAddr);
  end; *)
end;

function TLuaPair.GetValue: Variant;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetValue
end;

procedure __TLuaPairSetValue(const Self: TLuaPair; const AValue: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
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
  end;  *)
end;

procedure TLuaPair.SetValue(const AValue: Variant);
asm
  mov ecx, [esp]
  jmp __TLuaPairSetValue
end;

procedure __TLuaPairGetValueEx(const Self: TLuaPair; var Result: TLuaArg; const ReturnAddr: pointer);
begin
(*  with Self do
  case (Mode) of
    PAIRS_BROKEN: ThrowBroken(ReturnAddr, 'get value');
    PAIRS_ITERATING: Lua.stack_luaarg(Result, ValueIndex, true);
  else
    ThrowNotInitialized(ReturnAddr);
  end; *)
end;

function TLuaPair.GetValueEx: TLuaArg;
asm
  mov ecx, [esp]
  jmp __TLuaPairGetValueEx
end;

procedure __TLuaPairSetValueEx(const Self: TLuaPair; const AValue: TLuaArg; const ReturnAddr: pointer);
begin
(*  with Self do
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
  end;  *)
end;

procedure TLuaPair.SetValueEx(const AValue: TLuaArg);
asm
  mov ecx, [esp]
  jmp __TLuaPairSetValueEx
end;

function __TLuaPairNext(var Self: TLuaPair; const ReturnAddr: pointer): boolean;
begin
  Result := false;
(*  with Self do
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
  end; *)
end;

function TLuaPair.Next(): boolean;
asm
  mov edx, [esp]
  jmp __TLuaPairNext
end;

procedure __TLuaPairBreak(var Self: TLuaPair; const ReturnAddr: pointer);
begin
(*  with Self do
  case (Mode) of
    PAIRS_BROKEN: ;
 PAIRS_ITERATING: Mode := PAIRS_BROKEN;
  else
    ThrowNotInitialized(ReturnAddr);
  end; *)
end;

procedure TLuaPair.Break();
asm
  mov edx, [esp]
  jmp __TLuaPairBreak
end;



{ TLuaTable }

procedure TLuaTable.ThrowValueType(const CodeAddr: pointer; const pop: boolean);
begin
 (* if (pop) then Lua.stack_pop();
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
 *)
end;

// максимальный индекс целочисленного индекса
function TLuaTable.GetLength: integer;
begin
  GetLength := lua_objlen(Lua.Handle, Index_);
end;

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
  Result := false;
(*  if (not Self.Lua.push_variant(FromKey)) then Self.ThrowValueType(ReturnAddr);
  Result := Pair.Initialize(Self.Lua, Self.Index_, true);
*)
end;

function TLuaTable.Pairs(var Pair: TLuaPair; const FromKey: Variant): boolean;
asm
  push [esp]
  jmp __TLuaTablePairs
end;

procedure __TLuaTableGetValue(const Self: TLuaTable; const AIndex: integer; var Result: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    lua_rawgeti(Lua.Handle, Index_, AIndex);
    if (not Lua.stack_variant(Result, -1)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end; *)
end;

function  TLuaTable.GetValue(const AIndex: integer): Variant;
asm
  push [esp]
  jmp  __TLuaTableGetValue
end;

procedure __TLuaTableSetValue(const Self: TLuaTable; const AIndex: integer; const NewValue: Variant; const ReturnAddr: pointer);
begin
(*  if (not Self.Lua.push_variant(NewValue)) then Self.ThrowValueType(ReturnAddr);
  lua_rawseti(Self.Lua.Handle, Self.Index_, AIndex);
*)  
end;

procedure TLuaTable.SetValue(const AIndex: integer; const NewValue: Variant);
asm
  push [esp]
  jmp __TLuaTableSetValue
end;

procedure __TLuaTableGetValueEx(const Self: TLuaTable; const AIndex: integer; var Result: TLuaArg; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    lua_rawgeti(Lua.Handle, Index_, AIndex);
    if (not Lua.stack_luaarg(Result, -1, false)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
*)  
end;

function  TLuaTable.GetValueEx(const AIndex: integer): TLuaArg;
asm
  push [esp]
  jmp __TLuaTableGetValueEx
end;

procedure __TLuaTableSetValueEx(const Self: TLuaTable; const AIndex: integer; const NewValue: TLuaArg; const ReturnAddr: pointer);
begin
(*  if (not Self.Lua.push_luaarg(NewValue)) then Self.ThrowValueType(ReturnAddr);
  lua_rawseti(Self.Lua.Handle, Self.Index_, AIndex);
*)
end;

procedure TLuaTable.SetValueEx(const AIndex: integer; const NewValue: TLuaArg);
asm
  push [esp]
  jmp __TLuaTableSetValueEx
end;

procedure __TLuaTableGetKeyValue(const Self: TLuaTable; const Key: string; var Result: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    lua_push_pascalstring(Lua.Handle, Key);
    lua_rawget(Lua.Handle, Index_);
    if (not Lua.stack_variant(Result, -1)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
*)  
end;

function  TLuaTable.GetKeyValue(const Key: string): Variant;
asm
  push [esp]
  jmp __TLuaTableGetKeyValue
end;

procedure __TLuaTableSetKeyValue(const Self: TLuaTable; const Key: string; const NewValue: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    lua_push_pascalstring(Lua.Handle, Key);
    if (not Lua.push_variant(NewValue)) then ThrowValueType(ReturnAddr, true);
    lua_rawset(Lua.Handle, Index_);
  end;
*)  
end;

procedure TLuaTable.SetKeyValue(const Key: string; const NewValue: Variant);
asm
  push [esp]
  jmp __TLuaTableSetKeyValue
end;

procedure __TLuaTableGetKeyValueEx(const Self: TLuaTable; const Key: Variant; var Result: TLuaArg; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    if (not Lua.push_variant(Key)) then ThrowValueType(ReturnAddr);
    lua_rawget(Lua.Handle, Index_);
    if (not Lua.stack_luaarg(Result, -1, false)) then ThrowValueType(ReturnAddr, true);
    lua_settop(Lua.Handle, -1-1);
  end;
*)  
end;

function  TLuaTable.GetKeyValueEx(const Key: Variant): TLuaArg;
asm
  push [esp]
  jmp __TLuaTableGetKeyValueEx
end;         

procedure __TLuaTableSetKeyValueEx(const Self: TLuaTable; const Key: Variant; const NewValue: TLuaArg; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    if (not Lua.push_variant(Key)) then ThrowValueType(ReturnAddr);
    if (not Lua.push_luaarg(NewValue)) then ThrowValueType(ReturnAddr, true);
    lua_rawset(Lua.Handle, Index_);
  end; *)
end;

procedure TLuaTable.SetKeyValueEx(const Key: Variant; const NewValue: TLuaArg);
asm
  push [esp]
  jmp __TLuaTableSetKeyValueEx
end;



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
    global_free_ref(Index); // luaL_unref(Handle, LUA_GLOBALSINDEX, Index);

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
    global_alloc_ref(Index); // Index := luaL_ref(Handle, LUA_GLOBALSINDEX);
    lua_rawseti(Handle, LUA_GLOBALSINDEX, Index); // nil, который уже в стеке
  end else
  begin
    global_alloc_ref(Index); // Index := luaL_ref(Handle, LUA_GLOBALSINDEX);
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
(*  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
*)
end;

procedure __TLuaReferenceGetValue(const Self: TLuaReference; var Result: Variant; const ReturnAddr: pointer);
begin
(*  if (Self.Locked) then Self.ThrowLocked('get value', ReturnAddr);

  with Self.Lua do
  begin
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, Self.Index);
    stack_variant(Result, -1);
    lua_settop(Handle, -1-1);
  end;
*)  
end;

function  TLuaReference.GetValue(): Variant;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceGetValue
end;

procedure __TLuaReferenceSetValue(const Self: TLuaReference; const NewValue: Variant; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    if (Locked) then ThrowLocked('change value', ReturnAddr);
    if (not Lua.push_variant(NewValue)) then ThrowValueType(ReturnAddr);
    lua_rawseti(Lua.Handle, LUA_GLOBALSINDEX, Index);
  end;
*)  
end;

procedure TLuaReference.SetValue(const NewValue: Variant);
asm
  mov ecx, [esp]
  jmp __TLuaReferenceSetValue
end;

procedure __TLuaReferenceGetValueEx(const Self: TLuaReference; var Result: TLuaArg; const ReturnAddr: pointer);
begin
(*  if (Self.Locked) then Self.ThrowLocked('get value', ReturnAddr);
  with Self.Lua do
  begin
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, Self.Index);
    stack_luaarg(Result, -1, false);
    lua_settop(Handle, -1-1);
  end;
*)  
end;

function  TLuaReference.GetValueEx(): TLuaArg;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceGetValueEx
end;

procedure __TLuaReferenceSetValueEx(const Self: TLuaReference; const NewValue: TLuaArg; const ReturnAddr: pointer);
begin
(*  with Self do
  begin
    if (Locked) then ThrowLocked('change value', ReturnAddr);

    // Push
    if (NewValue.LuaType = ltTable) then
    begin
      lua_pushvalue(Lua.Handle, PLuaTable(@NewValue.FLuaType).Index_);
    end else
    if (not Lua.push_luaarg(NewValue)) then ThrowValueType(ReturnAddr);

    // SetValue
    lua_rawseti(Lua.Handle, LUA_GLOBALSINDEX, Index);
  end;
*)  
end;

procedure TLuaReference.SetValueEx(const NewValue: TLuaArg);
asm
  mov ecx, [esp]
  jmp __TLuaReferenceSetValueEx
end;


function __TLuaReferenceAsTableBegin(const Self: TLuaReference; var Table: PLuaTable; const ReturnAddr: pointer): boolean;
begin
  Result := false;
(*  with Self do
  begin
    if (Locked) then
    ELua.Assert('Can''t lock table value, because the reference is already locked', [], ReturnAddr);

    // посмотреть на данные, вернуть результат
    with Lua do
    begin
      lua_rawgeti(Handle, LUA_GLOBALSINDEX, Index);
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
*)  
end;

function TLuaReference.AsTableBegin(var Table: PLuaTable): boolean;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceAsTableBegin
end;

function __TLuaReferenceAsTableEnd(const Self: TLuaReference; var Table: PLuaTable; const ReturnAddr: pointer): boolean;
//var
  //Delta: integer;
begin
  Result := false;
(*  with Self do
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
  end; *)
end;

function TLuaReference.AsTableEnd(var Table: PLuaTable): boolean;
asm
  mov ecx, [esp]
  jmp __TLuaReferenceAsTableEnd
end;



{ TLuaRecordInfo }

function UnpackString(const Lua: TLua; const S: __luaname): LuaString; overload; forward;

function TLuaRecordInfo.GetName: LuaString;
begin
  GetName := UnpackString(Lua, FType.name);
end;

procedure __TLuaRecordInfoRegField(const Lua: TLua; const FieldName: LuaString;
          const FieldPointer: pointer; const tpinfo: pointer; const Self: PLuaRecordInfo);
begin
  // todo
end;

procedure TLuaRecordInfo.RegField;
asm
  pop ebp
  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoRegField
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRecordInfoRegProperty(const Lua: TLua; const PropertyName: LuaString;
          const tpinfo: pointer; const PGet, PSet: pointer; const Self: PLuaRecordInfo);
begin
  // todo
end;

procedure TLuaRecordInfo.RegProperty;
asm
  pop ebp
  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoRegProperty
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRecordInfoRegParameterProperty(const Lua: TLua; const AClass: TClass;
          const PropertyName: LuaString; const Parameters: PLuaRecordInfo;{todo LuaString-->autobinding}
          const tpinfo: pointer; const PGet, PSet: pointer; const is_default: boolean;
          const Self: PLuaRecordInfo);
begin
  // todo
end;          

procedure TLuaRecordInfo.RegParameterProperty;
asm
  pop ebp
  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoRegParameterProperty
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRecordInfoRegProc(const Lua: TLua; const AClass: TClass; const ProcName: LuaString;
          const Address: pointer{TTClassRecallProc}; const Self: PLuaRecordInfo);
begin
  // todo
end;          

procedure TLuaRecordInfo.RegProc;
asm
  pop ebp
  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoRegProc
  jmp TLua.InternalRegisterRecall
end;

{function TLuaRecordInfo.GetFieldsCount: integer;
begin
  Result := 0;
  (*Result := Length(FType.Lua.ClassesInfo[FClassIndex].Properties);
  *)
end;  }

// tpinfo может быть:
// - typeinfo(type)
// - PLuaRecordInfo
//procedure TLuaRecordInfo.InternalRegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer);
//begin

//end;
(*type
  TDataBuffer = array[0..sizeof(TLuaPropertyInfo)-1] of byte;
var
  i, j: integer;
  Buffer: TDataBuffer;
begin
  // провеки ?

  // регистрация
  FType.Lua.InternalAddProperty(false, @Self, FieldName, tpinfo, false, false,
       pointer(FieldOffset), pointer(FieldOffset), nil);

  // сортировка полей по возрастанию
  with FType.Lua.ClassesInfo[FClassIndex] do
  for i := 0 to Length(Properties)-2 do
  for j := i+1 to Length(Properties)-1 do
  if (integer(Properties[i].read_mode) > integer(Properties[j].read_mode)) then
  begin
    // swap i <--> j
    Buffer := TDataBuffer(Properties[i]);
    TDataBuffer(Properties[i]) := TDataBuffer(Properties[j]);
    TDataBuffer(Properties[j]) := Buffer;
  end;
end;  *)


(*procedure __TLuaRecordInfoRegField_1(const ALua: TLua; const FieldName: string;
          const FieldOffset: integer; const tpinfo: pointer; const Self: TLuaRecordInfo);
begin
  Self.InternalRegField(FieldName, FieldOffset, tpinfo);
end;

procedure TLuaRecordInfo.RegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer);
asm
  pop ebp

  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoRegField_1
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRecordInfoRegField_2(const ALua: TLua; const FieldName: string;
          const FieldPointer: pointer; const tpinfo: pointer; const pRecord: pointer; const Self: TLuaRecordInfo);
begin
  if (integer(pRecord) > integer(FieldPointer)) then
  ALua.Assert('Illegal parameters using: FieldPointer and pRecord');

  Self.InternalRegField(FieldName, integer(FieldPointer)-integer(pRecord), tpinfo);
end;

procedure TLuaRecordInfo.RegField(const FieldName: string; const FieldPointer: pointer; const tpinfo: pointer; const pRecord: pointer = nil);
asm
  pop ebp

  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoRegField_2
  jmp TLua.InternalRegisterRecall
end; *)



{procedure __TLuaRecordInfoRegProc(const ALua: TLua; const ProcName: string; const Proc: TTClassRecallProc;
                                  const ArgsCount: integer; const Self: TLuaRecordInfo);
begin
(*
  Self.FType.Lua.InternalAddProc(false, @Self, ProcName, ArgsCount, false, TMethod(Proc).Code);
*)  
end; }

(*procedure TLuaRecordInfo.RegProc(const ProcName: string; const Proc: TTClassRecallProc; const ArgsCount: integer=-1);
asm
  pop ebp

  // в связи со спецификой InternalRegisterRecall
  // надо указать Self.FLua в качестве eax
  // а Self тогда кидаем последним параметром, не забывая, что адрес возврата должен быть последним
  push [esp]
  mov [esp+4], eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua

  // вызываем TLuaRecordInfoRegProc
  push offset __TLuaRecordInfoRegProc
  jmp TLua.InternalRegisterRecall
end; *)

procedure __TLuaRecordInfoSetOperators(const Lua: TLua; const Value: TLuaOperators; const Self: PLuaRecordInfo);
begin
  Self.FOperators := Value;
end;

procedure TLuaRecordInfo.SetOperators(const Value: TLuaOperators);
asm
  mov ecx, eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoSetOperators
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRecordInfoSetOperatorCallback(const Lua: TLua; const Value: TLuaOperatorCallback; const Self: PLuaRecordInfo);
begin
  Self.FOperatorCallback := Value;
end;

procedure TLuaRecordInfo.SetOperatorCallback(const Value: TLuaOperatorCallback);
asm
  mov ecx, eax
  mov eax, [EAX].TLuaRecordInfo.FType.Lua
  push offset __TLuaRecordInfoSetOperatorCallback
  jmp TLua.InternalRegisterRecall
end;


{ TLuaArrayInfo }

function TLuaArrayInfo.GetName: LuaString;
begin
  GetName := UnpackString(Lua, FType.name);
end;

{ TLuaSetInfo }

function TLuaSetInfo.Description(const X: pointer): string;
var
  P1, P2, i: integer;

  procedure Add();
  const
    CHARS: array[boolean] of string = (', ', '..');
  begin
    if (P1 < 0) then exit;
    if (Result <> '') then Result := Result + ', ';
    Result := Result + TypInfo.GetEnumName(FTypeInfo, P1);
    if (P2 <> P1) then Result := Result + CHARS[P2>P1+1] + TypInfo.GetEnumName(FTypeInfo, P2);

    P1 := -1;
    P2 := -1;
  end;
begin
  P1 := -1;
  P2 := -1;

  for i := FLow to FHigh do
  if (SetBitContains(X, i-FCorrection)) then
  begin
    if (P1 < 0) then P1 := i;
    P2 := i;
  end else
  begin
    if (P1 >= 0) then Add();
  end;

  Add();
  Result := '['+Result+']';
end;


function TLuaSetInfo.GetName: LuaString;
begin
  GetName := UnpackString(Lua, FType.name);
end;


{ TLuaPropertyInfo }

const
  PROP_NONE_USE = pointer($80000000);
  TMETHOD_CLASS_INDEX = 1;
  TLUA_REFERENCE_CLASS_INDEX = 2;

  MODE_NONE_USE = integer(PROP_NONE_USE);
  MODE_PROC_USE = -1;

const
  // различные kind __TLuaType
  LUATYPE_GLOBAL = 0;
  LUATYPE_DIFFICULT_PROPERTY = 1;
  LUATYPE_EVENT_PROPERTY = 2; // todo
  //
  LUATYPE_CLASS = 3;
  LUATYPE_RECORD = 4;
  LUATYPE_ARRAY = 5;
  LUATYPE_SET = 6;
  LUATYPE_INTERFACE = 7; // todo
  //
  LUATYPE_NAMES: array[LUATYPE_GLOBAL..LUATYPE_INTERFACE] of AnsiString
                 = ('global','difficult property','event','class','record','array','set','interface');


function IsTypeInfo_Boolean(const tpinfo: ptypeinfo): boolean;
begin
  Result := (tpinfo <> nil) and
  ((tpinfo = typeinfo(boolean)) or (tpinfo = typeinfo(bytebool)) or
   (tpinfo = typeinfo(wordbool)) or (tpinfo = typeinfo(longbool)) );
end;

(*function GetOrdinalTypeName(const tpinfo: ptypeinfo): string;
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
end; *)


// важные регистрирующие функции, объявленные позже 
procedure __TLuaRegEnum(Self: TLua; EnumTypeInfo: ptypeinfo); forward;
function __TLuaRegClass(Self: TLua; AClass: TClass; UsePublished: boolean): __PLuaClassInfo; forward;
function __TLuaRegRecord(Self: TLua; Name: __luaname; tpinfo: pointer): PLuaRecordInfo; forward;
function __TLuaRegArray(Self: TLua; Identifier, itemtypeinfo: pointer; const ABounds: array of integer): PLuaArrayInfo; forward;
function __TLuaRegSet(Self: TLua; SetTypeInfo: ptypeinfo): PLuaSetInfo; forward;
function __TLuaInternalRegInterface(Self: TLua; tpinfo: ptypeinfo): __PLuaInterfaceInfo; forward;
function __TLuaInternalRegMethod(Self: TLua; const SIGNATURE: LuaString; OfObject: boolean): __PLuaMethodInfo; overload; forward;
function __TLuaInternalRegMethod(Self: TLua; tpinfo: ptypeinfo): __PLuaMethodInfo; overload; forward;

// заполнить базовую информацию по типу, имея typeinfo или иной (внутренний) указатель
// __TLuaFieldBaseInfo нужен только для того, чтобы адекватно без всякого typeinfo брать и изменять значения
//
// кроме того __TLuaFieldBaseInfo подразумевает под собой идентификатор (имя или nil для массивов)
//
// TODO погонять разные версии Delphi/FPC на правильное толкование RTTI 
function GetLuaFieldBase(Lua: TLua; Name: __luaname; tpinfo: pointer): __TLuaFieldBaseInfo;
var
  LuaType: __PLuaType;
  TypeData: PTypeData;

  procedure ThrowUnknown();
  begin
    Lua.Assert('', []);

    UnpackString(Lua, Name);
    {$MESSAGE 'здесь'}
    // todo
  end;
begin
  Result.Name := Name;
  Result.Values := 0;

  // если тип уже зарегистрирован среди сложных типов
  LuaType := Lua.FStorage.RegisteredTypes.find_ptr(tpinfo);
  if (LuaType <> nil) then
  begin
    pointer(Result.ClassInfo) := __PLuaClassInfo(LuaType);

    case LuaType.kind of
      LUATYPE_CLASS: Result.Kind := fkObject;
     LUATYPE_RECORD: Result.Kind := fkRecord;
      LUATYPE_ARRAY: Result.Kind := fkArray;
        LUATYPE_SET: Result.Kind := fkSet;
  LUATYPE_INTERFACE: Result.Kind := fkInterface;
      // registered event ?
    else
      ThrowUnknown();
    end;
    exit;
  end;

  // фейковая константа typeinfo(pointer). Необходима для версий < D2010
  if (tpinfo = typeinfoPointer) then
  begin
    Result.Kind := fkPointer;
    exit;
  end;

  // фейковая константа typeinfo(TClass). Необходима для версий < D2010
  if (tpinfo = typeinfoTClass) then
  begin
    Result.Kind := fkClass;
    Result.ClassInfo := Lua.FStorage.RegisteredTypes.find_ptr(pointer(TObject));
    exit;
  end;

  // универсальные свойства
  if (tpinfo = typeinfoUniversal) then
  begin
    Result.Kind := fkUniversal;
    exit;
  end;

  // булеаны
  if (IsTypeInfo_Boolean(tpinfo)) then
  begin
    Result.Kind := fkBoolean;

    case (GetTypeData(tpinfo).OrdType) of
      otUByte: Result.BoolType := btBoolean;
      otSByte: Result.BoolType := btByteBool;
      otSWord, otUWord: Result.BoolType := btWordBool;
      otSLong, otULong: Result.BoolType := btLongBool;
    end;
    exit;
  end;

  // смотрим сами данные tpinfo
  TypeData := GetTypeData(ptypeinfo(tpinfo));  
  case ptypeinfo(tpinfo).Kind of
    tkInteger,
tkEnumeration: begin
                 Result.Kind := fkInteger;
                 Result.OrdType := TypeData.OrdType;
                 Result.MinMax := @TypeData.MinValue;
                 if (Result.OrdType = otSLong) then
                 begin
                   if (TypeData.MinValue=low(integer))and(TypeData.MaxValue=high(integer)) then
                   Result.MinMax := nil;
                 end else
                 if (Result.OrdType = otULong) then
                 begin
                   if (TypeData.MinValue=0)and(TypeData.MaxValue=-1) then
                   Result.MinMax := nil;
                 end;

                 if (ptypeinfo(tpinfo).Kind = tkEnumeration) then __TLuaRegEnum(Lua, tpinfo);
               end;
      {$ifdef fpc}
      tkQWord,
      {$endif}
      tkInt64: begin
                 Result.Kind := fkInt64;
                 Result.IsUnsigned64 := (TypeData.MinInt64Value > TypeData.MaxInt64Value);
                 Result.Int64MinMax := @TypeData.MinInt64Value;
                 if (not Result.IsUnsigned64) then
                 begin
                   if (TypeData.MinInt64Value=low(int64))and(TypeData.MaxInt64Value=high(int64)) then
                   Result.Int64MinMax := nil;
                 end else
                 begin
                   if (TypeData.MinInt64Value=0)and(TypeData.MaxInt64Value=-1) then
                   Result.Int64MinMax := nil;
                 end;
               end;
      tkFloat: begin
                 Result.Kind := fkFloat;
                 Result.FloatType := TypeData.FloatType;
               end;
       tkChar: begin
                 Result.Kind := fkString;
                 Result.StringType := stAnsiChar;
               end;
      tkWChar: begin
                 Result.Kind := fkString;
                 Result.StringType := stWideChar;
               end;
  {$ifdef fpc}tkSString{$else}tkString{$endif}:
               begin
                 Result.Kind := fkString;
                 Result.StringType := stShortString;
                 Result.ShortStrMaxLen := TypeData.MaxLength;
               end;
{$ifdef fpc}tkAString,{$endif}
    tkLString: begin
                 Result.Kind := fkString;
                 Result.StringType := stAnsiString;

                 {$ifdef UNICODE}
                    Result.AnsiCodePage := TypeData.CodePage;
                    // ToDo FPC !!!
                 {$endif}

                 if (Result.AnsiCodePage = 0) then Result.AnsiCodePage := DEFAULT_CODEPAGE;
               end;
    tkWString: begin
                 Result.Kind := fkString;
                 Result.StringType := stWideString;
               end;
{$ifdef UNICODE}
    tkUString: begin
                 Result.Kind := fkString;
                 Result.StringType := stUnicodeString;
               end;
{$endif}
    tkVariant: begin
                 Result.Kind := fkVariant;
                 Result.IsOleVariant := (tpinfo=typeinfo(OleVariant));
               end;
      tkClass: begin
                 Result.Kind := fkObject;
                 Result.ClassInfo := __TLuaRegClass(Lua, TypeData.ClassType, false);
               end;

     // Unregistered Difficult Types
     tkRecord{$ifdef fpc},tkObject{$endif}:
               begin
                 Result.Kind := fkRecord;
                 Result.RecordInfo := __TLuaRegRecord(Lua,Lua.FStorage.Names.Identifier(@ptypeinfo(tpinfo).Name),tpinfo);
               end;
        tkSet: begin
                 Result.Kind := fkSet;
                 Result.SetInfo := __TLuaRegSet(Lua, tpinfo);
               end;
  tkInterface: begin
                 Result.Kind := fkInterface;
                 Result.InterfaceInfo := __TLuaInternalRegInterface(Lua, tpinfo);
               end;
     tkMethod: begin
                 Result.Kind := fkMethod;
                 Result.MethodInfo := __TLuaInternalRegMethod(Lua, tpinfo);
               end;

     // доступно только в хорошем RTTI!
      tkArray: begin
                 // todo
               end;
   tkDynArray: begin
                 //Result.Kind := fkArray;

                 //__TLuaRegArray(Lua, tpinfo, nil, []);
               end;

(* ToDo *)
{   tkClassRef: ;
    tkPointer: ;
  tkProcedure: ; } // CompilerVersion ?

 //  {$ifdef FPC}
//                  tkInterfaceRaw,tkProcVar,tkUChar, tkHelper);
//   {$endif}
  end;

  // дополнительная проверка
  if (Result.Kind = fkUnknown) then
  ThrowUnknown();
end;


//procedure {TLua.RegEnum()}  __TLuaRegEnum(const Self: TLua; const EnumTypeInfo: ptypeinfo); forward;
//function {TLua.RegSet()} __TLuaRegSet(tpinfo: pointer): PLuaSetInfo; forward;

(*function GetLuaPropertyBase(const Lua: TLua; const Prefix, PropertyName: string; const tpinfo: pointer; const auto_registrate: boolean=false): TLuaPropertyInfoBase;
var
  ClassIndex: integer;

  procedure ThrowUnknown();
  begin
    Lua.Assert('Can''t register "%s%s" because its type is unknown'#13+
               'typeinfo.Name = %s, typeinfo.Kind = %s',
               [Prefix, PropertyName, ptypeinfo(tpinfo).Name, TypeKindName(ptypeinfo(tpinfo).Kind)]);
  end;
begin
  Result.Information := tpinfo;
  Result.Kind := pkUnknown;
  Result.ShortStrMaxLen := 0;
  byte(Result.OrdType) := 0;


  // зарегистрированные вещи
  ClassIndex := Lua.internal_class_index(tpinfo);
  if (ClassIndex >= 0) then
  begin
    with Lua.ClassesInfo[ClassIndex] do
    case _ClassKind of
      ckClass: begin
                 Lua.Assert('Can''t register "%s%s", because typeinfo is not correct - %s is defined', [Prefix, PropertyName, _ClassName]);
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
                       if (auto_registrate) then __TLuaRegEnum(Lua, tpinfo);  // Lua.RegEnum(tpinfo);
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
                       Result.ShortStrMaxLen := GetTypeData(tpinfo).MaxLength;
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
                         Result.Information := __TLuaRegSet(tpinfo); //Lua.ClassesInfo[Lua.InternalAddSet(tpinfo)]._Class;
                       end; 
                     end;
  end;

  if (Result.Kind = pkUnknown) then
  ThrowUnknown();
end; *)


// что-то типа InspectType
(*function GetLuaItemSize(const Base: TLuaPropertyInfoBase): integer;
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
                stShortString: Result := Base.ShortStrMaxLen+1;

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
end; *)

// имея проперти инфо, необходимо определить - является ли его тип сложным
// необходимо ли его инициализировать/финализировать
(*function GetLuaDifficultTypeInfo(const Base: TLuaPropertyInfoBase): ptypeinfo;
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
end; *)


// функция задумывалась как аналог TypInfo-функций для работы со свойствами, только более сложный вариант
// для свойств -структур/-массивов/-множеств
// сейчас функция получает свойство и делает ему push в стек Lua (user data)
(*procedure GetPushDifficultTypeProp(const Lua: TLua; const Instance: pointer; const IsConst: boolean; const Info: TLuaPropertyInfo);
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
end; *)

// изначально функция задумалась как аналог TypInfo-функций для работы со сложными свойствами:
// структуры, массивы, множества
//
// сейчас функция берёт значение из стека и присваивает "значение" 
(*function PopSetDifficultTypeProp(const Lua: TLua; const instance: pointer; const stack_index: integer; const Info: TLuaPropertyInfo): boolean;
type
  TSetterProc = procedure(const instance: pointer; const Value: integer);
  TIndexedSetterProc = procedure(const instance: pointer; const Index, Value: integer);
var
  userdata: PLuaUserData;

  PValue: pointer;
  ProcValue: integer; // Value в сеттере
  luatype: integer absolute ProcValue;

  // данные для копирования и обнуления
  DataSize: integer;
  SimpleType: boolean;
  ItemTypeInfo: ptypeinfo;
  ItemsCount: integer;
begin
  Result := false;

  // определяемся с userdata, режим Clear(userdata=nil) и Result
  userdata := nil;
  luatype := lua_type(Lua.Handle, stack_index);
  case luatype of
         LUA_TNIL: {ничего не делаем - режим Clear};
    LUA_TUSERDATA: begin
                     userdata := lua_touserdata(Lua.Handle, stack_index);

                     if (userdata = nil) or (userdata.instance = nil) or
                        (byte(userdata.kind) > byte(ukSet)) then
                     begin
                       GetUserDataType(Lua.FBufferArg.str_data, Lua, userdata);
                       exit;
                     end;
                   end;
  else
    Lua.FBufferArg.str_data := LuaTypeName(luatype);
    exit;
  end;

  
  // узнаём информацию и сверяем совместимость (если есть userdata)
  case Info.Base.Kind of
    pkRecord: begin
                with PLuaRecordInfo(Info.Base.Information)^ do
                begin
                  DataSize := FSize;
                  ItemTypeInfo := FTypeInfo;
                  SimpleType := (ItemTypeInfo = nil);
                  ItemsCount := 1;
                end;

                if (userdata <> nil) then
                with userdata^ do
                if (kind <> ukInstance) or (Lua.ClassesInfo[ClassIndex]._Class <> Info.Base.Information) then
                begin
                  GetUserDataType(Lua.FBufferArg.str_data, Lua, userdata);
                  exit;
                end;
              end;

     pkArray: begin
                with PLuaArrayInfo(Info.Base.Information)^ do
                begin
                  DataSize := FSize;
                  ItemTypeInfo := FTypeInfo;
                  SimpleType := (ItemTypeInfo = nil);
                  ItemsCount := FItemsCount;
                end;

                if (userdata <> nil) then
                with userdata^ do
                if (kind <> ukArray) or (ArrayInfo <> Info.Base.Information) then
                begin
                  GetUserDataType(Lua.FBufferArg.str_data, Lua, userdata);
                  exit;
                end;
              end;

       pkSet: begin
                DataSize := PLuaSetInfo(Info.Base.Information).FSize;
                ItemTypeInfo := nil;
                SimpleType := true;
                ItemsCount := 1;

                if (userdata <> nil) then
                with userdata^ do
                if (kind <> ukSet) or (SetInfo <> Info.Base.Information) then
                begin
                  GetUserDataType(Lua.FBufferArg.str_data, Lua, userdata);
                  exit;
                end;
              end;
  else
    Lua.FBufferArg.str_data := 'unknown property?';
    exit; // warnings off            
  end;


  { -- если userdata nil - это режим обнуления переменной -- }
  Result := true; // если досюда дошли - значит всё нормально

  if (Info.write_mode >= 0) then
  begin
    // простой режим, где instance указывает на переменную
    if (userdata = nil {ClearMode}) then
    begin
      if (not SimpleType) then Finalize(instance, ItemTypeInfo, ItemsCount);
      FillChar(instance^, DataSize, #0);
    end else
    begin
      if (SimpleType) then Move(userdata.instance^, instance^, DataSize)
      else CopyArray(instance, userdata.instance, ItemTypeInfo, ItemsCount);
    end;
  end else
  begin
    // сложный режим, где нужно присвоить по сеттеру
    PValue := Info.PropInfo.SetProc;
    if (dword(PValue) >= $FE000000) then PValue := pointer(pointer(dword(Instance^) + dword(PValue) and $00FFFFFF)^);

    // параметр сеттера
    if (userdata = nil {ClearMode}) then
    begin
      if (DataSize <= 4) then ProcValue := 0
      else
      // использовать занулированный ResultBuffer.Memory
      with Lua.ResultBuffer do
      begin
        if (tpinfo <> nil) then Finalize();
        if (Size < DataSize) then
        begin
          Size := (DataSize+3) and (not 3);
          ReallocMem(Memory, Size);
        end;
        FillChar(Memory^, DataSize, #0);
        ProcValue := integer(Memory);
      end;
    end else
    begin
      ProcValue := integer(userdata.instance);

      case (DataSize) of
        1: ProcValue := pbyte(ProcValue)^;
        2: ProcValue := pword(ProcValue)^;
        3: ProcValue := integer(pword(ProcValue)^) or (ord(pansichar(ProcValue)[2]) shl 16);
        4: ProcValue := pinteger(ProcValue)^;
      end;
    end;

    // присвоить (вызвать сеттер)
    with Info.PropInfo^ do
    if (Index = integer(PROP_NONE_USE)) then TSetterProc(PValue)(instance, ProcValue)
    else TIndexedSetterProc(PValue)(instance, Index, ProcValue);
  end;
end; *)


// специальный калбек для универсальных свойств
//
// универсальные свойства характерны тем, что возвращают TLuaArg
// и кроме того содержат PropertyName в калбеке
(*procedure GetPushUniversalTypeProp(const Lua: TLua; Instance: pointer; const IsConst: boolean; const Info: TLuaPropertyInfo);
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
end; *)

// изменить универсальное свойство (через TLuaArg)
(*function PopSetUniversalTypeProp(const Lua: TLua; const instance: pointer; const stack_index: integer; const Info: TLuaPropertyInfo): boolean;
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
end;  *)



function VMTMethodsCount(const AClass: TClass): integer;
{$ifdef fpc}
const
  vmtSelfPtr = 0; {TODO !!!}
{$endif}
asm
        PUSH    EBX
        MOV     ECX, 8
        MOV     EBX, -1
        MOV     EDX, vmtSelfPtr
@@cycle:
        ADD     EDX, 4
        CMP     [EAX + EDX], EAX
        JE      @@vmt_not_found
        JB      @@continue
        CMP     [EAX + EDX], EBX
        JAE     @@continue
        MOV     EBX, [EAX + EDX]
@@continue:
        DEC     ECX
        JNZ     @@cycle
        SUB     EBX, EAX
        SHR     EBX, 2
        MOV     EAX, EBX
        JMP     @@exit
@@vmt_not_found:
        XOR     EAX, EAX
@@exit:
        POP     EBX
end;

// упаковать указатель по законам TypInfo
(*procedure PackPointer(const ClassInfo: TLuaClassInfo; var Dest: pointer; const P: pointer);
var
  VmtIndex: integer;
begin
  if (ClassInfo._Class = GLOBAL_NAME_SPACE) then
  begin
    // в global_name_space всегда хранится полный указатель
    integer(Dest) := integer(P);
  end else
  if (ClassInfo._ClassKind <> ckClass) then
  begin
    // структура
    integer(Dest) := integer($FF000000) or integer(P);
  end else
  begin
    // класс
    if (P = nil) then exit;
    if (integer(P) < TClass(ClassInfo._Class).InstanceSize) then
    begin
      integer(Dest) := integer($FF000000) or integer(P);
    end else
    begin
      VmtIndex := IntPos(integer(P), PInteger(ClassInfo._Class), VMTMethodsCount(ClassInfo._Class));

      if (VmtIndex < 0) then Dest := P
      else integer(Dest) := integer($FE000000) or VmtIndex*4;
    end;
  end;
end;  *)



// возвращаем "режим" чтения или записи по указателю на функцию
// в случаях когда не указатель на глобальную переменную
function PropMode(const P: pointer): integer;
var
  Value: dword absolute P;
begin
  if (P = PROP_NONE_USE) then Result := MODE_NONE_USE
  else
  if (Value and $FF000000 = $FF000000) then Result := Value and $00FFFFFF // смещение
  else
  Result := MODE_PROC_USE;
end;                          

// блок указателей на нужные TypInfo-функции
(*var
  TypInfoGetStrProp: function(Instance: TObject; PropInfo: PPropInfo): string;
  TypInfoSetStrProp: procedure(Instance: TObject; PropInfo: PPropInfo; const Value: string);
  TypInfoGetVariantProp: function(Instance: TObject; PropInfo: PPropInfo): Variant;
  TypInfoSetVariantProp: procedure(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);
  TypInfoGetInterfaceProp: function(Instance: TObject; PropInfo: PPropInfo): IInterface;
  TypInfoSetInterfaceProp: procedure(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface);


procedure InitTypInfoProcs();
begin
  TypInfoGetStrProp := TypInfo.GetStrProp;
  TypInfoSetStrProp := TypInfo.SetStrProp;
  TypInfoGetVariantProp := TypInfo.GetVariantProp;
  TypInfoSetVariantProp := TypInfo.SetVariantProp;
  TypInfoGetInterfaceProp := TypInfo.GetInterfaceProp;
  TypInfoSetInterfaceProp := TypInfo.SetInterfaceProp;
end; *)



// подчистить динамические данные - PropInfo и
(*procedure TLuaPropertyInfo.Cleanup();
begin
  if (not IsRTTI) and (PropInfo <> nil) then
  begin
    FreeMem(PropInfo);
    PropInfo := nil;
  end;

  PropertyName := '';
end; *)


// заполнение информации основываясь на PropInfo, созданном RTTI
(*procedure TLuaPropertyInfo.Fill(const RTTIPropInfo: PPropInfo; const PropBase: TLuaPropertyInfoBase);
begin
  if (Self.IsRTTI) and (Self.PropInfo = RTTIPropInfo) then exit;

  // удаление предыдущего propinfo
  if (IsRTTI) then PropInfo := nil;
  if (PropInfo <> nil) then
  begin
    FreeMem(PropInfo);
    PropInfo := nil;
  end;

  // занести новые параметры
  Self.IsRTTI := true;
  Self.Base := PropBase;
  Self.Parameters := nil;
  Self.PropInfo := RTTIPropInfo;

  // режимы чтения-записи
  read_mode := PropMode(PropInfo.GetProc);
  write_mode := PropMode(PropInfo.SetProc);
end;  *)


// искуственная функция, создающая PropInfo (если нужно)
// и заполняющая все необходимые поля
(*procedure TLuaPropertyInfo.Fill(const class_info; const PropBase: TLuaPropertyInfoBase;
                                const PGet, PSet: pointer; const AParameters: PLuaRecordInfo);
var
  ClassInfo: TLuaClassInfo absolute class_info;
  Len: integer;

  _GetProc, _SetProc: pointer;
  _read_mode, _write_mode: integer;
begin
  // удаление предыдущего propinfo если параметры не совпадают
  if (IsRTTI) then PropInfo := nil;

  // инициализация полей, которые могут быть
  // проверка на полное соответствие уже существующей информации
  if (ClassInfo._Class = GLOBAL_NAME_SPACE) then
  begin
    _read_mode := integer(PGet);
    _write_mode := _read_mode;
    _GetProc := PROP_NONE_USE;
    _SetProc := PROP_NONE_USE;

    if (Self.Base.Information = PropBase.Information) and
       (Self.Parameters = AParameters) and (_read_mode = Self.read_mode) then exit;
  end else
  begin
    _GetProc := PROP_NONE_USE;
    _SetProc := PROP_NONE_USE;
    PackPointer(ClassInfo, _GetProc, PGet);
    PackPointer(ClassInfo, _SetProc, PSet);
    _read_mode := PropMode(_GetProc);
    _write_mode := PropMode(_SetProc);
    if (_read_mode >= 0) then _GetProc := PROP_NONE_USE;
    if (_write_mode >= 0) then _SetProc := PROP_NONE_USE;

    if (Self.Base.Information = PropBase.Information) and (Self.Parameters = AParameters) and
       (_read_mode = Self.read_mode) and (_write_mode = Self.write_mode) then
    begin
      // сравнить калбеки
      if ((_GetProc <> PROP_NONE_USE) or (_SetProc <> PROP_NONE_USE)) = (PropInfo = nil) then
      begin
        if (PropInfo = nil) then exit;
        if (PropInfo.GetProc = _GetProc) and (PropInfo.SetProc = _SetProc) then exit;
      end;
    end;
  end;

  // очистить старый PropInfo(если нужно)
  if (PropInfo <> nil) then
  begin
    FreeMem(PropInfo);
    PropInfo := nil;
  end;

  // заполнение базовых полей
  Self.IsRTTI := false;
  Self.Base := PropBase;
  Self.Parameters := AParameters;
  Self.read_mode := _read_mode;
  Self.write_mode := _write_mode; 

  // создание нового PPropInfo (если нужно)
  if (_GetProc <> PROP_NONE_USE) or (_SetProc <> PROP_NONE_USE) then
  begin
    Len := sizeof(TPropInfo) - sizeof({TPropInfo.Name}ShortString) + 1 + Length(PropertyName) + 4;
    GetMem(PropInfo, Len);
    ZeroMemory(PropInfo, Len);

    PropInfo.Name := PropertyName;
    PropInfo.PropType := pointer(integer(PropInfo)+Len-4);
    PropInfo.PropType{$ifndef fpc}^{$endif} := PropBase.Information;
    PropInfo.GetProc := _GetProc;
    PropInfo.SetProc := _SetProc;
    PropInfo.StoredProc := PROP_NONE_USE;
    PropInfo.Index := integer(PROP_NONE_USE);
  end;
end; *)


(*function TLuaPropertyInfo.Description(): string;
const
  BOOL_STRS: array[__TLuaFieldBoolType] of string = ('boolean', 'ByteBool', 'WordBool', 'LongBool');
  FLOAT_STRS: array[TFloatType] of string = ('single', 'double', 'extended', 'Comp', 'currency');
  STRING_STRS: array[__TLuaFieldStringType] of string = ('string[%d]', 'AnsiString', 'WideString', {todo UnicodeString?,} 'AnsiChar', 'WideChar');
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
    with PLuaRecordInfo(Parameters).FType.Lua.ClassesInfo[PLuaRecordInfo(Parameters).FClassIndex] do
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
                 if (Base.StringType = stShortString) then typename := Format(typename, [Base.ShortStrMaxLen]);
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
end; *)


{ TLuaClassInfo }

// возвращает индекс. отрицательный (для свойств) или положительный (для методов)
(*function TLuaClassInfo.InternalAddName(const Name: string; const AsProc: boolean; {var Initialized: boolean;} const CodeAddr: pointer): integer;
const
  PROC_STR: array[boolean] of string = ('Property', 'Procedure');
var
  AHash: integer;

  function GetNamePos(): integer;
  label
    Done;
  var
    Len, Index: integer;
  begin
    Index := 0;
    // Result := InsortedPos8(AHash, Names);
    // if (Result < 0) then exit;
    Len := Length(Names);
    Result := InsortedPlace8(AHash, pointer(Names), Len);
    
    while (Result < Len) and (AHash = Names[Result].Hash) do
    begin
      Index := Names[Result].Index;

      if (Index >= 0) then
      begin
        if (SameStrings(Name, Procs[Index].ProcName)) then goto Done;
      end else
      begin
        if (SameStrings(Name, Properties[{InvertIndex} not (Index)].PropertyName)) then goto Done;
      end;

      inc(Result);
    end;

    Result := -1;
    exit;
  Done:
    // нарушение логики. пытаются зарегистрировать процедуру с таким же именем, как свойство
    // Или наоборот
    if (AsProc <> (Index >= 0)) then
    ELua.Assert('%s with name "%s" is already contains in %s. That is why you can''t add a %s with a same name',
               [PROC_STR[not AsProc], Name, _ClassName, PROC_STR[AsProc]], CodeAddr);
  end;

begin
  // найти в списке имён
  AHash := StringHash(Name);
  Result := GetNamePos();
  if (Result >= 0) then
  begin
    Result := Names[Result].Index; // индекс инвертировать не надо, он уже корректный
    exit;
  end;
//  Initialized := false;


  // добавление имени
  if (AsProc) then
  begin
    Result := Length(Procs);
    SetLength(Procs, Result+1);
    ZeroMemory(@Procs[Result], sizeof(TLuaProcInfo));
    Procs[Result].ProcName := Name;
    Procs[Result].ArgsCount := -1;
  end else
  begin
    Result := Length(Properties);
    SetLength(Properties, Result+1);
    ZeroMemory(@Properties[Result], sizeof(TLuaPropertyInfo));
    Properties[Result].PropertyName := Name;
    Result := {InvertIndex} not (Result);
  end;

  // добавить в список Names
  with TLuaHashIndex(DynArrayInsert(Names, typeinfo(TLuaHashIndexDynArray), InsortedPlace8(AHash, pointer(Names), Length(Names)))^) do
  begin
    Hash := AHash;
    Index := Result;
  end;
end;  *)

// быстрый поиск свойства (в простом "классе")
// сделано это в основном для простых структур
(*procedure FastFindProperty(const Properties: TLuaPropertyInfoDynArray; const Name: pchar; const NameLength: integer; var PropertyInfo: pointer);
const
  PROPERTY_SIZE = sizeof(TLuaPropertyInfo);
asm
  test eax, eax
  jz @ret
  push edi // счётчик Length(Properties)
  push esi // сравниваемая строка
  push ebx // буфер для сравнения
  push ebp // дополнительный буффер для хранения NameLength

  mov edi, [eax-4]
  {$ifdef fpc} inc edi {$endif}
  mov ebp, ecx
@property_loop:
  mov esi, [eax + TLuaPropertyInfo.PropertyName]
  cmp ecx, [esi-4]
  jne @property_next

  { сравнение строки esi и edx. длинна = ecx }
     jmp @1
     @loop_byte:
       dec ecx
       mov bl, [esi+ecx]
       cmp bl, [edx+ecx]
       jne @exit_compare
     @1:test ecx, 3
     jnz @loop_byte

     shr ecx, 2
     jz  @exit // если строка найдена

     @loop_dword:
       mov ebx, [esi + ecx*4 - 4]
       cmp ebx, [edx + ecx*4 - 4]
       jne @exit_compare
     dec ecx
     jnz @loop_dword
     jmp @exit // если строка найдена

     // строки не равны, "выход"
     @exit_compare: mov ecx, ebp
  { <-- сравнение строки esi и edx }
@property_next:
  add eax, PROPERTY_SIZE
  dec edi
  jnz @property_loop
  xor eax, eax // если не найден
@exit:
  pop ebp
  pop ebx
  pop esi
  pop edi
@ret:
  mov edx, PropertyInfo
  mov [edx], eax
end; *)

// найти индекс в списке глобальных имён
(*function TLuaClassInfo.NameSpacePlace(const Lua: TLua; const Name: pchar; const NameLength: integer; var ProcInfo, PropertyInfo: pointer): integer;
var
  Len, Value: integer;
  NameHash: integer;
  ClassInfo: ^TLuaClassInfo;
  HashInfo: ^TLuaHashIndex;
begin
  if (_ClassSimple) then
  begin                           
    ProcInfo := nil;
    FastFindProperty(Properties, Name, NameLength, PropertyInfo);
    Result := 0; { результат нужен только в AddToNameSpace(), но на тот момент флаг _ClassSimple выключен }
    exit;
  end;

  NameHash := StringHash(Name, NameLength);
  Len := Length(NameSpace);
  Result := InsortedPlace8(NameHash, pointer(NameSpace), Len);

  HashInfo := pointer(integer(NameSpace) + Result*sizeof(TLuaHashIndex));
  while (Result < Len) and (HashInfo.Hash = NameHash) do
  begin
    Value := HashInfo.Index;
    ClassInfo := @Lua.ClassesInfo[word(Value)];

    if (Value >= 0) then
    begin
      ProcInfo := @ClassInfo.Procs[Value shr 16];

      if (SameStrings(PLuaProcInfo(ProcInfo).ProcName, Name, NameLength)) then
      begin
        PropertyInfo := nil;
        exit;
      end;
    end else
    begin
      PropertyInfo := @ClassInfo.Properties[not smallint(Value shr 16)];

      if (SameStrings(PLuaPropertyInfo(PropertyInfo).PropertyName, Name, NameLength)) then
      begin
        ProcInfo := nil;
        exit;
      end;
    end;               

    inc(Result);
    inc(HashInfo);
  end;

  ProcInfo := nil;
  PropertyInfo := nil;
end;  *)

(*function  TLuaClassInfo.PropertyIdentifier(const Name: string = ''): string;
var
  S: string;
begin
  if (_Class = GLOBAL_NAME_SPACE) then Result := 'global variable'
  else
  if (_ClassKind <> ckClass) then Result := 'field'
  else
  Result := 'property';

  // полный вариант
  if (Name <> '') then
  begin
    if (_Class = GLOBAL_NAME_SPACE) then S := Format(' "%s"', [Name])
    else S := Format(' %s.%s', [_ClassName, Name]);

    Result := Result + S;
  end;
end; *)

(*procedure TLuaClassInfo.Cleanup();
var
  i: integer;
begin
  // очистить информацию по типам
  if (TClass(_Class) <> GLOBAL_NAME_SPACE) then
  case _ClassKind of
    ckRecord: Dispose(PLuaRecordInfo(_Class));
       ckSet: Dispose(PLuaSetInfo(_Class));
     ckArray: begin
                TLuaPropertyInfo(PLuaArrayInfo(_Class).ItemInfo).Cleanup();
                Dispose(PLuaArrayInfo(_Class));
              end;
  end;

  // подчистить исскуственные PPropInfo в свойствах
  for i := 0 to Length(Properties)-1 do
  Properties[i].Cleanup();

  Names := nil;
  NameSpace := nil;
  Procs := nil;
  Properties := nil;
  _ClassName := '';
end; *)





{ TLuaResultBuffer }

procedure TLuaResultBuffer.Finalize(const free_mem: boolean=false);
begin
  if (tpinfo <> nil) then
  begin
    {$ifdef NO_CRYSTAL}
      CrystalLUA.Finalize(Memory, tpinfo, items_count);
    {$else}
      SysUtilsEx.Finalize(Memory, tpinfo, items_count);
    {$endif}

    tpinfo := nil;
  end;

  if (free_mem) and (Memory <> nil) then
  begin
    FreeMem(Memory);
    Memory := nil;
    Size := 0;    
  end;
end;

function  TLuaResultBuffer.AllocRecord(const RecordInfo: PLuaRecordInfo): pointer;
var
  NewSize: integer;
begin
  if (tpinfo <> nil) then Finalize();

  if (RecordInfo = nil) then
  begin
    AllocRecord := nil;
    exit;
  end;

  NewSize := (RecordInfo.FSize+3) and (not 3);
  if (NewSize > Size) then
  begin
    Size := NewSize;
    ReallocMem(Memory, Size);
  end;

  tpinfo := RecordInfo.FTypeInfo;
  items_count := 1;
  FillDword(Memory^, NewSize shr 2, 0); // ZeroMemory

  AllocRecord := Memory;
end;

function TLuaResultBuffer.AllocArray(const ArrayInfo: PLuaArrayInfo): pointer;
var
  NewSize: integer;
begin
  if (tpinfo <> nil) then Finalize();

  if (ArrayInfo = nil) then
  begin
    AllocArray := nil;
    exit;
  end;

  NewSize := (ArrayInfo.FSize+3) and (not 3);
  if (NewSize > Size) then
  begin
    Size := NewSize;
    ReallocMem(Memory, Size);
  end;

  tpinfo := ArrayInfo.FTypeInfo;
  items_count := ArrayInfo.FItemsCount;

  if (NewSize = 4) then PInteger(Memory)^ := 0
  else FillDword(Memory^, NewSize shr 2, 0); // ZeroMemory

  AllocArray := Memory;
end;

function TLuaResultBuffer.AllocSet(const SetInfo: PLuaSetInfo): pointer;
var
  NewSize: integer;
begin
  if (tpinfo <> nil) then Finalize();

  if (SetInfo = nil) then
  begin
    AllocSet := nil;
    exit;
  end;

  NewSize := (SetInfo.FSize+3) and (not 3);
  if (NewSize > Size) then
  begin
    Size := NewSize;
    ReallocMem(Memory, Size);
  end;

  if (NewSize = 4) then PInteger(Memory)^ := 0
  else FillDword(Memory^, NewSize shr 2, 0); // ZeroMemory

  AllocSet := Memory;
end;


{ TLua }


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


// инициализация Хранилища
procedure __TLuaMemoryStorageInitialize(const Lua: TLua);
begin
  with Lua.FStorage do
  begin
    // дампы кода (перевызов с параметрами)
    Dumps.Initialize(20, __codedump_pool_allocator, __codedump_pool_cleaner);

    // пул для элементов хеш-массивов
    HashItems.Initialize(sizeof(__TLuaHashItem));

    // очередь свободных "ссылок" Ref
    RefQueue.Pool.Initialize(sizeof(__TLuaEmptyRef));
    RefQueue.Items := nil; // хотя он и так должен быть пустым

    // буфер накопления
    DataBuffer.Initialize();

    // используемые типы
    Types.Initialize(sizeof(__TLuaUniversalType));

    // используемые идентификаторы
    Identifiers.Initialize(sizeof(__TLuaIdentifier));

    // зарегистрированные типы
    RegisteredTypes.Initialize(Lua, @Types);

    // Lua-имена
    Names.Initialize(Lua);

    // выставляем флаг инициализации
    Initialized := true;
  end;
end;

// финализация Хранилища
procedure __TLuaMemoryStorageFinalize(const Lua: TLua);
begin
  with Lua.FStorage do
  begin
    // если так и не был инициализирован
    if (not Initialized) then exit;

    // финализация
    Dumps.Finalize();
    HashItems.Finalize();
    RefQueue.Pool.Finalize();
    DataBuffer.Finalize();
    Types.Finalize();
    Identifiers.Finalize();
    RegisteredTypes.Finalize();


    Names.Finalize();
    Initialized := false;
  end;                   
end;


{$ifdef MSWINDOWS}
procedure __TLuaScriptExceptionRecaller(const Self: TLua); forward;
{$endif}

function __TLuaInternalAddType(Self: TLua; Kind: integer; Identifier: __luaname): __PLuaType; overload; forward;

// конструктор
constructor TLua.Create();
begin
  // инициализация библиотеки
  if (not InitializeLUA) then
  ELua.Assert('Lua library was not initialized:'#13'"%s"', [LuaPath]);
  FHandle := lua_open();
  luaL_openlibs(Handle);

  // инициализация Хранилища
  __TLuaMemoryStorageInitialize(Self);

  // TestModule
  FTestModule.FLua := Self;
  FTestModule.FIndex := -1;

  // Exception correction routine
  {$ifdef MSWINDOWS}
    FExceptionRecaller := CreateCFunctionDump(Self, nil, nil, @__TLuaScriptExceptionRecaller);
  {$else}
    FExceptionRecaller := nil;
  {$endif}

  // метатаблицы, глобальное пространство, и т.д.
  FGlobalType := __TLuaInternalAddType(Self, 0{LUATYPE_GLOBAL}, nil);
  FDifficultProperties := __TLuaInternalAddType(Self, 1{LUATYPE_DIFFICULT_PROPERTY}, nil);
  // FEventType ???

  // TObject
  RegClass(TObject, false);

  // TMethod: структура для хранения событий
 (* with RegRecord('TMethod', pointer(sizeof(TMethod)))^, TMethod(nil^) do
  begin
    RegField('Code', @Code, typeinfoPointer);
    RegField('Data', @Data, typeinfoPointer);
    RegProc(LUA_CONSTRUCTOR, TClassRecallProc(TMethodConstructor));
    Operators := [loCompare];
    OperatorCallback := TMethodOperator;
  end; *)

  // TLuaReference
  RegClass(TLuaReference, false); 
end;

// деструктор
destructor TLua.Destroy();
var
  i: integer;
begin
  // todo в плане финализации ещё подумать что когда в какой момент
  // может произойти.

  // внутренние данные
//  FArgs := nil;
  if (FHandle <> nil) then lua_close(FHandle);
  FResultBuffer.Finalize(true);

  // массив ссылок
  for i := 0 to Length(FReferences)-1 do FReferences[i].FreeInstance();
  FReferences := nil;

  // подчистить данные
//  GlobalNative.Cleanup();
//  for i := 0 to Length(ClassesInfo)-1 do ClassesInfo[i].Cleanup();

  // чанки
  FModules := nil;
  FModulesCount := 0;

  // чистка памяти в Хранилище
  __TLuaMemoryStorageFinalize(Self);

  inherited;
end;

// строковые функции, распаковывающие внутренние "строки" к LuaString
// чаще всего это нужно для идентификации ошибки
function UnpackString(const RTTI: ShortString{ansi или utf8}): LuaString; overload;
var
  Length: integer;

  {$ifdef UNICODE}
    Len: integer;
  {$else}
  {$ifdef LUA_UNICODE}
    i: integer;
    Src: pansichar;
    Dest: pwidechar;
  {$endif}
  {$endif}
begin
  if (pointer(Result) <> nil) then Result := '';
  Length := pbyte(@RTTI)^;
  if (Length = 0) then exit;

  {$ifdef UNICODE}
     // S в кодировке utf8
     Len := Utf8Length(@RTTI[1], Length);
     SetLength(Result, Len);
     {$ifdef LUA_UNICODE}
        Utf16FromUtf8(pointer(Result), Len, @RTTI[1], Length);
     {$else .LUA_ANSI}
        AnsiFromUtf8(pointer(Result), Len, @RTTI[1], Length);
     {$endif}   
  {$else .ANSI}
     // S в кодировке Ansi
     SetLength(Result, Length);
     {$ifdef LUA_UNICODE}
        Src := @RTTI[1];
        Dest := pointer(Result);
        for i := 0 to Length-1 do Dest[i] := Src[i];
     {$else .LUA_ANSI}
        Move(RTTI[1], pointer(Result)^, Length);
     {$endif}
  {$endif}
end;

procedure __UnpackString(const S: __luaname; var Result: LuaString; const Length: integer);
{$ifdef LUA_UNICODE}
var
  Len: integer;
{$endif}
begin
  if (pointer(Result) <> nil) then Result := '';
  if (Length <= 0) then exit;

  {$ifdef LUA_ANSI}
     SetLength(Result, Length);
     Move(S^, pointer(Result)^, Length);
  {$else .LUA_UNICODE}
     Len := Utf8Length(S, Length);
     SetLength(Result, Len);
     Utf16FromUtf8(pointer(Result), Len, S, Length);
  {$endif}
end;

function UnpackString(const S: __luabuffer): LuaString; overload;
asm
  test eax, eax
  jz @call_zero

  mov ecx, [eax-4]
  jmp __UnpackString

@call_zero:
  xor ecx, ecx
  jmp __UnpackString
end;

// todo не знаю )
function UnpackString(const Lua: TLua; const S: __luaname): LuaString; overload;
asm
  test eax, eax
  jz @call_zero
  cmp byte ptr [eax], 0
  je @call_zero
  push edx
  push eax

  mov edx, eax
  neg edx
  add eax, 1
  and eax, -2
@scan_loop:
  mov cx, [eax]
  add eax, 2
  test cl, ch
  jnz @scan_loop
  test cl, cl
  jz @return_less2
  test ch, ch
  jnz @scan_loop
  lea ecx, [eax + edx - 1]
  jmp @call
@return_less2:
  lea ecx, [eax + edx - 2]
@call:
  pop eax
  pop edx
  jmp __UnpackString
@call_zero:
  xor ecx, ecx
  jmp __UnpackString
end;


// некая универсальная функция, дополняющая функционал Delphi >= 2009
function AnsiStringFormat(const FmtStr: AnsiString; const Args: array of const): AnsiString;
{$ifndef UNICODE}
begin
  SysUtils.FmtStr(Result, FmtStr, Args);
end;
{$else}
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of Char;
begin
  BufLen := SizeOf(Buffer);
  if Length(FmtStr) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := FormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(FmtStr)^, Length(FmtStr), Args)
  else
  begin
    BufLen := Length(FmtStr);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := FormatBuf(Pointer(Result)^, BufLen - 1, Pointer(FmtStr)^,
      Length(FmtStr), Args);
    end;
    SetLength(Result, Len);
  end else
  begin
    SetLength(Result, Len);
    Move(Buffer, pointer(Result)^, Len);
  end;
end;
{$endif}

// универсальная Format-функция, работающая со строкой LuaString
function LuaStringFormat(const FmtStr: LuaString; const Args: array of const): LuaString;
{$ifdef LUA_ANSI}
asm
  pop ebp //  todo проверить
  jmp AnsiStringFormat
end;
{$else .LUA_UNICODE}
begin
  {$ifdef UNICODE}
     SysUtils.FmtStr(Result, FmtStr, Args);
  {$else}
     SysUtils.WideFmtStr(Result, Format, Args);
  {$endif}
end;
{$endif}


// функция, преобразующая стандартный Format(FmtStr, Args) к строке внутреннего типа
function LuaBufferFormat(const FmtStr: LuaString; const Args: array of const): __luabuffer;
{$ifdef LUA_ANSI}
asm
  // и FmtStr, и результат - AnsiString
  pop ebp //  todo проверить
  jmp AnsiStringFormat
end;
{$else .LUA_UNICODE}
// нужно сделать format utf16, и результат преобразовать к utf8
var
  S: LuaString;
  Len, MaxLen: integer;
begin
  S := LuaStringFormat(FmtStr, Args);
  Len := Length(S);
  MaxLen := Len*3;

  // utf8 <-- utf16
  SetLength(Result, MaxLen);
  Len := Utf8FromUtf16(pointer(Result), MaxLen, pointer(S), Len);
  SetLength(Result, Len);
end;
{$endif}

// функция анализирует и изменяет(!) входной буфер
// в итоге получает Module, UpdateNumber, если получается Line
// в качестве результата возвращает количество "прочтённых" символов или -1 в случае провала
function __TLuaInspectScriptAddress(const Self: TLua;
                                    var Address: __TLuaScriptErrorAddress; var LineRetrieved: boolean;
                                    buffer: __luaname): integer;
label
  done;
var
  started: __luaname;
  index, Len: integer;

  function buffer_len_to_integer(): integer;
  var
    s: __luaname;
    E: integer;
  begin
    s := buffer;
    dec(s);
    pbyte(s)^ := Len;

    Val(PShortString(s)^, Result, E);
    if (E <> 0) then Result := -2;
  end;
begin
  started := buffer;
  LineRetrieved := false;
  Result := -1;
  // [string "__
  if (buffer = nil) or (pdword(@buffer[0])^<>$7274735B) or (pdword(@buffer[4])^<>$20676E69)
  or (pword(@buffer[8])^<>$5F22) or (buffer[10]<>'_') then exit;

  // ищем индекс
  begin
    inc(buffer, 11);
    Len := 0;
    while (true) do
    begin
      case buffer[Len] of
        '-','0'..'9': ;
        '_': break;
      else
        exit;
      end;
      inc(Len);
    end;

    index := buffer_len_to_integer();
    if (index < -1) or (index >= Self.FModulesCount) then exit;
    if (index = -1) then Address.Module := @Self.FTestModule else Address.Module := @Self.FModules[index];
    inc(buffer, Len+1);
  end;

  // ищем номер апдейта
  begin
    Len := 0;
    while (true) do
    begin
      case buffer[Len] of
        '0'..'9': ;
        '_': break;
      else
        exit;
      end;
      inc(Len);
    end;

    Address.UpdateNumber := buffer_len_to_integer();
    if (Address.UpdateNumber <= 0) then exit;
    if (pdword(@buffer[Len])^<>$5D225F5F{__"]}) then exit;
    inc(buffer, Len+4);
  end;

  // если есть возможность - узнаём номер линии
  if (buffer^ = ':') then
  begin
    inc(buffer);
    Len := 0;
    while (true) do
    begin
      case buffer[Len] of
        '0'..'9': ;
        ':': break;
      else
        dec(buffer); // возвращаем старое значение
        goto done;
      end;
      inc(Len);
    end;

    Address.Line := buffer_len_to_integer();
    if (Address.Line > 0) then
    begin
      dec(Address.Line); // там завышают на 1, надо сбавить
      LineRetrieved := true;
      inc(buffer, Len+1);
    end;
  end;

  // всё что можно прочитали - возвращаем количество прочтённых символов
done:
  Result := integer(buffer)-integer(started);
end;                                    

// получить информацию по текущему месту в скрипте
// возвращает true если получена корректная информация
function __TLuaInspectScriptDebugAddress(const Self: TLua; var Address: __TLuaScriptErrorAddress): boolean;
var
  DebugInfo: lua_Debug;
  buf: boolean;
begin
  Result := false;
  ZeroMemory(@DebugInfo, sizeof(DebugInfo));
  lua_getstack(Self.Handle, 1, DebugInfo);
  lua_getinfo(Self.Handle, 'Sln', DebugInfo);
  if (DebugInfo.currentline < 0) then exit;

  // получаем Module, UpdateNumber
  if (__TLuaInspectScriptAddress(Self, Address, buf, __luaname(@DebugInfo.short_src[4])) < 0) then exit;

  // линия
  Result := true;
  Address.Line := DebugInfo.currentline-1;
end;

// call Exception: internal(script) or external
procedure __TLuaAssert(const Self: TLua; const FmtStr: LuaString; const Args: array of const; CodeAddr: pointer);
var
  Error: __luabuffer;
  Address: __TLuaScriptErrorAddress;
begin
  if (Self.FScriptMode <> smNone) then
  begin
    // ошибка в скрипте. получаем информацию о чанке
    // чтобы сформировать текст ошибки в стандартном виде и вызвать lua_error()
    if (__TLuaInspectScriptDebugAddress(Self, Address)) then
    begin
      // Error := Format('%s:%d: ', [pchar(@DebugInfo.short_src[4]), DebugInfo.currentline]) + Format(FmtStr, Args);
      Error := AnsiStringFormat('[string "__%d_%d__"]:%d: ', [Address.Module.Index, Address.UpdateNumber, Address.Line+1]) +
               LuaBufferFormat(FmtStr, Args);

      // вызов ошибки
      lua_pushlstring(Self.Handle, pointer(Error), Length(Error));
      lua_error(Self.Handle);
      // exit;
    end;
  end;

  // ошибка скорее всего в нативном месте
  // внимание! похоже FCodeAddr нужно будет обнулять во внутренних вызовах!
  if (Self.FRecall.CodeAddr <> nil) then
  begin
    CodeAddr := Self.FRecall.CodeAddr;
    Self.FRecall.CodeAddr := nil;
  end;
  ELua.Assert(FmtStr, Args, CodeAddr);
end;

procedure TLua.Assert(const FmtStr: LuaString; const Args: array of const);
asm
  pop ebp
  push [esp]
  jmp __TLuaAssert
end;

procedure TLua.Assert(const Text: LuaString);
asm
  xor ecx, ecx
  push dword ptr -1
  // todo проверить в fpc
  jmp TLua.Assert // (const FmtStr: LuaString; const Args: array of const)
end;

function  __TLuaGetModule(const Self: TLua; const Index: integer; const CodeAddr: pointer): PLuaModule;
begin
  if (dword(Index) >= dword(Self.FModulesCount)) then
  {$ifdef NO_CRYSTAL}TExcept{$else}EWrongParameter{$endif}.Assert('Can''t get module[%d]. Modules count = %d', [Index, Self.FModulesCount], CodeAddr);

  __TLuaGetModule := @Self.FModules[Index];
end;

function  TLua.GetModule(const Index: integer): PLuaModule;
asm
  mov ecx, [esp]
  jmp __TLuaGetModule
end;

function  TLua.GetModuleIndex(const Name: LuaString): integer;
var
  Len: integer;
begin
  if (pointer(Name) <> nil) then
  begin
    Len := pinteger(integer(pointer(Name))-4)^;

    for Result := 0 to FModulesCount-1 do
    if (pinteger(integer(pointer(FModules[Result].FName))-4)^ = Len) then
    begin
      if SysUtils.CompareMem(pointer(Name), pointer(FModules[Result].FName),
                             Len {$if Defined(UNICODE) and Defined(LUA_UNICODE)}*siziof(WideChar){$ifend})
         then exit;
    end;
  end;

  Result := -1;
end;

function  TLua.GetModuleByName(const Name: LuaString): PLuaModule;
var
  Index: integer;

  // для Windows может быть другая логика
  // связанная с поиском файла в LowerCase
  {$ifdef MSWINDOWS}
  function GetMSWindowsFileIndex(): integer;
  var
    Len: integer;
    buffer: LuaString;
  begin
    // buffer := LowerCase(Name)
    Len := Length(Name);
    SetLength(buffer, Len);
    {$ifdef LUA_UNICODE}
      Move(pointer(Name)^, pointer(buffer)^, Len*sizeof(WideChar));
      Windows.CharLowerBuffW(pointer(buffer), Len);
    {$else .LUA_ANSI}
      Move(pointer(Name)^, pointer(buffer)^, Len*sizeof(AnsiChar));
      Windows.CharLowerBuffA(pointer(buffer), Len);
    {$endif}

    // поиск файлового
    Result := GetModuleIndex(buffer);
    if (Result >= 0) and (FModules[Result].FFileName = '') then Result := -1;
  end;
  {$endif}
begin
  if (pointer(Name) = nil) then
  begin
    Result := nil;
    exit;
  end;

  Index := GetModuleIndex(Name);

  {$ifdef MSWINDOWS}
  if (Index < 0) then Index := GetMSWindowsFileIndex();
  {$endif}

  if (Index < 0) then Result := nil
  else Result := @FModules[Index];
end;


// ошибка возникает тогда, когда вызывается какой-то внешний TLua метод
// в момент, когда происходит обработка скрипта
procedure __TLuaThrowInScriptCall(const Self: TLua; const CodeAddr: pointer);
begin
  Self.FRecall.CodeAddr := nil; // на всякий случай ?
  ELua.Assert('The native method can''t be called at script running time', CodeAddr);
end;

// очень важная функция.
// все внешние функции проходят через этот метод для того чтобы:
// 1) проверить флаг FScriptMode. И если он выставлен - вызвать Exception
// 2) автоматизированно заполнить поле FCodeAddr (чтобы не париться)
// 3) автоматическое очищение поля FCodeAddr после вызова функции
//
// на вершине стека должен быть адрес internal функции, которая в конечном счёте
// вызывается (со внешей строны). После неё конечно должен быть адрес возврата (он и заносится в FCodeAddr)
// ну и в eax должен быть Self
//
// единственно правильный вызов:
// [pop ebp]
// push offset <needed internal function>
// jmp TLua.InternalRecall
procedure TLua.InternalRecall;
asm
  cmp [EAX].TLua.FScriptMode, 0
  jz @1

  pop ecx // fail calling address
  pop edx // Exception address
  jmp __TLuaThrowInScriptCall
@1:
  // сохраняю регистр на будущее
  mov [EAX].TLua.FRecall.Reg, esi

  // сохраняем/подменяем адрес возврата,
  add esp, 4
  mov esi, [esp]
  mov [EAX].TLua.FRecall.CodeAddr, esi
  mov [EAX].TLua.FRecall.Back, esi
  mov [esp], offset @after_call

  // пишем Self в ESI, прыгаем в <needed internal function>
  mov esi, eax
  jmp [esp-4]

  // после вызова - пишем Self в ecx, восстанавливаем старое значение регистра esi
@after_call:  
  mov ecx, ESI
  mov esi, [ECX].TLua.FRecall.Reg

  // зануляем поле CodeAddr, прыгаем по этому возврата (Back)
  mov [ECX].TLua.FRecall.CodeAddr, 0
  jmp [ECX].TLua.FRecall.Back
end;

// ошибка возникает тогда, когда пользователь пытается зарегистрировать что-то
// но делать этого нельзя, потому что глобальное пространство уже прогружено
procedure __TLuaThrowAfterScriptsLoaded(const Self: TLua; const CodeAddr: pointer);
begin
  Self.FRecall.CodeAddr := nil; // на всякий случай
  ELua.Assert('The register method can''t be called after scripts loading time', CodeAddr);
end;


// эта функция нужна для автоматического взвешивани многих флагов
// и заполнения адреса возврата (потенциальной ошибки)
//
// отличие от "старшего брата" InternalRecall вот в чём
// InternalRegisterRecall "запрещает" вызов регистрирующих методов,
// если глобальное пространство уже прогружено !
procedure TLua.InternalRegisterRecall;
asm
  cmp [EAX].TLua.FNameSpaceInitalized, 0
  jz TLua.InternalRecall

  // ошибка detected
  // то есть вызов регистрирующей функции произошёл уже после инициализации глобального пространства.
  //
  // однако вместе с тем этот код может быть вызван в режиме работы скрипта
  // в этом случае вызываем ошибку "The native method can't be called at script running time" (перенаправляем в InternalRecall)
  cmp [EAX].TLua.FScriptMode, 0
  jnz TLua.InternalRecall

  // raise Exception
  pop ecx // fail calling address
  pop edx // Exception address
  jmp __TLuaThrowAfterScriptsLoaded
end;

// собственно название функции говорит само за себя
//
// возникла какая-то ошибка (в основном скриптовая)
// задача функции - показать её в IDE.
// если не получается - возвращаем false
function  TLua.ScriptErrorShowInIDE(const Error: __TLuaScriptError): boolean;
begin
  Result := false;

  // если нето возможности показать актуальный (файловый) модуль в IDE
  // то просто выходим
  with Error.Address do
  if (Module = nil) or (Module.FUpdate <> UpdateNumber) or (Module.FileName = '') then exit;

  // todo
end;


// произошла скриптовая ошибка. нужно её показать пользователю.
// соответственно либо показать в IDE,
// либо ELuaScript
procedure TLua.ScriptThrow(const Error: __TLuaScriptError);

  procedure ShowException();
  var
    CodeAddr: pointer;
    Err: string;
  begin
    CodeAddr := nil;
    if (Self.FRecall.CodeAddr <> nil) then
    begin
      CodeAddr := Self.FRecall.CodeAddr;
      Self.FRecall.CodeAddr := nil;
    end;

    if (Error.Address.Module = nil) then
    begin
      Err := Error.Text;
    end else
    begin
      // кодовое описание
      if (Error.Code[1] = 'c') then PChar(pointer(Error.Code))^ := 'C';

      // полное описание
      with Error.Address do
      Err := Format('module "%s", line %d.'#13'%s'#13#13'%s', [Module.Name, Line, Error.Text, Error.Code]);
    end;

    ELuaScript.Assert(Err, CodeAddr);    
  end;

begin
  // если есть возможность - просто показывам ошибку в CrystalLUA IDE
  if Self.ScriptErrorShowInIDE(Error) then exit;

  // иначе модифицируем текст ошибки
  // вываливаем ELuaScript
  // но по идее с этим Exception-ом можно жить
  ShowException();
end;


procedure TLua.InspectScriptError(var Error: __TLuaScriptError);
var
  buffer: __luabuffer;
  s: __luaname;
  count, len: integer;
  buf: boolean;    
begin
  s := lua_tolstring(Handle, -1, @len);
  SetLength(Buffer, len);
  Move(s^, pointer(buffer)^, len);
  Self.stack_clear();

  s := __luaname(buffer);
  count := __TLuaInspectScriptAddress(Self, Error.Address, buf, s);
  if (count < 0) or (not buf) then
  begin
    // не удалось восстановить модуль/апдейт/линию
    Error.Address.Module := nil;
    Error.Address.UpdateNumber := 0;
    Error.Address.Line := -1;
    Error.Code := Self.FTestModule.GetCodeString(-1, -1); // can't retrieve code
  end else
  begin
    // берём указатель на саму ошибку
    // там должен стоять пробел после ]:line:
    inc(s, count);
    dec(len, count);
    if (len > 0) and (s^ = #32) then
    begin
      inc(s);
      dec(len);
    end;

    // получаем слепок кода из модуля
    with Error.Address do
    Error.Code := Module.GetCodeString(Line, UpdateNumber);
  end;

  // текст ошибки заполняется в любом случае
  if (pointer(Error.Text) <> nil) then Error.Text := '';
  {$ifdef LUA_UNICODE}
     count := Utf8Length(s, len);
     SetLength(Error.Text, count);
     Utf16FromUtf8(pointer(Error.Text), count, s, len);
  {$else .LUA_ANSI}
     SetLength(Error.Text, len);
     Move(s^, pointer(Error.Text)^, len);
  {$endif}
end;


// процедура создана специально для реакции на Exception,
// произошедший при запуске скрипта (ScriptCall)!
//
// по сути мы должны сделать две вещи:
// 1) показать пользователю в CrystalLUA IDE место и текст ошибки (если это возможно)
// 2) дописать в текст эксепшна код из модуля!
procedure TLua.ScriptExceptionHandler(const E: Exception);
var
  Error: __TLuaScriptError;
begin
  // получаем общую информацию о скрипте
  // если Exception никакого отношения к скрипту не имеет - то просто выходим
  if (not __TLuaInspectScriptDebugAddress(Self, Error.Address)) then exit;
                              
  // дополняем информацию по ошибке текстом Exception-а
  // и показываем ошибку в CrystalLUA IDE если получается
  Error.Text := E.ClassName + ': ' + E.Message;
  Error.Code := Error.Address.Module.GetCodeString(Error.Address.Line, Error.Address.UpdateNumber);
  ScriptErrorShowInIDE(Error);

  // в любом случае модифицируем текст Exception-а.
  // Module есть !
  with Error.Address do
  E.Message := Format('%s'#13#13'module "%s", line %d.'#13'%s', [E.Message, Module.Name, Line, Error.Code]);
end;

{$ifdef MSWINDOWS}
var
  SYS_EXCEPTION_PROC: Pointer;

// эта функция нужна для "отлова" Exception-а, произошедшего в IDE,
// когда программа "запускала скрипт"
//
// вся идея сего действа - детализировать Exception, возникший в скрипте,
// информацией о самом скрипте: имя модуля, строка, код
//
// сначала подменяется System.RaiseExceptionProc на TLua.FExceptionRecaller
// в этой "функции" подменяется eax на конкретный экземпляр TLua,
// а потом перенаправляется в __TLuaScriptExceptionRecaller
//
// задача функции - по возможности перенаправить Exception
// вызвать TLua.ScriptExceptionHandler, а после вызвать стандартную функцию
procedure __TLuaScriptExceptionRecaller(const Self: TLua);
const
  {$ifdef fpc}
     {$MESSAGE 'Look constants!'}
  {$endif}
  cDelphiException    = $0EEDFADE;
  cNonDelphiException = $0EEDFAE4;   
  SysUtilsException: TClass = SysUtils.Exception;
asm
  push EAX // чтобы не потерять Self
  cmp [esp+4+4], cDelphiException
  je @check_exception
  cmp [esp+4+4], cNonDelphiException
  je @check_exception
jmp @run_default
@check_exception:
  // if (Parameter is SysUtils.Exception) 
  mov eax, [esp+4+24]
  mov edx, SysUtilsException
  mov eax, [eax]
  call TObject.InheritsFrom
  and eax, $ff
  jz @run_default

  // if (Self.FExceptionThreadId = GetCurrentThreadID)
  call GetCurrentThreadID
  mov ecx, eax
  mov eax, [esp]
  cmp ecx, [EAX].TLua.FExceptionThreadId
  jne @run_default

  // Self.ScriptExceptionHandler(ExceptionInstance)
  mov edx, [esp+4+24]
  call TLua.ScriptExceptionHandler

@run_default:
  // recall System.RaiseExceptionProc(Stack Parameters)
  mov eax, SYS_EXCEPTION_PROC
  pop ecx // убрать Self из стека
  mov System.RaiseExceptionProc, eax
  jmp eax
end;
{$else}
  // todo ?
{$endif}



{$if defined(UNICODE) and defined(LUA_ANSI)}
function IsAnsiValidIdent(const Ident: AnsiString): boolean;
const
  Alpha: set of AnsiChar = ['A'..'Z','a'..'z','_'];
  AlphaNumeric: set of AnsiChar = ['A'..'Z','a'..'z','_','0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;
{$ifend}

{$if (not defined(UNICODE)) and defined(LUA_UNICODE)}
function WideIsAlpha(C: WideChar): boolean;
asm
  cmp ax, '_'
  jne @call_os
  mov eax, 1
  ret
@call_os:
  {$ifdef MSWINDOWS}
     push eax
     call Windows.IsCharAlphaW
  {$else}
     {$MESSAGE ERROR 'OS specific'}
  {$endif}

  test eax, eax
  setnz al
end;
function WideIsAlphaNumber(C: WideChar): boolean;
asm
  cmp ax, '_'
  jne @call_os
  mov eax, 1
  ret
@call_os:
  {$ifdef MSWINDOWS}
     push eax
     call Windows.IsCharAlphaNumericW
  {$else}
     {$MESSAGE ERROR 'OS specific'}
  {$endif}

  test eax, eax
  setnz al
end;
function IsWideValidIdent(const Ident: WideString): boolean;
var
  I: Integer;               
begin
  Result := False;
  if (Length(Ident) = 0) or (not WideIsAlpha(Ident[1])) then Exit;
  for I := 2 to Length(Ident) do if not (WideIsAlphaNumber(Ident[I])) then  Exit;
  Result := True;
end;
{$ifend}

procedure __TLuaThrowIdentifier(const Self: TLua; const Identifier: LuaString);
begin
  Self.Assert('Non-supported identifier "%s"', [Identifier]);
end;

function TLua.CheckIdentifier(const Identifier: LuaString): __luaname;
asm
  push eax
  push edx
  mov eax, edx
  xor edx, edx

  {$ifdef UNICODE}
     {$ifdef LUA_UNICODE}
        call SysUtils.IsValidIdent
     {$else .LUA_ANSI}
        call CrystalLUA.IsAnsiValidIdent
     {$endif}
  {$else .ANSI}
     {$ifdef LUA_UNICODE}
        call CrystalLUA.IsWideValidIdent
     {$else .LUA_ANSI}
        call SysUtils.IsValidIdent
     {$endif}
  {$endif}

  mov ecx, eax
  pop edx
  pop eax
  and ecx, $ff
  jz __TLuaThrowIdentifier

  // Result := Self.FStorage.Names.Identifier(...)
  lea eax, [EAX].TLua.FStorage.Names
  jmp __TLuaNames.Identifier
end;


// комплексное действие, связанное с вызовом скрипта (lua_pcall),
// выставление флагов, проверки ошибок, чистки и т.д.
//
// Exception - фатальное действо приложения.
function TLua.ScriptCall(const ArgsCount: integer=-1; const auto_throw: boolean=true; const AScriptMode: __TLuaScriptCallMode=smScript): integer; // может сделать функцией ?
var
  p1, p2: integer;
  NewException: Exception;

  procedure __ScriptThrow();
  var
    Error: __TLuaScriptError;
  begin
    InspectScriptError(Error);
    ScriptThrow(Error);
  end;

  procedure RunScript();
  begin
    // todo чистка ?

    Result := lua_pcall(Handle, p1, p2, 0);
    if (Result = 0) then Result := lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);

    // todo чистка ?

    if (AScriptMode = smTestLoading) then
    begin
      // todo
    end;
  end;
begin
  // параметры lua_pcall
  p1 := 0;
  p2 := 0;
  if (ArgsCount >= 0) then
  begin
    p1 := ArgsCount;
    p2 := LUA_MULTRET;
  end;

  // вызов (с выставленным флагом FScriptMode и корректной реакцией на эксепшн)
  FScriptMode := AScriptMode;
  FExceptionThreadId := GetCurrentThreadID; {надеюсь "кроссплатформенная" функция}
  {$ifdef MSWINDOWS}
  if (System.DebugHook > 0) then
  begin
    try
      System.RaiseExceptionProc := Self.FExceptionRecaller;
      RunScript();
    finally
      FScriptMode := smNone;
      System.RaiseExceptionProc := SYS_EXCEPTION_PROC;
    end;
  end else
  {$endif}
  // comment: по идее надо реализовать нормальную реакцию на эксепшны при работе в IDE в других ОС
  begin
    try
      RunScript();
      FScriptMode := smNone; // имитация finally
    except
    on E: Exception do
    begin
      FScriptMode := smNone;

      if (FExceptionThreadId = GetCurrentThreadID) then
      begin
        NewException := Exception(E.ClassType.NewInstance());
        CopyObject(NewException, E);
        ScriptExceptionHandler(NewException);
        raise NewException;
      end else
      begin
        raise;
      end;
    end;
    else
      FScriptMode := smNone;
    end;
  end;

  // в случае скриптовой ошибки - взять описание из стека, показать ошибку пользователю
  if (Result <> 0) and (auto_throw) then __ScriptThrow();
end;


// одна из самых сложных функций - прогрузка скриптовых данных
// сложная - потому что приходится учитывать большое множество факторов
procedure TLua.InternalLoadScript(var unique_buffer: __luabuffer;
                                  const ModuleName: LuaString; const FileName: string;
                                  const TestMode: boolean);
const
  BOM_ANSI = 0; // fake
  BOM_UTF32 = 1; // fake

  BOM_DEFAULT = {$ifdef LUA_UNICODE}BOM_UTF8{$else}BOM_ANSI{$endif};
label
  __exit;
var
  Memory: __luadata;
  MemSize: integer;
  Module: PLuaModule;
  ScriptMode: __TLuaScriptCallMode;
  BOM, ret: integer;

  // прошлые значения в Module
  // в случае ошибки они могут возвращаться
  Last_Buffer: __luabuffer;
  Last_BufferOffset: integer;
  Last_Update: integer;

  // для описания ошибки, произошедшей в скрипте
  Error: __TLuaScriptError;

  // поменять местами 4 байта
  procedure Swap(var X, Y); far;
  asm
    push [eax]
    push [edx]
    pop [eax]
    pop [edx]
  end;

  // "откатить" информацию в модуле: буфер, смещение, строки, номер апдейта
  // возвращает true если не пустой
  function RollbackModule(): boolean;
  begin
    Swap(Last_Buffer, Module.FBuffer);
    Swap(Last_BufferOffset, Module.FBufferOffset);
    Swap(Last_Update, Module.FUpdate);

    Module.FLinesCount := 0;
    Module.FLinesInfo := nil;
    Result := Module.InitializeLines();
  end;

  // utf16 <-- utf16be
  procedure UnpackUtf16be(Memory: __luadata; MemSize: integer); far;
  var
    i: integer;
    c: ansichar;
  begin
    for i := 0 to (MemSize div 2)-1 do
    begin
      c := Memory[i*2];
      Memory[i*2] := Memory[i*2+1];
      Memory[i*2+1] := c;
    end;
  end;

  // utf32 <-- utf32be
  procedure UnpackUtf32be(Memory: __luadata; MemSize: integer); far;
  asm
    @loop:
      mov ecx, [eax]
      add eax, 4
      bswap ecx
    sub edx, 4
      mov [eax-4], ecx
    jnz @loop
  end;

  // utf16 <-- utf32
  procedure DecodeUtf32;
  var
    i: integer;
    Dest: pword;
    Src: pdword;
  begin
    Dest := pointer(Memory);
    Src := pointer(Memory);
    for i := 0 to (MemSize div 4)-1 do
    begin
      if (Src^ > $ffff) then Dest^ := ord('?')
      else Dest^ := word(Src^);

      inc(Dest);
      inc(Src);
    end;

    MemSize := MemSize div 2;
  end;


begin
  // проверка имени модуля
  if (not TestMode) then
  begin
     if (Length(ModuleName) = 0) or (Trim(ModuleName) <> ModuleName)
     or (LastDelimiter('":[]', ModuleName) <> 0) then
     Self.Assert('Incorrect module name: "%s"', [ModuleName]);
  end;

  // будем проводить анализ на основе данных в памяти
  Memory := pointer(unique_buffer);
  MemSize := Length(unique_buffer);

  // todo
  // узнать BOM для компилируемых модулей и реагировать соответственно !

  // todo
  // файл проекта

  // поиск предыдущей версии модуля, или заведение нового
  // инкрементация версий апдейта
  // если что ошибка
  if (TestMode) then
  begin
    Module := @FTestModule;
  end else
  begin
    Module := GetModuleByName(ModuleName);
    if (Module <> nil) and (Module.FileName <> FileName) then
    Self.Assert('Different file names of module "%s"', [ModuleName]);

    // добавление нового модуля
    if (Module = nil) then
    begin
      inc(FModulesCount);
      SetLength(FModules, FModulesCount);
      Module := @FModules[FModulesCount-1];
      ZeroMemory(Module, sizeof(TLuaModule));

      Module.FLua := Self;
      Module.FName := ModuleName;
      Module.FFileName := FileName;
      Module.FIndex := FModulesCount-1;
    end;
  end;

  // "сохраняем" предыдущие значения, заполняем новыми
  begin
    Last_BufferOffset := 0;
    Last_Update := 0;
    Swap(Last_Buffer, Module.FBuffer{теперь пустой});
    Swap(Last_BufferOffset, Module.FBufferOffset);
    Swap(Last_Update, Module.FUpdate);

    inc(Module.FUpdateCounter);
    Module.FUpdate := Module.FUpdateCounter;
    Module.FLinesCount := 0;
    Module.FLinesInfo := nil;

    // заносим unique_buffer в Module.FBuffer, unique_buffer теперь неактуальный и пустой
    // однако Memory и MemSize актуальны !
    Swap(Module.FBuffer, unique_buffer);
  end;


  // необходимо определить BOM
  // и смещение
  if (MemSize>=3)and(Memory[0]=#$ef)and(Memory[1]=#$bb)and(Memory[2]=#$ef) then
  begin
    Module.FBufferOffset := 3;
    BOM := BOM_UTF8;
  end else
  if (MemSize>=2)and(Memory[0]=#$ff)and(Memory[1]=#$fe) then
  begin
    if (MemSize>=4)and(Memory[2]=#$00)and(Memory[3]=#$00) then
    begin
      Module.FBufferOffset := 4;
      BOM := BOM_UTF32;
    end else
    begin
      Module.FBufferOffset := 2;
      BOM := BOM_UTF16;
    end;
  end else
  if (MemSize>=2)and(Memory[0]=#$fe)and(Memory[1]=#$ff) then
  begin
    // UTF-16BE
    Module.FBufferOffset := 2;
    UnpackUtf16be(pointer(@Memory[2]), MemSize-2);
    BOM := BOM_UTF16;
  end else
  if (MemSize>=4)and(Memory[0]=#$00)and(Memory[1]=#$00)and(Memory[2]=#$fe)and(Memory[3]=#$ff) then
  begin
    // UTF-32BE
    Module.FBufferOffset := 4;
    UnpackUtf32be(pointer(@Memory[4]), MemSize-4);
    BOM := BOM_UTF32;
  end else
  BOM := BOM_ANSI;


  // перекодировки (если не в родной кодировке)
  if (BOM <> BOM_DEFAULT) then
  begin
    // смещаю указатель памяти.
    // + проверка размера
    inc(Memory, Module.FBufferOffset);
    dec(MemSize, Module.FBufferOffset);
    if ((BOM=BOM_UTF16)and(MemSize and 1 <> 0)) or ((BOM=BOM_UTF32)and(MemSize and 3 <> 0)) then
    Self.Assert('Incorrect script size');

    // сейчас кодировка ansi, utf8, utf16 или utf32
    // правда utf32 вообще не предполагалось, поэтому если что - просто "преобразовываем" до utf16
    if (BOM = BOM_UTF32) then
    begin
      DecodeUtf32();
      BOM := BOM_UTF16;
    end;

    // переводим кодировку к дефолтной
    {$ifdef LUA_ANSI}
      if (BOM = BOM_UTF8) then
      begin
        MemSize := AnsiFromUtf8(pointer(Module.FBuffer), MemSize, Memory, MemSize);
      end else
      // BOM = BOM_UTF16
      begin
        MemSize := MemSize shr 1; // characters count
        AnsiFromUtf16(pointer(Module.FBuffer), pointer(Memory), MemSize);
      end;

      // корректировка буфера и смещения
      SetLength(Module.FBuffer, MemSize);
      Module.FBufferOffset := 0;
    {$else .LUA_UNICODE}
      // надо привести строку к кодировке UTF8
      // здесь unique_buffer используется как временный буфер
      if (BOM = BOM_ANSI) then
      begin
        SetLength(unique_buffer, MemSize*3);
        MemSize := Utf8FromAnsi(pointer(unique_buffer), MemSize*3, Memory, MemSize);
      end else
      // BOM = BOM_UTF16
      begin
        MemSize := MemSize shr 1; // characters count
        SetLength(unique_buffer, MemSize*3);
        MemSize := Utf8FromUtf16(pointer(unique_buffer), MemSize*3, pointer(Memory), MemSize);
      end;

      // реальная длинна, подмена буфера, обнуляем смещение
      SetLength(unique_buffer, MemSize);
      Swap(Module.FBuffer, unique_buffer);
      Module.FBufferOffset := 0;
    {$endif}
  end;

  // загружаем глобальное нативное пространство в Lua, регистрации больше не возможны
  if (not FNameSpaceInitalized) then INITIALIZE_NAME_SPACE();

  // раскидать в модуле строки
  // если модуль пустой - заканчиваем
  if (not Module.InitializeLines()) then exit;

  // компиляция модуля
  // если возникла ошибка (на этапе компиляции) - отреагировать соответствующе
  ret := Module.Compile();
  if (ret <> 0{в случае ошибки}) then
  begin
    InspectScriptError(Error);

    // возвращаем старый модуль к правильному виду
    RollbackModule();

    // всё заканчиваем, показываем ошибку пользователю
    goto __exit;
  end;

  // "выполнение" скриптового модуля
  // в случае возникновения Exception-а, место ошибки в CrystalLUA IDE покажется
  // и текст Exception-а детализируется...
  // но всёравно работа должна быть прекращена!
  ScriptMode := smLoading;
  if (TestMode) then inc(ScriptMode);
  ret := Self.ScriptCall(-1, FALSE, ScriptMode);
  if (ret <> 0{в случае ошибки}) then
  begin
    // получаем ошибку. её будем отображать в будущем!
    InspectScriptError(Error);

    // возвращаем модуль и пытаемся его скомпилировать/выполнить
    // надеемся, что не будет эксепшнов
    if (ScriptMode <> smTestLoading) and (RollbackModule) then
    begin
      ret := Module.Compile();
      if (ret = 0) then
      try
        ret := Self.ScriptCall(-1, FALSE, ScriptMode);
      except
      end;

      if (ret <> 0) then
      Self.stack_clear();
    end;

    goto __exit;
  end;

  // после всех действий, если всё-таки возникла ошибка
  // показать её пользователю
__exit:
  if (pointer(Error.Text) <> nil) then
  Self.ScriptThrow(Error);
end;


// загрузить скрипт из файла
procedure __TLuaLoadScriptFromFile(const Self: TLua; FileName: string);
var
  F: TFileStream;
  buffer: __luabuffer;
  Len: integer;
begin
  // проверяем существование
  if (not FileExists(FileName)) then
  Self.Assert('File "%s" not found', [FileName]);

  // получаем полное имя файла (с путём)
  FileName := ExpandFileName(FileName);

  // для Windows надо в нижнем регистре (уникальность)
  {$ifdef MSWINDOWS}
    UniqueString(FileName);
    {$ifdef UNICODE}
      Windows.CharLowerBuffW(pointer(FileName), Length(FileName));
    {$else .ANSI_MODE}
      Windows.CharLowerBuffA(pointer(FileName), Length(FileName));
    {$endif}
  {$endif}

  // загрузка
  F := SharedFileStream(FileName);
  try
    // чтение
    Len := F.Size;
    SetLength(buffer, Len);
    F.ReadBuffer(pointer(buffer)^, Len);

    // вызов
    Self.InternalLoadScript(buffer, ExtractFileName(FileName), FileName, false);
  finally
    Self.FRecall.CodeAddr := nil;
    F.Free;
  end;
end;

// загрузить скрипт из стрима
procedure __TLuaLoadScriptFromStream(const Self: TLua; const Stream: TStream; const ModuleName: LuaString);
var
  buffer: __luabuffer;
  Len: integer;
begin
  if (Stream = nil) then
  Self.Assert('Stream parameter not defined');

  try
    // чтение
    Len := Stream.Size;
    Stream.Position := 0;
    SetLength(buffer, Len);
    Stream.ReadBuffer(pointer(buffer)^, Len);

    // вызов
    Self.InternalLoadScript(buffer, ModuleName, '', false);
  finally
    Self.FRecall.CodeAddr := nil;
  end;
end;

// загрузить скрипт из буфера
procedure __TLuaLoadScriptFromBuffer(const Self: TLua; const Buffer: pointer; const BufferSize: integer; const ModuleName: LuaString);
var
  __buffer: __luabuffer;
begin
  if (BufferSize < 0) then
  Self.Assert('Fail BufferSize parameter (= %d)', [BufferSize]);

  if (BufferSize > 0) and (Buffer = nil) then
  Self.Assert('Fail Buffer parameter (= nil)');

  SetLength(__buffer, BufferSize);
  Move(Buffer^, pointer(__buffer)^, BufferSize);
  Self.InternalLoadScript(__buffer, ModuleName, '', false);
end;

// загрузить скрипт в режиме тестирования
procedure __TLuaLoadScriptAsTest(const Self: TLua; const Script: LuaString);
var
  buffer: __luabuffer;

  {$ifdef LUA_UNICODE}
     Len, MaxLen: integer;
  {$endif}
begin
  {$ifdef LUA_ANSI}
     buffer := Script;
     UniqueString(AnsiString(buffer));
  {$else .LUA_UNICODE}
     Len := Length(Script);
     MaxLen := Len*3;
     SetLength(buffer, 3+1+MaxLen);
     pinteger(buffer)^ := BOM_UTF8;
     Len := 3 + Utf8FromUtf16(@buffer[4], MaxLen, pointer(Script), Len);
     SetLength(buffer, Len);
  {$endif}

  // вызов
  Self.InternalLoadScript(buffer, '', '', TRUE);
end;


// рекалбеки на прогрузку скриптов с указанием адреса вызова
procedure TLua.LoadScript(const FileName: string);
asm
  push offset __TLuaLoadScriptFromFile
  jmp TLua.InternalRecall
end;
procedure TLua.LoadScript(const Stream: TStream; const ModuleName: LuaString);
asm
  push offset __TLuaLoadScriptFromStream
  jmp TLua.InternalRecall
end;
procedure TLua.LoadScript(const Buffer: pointer; const BufferSize: integer; const ModuleName: LuaString);
asm
  pop ebp
  push offset __TLuaLoadScriptFromBuffer
  jmp TLua.InternalRecall
end;
procedure TLua.TestScript(const Script: LuaString);
asm
  push offset __TLuaLoadScriptAsTest
  jmp TLua.InternalRecall
end;


(*procedure TLua.GarbageCollection();

  procedure __ScriptThrow();
  var
    Error: __TLuaScriptError;
  begin
    InspectScriptError(Error);
    ScriptThrow(Error);
  end;
begin
  // script mode ?
  if (lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0) <> 0) then
  __ScriptThrow();
end; *)

// ---------------------- MetaTable Callbacks ---->>>>


procedure __lua_get_value; forward;
procedure __lua_set_value; forward;
procedure __lua_push_string(const Lua: TLua; const S: pointer; const Mode: __TLuaFieldStringType); forward;
procedure __lua_pop_string(const Lua: TLua; const Dest: pointer; const StrInfo: __TLuaFieldBaseInfo; const Len: integer; const S: __luadata); forward;

// универсальная сложная функция
procedure VariantClear(var V: Variant);
asm
  {$ifdef VER140}
    // Delphi6
    jmp System.@VarClear
  {$else}
    cmp System.VarClearProc, 0
    jz @exit
    jmp System.VarClearProc
  {$endif}
@exit:
end;



const
  // режимы главного калбека
  MODE_GLOBAL_GET = 0; // глобальное пространство
  MODE_GLOBAL_SET = 1;
  MODE_OBJECT_GET = 2; // TClass или структура(record/object)
  MODE_OBJECT_SET = 3;
  MODE_ARRAY_GET  = 4; // массив
  MODE_ARRAY_SET  = 5;
  MODE_BITSET_GET = 6; // множество
  MODE_BITSET_SET = 7;
  __MODE_INTERFACE_GET = 8; // интерфейс (не реализовано)
  __MODE_INTERFACE_SET = 9;
  MODE_NAME_SPACED = __MODE_INTERFACE_SET; // top режимов, которые можно использовать для поиска глобальной идентификации (самые распространённые)

  // todo ещё какие-то

  MODE_PROC_CALL = MODE_NAME_SPACED + 1;
  MODE_EVENT_CALL = MODE_PROC_CALL; // todo

  // режимы-операторы для структур
  MODE_LUA_OPERATORS = 20;
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
  MODE_OPERATOR_NEG = MODE_LUA_OPERATORS+OPERATOR_NEG;
  MODE_OPERATOR_ADD = MODE_LUA_OPERATORS+OPERATOR_ADD;
  MODE_OPERATOR_SUB = MODE_LUA_OPERATORS+OPERATOR_SUB;
  MODE_OPERATOR_MUL = MODE_LUA_OPERATORS+OPERATOR_MUL;
  MODE_OPERATOR_DIV = MODE_LUA_OPERATORS+OPERATOR_DIV;
  MODE_OPERATOR_MOD = MODE_LUA_OPERATORS+OPERATOR_MOD;
  MODE_OPERATOR_POW = MODE_LUA_OPERATORS+OPERATOR_POW;
  MODE_OPERATOR_EQUAL = MODE_LUA_OPERATORS+OPERATOR_EQUAL;
  MODE_OPERATOR_LESS = MODE_LUA_OPERATORS+OPERATOR_LESS;
  MODE_OPERATOR_LESS_EQUAL = MODE_LUA_OPERATORS+OPERATOR_LESS_EQUAL;
  MODE_OPERATOR_CONCAT = MODE_LUA_OPERATORS+OPERATOR_CONCAT;

// перевызов функции
// из скрипта
// todo
procedure __lua_callproc(Lua: TLua);
asm
  mov ecx, MODE_PROC_CALL
  jmp TLua.Callback
end;

// это основной калбек, отвечающий за огромное количество калбеков.
//
// необходимость в этой функции появилась потому, что необходимо регистрировать
// ОЧЕНЬ много разных функций со схожим базовым функционалом
//
// во-первых, универсализация. во-вторых, оптимизация.
// ну и конечно убирание рутины типа поиска идентификатора, массив типов, чистки и т.д.
function TLua.Callback(const T: __TLuaType; const Mode: integer): integer;
asm
@prefix:
  push ebp
  push esi
  push edi
  push ebx
  push ecx

  mov ESI, eax
  mov EDI, edx
  mov EBX, ecx
@params:
  // цель этого блока привести стек в конечном счёте к такому виду:
  // Handle, <index>, <buf> | @done_finish, userdata/nil, ebp:[Count], <type1>, <type2>, <type3>, ...
  // количество переменных легко определить для namespaced-случаев
  cmp ecx, MODE_NAME_SPACED
  ja @gettop
    and ecx, 1
    sub esp, 4
    lea eax, [2 + ecx]
  jmp @aftergettop
  @gettop:
    push [ESI].TLua.FHandle
    call lua_gettop
  @aftergettop:
  lea ecx, [eax*4 + 16]
  mov ebp, eax
  sub esp, ecx  // esp := @<index>
  mov [esp+ 8], offset @done_finish
  mov [esp+12], 0 // userdata
  mov [esp+16], eax // count
  push [ESI].TLua.FHandle

  // цикл lua_type (количество хранится в ebp)
  @luatype_loop:
    mov [esp+4], ebp
    call lua_type
    mov [esp + ebp*4 + 20], eax
  dec ebp
  jnz @luatype_loop

  // запоминаем стек в нужном месте
  lea EBP, [esp+20]
  // userdata/nil(MetaTable)
  cmp eax, LUA_TUSERDATA
  jne @clearing
  mov [esp+4], 1
  call lua_touserdata
  mov [ebp-4], eax
@clearing:
  // здесь надо произвести чистку Self.temporary_clear() или более простой вариант чистки
  cmp [ESI].TLua.FTemporary, 0
  jz @clearing_fast
  push dword ptr offset @begin
  mov eax, ESI
  jmp TLua.temporary_clear
@clearing_fast:
  // упрощенный вариант чистки
  // todo

@begin:
  cmp EBX, MODE_NAME_SPACED
  ja @case_callback_mode
  cmp [ebp+8], LUA_TSTRING
  jne @namespace_not_identifier
@namespace_find:
  // идентификатор - строка
  // необходимо найти его в хеш-таблице идентификаторов
  // указатель на Len пока в @done_finish, но потом надо вернуть!
  lea eax, [esp+12]
  mov [esp+4], 2
  mov [esp+8], eax 
  call lua_tolstring
  // сохраняем идентификатор __luaname в <index>, Len сохраняем в <buf>, восстанавливаем @done_finish
  mov edx, [esp+12]
  mov [esp+4], eax
  mov [esp+12], offset @done_finish
  mov [esp+8], edx
  // ищем идентификатор eax: __luaname
  shr eax, 2 // key
  mov edx, [EDI+__TLuaType.namespace+__TLuaHashArray.FArray]
  mov ecx, eax // key to compare
  and eax, [EDI+__TLuaType.namespace+__TLuaHashArray.FAndMask]
  mov edx, [edx + eax*4]
  @namespace_find_loop:
    test edx, edx
    jz @namespace_identifier_not_found
    cmp [EDX].__TLuaHashItem.Key, ecx
    je @namespace_identifier_found
    mov edx, [EDX].__TLuaHashItem.next
  jmp @namespace_find_loop
@namespace_identifier_found:
  mov edx, [EDX].__TLuaHashItem.Value
  test edx, edx
  jge @namespace_identifier_found_nonstd
// стандартный идентификатор
// перенаправляем вызов в TLua.StdIdentifierCallback (где реализация на языке высокого уровня)
// function TLua.StdIdentifierCallback(const userdata: PLuaUserData; const T: __TLuaType; const stdindex: integer; const getter: boolean): integer;
  not edx
  not ebx
  mov [esp+8], edx // stdindex
  and ebx, 1
  mov eax, esi // Self
  mov [esp+4], ebx // getter
  mov edx, [ebp-4] // userdata
  mov ecx, edi // T: __TLuaType
  mov [esp], offset @done_finish
  jmp TLua.StdIdentifierCallback
@namespace_identifier_found_nonstd:
  // идентификатор найден. это либо экземпляр класса, либо структура, либо глобальное пространство, (либо интерфейс)
  // !!! не массив и не множество.
  // необходимо произвести ряд проверок и подготовок данных
  // сначала производим проверку на Метод. если его надо "взять" - то возвращаем его по lua_pushcclosure()
  cmp [EDX].__TLuaIdentifier.Mode, imProcedure
  jne @namespace_property
  test EBX, 1
  jnz @error_identifier_not_changable // если пытаются поменять функцию

  // пушим lua-функцию (возвращаемся с результатом 1)
  mov ecx, [EDX].__TLuaIdentifier.Proc.lua_CFunction
  mov [esp+8], 0
  mov [esp+4], ecx
  push dword ptr offset @done_finish_one
  jmp lua_pushcclosure
@namespace_property:
  // на данный момент известно, что нужно взять или присвоить конкретное "свойство" (с конкретным именем)
  // это может быть свойство, поле или глобальная переменная.
  // необходимо провести ряд проверок и перенаправить вызовы куда надо

  // для начала сохраняем __PLuaIdentifier в <index>(esp+4), адрес вызова в <buf>(esp+8)
  mov ecx, offset __lua_get_value
  mov eax, offset __lua_set_value
  test ebx, 1
  cmovnz ecx, eax
  mov [esp+4], edx
  mov [esp+8], ecx
  cmp ebx, MODE_GLOBAL_SET
  je @namespace_global_setter
  jb @namespace_global_getter
@namespace_property_check:
  // известно, что нужно взять или присвоить свойство/поле в каком-то классе или структуре
  // для этого нужно провести ряд проверок

@namespace_call_property_getter_setter:
  // вызвать либо геттер, либо сеттер

// esi: Self(TLua)
// eax: Instance (или непосредственный указатель с учётом смещения)
// edx: адаптированный Index (или мусор)
// ebx: PLuaPropertyInfoCompact(PLuaPropertyInfo)
// edi: адрес вызова функции (или nil если обычный режим чтения)

//  {$MESSAGE 'здесь'}

{НАДО НАЧАТЬ С ПРОВЕРОК}



  mov ebx, [esp+4]




  // указатель на функцию
  mov ecx, [ebp]
  xor edi, edi
  mov ecx, [ebp + ecx*4 + 20] // mode
  and ecx, 1
//  mov edx, []


  // присвоение Index(или мусор). может быть в будущем иначе
//  @
  mov edx, [ebp]
  mov edx, [ebp + edx*4 + 24]
  // прыгаем в __lua_get_value или __lua_set_value
  jmp [esp+8]
@namespace_global_getter:
  // нужно что-то взять из глобального пространства
   // todo

@namespace_global_setter:
  // нужно что-то взять изменить в глобальном пространстве
   // todo 


@namespace_identifier_not_found:
  // нужно было найти идентификатор. но не нашли
  // скорее всего ошибка, но для глобального пространства возможно нужно создавать поле
  // todo что-то для интерфейсов
  cmp EBX, MODE_GLOBAL_SET
  jne @error_identifier_not_found

  // необходимо добавить глобальный идентификатор !!!
  // <index> = __luaname, <buf> = Len
  // todo
                   

@namespace_not_identifier:
  // namespace-режим, но указана не строка в качестве идентификатора
  // todo


@case_callback_mode:
  // в этом блоке происходит распределение калбеков.
  // на данный момент известно, что это не namespace-калбеки
  // todo





@error_identifier_not_found:
  // ошибка, возникающая в случае обращения по некорректному идентификатору
  // todo

@error_identifier_not_changable:
  // если идентификатор нельзя менять
  // хз может по факту останется только методы
  // todo

{@done_finish_zero:
  xor eax, eax
  jmp @done_finish }
@done_finish_one:
  mov eax, 1
@done_finish:
  // сюда будут все возвращения из функций
  // надо, имея ebp вернуть esp на нужное место (при этом не трогая eax!!!)
  mov edx, [ebp]
  lea esp, [ebp + edx*4 + 4]
  
@postfix:
  pop ecx
  pop ebx
  pop edi
  pop esi
  pop ebp

  // может быть дополнительный(искуственный) параметр в стеке, который надо убрать перед ret
  cmp ecx, 0 // todo
  jmp @exit // todo
  pop ecx 
@exit:
end;


// по большому счёту это самая частая внутрення функция после TLua.Callback
// суть её заключается в том, чтобы запушить какое-то значение из памяти:
//   поля, свойства, массива, глобального объекта
//
// функция написана на ассемблере и ориентирована на джамп из TLua.Callback
// с целью оптимизации по выполнению, минимальному размеру
//
// функция не содержит никаких проверок ! (или надо?)
//
// esi: Self(TLua)
// eax: Instance (или непосредственный указатель с учётом смещения)
// edx: адаптированный Index (или мусор)
// ebx: __PLuaFieldInfo(__PLuaPropertyInfo)
// edi: адрес вызова функции (или nil если обычный режим чтения)
// ecx - буфер
procedure __lua_get_value;
const
  CURRENCY_CORRECTION: double = 1/1000;
  SIZEOF_USERDATA = sizeof(TLuaUserData);
  TLuaReferenceClass: TClass = TLuaReference;
  varDeepData = $BFE8; // константа из модуля Variants
asm
  // пушим адрес возврата после lua_-функции
  push dword ptr offset @exit

  // case Base.Kind of
  // fkBoolean,fkInteger,fkInt64,fkFloat,fkPointer,fkString,fkVariant,
  // fkObject,fkClass,fkRecord,fkArray,fkSet,fkInterface,fkUniversal,fkMethod
  movzx ecx, [EBX].__TLuaFieldInfo.Base.Kind
  jmp [offset @case_basekind + ecx*4 - 4]
  @case_basekind: DD @boolean,@integer,@int64,@float,@pointer,@string,@variant,@object,@class,@record,@array,@set,@interface,@universal,@method
@boolean:
  test edi, edi
  jz @boolean_ptr
  // если вызываем функцию - то всёравно потом надо приводить размер операнда
  call edi
  mov [esp+8], eax
  lea eax, [esp+8]
  // распаковка по адресу
  @boolean_ptr:
  movzx ecx, [EBX].__TLuaFieldInfo.Base.BoolType
  jmp [offset @case_booltype + ecx*4]
  @case_booltype: DD @btBoolean,@btByteBool,@btWordBool,@btLongBool
  @btBoolean:
  @btByteBool: movzx eax, byte ptr [eax]
               jmp @push_boolean
  @btWordBool: movzx eax, word ptr [eax]
               jmp @push_boolean
  @btLongBool: mov eax, [eax]
               jmp @push_boolean
@integer:
  test edi, edi
  jz @integer_ptr
  call edi
  mov [esp+8], eax
  lea eax, [esp+8]
  
  @integer_ptr:
  movzx ecx, [EBX].__TLuaFieldInfo.Base.OrdType
  jmp [offset @case_ordtype + ecx*4]
  @case_ordtype: DD @otSByte,@otUByte,@otSWord,@otUWord,@otSLong,@otULong
  @otSByte:  movsx eax, byte ptr [eax]
             jmp @push_integer
  @otUByte:  movzx eax, byte ptr [eax]
             jmp @push_integer
  @otSWord:  movsx eax, word ptr [eax]
             jmp @push_integer
  @otUWord:  movzx eax, word ptr [eax]
             jmp @push_integer
  @otSLong:  fild dword ptr [eax]
             jmp @push_float
  @otULong:  mov eax, [eax]
             test eax, eax
             jge @otSLong //@push_integer
             xor edx, edx
             jmp @push_int64
@int64:
  test edi, edi
  jz @int64_ptr
  push dword ptr offset @push_int64 // вызов с возвратом в нужном месте
  jmp edi

  @int64_ptr:
  fild qword ptr [eax]
  jmp @push_float
@float:
  test edi, edi
  jz @float_ptr
  call edi
  cmp [EBX].__TLuaFieldInfo.Base.FloatType, ftCurr
  je @ftCurrEx
  jmp @push_float
  @float_ptr:
  movzx ecx, [EBX].__TLuaFieldInfo.Base.FloatType
  jmp [offset @case_floattype + ecx*4]
  @case_floattype: DD @ftSingle,@ftDouble,@ftExtended,@ftComp,@ftCurr
  @ftSingle: fld dword ptr [eax]
             jmp @push_float
  @ftDouble: fld qword ptr [eax]
             jmp @push_float
  @ftExtended: fld tbyte ptr [eax]
               jmp @push_float
  @ftComp:   fild qword ptr [eax]
             jmp @push_float
  @ftCurr:   fild qword ptr [eax]
  @ftCurrEx: fmul CURRENCY_CORRECTION
             jmp @push_float
@pointer:
@object:
@class:
@interface:
  test edi, edi
  jz @get_ptr
  cmp [EBX].__TLuaFieldInfo.Base.Kind, fkInterface
  jne @call_ptr
  @interface_ptr:
    lea ecx, [esp+8]
    call @__call_edi_getter
    // todo: скорее всего другая логика для интерфейсов / fpc
    lea eax, [esp+8]
    mov ebx, [eax]
    test ebx, ebx
    jz @push_nil
    call System.@IntfClear
    mov eax, ebx
    jmp @__interface
  @call_ptr:
  push dword ptr offset @ptr_look
  jmp edi
  @get_ptr:
     mov eax, [eax]
  @ptr_look:
  test eax, eax
  jz @push_nil
  movzx ecx, [EBX].__TLuaFieldInfo.Base.Kind
  cmp ecx, fkObject
  je @__object
  cmp ecx, fkPointer
  je @push_pointer
  cmp ecx, fkClass
  je @__class
  @__interface:
     // пока так
     jmp @push_pointer
  @__object:
     mov edi, eax // instance

     mov eax, [eax] // TClass
     mov edx, 1
     cmp eax, TLuaReferenceClass
     je @__object_reference
     call @__get_type_info

     pop ecx // clear ret @exit address
     mov [esp+4], SIZEOF_USERDATA
     mov [esp+8], eax // store __PLuaType (сейчас PLuaClassInfo)
     call lua_newuserdata // lua_newuserdata(Handle, sizeof(TLuaUserData))
     mov [EAX].TLuaUserData.instance, edi
     mov edx, [esp+8]
     mov dword ptr [EAX].TLuaUserData.kind, 0 // kind=ukInstance, is_const=FALSE, gc_destroy=FALSE
     (*mov ecx, [EDX].TLuaClassInfo._ClassIndex*)
     mov [EAX].TLuaUserData.ClassIndex, ecx

     mov eax, edx
     jmp @__set_metatable
  @__object_reference:
     // lua_rawgeti(Handle, LUA_GLOBALSINDEX, TLuaReference(Value.p).Index)
     mov eax, [EDI].TLuaReference.Index
     mov [esp+8], LUA_GLOBALSINDEX
     mov [esp+12], eax
     jmp lua_rawgeti
  @__class:
     // lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassesInfo[internal_class_index(Value.p, true)].Ref)
     mov edx, 1
     call @__get_type_info
     (*mov eax, [EAX].TLuaClassInfo.Ref*)
     mov [esp+8], LUA_GLOBALSINDEX
     mov [esp+12], eax
     jmp lua_rawgeti

@__get_type_info: // function (AType: pointer; AIsClass: boolean): __PLuaType (сейчас PLuaClassInfo)
  // пока вызываем internal_class_index()
  // TODO(!!!)
  mov ecx, edx
  mov edx, eax
  mov eax, esi
  (*call TLua.internal_class_index*)
  (*mov edx, [ESI].TLua.ClassesInfo*)
  //lea eax, [edx + eax*sizeof(TLuaClassInfo=64)]
  shl eax, 6
  pop ecx
  add eax, edx
  jmp ecx

@__set_metatable:
  // это финальная стадия обработки userdata - навесить метатаблицу
  // eax - __PLuaType (сейчас PLuaClassInfo), на вершине стека - Handle

  // lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassInfo.Ref);
  (*mov eax, [EAX].TLuaClassInfo.Ref*)
  mov [esp+4], LUA_GLOBALSINDEX
  mov [esp+8], eax
  call lua_rawgeti
  // lua_setmetatable(Handle, -2);
  mov [esp+4], -2
  push dword ptr offset @exit
  jmp lua_setmetatable

@__call_edi_getter:
  // необходимость в этом коде возникла потому, что есть функции, которые
  // должны возвращать "сложный" результат (указатель на результат - параметром)
  // eax-instance, edx-index(или мусор), ecx-указатель на результат, ebx-PLuaPropertyInfo(портится)
  // (!!!) первые 4 байта результата [на всякий случай] обнуляются
  (*mov ebx, [EBX].__TLuaPropertyInfo.PropInfo*)
  mov [ecx], 0
  cmp [EBX].TPropInfo.Index, MODE_NONE_USE //PROP_NONE_USE
  cmove edx, ecx
  jmp edi

@string:
  movzx ecx, [EBX].__TLuaFieldInfo.Base.StringType
  test edi, edi
  mov [ebp-4], ecx
  jz @__call_push_string
    jmp [offset @case_stringtype + ecx*4]
    @case_stringtype: DD @stAnsiChar,@stWideChar,@stShortString,@stAnsiString,@stWideString{$ifdef UNICODE},@stUnicodeString{$endif}
    @stAnsiChar: call edi
                 mov [esp+4], eax
                 lea eax, [esp+4]
                 jmp @__call_push_string
    @stWideChar: call edi
                 mov [esp+4], eax
                 lea eax, [esp+4]
                 jmp @__call_push_string
    @stShortString:
                 sub esp, 256
                 mov ecx, esp
                 call @__call_edi_getter
                 mov eax, esp
                 push dword ptr offset @exit
                 jmp @__call_push_string
    @stAnsiString:
    @stWideString:
    {$ifdef UNICODE}@stUnicodeString:{$endif}
                 lea ecx, [esp+12]
                 call @__call_edi_getter
                 lea eax, [esp+12]
                 call @__call_push_string
                 // очистка строки
                 lea eax, [esp+12]
                 mov ecx, [ebp-4]
                 cmp [eax], 0
                 jz @exit // если строка пустая - то и удалять не надо
                 sub ecx, stAnsiString
                 jmp [offset @case_stringfinalize + ecx*4]
                 @case_stringfinalize: DD System.@LStrClr,System.@WStrClr{$ifdef UNICODE},System.@UStrClr{$endif}

  @__call_push_string:
    // вызов глобальной универсальной функции __lua_push_string с возвратом в адрес на вершине стека (например @exit)
    // для stAnsiString, stWideString и stUnicodeString - в качестве указателя берутся сами символы
    mov edx, eax
    mov ecx, [ebp-4]
    mov eax, [edx]
    cmp ecx, stAnsiString
    cmovnb edx, eax
    mov eax, ESI
    jmp __lua_push_string

@variant:
  test edi, edi
  jz @__call_pushvariant // вызов push_variant c возвратом в @exit
  @__variant_getter:
    mov ecx, esp // сейчас в стеке как раз есть 16 байт
    call @__call_edi_getter
    mov eax, esp
    call @__call_pushvariant
    // удаление варианта
    movzx ecx, [ESP].TVarData.VType
    mov edx, 1
    test ecx, varByRef
    jnz @exit
    cmp ecx, varInt64
    ja  @__call_clearvariant
    shl edx, cl
    test edx, MASK_VARIANTS_DIFFICULT
    jz @exit
  @__call_clearvariant:
    // todo посмотреть ещё этот код
    mov eax, esp
    push dword ptr offset @exit
    jmp VariantClear

  @__call_pushvariant:
    // if (not push_variant(PVariant(eax)^)) then lua_pushnil(Handle);
    mov edx, eax
    mov eax, ESI
    (*call TLua.push_variant*)
    and eax, $ff
    jz @push_nil
    pop ecx
    jmp ecx

@record:
@array:
@set:
  // todo может переписать на асм
  // CrystalLUA.GetPushDifficultTypeProp(Self, instance, is_const, PropertyInfo^);
  mov edx, [ebp-4]
  xor ecx, ecx
  test edx, edx
  jz @__difficult
  movzx ecx, [EDX].TLuaUserData.is_const
  @__difficult:
    mov edx, eax
    mov [esp+4], ebx
    mov eax, esi
    (*jmp CrystalLUA.GetPushDifficultTypeProp*)

@universal:
  // CrystalLUA.GetPushUniversalTypeProp(Self, instance, is_const, PropertyInfo^);
  mov edx, [ebp-4]
  xor ecx, ecx
  test edx, edx
  jz @__universal
  movzx ecx, [EDX].TLuaUserData.is_const
  @__universal:
    mov edx, eax
    mov [esp+4], ebx
    mov eax, esi
    (*jmp CrystalLUA.GetPushUniversalTypeProp*)

@method:
  // ToDo

@push_nil:
  jmp lua_pushnil
@push_boolean:
  // результат в eax
  mov edx, -1
  and eax, 1
  cmovnz eax, edx
  mov [esp+8], eax
  jmp lua_pushboolean
@push_pointer:
  // результат в eax, не nil
  mov [esp+8], eax
  jmp lua_pushlightuserdata
@push_int64:
  // результат в eax:edx. надо запушить в st(0) 
  push edx
  push eax
  fild qword ptr [esp]
  add esp, 8
  jmp @push_float
@push_integer:
  // результат в eax. надо запушить в st(0)
  mov [esp+8], eax // буфер
  fild dword ptr [esp+8]
@push_float:
  // результат в st(0). надо вызвать push_number
  fstp qword ptr [esp+8]
  jmp lua_pushnumber
@exit:
  mov eax, 1
  jmp [ebp-8]
end;


// запушить строку во внутреннем lua типе (__luadata)
// в случае чего - выполнить необходимые преобразования
procedure __lua_push_string(const Lua: TLua; const S: pointer; const Mode: __TLuaFieldStringType);
var
  Data: __luadata;
  Length: integer;
  LastOffset: __luapointer;
  charbuf: dword;
begin
  Data := S;
  Length := 0;
  LastOffset := Lua.FStorage.DataBuffer.MemoryOffset;

  if (S <> nil) then
  case (Mode) of
       stAnsiChar: begin
                     {$ifdef LUA_ANSI}
                        Length := 1;
                     {$else .LUA_UNICODE}
                        charbuf := pbyte(S)^;
                        if (charbuf <= 127) then
                        begin
                          Length := 1;
                        end else
                        begin
                          charbuf := map_ansi_to_utf8[charbuf-128];
                          Length := byte(charbuf);
                          charbuf := charbuf shr 8;
                          Data := pointer(@charbuf);
                        end;
                     {$endif}
                   end;
       stWideChar: begin
                     charbuf := pword(S)^;
                     if (charbuf <= 127) then
                     begin
                       Length := 1;
                     end else
                     begin
                       Data := pointer(@charbuf);
                       {$ifdef LUA_ANSI}
                         AnsiFromUtf16(Data, S, 1);
                         Length := 1;
                       {$else .LUA_UNICODE}
                         Length := Utf8FromUtf16(Data, 4, S, 1);
                       {$endif}
                     end;
                   end;
    stShortString: begin
                     // считаем, что кодировка пользовательского ShortString - Ansi
                     {$ifdef LUA_ANSI}
                       Length := pbyte(Data)^;
                       inc(Data);
                     {$else .LUA_UNICODE}
                       Length := pbyte(S)^;
                       if (Length <> 0) then
                       begin
                         Data := Lua.FStorage.DataBuffer.alloc(Length*3); {!!!}
                         Length := Utf8FromAnsi(Data, Length*3, pointer(integer(S)+1), Length);
                       end;
                     {$endif}
                   end;
     stAnsiString: begin
                     Length := pinteger(integer(S)-4)^;
                     {$ifdef LUA_UNICODE}
                       if (Length <> 0) then
                       begin
                         Data := Lua.FStorage.DataBuffer.alloc(Length*3); {!!!}
                         Length := Utf8FromAnsi(Data, Length*3, S, Length);
                       end;
                     {$endif}
                   end;
     stWideString: begin
                     Length := pinteger(integer(S)-4)^ shr 1;
                     if (Length <> 0) then
                     begin
                       {$ifdef LUA_ANSI}
                         Data := Lua.FStorage.DataBuffer.alloc(Length); {!!!}
                         AnsiFromUtf16(Data, S, Length);
                       {$else .LUA_UNICODE}
                         Data := Lua.FStorage.DataBuffer.alloc(Length*3); {!!!}
                         Length := Utf8FromUtf16(Data, Length*3, S, Length);
                       {$endif}
                     end;
                   end;
  {$ifdef UNICODE}
  stUnicodeString: begin
                     Length := pinteger(integer(S)-4)^;
                     if (Length <> 0) then
                     begin
                       {$ifdef LUA_ANSI}
                         Data := Lua.FStorage.DataBuffer.alloc(Length*3); {!!!}
                         Length := Utf8FromAnsi(Data, Length*3, S, Length);
                       {$else .LUA_UNICODE}
                         Data := Lua.FStorage.DataBuffer.alloc(Length*3); {!!!}
                         Length := Utf8FromUtf16(Data, Length*3, S, Length);
                       {$endif}
                     end;
                   end;
  {$endif}
  end;

  // коррекция (если пустой), непосредственно push
  if (Data = nil) or (Length = 0) or ((Length=1)and(Data^=#0)) then
  begin
    Data := @NULL_CHAR;
    Length := 0;
  end;
  lua_pushlstring(Lua.Handle, Data, Length);

  // восстанавливаем если что смещение
  Lua.FStorage.DataBuffer.MemoryOffset := LastOffset;
end;

// задача функции - записать данные из внутреннего формата utf8 или ansi (S:Len)
// в реальную строку: символ(ы), котороткую строку(ansi) или динамическую строку: Ansi, Wide, Unicode
// для сложных строк Dest^ должен быть нулевым
procedure __lua_pop_string(const Lua: TLua; const Dest: pointer; const StrInfo: __TLuaFieldBaseInfo; const Len: integer; const S: __luadata);
var
  charbuf: dword;
  destlen: dword absolute charbuf;
begin
  // пустая строка
  if (Len = 0) then
  begin
    case StrInfo.StringType of
      stWideChar: pword(Dest)^ := 0;
      stAnsiChar,
   stShortString: pbyte(Dest)^ := 0;
    end;
    exit;
  end;

  // непустая строка
  case StrInfo.StringType of
      stAnsiChar: {$ifdef LUA_ANSI}
                     PAnsiChar(Dest)^ := S^;
                  {$else .LUA_UNICODE}
                  begin
                    if (S^ <= #127) then PAnsiChar(Dest)^ := S^
                    else AnsiFromUtf8(Dest, 1, S, Len);
                  end;
                  {$endif}
      stWideChar: begin
                     charbuf := pbyte(S)^;
                  {$ifdef LUA_ANSI}
                     if (charbuf <= 127) then pword(Dest)^ := charbuf
                     else PWideChar(Dest)^ := map_ansi_to_wide[charbuf-128];
                  {$else .LUA_UNICODE}
                     if (charbuf <= 127) then pword(Dest)^ := charbuf
                     else Utf16FromUtf8(Dest, 1, S, Len);
                  {$endif}
                  end;
   stShortString: begin
                    {$ifdef LUA_ANSI}
                      destlen := Len;
                      if (destlen > StrInfo.ShortStrMaxLen) then destlen := StrInfo.ShortStrMaxLen;
                      pbyte(Dest)^ := destlen;
                      Move(S^, pointer(integer(Dest)+1)^, destlen);
                    {$else .LUA_UNICODE}
                      pbyte(Dest)^ := AnsiFromUtf8(pointer(integer(Dest)+1), StrInfo.ShortStrMaxLen, S, Len);
                    {$endif}
                  end;
    stAnsiString: begin
                    {$ifdef LUA_ANSI}
                       SetLength(PAnsiString(Dest)^, Len);
                       Move(S^, pointer(Dest^{AnsiString})^, Len);
                    {$else .LUA_UNICODE}
                       destlen := Utf8Length(S, Len);
                       SetLength(PAnsiString(Dest)^, destlen);
                       AnsiFromUtf8(ppointer(Dest)^, destlen, S, Len);
                    {$endif}
                  end;
    stWideString: begin
                    {$ifdef LUA_ANSI}
                      SetLength(PWideString(Dest)^, Len);
                      Utf16FromAnsi(ppointer(Dest)^, S, Len);
                    {$else .LUA_UNICODE}
                      destlen := Utf8Length(S, Len);
                      SetLength(PWideString(Dest)^, destlen);
                      Utf16FromUtf8(ppointer(Dest)^, destlen, S, Len);
                    {$endif}
                  end;
  {$ifdef UNICODE}
 stUnicodeString: begin
                    {$ifdef LUA_ANSI}
                      SetLength(PUnicodeString(Dest)^, Len);
                      Utf16FromAnsi(ppointer(Dest)^, S, Len);
                    {$else .LUA_UNICODE}
                      destlen := Utf8Length(S, Len);
                      SetLength(PUnicodeString(Dest)^, destlen);
                      Utf16FromUtf8(ppointer(Dest)^, destlen, S, Len);
                    {$endif}
                  end;
  {$endif}
  end;
end;

procedure ThrowAssignValue(const Lua: TLua; const userdata: PLuaUserData; const PropertyInfo: __TLuaPropertyInfo);
begin
  Lua.Assert('Some exception. todo');
end;

// преобразовать нестроковые данные к строке во внутреннем формате: utf8 или ansi
type
  TChar256Buffer = array[byte] of AnsiChar;

procedure CastAs__luadata(var S: __luadata; var Len: integer; var Buffer: TChar256Buffer;
                          var X; const luatype, stackindex: integer);
const
  HEX_CHARS: array[0..15] of ansichar = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  VInteger: integer absolute X;
  VExtended: extended absolute X;
  VPoint: TPoint absolute X;
  VLuaType: __PLuaType absolute X; // todo __PLuaType

  i, buf: integer;
  p: pointer absolute buf;
  StringBuffer: ShortString absolute Buffer;
begin
  case luatype of
            LUA_TNIL: begin
                        S := 'nil';
                        Len := 3;
                      end;
        LUA_TBOOLEAN: if (VInteger = 0) then
                      begin
                        S := 'false';
                        Len := 5;
                      end else
                      begin
                        S := 'true';
                        Len := 4;
                      end;
  LUA_TLIGHTUSERDATA: begin
                        buf := VInteger;
                        S := @Buffer[0];
                        Len := 8;
                        for i := 7 downto 0 do
                        begin
                          Buffer[i] := HEX_CHARS[buf and $f];
                          buf := buf shr 4;
                        end;
                      end;
         LUA_TNUMBER: begin
                        Str(VExtended:0:3, StringBuffer);
                        S := @StringBuffer[1];
                        Len := Length(StringBuffer);
                        for i := 3 downto 0 do
                        begin
                          if (not (StringBuffer[Len] in ['0', '.'])) then break;
                          dec(Len);
                        end;
                      end;
          LUA_TTABLE: if (VLuaType = nil) then
                      begin
                        S := 'LuaTable';
                        Len := 8;
                      end else
                      begin
                        S := 'ToDo!!!';
                        Len := 7;
                      end;
       LUA_TFUNCTION: begin
                        // todo ?
                        S := 'Proc';
                        Len := 4;
                      end;
       LUA_TUSERDATA: begin
                        p := pointer(VInteger);
                        VLuaType := pointer(VPoint.Y);
                        //if (VLuaType._ClassKind = ckClass) then
                        if (VLuaType.kind = LUATYPE_CLASS) then
                        begin
                          if (TObject(p) is TComponent) then
                          begin
                            i := 0;
                            string(i) := TComponent(p).Name;
                            if (i = 0) then
                            begin
                              Len := 0;
                              S := nil;
                            end else
                            begin
                              S := @Buffer[0];
                              Len := pinteger(i-4)^;
                              {$ifdef LUA_UNICODE}
                                 {$ifdef UNICODE}
                                    Len := Utf8FromUtf16(S, 256, pointer(i), Len);
                                 {$else .ANSI}
                                    Len := Utf8FromAnsi(S, 256, pointer(i), Len);
                                 {$endif}
                              {$else .LUA_ANSI}
                                 {$ifdef UNICODE}
                                    if (Len > 256) then Len := 256;
                                    AnsiFromUtf16(S, pointer(i), Len);
                                 {$else .ANSI}
                                    S := pointer(i);
                                 {$endif}
                              {$endif}
                              string(i) := '';
                            end;
                          end else
                          begin
                            p := PShortString(PPointer(Integer(p^) + vmtClassName)^);
                            S := pointer(integer(p)+1);
                            Len := pbyte(p)^;

                            {$ifdef UNICODE}
                              {$ifdef LUA_UNICODE}
                                 (*done*)
                              {$else .LUA_ANSI}
                                 Len := AnsiFromUtf8(@Buffer[0], 256, S, Len);
                                 S := @Buffer[0];
                              {$endif}
                            {$else .ANSI}
                              (*done - в старых версиях внутренние ShortString был в кодировке utf8 )*)
                            {$endif}
                          end;
                        end else
                        begin
                          S := 'ToDo!!!';
                          Len := 7;
                        end;
                      end;
  else
    S := nil;
    Len := 0;
  end;
end;

// esi: Self(TLua)
// eax: Instance (или непосредственный указатель с учётом смещения)
// edx: адаптированный Index (или мусор)
// ebx: __PLuaFieldInfo(__PLuaPropertyInfo)
// edi: адрес вызова функции (или nil если обычный режим чтения)
// ecx - буфер
procedure __lua_set_value;
const
  CURRENCY_CORRECTION: single = 1000;
asm
  // прежде чем прочитать присваиваемое значение (последнее),
  // нужно привести стек к следующему виду(было 3 стало 5):
  // Handle,LastIndex,ecx(=luatype),edx,eax | @done_finish, userdata/nil, ebp:[Count], ...
  add esp, 12
  push eax
  push edx
  mov eax, [ebp]
  mov ecx, [ebp + eax*4]
  push ecx
  push eax
  push [ESI].TLua.FHandle

  // получение данных
  // либо [esp], либо [esp]:[esp+4], либо st(0)
  cmp ecx, LUA_TUSERDATA
  ja @throw_assign_value
  cmp [EBX].__TLuaFieldInfo.Base.Kind, fkRecord
  jae @after_case_luatype
  cmp [EBX].__TLuaFieldInfo.Base.Kind, fkVariant
  je @after_case_luatype
  jmp [offset @case_luatype + ecx*4]
  @case_luatype: DD @TNIL,@TBOOLEAN,@TLIGHTUSERDATA,@TNUMBER,@TSTRING,@TTABLE,@TFUNCTION,@TUSERDATA
  @TNIL:           xor eax, eax
                   jmp @fill_esp_eax
  @TBOOLEAN:       call lua_toboolean
                   and eax, 1
                   jz @fill_esp_eax
                   mov edx, -1
                   cmp [EBX].__TLuaFieldInfo.Base.Kind, fkBoolean
                   jne @fill_esp_eax
                   cmp [EBX].__TLuaFieldInfo.Base.BoolType, btBoolean
                   cmovne eax, edx
                   jmp @fill_esp_eax
  @TLIGHTUSERDATA: push dword ptr offset @fill_esp_eax
                   jmp lua_touserdata
  @TUSERDATA:      call lua_touserdata
                   test eax, eax
                   jz @throw_assign_value
                   mov [esp+4], eax // PLuaUserData
                   mov eax, [eax]
                   test eax, eax
                   // jz Exception already destroyed
                   jmp @fill_esp_eax
  @TNUMBER:        push dword ptr offset @after_case_luatype
                   jmp lua_tonumber
  @TFUNCTION:      {todo может потом сделать событие!!!}
                   xor eax, eax
                   jmp @fill_esp_eax
  @TSTRING:        lea ecx, [esp+4]
                   mov edx, [ebp]
                   mov eax, [esp]
                   // lua_tolstring: function(L: lua_State; idx: integer; len: pinteger): __luaname; cdecl;
                   push ecx
                   push edx
                   push eax
                   call lua_tolstring
                   add esp, 12
                   jmp @fill_esp_eax
  @TTABLE:         // __PLuaType (сейчас PLuaClassInfo) или nil (таблица)
                   // todo переделать во что-нибудь нормальное
                   mov eax, [esp]
                   mov edx, [ebp]
                   call LuaTableToClass
                   inc eax
                   jz @fill_esp_eax
                   dec eax
                   (*mov edx, [ESI].TLua.ClassesInfo*)
                   shl eax, 6
                   add eax, edx
                   // jmp @fill_esp_eax
  @fill_esp_eax:
    mov [esp], eax
  @after_case_luatype:
  // luatype(ecx), указатель на Dest данные (eax)
  mov ecx, [ebp-20]
  mov eax, [ebp-12]
  test edi, edi
  cmovnz eax, esp

  // данные получены. пытаемся засунуть
  // case Base.Kind of
  // fkBoolean,fkInteger,fkInt64,fkFloat,fkPointer,fkString,fkVariant,
  // fkObject,fkClass,fkRecord,fkArray,fkSet,fkInterface,fkUniversal,fkMethod
  movzx edx, [EBX].__TLuaFieldInfo.Base.Kind
  jmp [offset @case_basekind + edx*4-4]
  @case_basekind: DD @boolean,@integer,@int64,@float,@pointer,@string,@variant,@object,@class,@record,@array,@set,@interface,@universal,@method
@boolean:
  cmp ecx, LUA_TBOOLEAN
  jbe @boolean_fill
  cmp ecx, LUA_TSTRING
  jne @throw_assign_value
  @boolean_as_string:
    // todo

  @boolean_fill:
    // если нужно вызывать функцию, то в стеке уже лежит всё, что нужно
    test edi, edi
    jnz @call_edi
    // переносим значение из [esp] в ячейку памяти [eax]
    mov edx, [esp]
    movzx ecx, [EBX].__TLuaFieldInfo.Base.BoolType
    jmp [offset @case_booltype + ecx*4]
    @case_booltype: DD @btBoolean,@btByteBool,@btWordBool,@btLongBool
    @btBoolean:
    @btByteBool: mov [eax], dl
                 jmp @exit
    @btWordBool: mov [eax], dx
                 jmp @exit
    @btLongBool: mov [eax], edx
                 jmp @exit

@integer:
  jmp [offset @case_luatype_integer + ecx*4]
  @case_luatype_integer: DD @integer_fill,@integer_fill,@integer_fill,@integer_as_number,@integer_as_string,@integer_as_table,@integer_as_function,@integer_as_userdata
  @integer_as_string:
    // todo
  @integer_as_table:
    cmp [esp], 0
    jz @throw_assign_value
    jmp @integer_fill
  @integer_as_function:
    // todo
    jmp @throw_assign_value
  @integer_as_userdata:
    add esp, 4 // теперь [esp]=instance
    jmp @integer_fill
  @integer_as_number:
    fistp qword ptr [esp]
  @integer_fill:
    // значение берём в [esp], заносим в [eax]
    test edi, edi
    jnz @call_edi
    mov edx, [esp]
    movzx ecx, [EBX].__TLuaFieldInfo.Base.OrdType
    jmp [offset @case_ordtype + ecx*4]
    @case_ordtype: DD @otSByte,@otUByte,@otSWord,@otUWord,@otSLong,@otULong
    @otSByte:
    @otUByte:  mov [eax], dl
               jmp @exit
    @otSWord:
    @otUWord:  mov [eax], dx
               jmp @exit
    @otSLong:
    @otULong:  mov [eax], edx
               jmp @exit

@int64:
  jmp [offset @case_luatype_int64 + ecx*4]
  @case_luatype_int64: DD @int64_fill_4,@int64_fill_4,@int64_fill_4,@int64_as_number,@int64_as_string,@int64_as_table,@int64_as_function,@int64_as_userdata
  @int64_as_string:
    // todo
  @int64_as_table:
    cmp [esp], 0
    jz @throw_assign_value
    jmp @int64_fill_4
  @int64_as_function:
    // todo
    jmp @throw_assign_value
  @int64_as_userdata:
    mov edx, [esp+4]
    mov [esp], edx // instance
  @int64_fill_4:
    mov [esp+4], 0
    jmp @int64_fill
  @int64_as_number:
    fistp qword ptr [esp]
  @int64_fill:
    test edi, edi
    jnz @call_edi
    pop [eax]
    pop [eax+4]
    jmp @exit

@float:
  jmp [offset @case_luatype_float + ecx*4]
  @case_luatype_float: DD @float_fill_int,@float_fill_int,@throw_assign_value,@float_fill,@float_as_string,@throw_assign_value,@throw_assign_value,@throw_assign_value
  @float_as_string:
    // todo
  @float_fill_int:
    fild dword ptr [esp]
  @float_fill:
  movzx ecx, [EBX].__TLuaFieldInfo.Base.FloatType
  jmp [offset @case_floattype + ecx*4]
  @case_floattype: DD @ftSingle,@ftDouble,@ftExtended,@ftComp,@ftCurr
  @ftSingle: fstp dword ptr [eax]
             jmp @done
  @ftDouble: fstp qword ptr [eax]
             jmp @done
  @ftExtended: fstp tbyte ptr [eax]
               jmp @done
  @ftCurr:   fmul CURRENCY_CORRECTION
  @ftComp:   fistp qword ptr [eax]
             jmp @done

@object:              
  cmp ecx, LUA_TNIL
  je @object_fill
  cmp ecx, LUA_TUSERDATA
  jne @throw_assign_value
  @object_as_userdata:
    // здесь надо определить, лежит ли в userdata TObject
    // если нет - то exception
    //* TODO откорректировать под __PLuaType (сейчас PLuaClassInfo)
    mov edx, [esp+4]
    cmp [EDX].TLuaUserData.kind, ukInstance
    jne @throw_assign_value
    mov edx, [EDX].TLuaUserData.ClassIndex
    (*mov ecx, [ESI].TLua.ClassesInfo*)
    shl edx, 6
    (*cmp [ecx+edx].TLuaClassInfo._ClassKind, ckClass*)
    jne @throw_assign_value
  @object_fill:
    test edi, edi
    jnz @call_edi
    pop [eax]
    jnz @exit

@pointer:
  {код заполнения указателя идентичен коду заполнения TObject, поэтому перенаправляем в @object_fill}
  jmp [offset @case_luatype_pointer + ecx*4]
  @case_luatype_pointer: DD @object_fill,@throw_assign_value,@object_fill,@throw_assign_value,@throw_assign_value,@pointer_as_table,@pointer_as_function,@object_fill
  @pointer_as_table:
    cmp [esp], 0
    jnz @object_fill
    jmp @throw_assign_value
  @pointer_as_function:
    // todo
    jmp @throw_assign_value

@class:
  {код заполнения TClass идентичен коду заполнения TObject, поэтому перенаправляем в @object_fill}
  cmp ecx, LUA_TNIL
  je @object_fill
  cmp ecx, LUA_TTABLE
  je @pointer_as_table // там смотрится класс
  cmp ecx, LUA_TUSERDATA
  jne @throw_assign_value
  @class_as_userdata:
    //* TODO откорректировать под __PLuaType (сейчас PLuaClassInfo)
    mov edx, [esp+4]
    cmp [EDX].TLuaUserData.kind, ukInstance
    jne @throw_assign_value
    mov edx, [EDX].TLuaUserData.ClassIndex
    (*mov ecx, [ESI].TLua.ClassesInfo*)
    shl edx, 6
    (*cmp [ecx+edx].TLuaClassInfo._ClassKind, ckClass*)
    jne @throw_assign_value
    (*push [ecx+edx].TLuaClassInfo._Class*)
    jmp @object_fill

@interface:
  // todo nil, TObject, IInterface
  jmp @throw_assign_value

@string:
  cmp ecx, LUA_TSTRING
  je @string_fill
  cmp ecx, LUA_TNUMBER
  jne @string_convert
    fstp tbyte ptr [esp]
  @string_convert:
    // eax = S(esp); edx = Len(esp+4); ecx = Buffer(esp+8)
    // кроме того в стек: stackindex,luatype,var X
    mov edx, esp
    sub esp, 268  // 256 + S + Len + strbuffer
    push edx  // var X
    push ecx  // luatype
    push [ebp]// stackindex
    lea eax, [esp + 12]
    lea edx, [esp + 16]
    lea ecx, [esp + 24]
    call CastAs__luadata

  @string_fill:
  // на данный момент данные лежат в стеке: [esp]=S, [esp+4]=Length
  // необходимо занести занести распакованные данные в instance (или стек для вызова)
  push dword ptr offset @done // адрес возврата из функции __lua_pop_string
  movzx ecx, [EBX].__TLuaFieldInfo.Base.StringType
  jmp [offset @case_stringtype + ecx*4]
  @case_stringtype: DD @call_lua_pop_string,@call_lua_pop_string,@stShortString,@stAnsiString,@stWideString{$ifdef UNICODE},@stUnicodeString{$endif}
  @stShortString: test edi, edi
                  jz @call_lua_pop_string  // если пишем в конкретную память

                  // нужно выделить ShortString-буфер, и записать в него
                  // потом вызвать сеттер
                  pop ecx
                  sub esp, 256
                  mov eax, esp
                  push eax
                  push dword ptr [esp + 256 + 8]
                  push dword ptr [esp + 256 + 8]
                  push dword ptr @call_edi
                  jmp @call_lua_pop_string
  @stAnsiString:
  @stWideString:
  {$ifdef UNICODE}@stUnicodeString:{$endif}
                  test edi, edi
                  jz @call_lua_pop_string  // если пишем в конкретную память

                  // addr, data, len, buffer(last ecx) ---> по выходу буфер будет на вершине [esp]
                  // после функции направляемся в @string_call_and_finalize
                  lea eax, [esp+12]
                  mov [esp], dword ptr offset @string_call_and_finalize
                  mov [eax], 0
                  // jmp @call_lua_pop_string

  // procedure __lua_pop_string(const Lua: TLua; const Dest: pointer; const StrInfo: TLuaPropertyInfoBase; const Len: integer; const S: __luadata);
  @call_lua_pop_string:
    mov edx, eax
    mov ecx, ebx
    mov eax, ESI
    jmp __lua_pop_string

  // сначала вызвать сеттер (строка на вершине стека),
  // потом финализировать строку после вызова
  // todo переделать под внутренний финализированный буфер ?
  @string_call_and_finalize:
    push dword ptr offset @string_finalize
    jmp @jump_edi

  @string_finalize:
    mov eax, esp
    cmp [esp], 0
    jz @exit
    movzx ecx, [EBX].__TLuaFieldInfo.Base.StringType
    push dword ptr offset @exit
    sub ecx, stAnsiString
    jmp [offset @case_stringfinalize + ecx*4]
    @case_stringfinalize: DD System.@LStrClr,System.@WStrClr{$ifdef UNICODE},System.@UStrClr{$endif}


//---------------------
@variant:
  test edi, edi
  jz @variant_is_instance
  @variant_is_buffer:
    xor edx, edx
    push edx
    mov eax, esp
    call @variant_call_stackvariant
    push dword ptr offset @variant_clear
    jmp @jump_edi

  @variant_is_instance:
    push dword ptr offset @exit

  @variant_call_stackvariant:
    // function TLua.stack_variant(var Ret: Variant; const StackIndex: integer): boolean;
    mov edx, eax
    mov ecx, [ebp]
    mov eax, ESI
    (*call TLua.stack_variant*)
    and eax, $ff
    jz @throw_assign_value 
    pop ecx
    jmp ecx

  @variant_clear:
    // удаление варианта
    movzx ecx, [ESP].TVarData.VType
    mov edx, 1
    test ecx, varByRef
    jnz @exit
    cmp ecx, varInt64
    ja  @__call_clearvariant
    shl edx, cl
    test edx, MASK_VARIANTS_DIFFICULT
    jz @exit
  @__call_clearvariant:
    // todo посмотреть ещё этот код
    mov eax, esp
    push dword ptr offset @exit
    jmp VariantClear

@method:
   //  ToDo

@record:
@array:
@set:
   // todo может переписать на асм
   //if (not CrystalLUA.PopSetDifficultTypeProp(Self, instance, stack_index, PropertyInfo^)) then ThrowAssignValue({false});
   (*mov edi, offset CrystalLUA.PopSetDifficultTypeProp*)
   jmp @call_difficult_proc
@universal:
   // if (not CrystalLUA.PopSetUniversalTypeProp(Self, instance, stack_index, PropertyInfo^)) then ThrowAssignValue({false});
   (*mov edi, offset CrystalLUA.PopSetUniversalTypeProp*)
   @call_difficult_proc:
   push ebx
   mov eax, ESI
   mov edx, [ebp-12]
   mov ecx, [ebp]
   call edi
   and eax, $ff
   jnz @exit

@throw_assign_value:
  ffree st(0)
  mov eax, ESI
  mov edx, [ebp-4]
  mov ecx, ebx
  call ThrowAssignValue
@done:
  test edi, edi
  jz @exit
@call_edi:
  push dword ptr offset @exit
@jump_edi:
  (*mov ecx, [EBX].__TLuaPropertyInfo.PropInfo*)
  mov eax, [ebp-12]
  cmp [ECX].TPropInfo.Index, MODE_NONE_USE //PROP_NONE_USE
  mov edx, [ebp-16]
  mov ecx, [esp+4]
  cmove edx, ecx
  jmp edi
@exit:
  xor eax, eax
  jmp [ebp-8]
end;





// большая обработка всех стандартных свойств/методов
function TLua.StdIdentifierCallback(const userdata: PLuaUserData; const T: __TLuaType; const stdindex: integer; const getter: boolean): integer;
begin
  Result := 1;
end;

// очистка всех временных данных, нуждающихся в чистке
procedure TLua.temporary_clear();
var
  offset: __luapointer;
  buffer: pansichar;
  Value: pointer;
  Info: __PLuaFieldBaseInfo;
begin
  offset := FTemporary;
  buffer := FStorage.DataBuffer.Memory;

  while (offset <> 0) do
  begin
    Value := @buffer[offset];
    Info := ppointer(@buffer[offset-4])^;
    offset := pinteger(@buffer[offset-8])^;

    case Info.Kind of
      fkString: if (pointer(Value^) <> nil) then
                case Info.StringType of
                  stAnsiString: AnsiString(Value^) := '';
                  stWideString: WideString(Value^) := '';
                  {$ifdef UNICODE}
                  stUnicodeString: UnicodeString(Value^) := '';
                  {$endif}
                end;
     fkVariant: begin
                  VariantClear(PVariant(Value)^);
                end;
      fkRecord: begin
                  Finalize(Value, Info.RecordInfo.FTypeInfo);
                end;
       fkArray: begin
                  if (Info.ArrayInfo.IsDynamic) then Finalize(Value, Info.ArrayInfo.FTypeInfo)
                  else Finalize(Value, Info.ArrayInfo.FTypeInfo, Info.ArrayInfo.FItemsCount);
                end;
   fkInterface: begin
                  if (pointer(Value^) <> nil) then IInterface(Value^) := nil;
                end;
   fkUniversal: begin
                  PLuaArg(Value).Empty := true;
                  // todo todo посмотреть
                end;
    end;
  end;

  FTemporary := 0;
  FStorage.DataBuffer.MemoryOffset := 0;
end;

// выделить необходимый кусок памяти, в случае необходимости внести информацию по финализации
// занулить результат
function TLua.temporary_alloc(const Info: __PLuaFieldBaseInfo): __luapointer;
const
  ALIGNED_SIZEOF_TLUAARG = (sizeof(TLuaArg)+3) div 4;
asm
  push esi
  push ebx
  push edi
@prefix:
  mov esi, eax
  mov ebx, [EAX].TLua.FStorage.DataBuffer.MemoryOffset
  mov edi, edx
@begin:
  // определить размер (в dword-ах), если сложный тип - перенаправить в @difficult_allocation
  movzx ecx, [EDX].__TLuaFieldBaseInfo.Kind
  jmp [offset @case_basekind + ecx*4-4]
  @case_basekind: DD @boolean,@integer,@int64,@float,@pointer,@string,@variant,@object,@class,@record,@array,@set,@interface,@universal,@method
  @boolean:
  @integer:
  @pointer:
  @object:
  @class:
    mov ecx, 1
    jmp @allocation
  @int64:
    mov ecx, 2
    jmp @allocation
  @float:
    movzx ecx, [EDX].__TLuaFieldInfo.Base.FloatType
    jmp [offset @case_floattype + ecx*4]
    @case_floattype: DD @ftSingle,@ftDouble,@ftExtended,@ftComp,@ftCurr
    @ftSingle:   mov ecx, 1
                 jmp @allocation
    @ftDouble:
    @ftComp:
    @ftCurr:     mov ecx, 2
                 jmp @allocation
    @ftExtended: mov ecx, 3
                 jmp @allocation
  @string:
    movzx ecx, [EDX].__TLuaFieldInfo.Base.StringType
    jmp [offset @case_stringtype + ecx*4]
    @case_stringtype: DD @stAnsiChar,@stWideChar,@stShortString,@stPAnsiChar,@stPWideChar,@stAnsiString,@stWideString{$ifdef UNICODE},@stUnicodeString{$endif}
    @stAnsiChar:
    @stWideChar:
    @stPAnsiChar:
    @stPWideChar:   mov ecx, 1
                    jmp @allocation
    @stShortString: mov ecx, [EDX].__TLuaFieldInfo.Base.ShortStrMaxLen
                    // ecx := (ecx + 1(len) + 3) shr 2
                    shr ecx, 2
                    inc ecx
                    jmp @allocation
    @stAnsiString:
    @stWideString:
    {$ifdef UNICODE}@stUnicodeString:{$endif}
                    mov ecx, 1
                    jmp @difficult_allocation
  @variant:
    mov ecx, 4
    jmp @difficult_fill
  @record:
    mov eax, [EDX].__TLuaFieldInfo.Base.RecordInfo
    mov ecx, [EAX].TLuaRecordInfo.FSize
    mov edx, [EAX].TLuaRecordInfo.FTypeInfo
    jmp @difficult_alloc_iftypeinfo
  @array:
    mov eax, [EDX].__TLuaFieldInfo.Base.ArrayInfo
    mov ecx, [EAX].TLuaArrayInfo.FSize
    mov edx, [EAX].TLuaArrayInfo.FTypeInfo
    jmp @difficult_alloc_iftypeinfo
  @set:
    mov eax, [EDX].__TLuaFieldInfo.Base.SetInfo
    xor edx, edx
    mov ecx, [EAX].TLuaSetInfo.FSize
    jmp @difficult_alloc_iftypeinfo
  @interface:
    mov ecx, 1
    jmp @difficult_allocation
  @universal:
    mov ecx, ALIGNED_SIZEOF_TLUAARG
    jmp @difficult_allocation
  @method:
    mov ecx, 1 // todo 2?
    jmp @allocation

@difficult_alloc_iftypeinfo:
  // привести размер ecx к размеру в dword-ах
  // если typeinfo(edx)=nil - то обычный аллок, без сложностей
  add ecx, 3
  shr ecx, 2
  test edx, edx
  jz @allocation
@difficult_allocation:
  // перед обычным выделением ещё нужно выделить 8 байт служебной информации
  add ebx, 8
  cmp ebx, [ESI].TLua.FStorage.DataBuffer.Size
  jbe @difficult_fill
    push ecx
    lea eax, [ESI].TLua.FStorage.DataBuffer
    call __TLuaDataBuffer.Grow
    pop ecx
@difficult_fill:
  // 8 байт служебной информации выделено, заполнить [ebx-8]:LastTemporary, [ebx-4]:FinalizeInfo
  mov eax, [ESI].TLua.FTemporary
  mov edx, [ESI].TLua.FStorage.DataBuffer.Memory
  mov [ESI].TLua.FTemporary, ebx
  mov [edx+ebx-8], eax
  mov [edx+ebx-4], edi
@allocation:
  // выделение ecx dword-ов
  lea edx, [ebx + ecx*4]
  mov [ESI].TLua.FStorage.DataBuffer.MemoryOffset, edx
  cmp edx, [ESI].TLua.FStorage.DataBuffer.Size
  jbe @clearing
    push ecx
    lea eax, [ESI].TLua.FStorage.DataBuffer
    call __TLuaDataBuffer.Grow
    pop ecx
@clearing:
  // чистка ecx dword-ов в буфере, начиная с
  mov edi, [ESI].TLua.FStorage.DataBuffer.Memory
  xor eax, eax
  add edi, ebx
  REP STOSD
@end:
  mov eax, ebx
@postfix:
  pop edi
  pop ebx
  pop esi
end;


// ToDo Method call routine !!!



// <<<<---- MetaTable Callbacks ----------------------



// сохранить неймспейс в файл
// TODO
(*type TLuaStringList = class(TStringList) public Lua: TLua; end;
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
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); 
    Result := lua_type(Handle, -1);
    lua_settop(Handle, -1-1);
  end;

  function global_index_value(const Ref: integer): string;
  begin
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
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
    proc_info: ^TLuaProcInfo;
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
end;  *)


// если указано глобальное имя, то запушить значение в стек. если не указано - то запушить nil
// TLuaReference.Create/Initialize добавляется в список ссылок и производит инициализауию в Lua
(*function __TLuaCreateReference(const Self: TLua; const global_name: string{=''}; const ReturnAddr: pointer): TLuaReference;
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
        lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
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
end;

function TLua.CreateReference(const global_name: string=''): TLuaReference;
asm
  mov ecx, [esp]
  jmp __TLuaCreateReference
end;


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
    Self.Assert('Wrong arguments count (%d)%s. Required %s.', [Self.FArgsCount, S, Required]);
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
end;   

// строковое описание аргумента
// в основном для описания ошибок
function  TLua.StackArgument(const Index: integer): string;
var
  Buf: TLuaArg;
begin
  if (not stack_luaarg(Buf, Index, true)) then Result := FBufferArg.str_data
  else Result := Buf.ForceString;

  if (Result = '') then Result := 'nil';
end;  *)


// основная функция пуша для сложных типов: объектов класса, структур, массивов и множеств
(*function  TLua.push_userdata(const ClassInfo: TLuaClassInfo; const gc_destroy: boolean; const Data: pointer): PLuaUserData;
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
  lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassInfo.Ref); // global_push_value(Ref);
  lua_setmetatable(Handle, -2);
end; *)


// запушить свойство
(*function  TLua.push_difficult_property(const Instance: pointer; const PropertyInfo: TLuaPropertyInfo): PLuaUserData;
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
  lua_rawgeti(Handle, LUA_GLOBALSINDEX, mt_properties); // global_push_value(Ref);
  lua_setmetatable(Handle, -2);
end;  *)


(*function TLua.push_variant(const Value: Variant): boolean;
type
  TDateProc = procedure(const DateTime: TDateTime; var Ret: string);
  TIntToStr = procedure(const Value: integer; var Ret: string);
  
var
  VType: integer;
  PValue: pointer;
begin
  // получить тип и указатель на значение
  VType := TVarData(Value).VType;
  PValue := @TVarData(Value).VWords[3];

  // если Variant вызван по ссылке
  if (VType and varByRef <> 0) then
  begin
    VType := VType and (not varByRef);
    PValue := ppointer(PValue)^;
  end;

  // push
  case (VType) of
    varEmpty, varNull, varError{EmptyParam}: lua_pushnil(Handle);
    varSmallint: lua_pushinteger(Handle, PSmallInt(PValue)^);
    varInteger : lua_pushinteger(Handle, PInteger(PValue)^);
    varSingle  : lua_pushnumber(Handle, PSingle(PValue)^);
    varDouble  : lua_pushnumber(Handle, PDouble(PValue)^);
    varCurrency: lua_pushnumber(Handle, PCurrency(PValue)^);
    varDate    : with FBufferArg do
                 begin
                   if (str_data <> '') then str_data := '';
                   case InspectDateTime(PValue) of
                     0: TDateProc(@DateTimeToStr)(PDate(PValue)^, str_data);
                     1: TDateProc(@DateToStr)(PDate(PValue)^, str_data);
                     2: TDateProc(@TimeToStr)(PDate(PValue)^, str_data);
                   end;
                   lua_push_pascalstring(Handle, str_data);
                 end;
    varOleStr  : with FBufferArg do
                 begin
                   str_data := PWideString(PValue)^;
                   lua_push_pascalstring(Handle, str_data);
                 end;
    varBoolean : lua_pushboolean(Handle, PBoolean(PValue)^);
    varShortInt: lua_pushinteger(Handle, PShortInt(PValue)^);
    varByte    : lua_pushinteger(Handle, PByte(PValue)^);
    varWord    : lua_pushinteger(Handle, PWord(PValue)^);
    varLongWord: lua_pushnumber(Handle, PLongWord(PValue)^);
    varInt64   : lua_pushnumber(Handle, PInt64(PValue)^);
    varString  : lua_push_pascalstring(Handle, PString(PValue)^);
  else
    if (FBufferArg.str_data <> '') then FBufferArg.str_data := '';
    TIntToStr(@IntToStr)(VType, FBufferArg.str_data);
    push_variant := false;
    exit;
  end;

  push_variant := true;
end; *)

(*function TLua.push_luaarg(const LuaArg: TLuaArg): boolean;
type
  TIntToStr = procedure(const Value: integer; var Ret: string);
  
begin
  with LuaArg do
  case (LuaType) of
      ltEmpty: lua_pushnil(Handle);
    ltBoolean: lua_pushboolean(Handle, LongBool(Data[0]));
    ltInteger: lua_pushinteger(Handle, Data[0]);
     ltDouble: lua_pushnumber(Handle, pdouble(@Data)^);
     ltString: lua_push_pascalstring(Handle, str_data);
    ltPointer: lua_pushlightuserdata(Handle, pointer(Data[0]));
      ltClass: lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassesInfo[internal_class_index(pointer(Data[0]), true)].Ref);
     ltObject: begin
                 if (TClass(pointer(Data[0])^) = TLuaReference) then lua_rawgeti(Handle, LUA_GLOBALSINDEX, TLuaReference(Data[0]).Index)
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
               end;
      ltTable: begin
                 FBufferArg.str_data := 'LuaTable';
                 push_luaarg := false;
                 exit;
               end;
  else
    if (FBufferArg.str_data <> '') then FBufferArg.str_data := '';
    TIntToStr(@IntToStr)(byte(FLuaType), FBufferArg.str_data);
    push_luaarg := false;
    exit;
  end;

  push_luaarg := true;
end; *)


(*function TLua.push_argument(const Value: TVarRec): boolean;
type
  TIntToStr = procedure(const Value: integer; var Ret: string);
var
  Buf: array[0..3] of char;
begin
  with Value do
  case (VType) of
    vtInteger:   lua_pushinteger(Handle, VInteger);
    vtBoolean:   lua_pushboolean(Handle, VBoolean);
    vtChar:      begin
                   Buf[0] := VChar;
                   Buf[1] := #0;
                   lua_push_pchar(Handle, @Buf[0]);
                 end;
    vtExtended:  lua_pushnumber(Handle, VExtended^);
    vtString:    with FBufferArg do
                 begin
                   str_data := VString^;
                   lua_push_pascalstring(Handle, str_data);
                 end;
    vtPointer:   if (VPointer = nil) then lua_pushnil(Handle) else lua_pushlightuserdata(Handle, VPointer);
    vtPChar:     lua_push_pchar(Handle, VPChar);
    vtObject:    if (VObject = nil) then lua_pushnil(Handle) else
                 begin
                   if (TClass(pointer(VObject)^) = TLuaReference) then lua_rawgeti(Handle, LUA_GLOBALSINDEX, TLuaReference(VObject).Index)
                   else
                   push_userdata(ClassesInfo[internal_class_index(TClass(pointer(VObject)^), true)], false, pointer(VObject));
                 end;  
    vtClass:     if (VClass = nil) then lua_pushnil(Handle) else lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassesInfo[internal_class_index(pointer(VClass), true)].Ref);
    vtWideChar:  begin
                   integer(Buf) := 0;
                   PWideChar(@Buf)^ := VWideChar;
                   FBufferArg.str_data := PWideChar(@Buf);
                   lua_push_pascalstring(Handle, FBufferArg.str_data);
                 end;
    vtPWideChar: begin
                   FBufferArg.str_data := VPWideChar;
                   lua_push_pascalstring(Handle, FBufferArg.str_data);
                 end;
    vtAnsiString:lua_push_pascalstring(Handle, string(VAnsiString));
    vtCurrency:  lua_pushnumber(Handle, VCurrency^);
    vtVariant:   begin
                   push_argument := push_variant(VVariant^);
                   exit;
                 end;  
    vtWideString:begin
                   FBufferArg.str_data := pwidestring(VWideString)^;
                   lua_push_pascalstring(Handle, FBufferArg.str_data);
                 end;
    vtInt64:     lua_pushnumber(Handle, VInt64^);
  else
    if (FBufferArg.str_data <> '') then FBufferArg.str_data := '';
    TIntToStr(@IntToStr)(VType, FBufferArg.str_data);
    push_argument := false;
    exit;
  end;

  push_argument := true;
end;  *)

procedure TLua.stack_clear;
begin
  lua_settop(Handle, 0);
end;

procedure TLua.stack_pop(const count: integer);
begin
  //lua_settop(Handle, -count - 1);//lua_pop(Handle, count);
  lua_settop(Handle, not count);
end;

(*function TLua.stack_variant(var Ret: Variant; const StackIndex: integer): boolean;
var
  VarData: TVarData absolute Ret;
  Number: double;
  IntValue: integer absolute Number;
  luatype: integer;
begin
  // очистка если был занят
  if (VarData.VType = varString) or (not (VarData.VType in VARIANT_SIMPLE)) then VarClear(Ret);

  // получение результата
  luatype := lua_type(Handle, StackIndex);
  case (luatype) of
        LUA_TNIL: begin
                    VarData.VType := varEmpty;
                  end;  
    LUA_TBOOLEAN: begin
                    VarData.VType := varBoolean;
                    VarData.VBoolean := lua_toboolean(Handle, StackIndex);
                  end;
     LUA_TNUMBER: if (NumberToInteger(Number, Handle, StackIndex)) then
                  begin
                    VarData.VType := varInteger;
                    VarData.VInteger := IntValue;
                  end else
                  begin
                    VarData.VType := varDouble;
                    VarData.VDouble := Number;
                  end;
     LUA_TSTRING: begin
                    VarData.VType := varString;
                    VarData.VInteger := 0;

                    { Unicode ??? todo }
                    lua_to_pascalstring(string(VarData.VString), Handle, StackIndex);
                  end;

  else
    VarData.VType := varEmpty;

    // ошибочная ситуация
    FBufferArg.str_data := LuaTypeName(luatype);

    // результат - false
    stack_variant := false;
    exit;
  end;

  stack_variant := true;
end; *)

(*function TLua.stack_luaarg(var Ret: TLuaArg; const StackIndex: integer; const lua_table_available: boolean): boolean;
var
  userdata: PLuaUserData;
  ClassIndex: integer;
  LuaTable: PLuaTable;
  luatype: integer;
begin
  Result := true;

  Ret.FLuaType := ltEmpty;
  luatype := lua_type(Handle, StackIndex);
  case (luatype) of
    LUA_TNIL          : {всё хорошо};
    LUA_TBOOLEAN      : begin
                          //Ret.AsBoolean := lua_toboolean(Handle, StackIndex);
                          Ret.FLuaType := ltBoolean;
                          Ret.Data[0] := ord(lua_toboolean(Handle, StackIndex));
                        end;
    LUA_TNUMBER       : begin
                          // Ret.AsDouble := lua_tonumber(Handle, StackIndex); // автоматическая проверка на Int делается
                          if (NumberToInteger(Ret.Data, Handle, StackIndex)) then
                          Ret.FLuaType := ltInteger else Ret.FLuaType := ltDouble;
                        end;
    LUA_TSTRING       : begin
                          // Ret.AsString := lua_tolstring(Handle, StackIndex, 0);
                          // во избежание LStrClr и HandleFinally
                          Ret.FLuaType := ltString;
                          lua_to_pascalstring(Ret.str_data, Handle, StackIndex);
                        end;
    LUA_TLIGHTUSERDATA: begin
                          // Ret.AsPointer := lua_touserdata(Handle, StackIndex);
                          Ret.FLuaType := ltPointer;
                          pointer(Ret.Data[0]) := lua_touserdata(Handle, StackIndex);
                        end;
    LUA_TFUNCTION     : {указатель на функцию}
                        begin
                          // Ret.AsPointer := CFunctionPtr(lua_tocfunction(Handle, StackIndex));
                          Ret.FLuaType := ltPointer;
                          pointer(Ret.Data[0]) := CFunctionPtr(lua_tocfunction(Handle, StackIndex));

                          // может быть что-то ещё ?
                          if (dword(Ret.Data[0]) >= $FE000000) then
                          begin
                            Ret.FLuaType := ltInteger;
                            Ret.Data[0] := Ret.Data[0] and $00FFFFFF;
                          end;
                        end;

    LUA_TUSERDATA     : begin
                          // объект класса или структура или массив или ...
                          userdata := lua_touserdata(Handle, StackIndex);
                          Result := (userdata <> nil);

                          if (Result) then
                          with userdata^ do
                          case kind of
                             ukInstance: with ClassesInfo[ClassIndex] do
                                         if (_ClassKind = ckClass) then
                                         begin
                                           if (instance <> nil) then
                                           begin
                                             // Ret.AsObject := TObject(instance);
                                             Ret.FLuaType := ltObject;
                                             pointer(Ret.Data[0]) := instance;
                                           end;
                                         end else
                                         begin
                                           // Ret.AsRecord := LuaRecord(instance, PLuaRecordInfo(_Class), not gc_destroy, is_const);
                                           Ret.FLuaType := ltRecord;
                                           with PLuaRecord(@Ret.FLuaType)^ do
                                           begin
                                             Data := instance;
                                             Info := PLuaRecordInfo(_Class);
                                             FIsRef := not gc_destroy;
                                             FIsConst := is_const;
                                           end;
                                         end;

                                ukArray: begin
                                           // ckArray
                                           if (array_params and $f = 0) then
                                           begin
                                             // Ret.AsArray := LuaArray(instance, ArrayInfo, not gc_destroy, is_const)
                                             Ret.FLuaType := ltArray;
                                             with PLuaArray(@Ret.FLuaType)^ do
                                             begin
                                               Data := instance;
                                               Info := ArrayInfo;
                                               FIsRef := not gc_destroy;
                                               FIsConst := is_const;
                                             end;
                                           end else
                                           begin
                                             // Ret.AsPointer := instance;
                                             Ret.FLuaType := ltPointer;
                                             pointer(Ret.Data[0]) := instance;
                                           end;
                                         end;

                                  ukSet: begin
                                           // Ret.AsSet := LuaSet(instance, SetInfo, not gc_destroy, is_const);
                                           Ret.FLuaType := ltSet;
                                           with PLuaSet(@Ret.FLuaType)^ do
                                           begin
                                             Data := instance;
                                             Info := SetInfo;
                                             FIsRef := not gc_destroy;
                                             FIsConst := is_const;
                                           end;
                                         end;

                            ukProperty: Result := false; // Ret.Empty уже = true
                          end;

                          if (not Result) then
                          GetUserDataType(FBufferArg.str_data, Self, userdata);
                        end;
    LUA_TTABLE        : begin
                          // TClass, Info или Таблица
                          ClassIndex := LuaTableToClass(Handle, StackIndex);

                          if (ClassIndex >= 0) then
                          begin
                            with ClassesInfo[ClassIndex] do
                            if (_ClassKind = ckClass) then
                            begin
                              // Ret.AsClass := TClass(_Class);
                              Ret.FLuaType := ltClass;
                              pointer(Ret.Data[0]) := _Class;
                            end else
                            begin
                              // информация по структуре, массиву или множеству
                              // Ret.AsPointer := _Class;
                              Ret.FLuaType := ltPointer;
                              pointer(Ret.Data[0]) := _Class;
                            end;
                          end else
                          if (lua_table_available) then
                          begin
                            // луа-таблица
                            Ret.FLuaType := ltTable;
                            LuaTable := PLuaTable(@Ret.FLuaType);
                            pinteger(@LuaTable.align)^ := 0; // просто очистить align

                            LuaTable.Index_ := StackIndex;
                            LuaTable.Lua := Self;
                          end else
                          begin
                            Result := false;
                            FBufferArg.str_data := LuaTypeName(luatype{LUA_TTABLE});
                          end;
                        end;
    else
      // ошибочная ситуация
      FBufferArg.str_data := LuaTypeName(luatype);
      Result := false;
  end;
end; *)

procedure TLua.global_alloc_ref(var ref: integer);
var
  EmptyRef: __PLuaEmptyRef;
begin
  EmptyRef := FStorage.RefQueue.Items;
  if (EmptyRef <> nil) then
  begin
    ref := EmptyRef.ref;
    FStorage.RefQueue.Items := EmptyRef.next;
    FStorage.RefQueue.Pool.release(EmptyRef);
    exit;
  end;

  inc(FRef);
  ref := FRef;
end;

procedure TLua.global_free_ref(var ref: integer);
var
  EmptyRef: __PLuaEmptyRef;
begin
  if (ref <= 0) then exit;

  // зануяем место в таблице
  lua_pushnil(Handle);
  lua_rawseti(Handle, LUA_GLOBALSINDEX, ref);

  // заносим ref в список свободных
  if (FRef = ref) then dec(FRef)
  else
  begin
    EmptyRef := FStorage.RefQueue.Pool.alloc();
    EmptyRef.ref := ref;
    EmptyRef.next := FStorage.RefQueue.Items;
  end;

  // очищаем указатель
  ref := 0;
end;

procedure TLua.global_fill_value(const ref: integer);
{begin
  if (ref <= 0) then stack_pop()
  else lua_rawseti(Handle, LUA_GLOBALSINDEX, ref);
end;}
asm
  mov ecx, [eax + TLua.FHandle]
  test edx, edx
  jg @rawset
    mov edx, 1
    jmp TLua.stack_pop
@rawset:
  push edx
  push LUA_GLOBALSINDEX
  push ecx
  call [lua_rawseti]
  add esp, 12
end;

procedure TLua.global_push_value(const ref: integer);
{begin
  if (ref <= 0) then lua_pushnil(Handle)
  else lua_rawgeti(Handle, LUA_GLOBALSINDEX, ref);
end;}
asm
  mov ecx, [eax + TLua.FHandle]
  test edx, edx
  jg @rawget
    push ecx
    call [lua_pushnil]
    pop eax
    ret
@rawget:
  push edx
  push LUA_GLOBALSINDEX
  push ecx
  call [lua_rawgeti]
  add esp, 12
end; 

// Index - позиция переменной в глобальном списке GlobalVariables если результат = true
// если false, то Index = place в массиве NameSpaceHash
// если выставлен флаг auto_create, то переменная создаётся, но результат всёравно False
(*function  TLua.GlobalVariablePos(const Name: pchar; const NameLength: integer; var Index: integer; const auto_create: boolean): boolean;
var
  NameHash, Len, Ret: integer;
begin
  NameHash := StringHash(Name, NameLength);
  Len := Length(NameSpaceHash);
  Ret := InsortedPlace8(NameHash, pointer(NameSpaceHash), Len);

  // найти
  while (Ret < Len) and (NameSpaceHash[Ret].Hash = NameHash) do
  begin
    Index := NameSpaceHash[Ret].Index;
    if (SameStrings(GlobalVariables[Index]._Name, Name, NameLength)) then
    begin
      Result := true;
      exit;
    end;

    inc(Ret);
  end;

  // не найден
  Index := Ret;
  Result := false;

  // если необходимо создать
  if (auto_create) then
  begin
    Len := Length(GlobalVariables);
    SetLength(GlobalVariables, Len+1);

    // инициализация переменной
    with GlobalVariables[Len] do
    begin
      _Name := Name;
      _Kind := low(__TLuaGlobalKind);
      IsConst := false;
      Ref := 0; 
    end;

    // добавление в Hash-список
    with TLuaHashIndex(DynArrayInsert(GlobalNative.NameSpace, typeinfo(TLuaHashIndexDynArray), Ret)^) do
    begin
      Hash := NameHash;
      Index := Len;
    end;

    // результат
    Index := Len;
  end;
end; *)






(*function TLua.internal_class_index_by_name(const AName: string): integer;
{begin
  for Result := 0 to Length(ClassesInfo)-1 do
  if (SameStrings(AName, ClassesInfo[Result]._ClassName)) then exit;

  Result := -1;
end;}
var
  Len, Index: integer;
  NameHash: integer;
  HashInfo: ^TLuaHashIndex;
begin
  NameHash := StringHash(AName);
  Len := Length(ClassesIndexesByName);
  Index := InsortedPlace8(NameHash, pointer(ClassesIndexesByName), Len);

  HashInfo := pointer(integer(ClassesIndexesByName) + Index*sizeof(TLuaHashIndex));
  while (Index < Len) and (HashInfo.Hash = NameHash) do
  begin
    if (SameStrings(AName, ClassesInfo[HashInfo.Index]._ClassName)) then
    begin
      Result := HashInfo.Index;
      exit;
    end;

    inc(Index);
    inc(HashInfo);
  end;

  Result := -1;
end;  *)

function __TLuaFindLuaType(const Self: TLua; const Name: LuaString; const Kind: integer): __PLuaType;
var
  identifier: __luaname;
begin
  Self.FStorage.Names.FAutoAdd := false;
  identifier := Self.FStorage.Names.Identifier(Name);
  Self.FStorage.Names.FAutoAdd := true;

  if (identifier = nil) then Result := nil
  else
  begin
    Result := Self.FStorage.RegisteredTypes.find_ptr(identifier);
    if (Result <> nil) and (Result.kind <> Kind) then Result := nil;
  end;
end;

function TLua.GetRecordInfo(const Name: LuaString): PLuaRecordInfo;
asm
  mov ecx, LUATYPE_RECORD
  jmp __TLuaFindLuaType
end;

function TLua.GetArrayInfo(const Name: LuaString): PLuaArrayInfo;
asm
  mov ecx, LUATYPE_ARRAY
  jmp __TLuaFindLuaType
end;

function TLua.GetSetInfo(const Name: LuaString): PLuaSetInfo;
asm
  mov ecx, LUATYPE_SET
  jmp __TLuaFindLuaType
end;

(*procedure __TLuaGetVariable(const Self: TLua; const Name: string; var Result: Variant; const ReturnAddr: pointer);
var
  modify_info: TLuaGlobalModifyInfo;
begin
  modify_info.Name := Name;
  modify_info.CodeAddr := ReturnAddr;
  modify_info.IsVariant := true;
  modify_info.V := @Result;

  Self.__global_index(true, modify_info);
end; *)

(*function TLua.GetVariable(const Name: string): Variant;
asm
  push [esp]
  jmp __TLuaGetVariable
end;*)


(*procedure __TLuaSetVariable(const Self: TLua; const Name: string; const Value: Variant; const ReturnAddr: pointer);
var
  modify_info: TLuaGlobalModifyInfo;
begin
  modify_info.Name := Name;
  modify_info.CodeAddr := ReturnAddr;
  modify_info.IsVariant := true;
  modify_info.V := @Value;

  Self.__global_newindex(true, modify_info);
end;*)

(*procedure TLua.SetVariable(const Name: string; const Value: Variant);
asm
  push [esp]
  jmp __TLuaSetVariable
end;*)

(*procedure __TLuaGetVariableEx(const Self: TLua; const Name: string; var Result: TLuaArg; const ReturnAddr: pointer);
var
  modify_info: TLuaGlobalModifyInfo;
begin
  modify_info.Name := Name;
  modify_info.CodeAddr := ReturnAddr;
  modify_info.IsVariant := false;
  modify_info.Arg := @Result;

  Self.__global_index(true, modify_info);
end;

function  TLua.GetVariableEx(const Name: string): TLuaArg;
asm
  push [esp]
  jmp __TLuaGetVariableEx
end;  *)

(*procedure __TLuaSetVariableEx(const Self: TLua; const Name: string; const Value: TLuaArg; const ReturnAddr: pointer);
var
  modify_info: TLuaGlobalModifyInfo;
begin
  modify_info.Name := Name;
  modify_info.CodeAddr := ReturnAddr;
  modify_info.IsVariant := false;
  modify_info.Arg := @Value;

  Self.__global_newindex(true, modify_info);
end;

procedure TLua.SetVariableEx(const Name: string; const Value: TLuaArg);
asm
  push [esp]
  jmp __TLuaSetVariableEx
end; *)


// зарегистрировать глобальную переменную
// при необходимости создать/удалить Ref и Index
// урегулировать конфликты или вызвать exception
// Kind - Type (Class или Record), Variable, Proc или Enum
// инициализация gkLuaData происходит в global_newindex если переменная не найдена
(*function TLua.internal_register_global(const Name: string; const Kind: __TLuaGlobalKind; const CodeAddr: pointer): PLuaGlobalVariable;
const
  KIND_NAMES: array[__TLuaGlobalKind] of string = ('type', 'variable', 'method', 'enum', '');
var
  Ind: integer;
  new: boolean;
begin
  // проверка на корректность имени
  if (not IsValidIdent(Name)) then
  ELua.Assert('Non-supported %s name "%s"', [KIND_NAMES[Kind], Name], CodeAddr);

  // создать или найти имеющуюся
  new := (not GlobalVariablePos(pchar(Name), Length(Name), Ind, true));
  Result := @GlobalVariables[Ind];

  // урегулировать конфликты
  if (not new) then
  begin
    // если Kind-ы равны, то конфликтов нет, просто могут поменять параметры
    // если не равны, то 100% конфликт
    // если прошлое значение не LuaData, то 100% exception
    if (Result._Kind = Kind) then
    begin
      exit;
    end;

    // если прошлое значение хранится в GLOBAL_INDEX,
    // то либо установить его в nil, либо разрегистрировать Ref
    if (Result._Kind = gkLuaData) then
    begin
      if (Kind in GLOBAL_INDEX_KINDS) then
      begin
        // очистить lua-переменную, но Ref не удалять
        lua_pushnil(Handle);
        global_fill_value(Result.Ref);
      end else
      begin
        // под данным именем будет зарегистрировано другое нативное значение
        // глобальная процедура или глобальная перменная
        global_free_ref(Result.Ref);
      end;

    end else
    begin
      ELua.Assert('Global %s "%s" is already registered', [KIND_NAMES[Result._Kind], Name], CodeAddr);
    end;
  end;


// проинициализировать переменную.
// приходит либо в new случае, либо после того как урегулирован конфликт
// и Ref уже заполнен. Хотя может быть и нулевой в случае new
  Result._Kind := Kind;
  Result.IsConst := (Kind in CONST_GLOBAL_KINDS);

  // Ref или Index
  if (Kind in NATIVE_GLOBAL_KINDS) then
  begin
    Result.Index := GlobalNative.InternalAddName(Name, (Kind = gkProc), {FInitialized, }CodeAddr);
  end else
  begin
    global_alloc_ref(Result.Ref);
  end;

  // меняем флаг инициализации глобального пространства имён
//  if (Kind <> gkConst) then FInitialized := false;
end; *)

// создать и проинициализировать метатаблицу
(*function  TLua.internal_register_metatable(const CodeAddr: pointer; const GlobalName: string=''; const ClassIndex: integer = -1; const is_global_space: boolean = false): integer;
begin
  // получить Ref
  // если нужно = зарегистрировать среди глобальных списков классов
  if (GlobalName <> '') then Result := internal_register_global(GlobalName, gkType, CodeAddr).Ref
  else
  begin
    Result := 0;
    global_alloc_ref(Result);
  end;

  // создать метатаблицу, заполнить ClassIndex
  lua_createtable(Handle, 0, 0);
  if (ClassIndex <> -1) then
  begin
      lua_pushinteger(Handle, integer(typeinfoTClass) or ClassIndex);
      lua_rawseti(Handle, -2, 0);
  end;
  global_fill_value(Result);

  if (is_global_space) then
  begin
    global_push_value(Result);
    lua_setmetatable(Handle, LUA_GLOBALSINDEX);
  end else
  begin
    global_push_value(Result);
    lua_pushvalue(Handle, 1);
    lua_setmetatable(Handle, -2);
    stack_pop();
  end;
end; *)

(*function  TLua.internal_add_class_info(const is_global_space: boolean = false): integer;
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
end; *)

(*function  TLua.internal_add_class_index(const AClass: pointer; const AIndex: integer): integer;
begin
  Result := InsortedPlace8(integer(AClass), pointer(ClassesIndexes), Length(ClassesIndexes));
  with TLuaClassIndex(DynArrayInsert(ClassesIndexes, typeinfo(TLuaClassIndexDynArray), Result)^) do
  begin
    _Class := AClass;
    Index := AIndex;
  end;
end; *)

(*function  TLua.internal_add_class_index_by_name(const AName: string; const AIndex: integer): integer;
var
  AHash: integer;
begin
  AHash := StringHash(AName);

  Result := InsortedPlace8(integer(AHash), pointer(ClassesIndexesByName), Length(ClassesIndexesByName));
  with TLuaClassIndex(DynArrayInsert(ClassesIndexesByName, typeinfo(TLuaClassIndexDynArray), Result)^) do
  begin
    _Class := pointer(AHash);
    Index := AIndex;
  end;
end; *)

// быстро найти индекс класса в массиве ClassesInfo
//function TLua.internal_class_index(AClass: pointer; const look_class_parents: boolean): integer;
(*begin
  Result := -1;

  while (AClass <> nil) do
  begin
    Result := InsortedPos8(integer(AClass), ClassesIndexes);
    if (Result >= 0) or (not look_class_parents) then break;

    // look_parents-вариант: TClass(AClass) := TClass(AClass).ClassParent;
    AClass := ppointer(integer(AClass) + vmtParent)^;
    {$ifndef fpc}if (AClass <> nil) then AClass := TClass(AClass^);{$endif}
  end;

  // результат
  if (Result >= 0) then Result := ClassesIndexes[Result].Index;
end;*)
(*asm
  test edx, edx
  jz   @fail
  mov  eax, [eax + TLua.ClassesIndexes]
  test eax, eax
  jnz @1
@fail:
  mov eax, -1
  ret
@1:
  push edi // look_class_parents
  push ebx // хранилище AClass
  // edx - указатель на ClassesIndexes, не изменяется при вызове InsortedPlace
  // ecx - Length(ClassesIndexes), не изменяется при вызове InsortedPlace

  mov ebx, edx
  mov edi, ecx
  mov edx, eax
  mov ecx, [eax-4]
  {$ifdef fpc} inc ecx {$endif}

@loop:
  mov eax, ebx
  call InsortedPlace8
  cmp eax, ecx  // if (Result >= ArrLength)
  jge @next

  // if (pinteger( integer(Arr)+Result*8 )^ <> Value)
  cmp ebx, [edx + eax*8]
  jne @next

  // return ClassesIndexes[Result].Index;
  mov eax, [edx + eax*8 + 4]
  jmp @exit

@next:
  test edi, edi
  jz @exit_fail // если не look_class_parents
  
  { TClass(AClass) := TClass(AClass).ClassParent; }
  mov ebx, [ebx + vmtParent]
  {$ifndef fpc}
    test ebx, ebx
    jz @exit_fail
    mov ebx, [ebx]
  {$endif}
  test ebx, ebx
  jnz @loop

@exit_fail:
  mov eax, -1
@exit:
  pop ebx
  pop edi
end; *)


(*function TLua.InternalAddProc(const IsClass: boolean; AClass: pointer; const ProcName: string; ArgsCount: integer; const with_class: boolean; Address: pointer): integer;
var
  Index: integer;
  IsConstructor: boolean;
  ClassInfo: ^TLuaClassInfo;
  ProcInfo: ^TLuaProcInfo;

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

//    FInitialized := false;
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
//    FInitialized := false;
  end;


begin
  // проверки
  if (Address = nil) then
  Self.Assert('ProcAddress = NIL');

  if (ArgsCount < -1) or (ArgsCount > 20) then
  Self.Assert('Non-available ArgsCount value (%d)', [ArgsCount]);

  if (not IsValidIdent(ProcName)) then
  Self.Assert('Non-supported ProcName ("%s")', [ProcName]);

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
    Self.Assert('Contructor can''t be a class method');

    if IsClass then Index := InternalAddClass(AClass, False)
    else Index := internal_class_index(AClass);

    FillConstructor(Index, Address, ArgsCount);
    Result := -1;
    exit;
  end;

  // метод Assign()
  if (AClass <> nil) and (SameStrings(LUA_ASSIGN, ProcName)) then
  begin
    if (with_class) then
    Self.Assert('Assign() method can''t be a class method');

    if IsClass then Index := InternalAddClass(AClass, False)
    else Index := internal_class_index(AClass);

    if (ArgsCount <> -1) and (ArgsCount <> 1) then
    Self.Assert('Assign() method should have just 1 argument');

    FillAssignProc(Index, Address);
    Result := -1;
    exit;                          
  end;


  // зарегистрировать класс, глобальную переменную (если нужно) и получить указатель на ProcInfo
  if (AClass = nil) then
  begin
    Result := internal_register_global(ProcName, gkProc, nil{CodeAddr}).Index;
    ProcInfo := @GlobalNative.Procs[Result];
  end else
  begin
    if IsClass then Index := InternalAddClass(AClass, False{, CodeAddr})
    else Index := internal_class_index(AClass);

    ClassInfo := @ClassesInfo[Index];
    Result := ClassInfo.InternalAddName(ProcName, true, {FInitialized, }nil{CodeAddr});
    ProcInfo := @ClassInfo.Procs[Result];
  end;

  // заполнение
  ProcInfo.ArgsCount := ArgsCount;
  ProcInfo.with_class := with_class;
  ProcInfo.Address := Address;
end; *)

// IsConst имеет значение только для глобальных переменных (AClass = GLOBAL_NAME_SPACE)
// tpinfo может быть как обычным typeinfo, так и PLuaRecordInfo
// проперти работают только для классов, структур (поля) и глобальные перменные
(*function  TLua.InternalAddProperty(const IsClass: boolean; AClass: pointer; const PropertyName: string; tpinfo: ptypeinfo; const IsConst, IsDefault: boolean; const PGet, PSet, Parameters: pointer): integer;
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
//    FInitialized := false;
  end;
begin
  // определиться ClassInfo, найти или зарегистрировать при необходимости
  IsGlobal := (AClass = GLOBAL_NAME_SPACE);
  if (IsGlobal) then ClassInfo := @GlobalNative
  else
  begin
    if (IsClass) then Index := InternalAddClass(AClass, False{, CodeAddr})
    else Index := internal_class_index(AClass);

    ClassInfo := @ClassesInfo[Index];
  end;

  // проверки
  if (not IsValidIdent(PropertyName)) then
  Self.Assert('Non-supported %s name "%s"', [ClassInfo.PropertyIdentifier, PropertyName]);

  if (tpinfo = nil) then
  Self.Assert('TypeInfo of %s "%s" is not defined', [ClassInfo.PropertyIdentifier, PropertyName]);

  if (IsDefault) and (Parameters = nil) then
  Self.Assert('Simple property ("%s") can''t be default property', [PropertyName]);

  case integer(Parameters) of
    0, integer(INDEXED_PROPERTY), integer(NAMED_PROPERTY): ;
    else
      if (PLuaRecordInfo(Parameters).FieldsCount = 0) then
      Self.Assert('Property information "%s" has no fields', [PLuaRecordInfo(Parameters).Name]);
  end;  

  // определение базовых параметров
  PropBase := GetLuaPropertyBase(Self, '', PropertyName, tpinfo);

  // найти/добавить/зарегистрировать свойство
  if (not IsGlobal) then
  begin
    Result := ClassInfo.InternalAddName(PropertyName, false, {FInitialized,} nil{CodeAddr});

    if (IsDefault) then
    begin
      //ClassInfo._DefaultProperty := (not Result);
      TSmallPoint(Index).x := ClassInfo._ClassIndex;
      TSmallPoint(Index).y := not Result;
      FillDefaultProperty(ClassInfo._ClassIndex, Index);
    end;  
  end else
  begin
    GlobalVariable := internal_register_global(PropertyName, gkVariable, nil{CodeAddr});
    GlobalVariable.IsConst := IsConst;
    Result := GlobalVariable.Index;
  end;                            
  PropInfo := @ClassInfo.Properties[not Result];

  // заполнить информацию
  PropInfo.Fill(ClassInfo^, PropBase, PGet, PSet, Parameters);
  if (IsConst) then PropInfo.write_mode := MODE_NONE_USE;
end;


const
  PTR_FALSE = pointer(ord(false));
  PTR_TRUE = pointer(ord(true));
(*  OPERATOR_NEG = 0;
  OPERATOR_ADD = 1;
  OPERATOR_SUB = 2;
  OPERATOR_MUL = 3;
  OPERATOR_DIV = 4;
  OPERATOR_MOD = 5;
  OPERATOR_POW = 6;
  OPERATOR_EQUAL = 7;
  OPERATOR_LESS = 8;
  OPERATOR_LESS_EQUAL = 9;
  OPERATOR_CONCAT = 10; {бонусный. только для динамических массивов} *)


// проинициализировать пространства имён и создать калбеки
procedure TLua.INITIALIZE_NAME_SPACE();
begin
end;
(*var
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
    else add_metatable_callback(proc_name, pointer(CreateCFunctionDump(Self, P1, P2, CallbackProc)));
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
      if (ClassInfo._ClassKind = ckClass) then ClassInfo.__Create := pointer(CreateCFunctionDump(Self, @ClassInfo, PTR_TRUE, @TLua.__constructor));
      if (ClassInfo._ClassKind = ckClass) then ClassInfo.__Free := pointer(CreateCFunctionDump(Self, @ClassInfo, PTR_TRUE, @TLua.__destructor));

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
    ClassInfo.Procs[i].lua_CFunction := pointer(CreateCFunctionDump(Self, @ClassInfo, @ClassInfo.Procs[i], @TLua.ProcCallback));
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
  if (FNameSpaceInitalized) then exit;
  // TODO FStorage.Dumps.Clear(); // DeleteCFunctionDumps(Self);

  // создать список всевозможножных методов и свойств
  // включай методы и свойства предков
  // !!! для GlobalNative этого делать не нужно. Глобальные переменные всегда актуальны
  FillNameSpace();


  // создать универсальные калбеки
  begin
    cfunction_tostring := pointer(CreateCFunctionDump(Self, nil, nil, @TLua.__tostring));
    cfunction_inherits_from := pointer(CreateCFunctionDump(Self, nil, nil, @TLua.__inherits_from));
    cfunction_assign := pointer(CreateCFunctionDump(Self, nil, nil, @TLua.__assign));
    cfunction_dynarray_resize := pointer(CreateCFunctionDump(Self, nil, nil, @TLua.__array_dynamic_resize));
    cfunction_array_include := pointer(CreateCFunctionDump(Self, pointer(1), nil, @TLua.__array_include));
    cfunction_set_include  := pointer(CreateCFunctionDump(Self, PTR_FALSE, pointer(0), @TLua.__set_method));
    cfunction_set_exclude  := pointer(CreateCFunctionDump(Self, PTR_FALSE, pointer(1), @TLua.__set_method));
    cfunction_set_contains := pointer(CreateCFunctionDump(Self, PTR_FALSE, pointer(2), @TLua.__set_method));
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

  // флаг "всё проинициализированно"
  FNameSpaceInitalized := true;  
end;  *)


// преобразование userdata или метатаблицы в строку
(*function __arrayindex_description(const Prefix, Value: string; const Index, Dimention: integer): string; forward;
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
end; *)


// является ли объект наследником (Класса, PLuaRecordInfo, PLuaArrayInfo, PLuaSetInfo)
(*function TLua.__inherits_from(): integer;
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
  Self.Assert('Wrong arguments count(%d) in InheritsForm() method', [lua_gettop(Handle)]);

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
end;  *)


// одной переменной присваивается значение другой переменной
// что-то типа копирования
//
// ещё assign() работает в режиме инициализации по таблице
(*function TLua.__assign(): integer;
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

    Self.Assert('Can''t realize %s.Assign(%s). %s', [X1.ForceString, X2.ForceString, Description]);
  end;

begin
  Result := 0;

  if (lua_gettop(Handle) <> 2) then
  Self.Assert('Not found instance or wrong arguments count in an Assign() method', []);

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

      TTClassRecallProc16(assign_addr)(Dest.instance^, FArgs, TLuaArg(nil^));
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
end; *)

// эта процедура отвечает за инициализацю
// свойств (полей) класса или структуры по таблице.
// функция рекурсивная (т.е. можно инициализировать вложенные структуры и экземпляры класса)
//
// функция напрямую не вызывается из Lua, но имеет схожую стилистику
// только чтобы попадать в общий вид подобных функций.
// с другой стороны функция 100% вызывается не на нативной стороне, поэтому в случае ошибки - Assert()
//
// stack_index здесь всегда в прямой адресации!
(*function TLua.__initialize_by_table(const userdata: PLuaUserData; const stack_index: integer): integer;
const
  FIELD_PROPERTY: array[boolean] of string = ('Field', 'Property');

var
  S: pansichar;
  SLength: integer;
  ClassInfo: PLuaClassInfo;
  is_class: boolean;
  ProcInfo: ^TLuaProcInfo;
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

    Self.Assert('Can''t change %s.%s to "%s". %s.', [ClassInfo._ClassName,
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
  Self.Assert('"%s" instance is constant', [ClassInfo._ClassName]);

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
end; *)


// вызов метода + корректное восстановление стека
(*procedure TMethod_Call(const ASelf, P1, P2: integer; const Code: pointer);
asm
  push esi
  mov esi, esp
    CALL [EBP+8]
  mov esp, esi
  pop esi
end; *)

// простой вызов события с 2мя возможными простыми типами
(*function TLua.__tmethod_call(const Method: TMethod): integer;
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
    Self.Assert('%d parameter of TMethod() call has unsupported type: "%s"' , [i+1, ParamType]);
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
  Self.Assert('Method is not assigned (Code = %p, Data = %p)', [Method.Code, Method.Data]);

  // определиться с Offset
  // Stack[1] 100% = Method. А вот со вторым параметром могут быть проблемы
  LuaTypeName(lua_type(Handle, 2));
  userdata := lua_touserdata(Handle, 2);
  Offset := ord((userdata <> nil) or (userdata.kind <> ukInstance));

  // количество аргументов
  ArgsCount := lua_gettop(Handle)-1-Offset;
  if (ArgsCount < 0) or (ArgsCount > 2) then
  Self.Assert('Wrong TMethod() arguments count = %d. Max arguments count = 2', [ArgsCount]);

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
end; *)


const
  // для всех userdata (кроме сложных свойств)
  STD_TYPE = 1;
  STD_TYPE_NAME = 2;
  STD_TYPE_PARENT = 3;
  STD_INHERITS_FROM = 4;
  STD_ASSIGN = 5;
  STD_IS_REF = 6;
  STD_IS_CONST = 7;
  STD_IS_CLASS = 8;
  STD_IS_RECORD = 9;
  STD_IS_ARRAY = 10;
  STD_IS_SET = 11;
  STD_IS_EMPTY = 12;

  // для классов
  STD_CREATE = 13;
  STD_FREE = 14;

  // массивы
  STD_LOW = 15;
  STD_HIGH = 16;
  STD_LENGTH = 17;
  STD_RESIZE = 18;

  // множества
  STD_INCLUDE = 19;
  STD_EXCLUDE = 20;
  STD_CONTAINS = 21;

  // TLuaReference
  STD_VALUE = 22;


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
end; *)


// проверка на допустимость вызова дефолтного свойства
(*function __can_jump_default_property(const Lua: TLua; const luatype: integer; const PropInfo: TLuaPropertyInfo): boolean;
begin
  Result := (luatype <> LUA_TSTRING);

  // проверка, если аргумент - строка
  if (not Result) then
  with PropInfo, PLuaRecordInfo(Parameters)^ do
  if (Parameters <> INDEXED_PROPERTY) then                                       {todo потом посмотреть более детально}
  Result := (Parameters = NAMED_PROPERTY) or (Lua.ClassesInfo[FClassIndex].Properties[0].Base.Kind = pkString);
end; *)

// функция взятия стандартного свойства или метода.
// функция вызывается редко, поэтому вынесена в отдельный код
// true если "попал"
(*function __push_std_prop(const Lua: TLua; const _ClassInfo: TLuaClassInfo; const stdindex: integer; const userdata: PLuaUserData; const S: pansichar): boolean;

  // невозможно вызвать метод, потому что инстенст - константа
  procedure ThrowConst();
  begin
    Lua.Assert('%s() method can''t be called, because %s instance is const', [S, _ClassInfo._ClassName]);
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
                         lua_rawgeti(Handle, LUA_GLOBALSINDEX, TLuaReference(userdata.instance).Index);
                         Result := true;
                       end;
                     end;
    end;
end; *)


type
  // универсальный тип для хранилища данных при get/set свойств
  TPackedValue = packed record
    case Integer of
      0: (b: boolean);
      1: (i: integer);
      2: (i64: int64);
      3: (e: extended);
      4: (v: TVarData);
      5: (lb: LongBool);
      6: (p: pointer);
  end;


// на данный момент это рекалбекер для __lua_get_value
//
// в будущем его не будет. сейчас мы тестируем возможность работы библиотеки
// через этот (внутренний) геттер
(*function __lua_get_value__recaller(Self: TLua; instance: pointer; userdata: PLuaUserData; const PropertyInfo: PLuaPropertyInfo): integer;
asm
@prefix:
  push ebx
  mov ebx, PropertyInfo
  push esi
  push edi
  push ebp
@begin:
  // Handle, <index>, <buf> | @done_finish, userdata/nil, ebp:[Count], <type1>, <type2>, <type3>, ...
  push dword ptr 0
  mov ebp, esp
  push ecx
  push dword ptr offset @end
  sub esp, 8
  push [EAX].TLua.FHandle
  mov ESI, eax
  mov eax, edx

  xor edi, edi
  cmp [EBX].TLuaPropertyInfo.read_mode, 0
  jge __lua_get_value

  mov ecx, [EBX].TLuaPropertyInfo.PropInfo
  mov edx, [ECX].TPropInfo.Index
  mov ecx, [ECX].TPropInfo.GetProc
  mov edi, ecx
  shr ecx, 24
  cmp ecx, $fe
  jne __lua_get_value

  mov ecx, [eax] // Class
  and edi, $00ffffff
  add edi, ecx
  jmp __lua_get_value
@end:
  lea esp, [ebp + 4]
@postfix:
  pop ebp
  pop edi
  pop esi
  pop ebx
end;  *)

// самый распространённый метод
// когда у класса/структуры нужно найти свойство или метод. В этом случае prop_struct = nil
//
// но так как концепция свойств основополагающая в CrystalLUA, то метод по сути является
// базовым для чтения данных так же из массивов, глобального пространства, при создании референсов.
// в этом случае prop_struct заполнен (<> nil)
(*function TLua.__index_prop_push(const ClassInfo: TLuaClassInfo; const prop_struct: PLuaPropertyStruct): integer;
label
  PROP_PUSHING;
type
  TGetStrProp = procedure(Instance: TObject; PropInfo: PPropInfo; var Ret: string);
  TGetVariantProp = procedure(Instance: TObject; PropInfo: PPropInfo; var Ret: Variant);
  TGetInterfaceProp = procedure(Instance: TObject; PropInfo: PPropInfo; var Ret: IInterface);

  TGetUniversal = procedure(const instance: pointer; var Result: TLuaArg);
  TSetIndexedUniversal = procedure(const instance: pointer; const index: integer; var Result: TLuaArg);

var
  S: pansichar;
  luatype, SLength, stdindex: integer;
  // todo чё-то с этим сделать

  jump_to_default: boolean;
  userdata: PLuaUserData;
  ProcInfo: ^TLuaProcInfo;
  PropertyInfo: ^TLuaPropertyInfo;

  is_const: boolean absolute jump_to_default;
  instance: pointer;// absolute userdata;
//  Obj: TObject absolute instance;
 // Value: TPackedValue;

  fake_userdata: TLuaUserData;

  // ошибка: ничего не нaйдено
  procedure ThrowFoundNothing();
  const
    instance_type: array[boolean] of string = ('instance', 'type');
  begin
    TStackArgument(@TLua.StackArgument)(Self, 2, FBufferArg.str_data);
    Self.Assert('"%s" not found in %s %s', [FBufferArg.str_data, ClassInfo._ClassName, instance_type[userdata = nil]]);
  end;

  // если что-то не так в user-data
  procedure ThrowFailInstance();
  begin
    // анализ userdata
    if (userdata = nil) then
    Assert('%s.%s property is not class property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);

    // instance empty ?
    if (userdata.instance = nil) then
    Assert('Instance (%s) is already destroyed', [ClassInfo._ClassName]);

    // writeonly
    if (PropertyInfo.read_mode = MODE_NONE_USE) then
    Assert('%s.%s property is writeonly property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);
  end;

begin
  Result := 1;
  userdata := nil;

  // функция может вызываться не только стандартно - по __index из класса/структуры
  // но так же и из многих других мест.
  // в этом случае prop_struct - не nil
  if (prop_struct <> nil) then
  begin
    PropertyInfo := prop_struct.PropertyInfo;
    instance := prop_struct.Instance;
    is_const := prop_struct.IsConst;
    if (PropertyInfo.Parameters <> nil) then PropertyInfo.PropInfo.Index := integer(prop_struct.Index); // индекс или указатель на структуру
    goto PROP_PUSHING;
  end;


  // стандартный случай
  // сначала читаем userdata и информацию по первому аргументу
  if (not __read_lua_arguments(Handle, userdata, S, luatype, SLength, stdindex)) then
  begin
    lua_pushnil(Handle);
    exit;
  end;

  // для стандартных случаев
  if (stdindex > 0) and (__push_std_prop(Self, ClassInfo, stdindex, userdata, S)) then exit;

  // это не "стандартый" метод/свойство
  // нужно найти
  ProcInfo := nil;
  PropertyInfo := nil;
  ClassInfo.NameSpacePlace(Self, S, SLength, pointer(ProcInfo), pointer(PropertyInfo));

  // функция
  if (ProcInfo <> nil) then
  begin
    lua_pushcclosure(Handle, ProcInfo.lua_CFunction, 0);
    exit;
  end;

  // свойство
  jump_to_default := false;
  if (PropertyInfo = nil) {возможно дефолтное} then
  with ClassInfo do
  if (_DefaultProperty >= 0) then
  begin
    PropertyInfo := @ClassesInfo[_DefaultProperty and $FFFF].Properties[_DefaultProperty shr 16];

    if (__can_jump_default_property(Self, luatype, PropertyInfo^)) then jump_to_default := true
    else PropertyInfo := nil;
  end;

  // ошибка: ничего не нaйдено
  if (PropertyInfo = nil) then
  ThrowFoundNothing();

  // если что-то не так в user-data
  if (userdata = nil) or (userdata.instance = nil) or
     ((PropertyInfo.Parameters = nil) and (PropertyInfo.read_mode = MODE_NONE_USE)) then
     ThrowFailInstance();


  // если сложное свойство, то вернуть не значение, а ссылку на свойство
  // или прыгнуть в калбек __array_index
  if (PropertyInfo.Parameters <> nil) then
  begin
    push_difficult_property(userdata.instance, PropertyInfo^);

    if (jump_to_default) then
    begin
      lua_remove(Handle, 1);
      lua_insert(Handle, 1);
      __array_index(TLuaClassInfo(nil^){TODO ?}, true);
    end;
    exit;
  end;

  // доводим параметры
  is_const := userdata.is_const;
  instance := userdata.instance;


PROP_PUSHING:
  if (PropertyInfo.read_mode >= 0) then inc(integer(instance), PropertyInfo.read_mode);

  if (userdata = nil) then
  begin
    userdata := @fake_userdata;
    userdata.is_const := is_const;
  end;

  Result := __lua_get_value__recaller(Self, instance, userdata, PropertyInfo);

  // основной код функции - кладём в стек значение свойства
(*  with PropertyInfo^ do
  begin
    if (read_mode >= 0) then inc(integer(instance), read_mode);

    case Base.Kind of
        pkBoolean: begin
                     if (read_mode >= 0) then
                     begin
                       case Base.BoolType of
                         btBoolean: Value.i := pbyte(instance)^;
                        btByteBool: Value.i := pbyte(instance)^;
                        btWordBool: Value.i := pword(instance)^;
                        btLongBool: Value.i := pinteger(instance)^;
                       end;
                     end
                     else Value.i := TypInfo.GetOrdProp(Obj, PropInfo);

                     if (Value.i <> 0) then Value.i := -1;
                     lua_pushboolean(Handle, Value.lb);
                   end;
        pkInteger: begin
                     if (read_mode >= 0) then
                     begin
                       case Base.OrdType of
                         otSByte: Value.i := pshortint(instance)^;
                         otUByte: Value.i := pbyte(instance)^;
                         otSWord: Value.i := psmallint(instance)^;
                         otUWord: Value.i := pword(instance)^;
                         otSLong,
                         otULong: Value.i := pinteger(instance)^;
                       end;
                     end
                     else Value.i := TypInfo.GetOrdProp(Obj, PropInfo);

                     if (Base.OrdType <> otULong) then lua_pushinteger(Handle, Value.i)
                     else lua_pushnumber(Handle, dword(Value.i));
                   end;
          pkInt64: begin
                     if (read_mode >= 0) then Value.i64 := pint64(instance)^
                     else Value.i64 := TypInfo.GetInt64Prop(Obj, PropInfo);

                     lua_pushnumber(Handle, Value.i64);
                   end;
          pkFloat: begin
                     if (read_mode >= 0) then
                     begin
                       case Base.FloatType of
                         ftSingle: lua_pushnumber(Handle, psingle(instance)^);
                         ftDouble: lua_pushnumber(Handle, pdouble(instance)^);
                       ftExtended: lua_pushnumber(Handle, pextended(instance)^);
                           ftComp: lua_pushnumber(Handle, PComp(instance)^);
                           ftCurr: lua_pushnumber(Handle, PCurrency(instance)^);
                       end;
                     end
                     else lua_pushnumber(Handle, TypInfo.GetFloatProp(Obj, PropInfo));
                   end;
         pkString: with FBufferArg do
                   begin
                     if (str_data <> '') then str_data := '';
                     {todo UnicodeString?,}

                     if (not (Base.StringType in [stAnsiChar, stWideChar])) then
                     begin
                       // строки
                       if (read_mode >= 0) then
                       begin        {todo умное присвоение}
                         case Base.StringType of
                         stShortString: if (pbyte(instance)^ <> 0) then str_data := pshortstring(instance)^;
                          stAnsiString: if (ppointer(instance)^ <> nil) then str_data := pansistring(instance)^;
                          stWideString: if (ppointer(instance)^ <> nil) then str_data := pwidestring(instance)^;
                         end;
                       end
                       else TGetStrProp(TypInfoGetStrProp)(Obj, PropInfo, str_data);
                     end else
                     begin
                       // AnsiChar or WideChar
                       if (read_mode >= 0) then
                       begin
                         if (Base.StringType = stAnsiChar) then Value.i := pbyte(instance)^
                         else Value.i := pword(instance)^;
                       end
                       else Value.i := TypInfo.GetOrdProp(Obj, PropInfo);

                       if (Value.i <> 0) then
                       begin
                         if (Base.StringType = stAnsiChar) then str_data := ansichar(Value.i)
                         else System.WideCharLenToStrVar(PWideChar(@Value.i), 1, str_data);
                       end;
                     end;

                     lua_push_pascalstring(Handle, str_data);
                   end;

        pkVariant: begin
                     // "получить" значение
                     Value.v.VType := varEmpty;
                     if (not(read_mode >= 0)) then
                     begin
                       TGetVariantProp(TypInfoGetVariantProp)(Obj, PropInfo, PVariant(@Value.v)^);
                       instance := @Value.v;
                     end;

                     // push: Variant или nil
                     if (not push_variant(PVariant(instance)^)) then lua_pushnil(Handle);

                     // очистка если был занят
                     if (Value.v.VType = varString) or (not (Value.v.VType in VARIANT_SIMPLE)) then VarClear(PVariant(instance)^);
                   end;

      pkInterface,
        pkPointer,
          pkClass,
         pkObject: begin
                     if (read_mode >= 0) then
                     begin
                       Value.p := ppointer(instance)^;
                     end else
                     if (Base.Kind = pkInterface) then
                     begin
                       Value.p := nil;
                       TGetInterfaceProp(TypInfoGetInterfaceProp)(Obj, PropInfo, IInterface(Value.p));
                       if (Value.p <> nil) then IInterface(Value.p)._Release;
                     end else
                     begin
                       Value.i := TypInfo.GetOrdProp(Obj, PropInfo);
                     end;

                     if (Value.p = nil) then lua_pushnil(Handle)
                     else
                     if (Base.Kind in [pkInterface,pkPointer]) then lua_pushlightuserdata(Handle, Value.p)
                     else
                     if (Base.Kind = pkClass) then lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassesInfo[internal_class_index(Value.p, true)].Ref)
                     else
                     // pkObject
                     if (TClass(Value.p^) = TLuaReference) then lua_rawgeti(Handle, LUA_GLOBALSINDEX, TLuaReference(Value.p).Index)
                     else
                     push_userdata(ClassesInfo[internal_class_index(TClass(Value.p^), true)], false, Value.p);
                   end;
         pkRecord,
          pkArray,
            pkSet: CrystalLUA.GetPushDifficultTypeProp(Self, instance, is_const, PropertyInfo^);

      pkUniversal: CrystalLUA.GetPushUniversalTypeProp(Self, instance, is_const, PropertyInfo^);
      end;
  end;       *)
//end;



// на данный момент это рекалбекер для __lua_set_value
(*function __lua_set_value__recaller(Self: TLua; instance: pointer; userdata: PLuaUserData; PropertyInfo: PLuaPropertyInfo; const luatype, stack_index: integer): integer;
asm
@prefix:
  push ebx
  mov ebx, PropertyInfo
  push esi
  push edi
  push ebp
@begin:
  // Handle, <index>, <buf> | @done_finish, userdata/nil, ebp:[Count], <type1>, <type2>, <type3>, ...
  mov esi, stack_index
  push luatype
  shl esi, 2
  sub esp, esi
  shr esi, 2
  mov ebp, esp
  mov [esp], esi
  push ecx
  push dword ptr offset @end
  sub esp, 8
  push [EAX].TLua.FHandle
  mov ESI, eax
  mov eax, edx

  xor edi, edi
  cmp [EBX].TLuaPropertyInfo.write_mode, 0
  jge __lua_set_value

  mov ecx, [EBX].TLuaPropertyInfo.PropInfo
  mov edx, [ECX].TPropInfo.Index
  mov ecx, [ECX].TPropInfo.SetProc
  mov edi, ecx
  shr ecx, 24
  cmp ecx, $fe
  jne __lua_set_value

  mov ecx, [eax] // Class
  and edi, $00ffffff
  add edi, ecx
  jmp __lua_set_value
@end:
  mov edx, [ebp]
  lea esp, [ebp + edx*4 + 4]
@postfix:
  pop ebp
  pop edi
  pop esi
  pop ebx
end; *)


// изменить свойство
// изменить функцию нельзя !
(*function TLua.__newindex_prop_set(const ClassInfo: TLuaClassInfo; const prop_struct: PLuaPropertyStruct): integer;
label
  PROP_POPSET;
type
  PInterface = ^IInterface;  
const
  instance_type: array[boolean] of string = ('instance', 'type');  
var
  S: pansichar;
  luatype, SLength, stdindex: integer;
  // todo чё-то с этим сделать

  jump_to_default: boolean;
  userdata: PLuaUserData;
//  fake_userdata: TLuaUserData;

  ProcInfo: ^TLuaProcInfo;
  PropertyInfo: ^TLuaPropertyInfo;

  stack_index: integer; // откуда брать параметр (для присвоения)
  instance: pointer;// absolute userdata;
  Obj: TObject absolute instance;
  PV: PVarData absolute instance;
  ClassIndex: integer absolute SLength;
  //Value: TPackedValue;

  // ошибка: ничего не нaйдено
  procedure ThrowFoundNothing();
  begin
    TStackArgument(@TLua.StackArgument)(Self, 2, FBufferArg.str_data);
    Assert('"%s" not found in %s %s', [FBufferArg.str_data, ClassInfo._ClassName, instance_type[userdata = nil]]);
  end;

  // если что-то не так в user-data
  procedure ThrowFailInstance();
  begin
    // анализ userdata
    if (userdata = nil) then
    Assert('%s.%s property is not class property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);

    // instance empty ?
    if (userdata.instance = nil) then
    Assert('Instance (%s) is already destroyed', [ClassInfo._ClassName]);

    // неизменяемая структура
    if (UserData.is_const) then
    Assert('Field "%s" is a constant field', [PropertyInfo.PropertyName]);

    // readonly
    if (PropertyInfo.write_mode = MODE_NONE_USE) then
    Assert('%s.%s property is readonly property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);
  end;

  // если не получается присвоить ("тип" в FBufferArg.str_data)
 (* procedure ThrowAssignValue(const look_luatype: boolean=false);
  var
    ReturnAddr: pointer;
    Desc: string;
  begin
    if (look_luatype) then FBufferArg.str_data := LuaTypeName(luatype);

    Desc := '';
    if (prop_struct = nil) then
    begin
      Desc := Format('%s.%s property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);
    end else
    if (@ClassInfo <> nil) then
    begin
      if (ClassInfo._Class = GLOBAL_NAME_SPACE) then Desc := 'global variable ' + PropertyInfo.PropertyName
      else Desc := ClassInfo._ClassName + '.' + PropertyInfo.PropertyName + ' property';
    end;

    // total description of error
    if (Desc <> '') then Desc := Desc + ' ';
    Desc := Format('Can''t assign "%s" as %svalue', [FBufferArg.str_data, Desc]);

    // native or from script
    ReturnAddr := nil;
    if (prop_struct <> nil) then ReturnAddr := prop_struct.ReturnAddr;

    // call
    if (ReturnAddr <> nil) then ELua.Assert(Desc, ReturnAddr)
    else Assert(Desc, []);
  end; *)

  // преобразовать значение в стеке в число
  // или вызывать ошибку
 (* function CastValueAsNumber(): extended;
  begin
    Result := 0;

    case luatype of
         LUA_TNIL: {уже 0};
     LUA_TBOOLEAN: if (lua_toboolean(Handle, stack_index)) then Result := 1;
      LUA_TNUMBER: Result := lua_tonumber(Handle, stack_index);
      LUA_TLIGHTUSERDATA: Result := integer(lua_touserdata(Handle, stack_index));
      LUA_TUSERDATA: begin
                      Value.p := lua_touserdata(Handle, stack_index);
                      if (Value.p <> nil) then Result := integer(PLuaUserData(Value.p).instance);
                     end;
      LUA_TSTRING: begin
                     lua_to_pascalstring(FBufferArg.str_data, Handle, stack_index);

                     System.Val(FBufferArg.str_data, Result, luatype);
                     if (luatype <> 0) then ThrowAssignValue({false});
                   end;
    else
      ThrowAssignValue(true);
    end;
  end; *)

  // проверить datauser, находящийся в Value.p на экземпляр класса
  // если всё нет - вызывается ошибка
  // если всё хорошо - то в Value.p заносится instance (TObject)
(*  procedure CastUserDataAsTObject();
  begin
    ClassIndex := -1;
    if (Value.p <> nil) and (PLuaUserData(Value.p).instance <> nil)
    and (PLuaUserData(Value.p).kind = ukInstance) then
      ClassIndex := PLuaUserData(Value.p).ClassIndex;

    if (ClassIndex >= 0) and (ClassesInfo[ClassIndex]._ClassKind = ckClass) then
    Value.p := PLuaUserData(Value.p).instance
    else
    ClassIndex := -1;

    if (Value.p = nil) or (ClassIndex < 0) then
    begin
      GetUserDataType(FBufferArg.str_data, Self, PLuaUserData(Value.p));
      ThrowAssignValue();
    end;
  end; *)


  // в случае изменения "стандартного свойства"
  // true если "прокатило"
(*  function change_std_prop(): boolean;
  begin
    Result := false;

    // изменение значения TLuaReference
    if (ClassInfo._ClassIndex = TLUA_REFERENCE_CLASS_INDEX) and (stdindex = STD_VALUE) and
       (userdata <> nil) and (userdata.instance <> nil) then
    begin
      lua_pushvalue(Handle, 3);
      lua_rawseti(Handle, LUA_GLOBALSINDEX, TLuaReference(userdata.instance).Index);
      Result := true;
      exit;
    end;

    // проверка на изменение стандартного поля
    if (stdindex in [STD_TYPE..STD_IS_EMPTY]) or ((ClassInfo._ClassKind=ckClass)=(stdindex in [STD_CREATE, STD_FREE]))
    or ((ClassInfo._ClassKind=ckSet)=(stdindex in [STD_LOW, STD_HIGH, STD_INCLUDE..STD_CONTAINS]))
    then
    Assert('Standard field "%s" can not be changed in %s %s', [S, ClassInfo._ClassName, instance_type[userdata = nil]]);
  end;


begin
  Result := 0;

  // функция может вызываться не только стандартно - по __index из класса/структуры
  // но так же и из многих других мест.
  // в этом случае prop_struct - не nil
  if (prop_struct <> nil) then
  begin
    PropertyInfo := prop_struct.PropertyInfo;
    instance := prop_struct.Instance;
    stack_index := prop_struct.StackIndex;
    if (PropertyInfo.Parameters <> nil) then PropertyInfo.PropInfo.Index := integer(prop_struct.Index); // индекс или указатель на структуру
    goto PROP_POPSET;
  end;

  // прочитать userdata и информацию по первому аргументу
  if (not __read_lua_arguments(Handle, userdata, S, luatype, SLength, stdindex)) then
  begin
    exit;
  end;

  // для стандартных случаев
  if (stdindex > 0) and change_std_prop() then exit;

  // это не "стандартый" метод/свойство
  // нужно найти
  ProcInfo := nil;
  PropertyInfo := nil;
  ClassInfo.NameSpacePlace(Self, S, SLength, pointer(ProcInfo), pointer(PropertyInfo));

  // методы нельзя менять
  if (ProcInfo <> nil) then
  Assert('Method %s.%s can not be changed', [ClassInfo._ClassName, S]);

  // свойство
  jump_to_default := false;
  if (PropertyInfo = nil) {возможно дефолтное} then
  with ClassInfo do          
  if (_DefaultProperty >= 0) then
  begin
    PropertyInfo := @ClassesInfo[_DefaultProperty and $FFFF].Properties[_DefaultProperty shr 16];

    if (__can_jump_default_property(Self, luatype, PropertyInfo^)) then jump_to_default := true
    else PropertyInfo := nil;
  end;


  // ошибка: ничего не нaйдено
  if (PropertyInfo = nil) then
  ThrowFoundNothing();

  // если что-то не так в user-data
  if (userdata = nil) or (userdata.instance = nil) or (userdata.is_const) or
     ((PropertyInfo.Parameters = nil) and (PropertyInfo.read_mode = MODE_NONE_USE)) then
     ThrowFailInstance();


  // если параметризированное свойство
  if (PropertyInfo.Parameters <> nil) then
  begin
    if (jump_to_default) then
    begin
      push_difficult_property(userdata.instance, PropertyInfo^);

      lua_remove(Handle, 1);
      lua_insert(Handle, 1);
      __array_newindex(TLuaClassInfo(nil^){TODO ?}, true);
    end else
    begin
      Assert('%s.%s property should have parameters', [ClassInfo._ClassName, PropertyInfo.PropertyName])
    end;

    exit;
  end;

  // доводим параметры
  instance := userdata.instance;
  stack_index := 3;


PROP_POPSET:
  if (PropertyInfo.write_mode >= 0) then inc(integer(instance), PropertyInfo.write_mode);

  {if (userdata = nil) then
  begin
    userdata := @fake_userdata;
    userdata.is_const := is_const;
  end; }

  if (stack_index < 0) then stack_index := lua_gettop(Handle)+1-stack_index;
  Result := __lua_set_value__recaller(Self, instance, userdata, PropertyInfo, lua_type(Handle, stack_index), stack_index);


  // очень важно, какой тип пытаемся присвоить
  // если не получается - надо вызывать ошибку
(*  luatype := lua_type(Handle, stack_index);

  // основной код функции - берём значение из стека и присваиваем свойство
  with PropertyInfo^ do
  begin
    if (write_mode >= 0) then inc(integer(instance), write_mode);

    case Base.Kind of
      pkBoolean: begin
                   case (luatype) of
                         LUA_TNIL: Value.i := 0;
                     LUA_TBOOLEAN: Value.i := integer(lua_toboolean(Handle, stack_index)) and 1;
                      LUA_TSTRING: begin
                                     lua_to_pascalstring(FBufferArg.str_data, Handle, stack_index);
                                     Value.i := __cast_string_as_boolean(FBufferArg.str_data);
                                     if (Value.i < 0) then ThrowAssignValue({false});
                                   end;
                   else
                     ThrowAssignValue(true);
                   end;

                   // "доведение" типа
                   if (Base.BoolType <> btBoolean) and (Value.i <> 0) then Value.i := -1;

                   // присвоение
                   if (write_mode >= 0) then
                   begin
                     case Base.BoolType of
                       btBoolean: pboolean(instance)^ := Value.lb;
                      btByteBool: pbyte(instance)^ := pbyte(@Value.lb)^;
                      btWordBool: pword(instance)^ := pword(@Value.lb)^;
                      btLongBool: pdword(instance)^ := pdword(@Value.lb)^;
                     end;
                   end
                   else TypInfo.SetOrdProp(Obj, PropInfo, Value.i);
                 end;
      pkInteger: begin
                   if (luatype = LUA_TNUMBER) then Value.i64 := lua_toint64(Handle, stack_index)
                   else Value.i64 := round(CastValueAsNumber());

                   if (write_mode >= 0) then
                   begin
                     case Base.OrdType of
                       otSByte: pshortint(instance)^ := Value.i;
                       otUByte: pbyte(instance)^ := Value.i;
                       otSWord: psmallint(instance)^ := Value.i;
                       otUWord: pword(instance)^ := Value.i;
                       otSLong,
                       otULong: pinteger(instance)^ := Value.i;
                     end;
                   end
                   else TypInfo.SetOrdProp(Obj, PropInfo, Value.i);
                 end;
        pkInt64: begin
                   if (luatype = LUA_TNUMBER) then Value.i64 := lua_toint64(Handle, stack_index)
                   else Value.i64 := round(CastValueAsNumber());

                   if (write_mode >= 0) then
                   begin
                     pint64(instance)^ := Value.i64;
                   end else
                   begin
                     TypInfo.SetInt64Prop(Obj, PropInfo, Value.i64);
                   end;
                 end;
        pkFloat: begin
                   if (write_mode >= 0) and (luatype = LUA_TNUMBER) then
                   begin
                     case Base.FloatType of
                       ftSingle: psingle(instance)^ := lua_tonumber(Handle, stack_index);
                       ftDouble: pdouble(instance)^ := lua_tonumber(Handle, stack_index);
                     ftExtended: pextended(instance)^ := lua_tonumber(Handle, stack_index);
                         ftComp: PComp(instance)^ := lua_tonumber(Handle, stack_index);
                         ftCurr: PCurrency(instance)^ := lua_tonumber(Handle, stack_index);
                     end;
                   end else
                   begin
                     if (luatype <> LUA_TNUMBER) then TypInfo.SetFloatProp(Obj, PropInfo, CastValueAsNumber())
                     else TypInfo.SetFloatProp(Obj, PropInfo, lua_tonumber(Handle, stack_index));
                   end;
                 end;
       pkString: begin
                   // Cast as string (soft mode)
                   if (luatype = LUA_TSTRING) then lua_to_pascalstring(FBufferArg.str_data, Handle, stack_index)
                   else
                   begin
                     if (not Self.stack_luaarg(FBufferArg, stack_index, true{чтобы отобразилось "LuaTable"})) then ThrowAssignValue({false});
                     TForceString(@TLuaArg.ForceString)(FBufferArg, FBufferArg.str_data);
                   end;

                   {todo UnicodeString?,}

                   {todo более умное преобразование}

                   if (not (Base.StringType in [stAnsiChar, stWideChar])) then
                   begin
                     // строки
                     if (write_mode >= 0) then
                     begin
                       case Base.StringType of
                       stShortString: pshortstring(instance)^ := FBufferArg.str_data;
                        stAnsiString: pansistring(instance)^ := FBufferArg.str_data;
                        stWideString: pwidestring(instance)^ := FBufferArg.str_data;
                       end;
                     end
                     else TypInfo.SetStrProp(Obj, PropInfo, FBufferArg.str_data);
                   end else
                   begin
                     // AnsiChar or WideChar
                     if (FBufferArg.str_data = '') then Value.i := 0
                     else
                     if (Base.StringType = stAnsiChar) then Value.i := pbyte(FBufferArg.str_data)^
                     else
                     System.StringToWideChar(FBufferArg.str_data, pwidechar(@Value.i), 1);

                     if (write_mode >= 0) then
                     begin
                       if (Base.StringType = stAnsiChar) then pansichar(instance)^ := ansichar(Value.i)
                       else pwidechar(instance)^ := widechar(Value.i);
                     end
                     else TypInfo.SetOrdProp(Obj, PropInfo, Value.i);
                   end;
                 end;
      pkVariant: begin
                   if (write_mode >= 0) then
                   begin
                     if (not Self.stack_variant(PVariant(PV)^, stack_index)) then ThrowAssignValue({false});
                   end else
                   begin
                     if (not Self.stack_variant(PVariant(PV)^, stack_index)) then ThrowAssignValue({false});
                     TypInfo.SetVariantProp(Obj, PropInfo, PVariant(@Value.v)^);

                     // очистка буфера
                     if (Value.v.VType = varString) or (not (Value.v.VType in VARIANT_SIMPLE)) then VarClear(PVariant(@Value.v)^);
                   end;
                 end;  
    pkInterface: begin

                   if (luatype = LUA_TNIL) then Value.p := nil
                   else
                   if (luatype = LUA_TLIGHTUSERDATA) then Value.p := lua_touserdata(Handle, stack_index)
                   else
                   ThrowAssignValue(true);

                   if (write_mode >= 0) then
                   begin
                     if (Value.p = nil) then PInterface(instance)^ := nil
                     else PInterface(instance)^ := IInterface(Value.p);
                   end else
                   begin
                     TypInfo.SetInterfaceProp(Obj, PropInfo, IInterface(Value.p));
                   end;
                 end;  
      pkPointer: begin
                   Value.p := nil;
                   case (luatype) of
                                  LUA_TNIL: {всё хорошо};
                               LUA_TSTRING: Value.p := lua_tolstring(Handle, stack_index, nil);
                        LUA_TLIGHTUSERDATA: Value.p := lua_touserdata(Handle, stack_index);
                             LUA_TUSERDATA: begin
                                              Value.p := lua_touserdata(Handle, stack_index);
                                              if (Value.p <> nil) then Value.p := PLuaUserData(Value.p).instance;
                                            end;
                             LUA_TFUNCTION: begin
                                              Value.p := pointer(lua_tocfunction(Handle, stack_index));
                                              if (Value.p <> nil) then Value.p := CFunctionPtr(Value.p);

                                              // возможно в будущем как-то иначе
                                              if (dword(Value.p) >= $FE000000) then Value.p := nil; {todo ?}
                                            end;
                                LUA_TTABLE: begin
                                              ClassIndex := LuaTableToClass(Handle, stack_index);
                                              if (ClassIndex >= 0) then Value.p := ClassesInfo[ClassIndex]._Class
                                              else ThrowAssignValue(true{LUA_TTABLE});
                                            end;
                   else
                     ThrowAssignValue(true);
                   end;

                   // присваиваем
                   if (write_mode >= 0) then ppointer(instance)^ := Value.p
                   else TypInfo.SetOrdProp(Obj, PropInfo, Value.i);
                 end;
        pkClass: begin
                   Value.p := nil;
                   case (luatype) of
                        LUA_TNIL: {всё хорошо}; 
                      LUA_TTABLE: begin
                                    ClassIndex := LuaTableToClass(Handle, stack_index);

                                    if (ClassIndex < 0) then
                                    ThrowAssignValue(true{LUA_TTABLE});

                                    if (ClassesInfo[ClassIndex]._ClassKind = ckClass) then
                                    begin
                                      Value.p := ClassesInfo[ClassIndex]._Class;
                                    end else
                                    begin
                                      FBufferArg.str_data := ClassesInfo[ClassIndex]._ClassName;
                                      ThrowAssignValue({false});
                                    end;  
                                  end;
                   LUA_TUSERDATA: begin
                                    Value.p := lua_touserdata(Handle, stack_index);
                                    CastUserDataAsTObject();
                                    {get class} Value.p := ppointer(Value.p)^;
                                  end;
                   else
                     ThrowAssignValue(true);
                   end;

                   // присваиваем
                   if (write_mode >= 0) then ppointer(instance)^ := Value.p
                   else TypInfo.SetOrdProp(Obj, PropInfo, Value.i);
                 end;
       pkObject: begin
                   Value.p := nil;
                   case (luatype) of
                        LUA_TNIL: {всё хорошо}; 
                   LUA_TUSERDATA: begin
                                    Value.p := lua_touserdata(Handle, stack_index);
                                    CastUserDataAsTObject();
                                  end;
                   else
                     ThrowAssignValue(true);
                   end;         

                   // присваиваем
                   if (write_mode >= 0) then ppointer(instance)^ := Value.p
                   else TypInfo.SetOrdProp(Obj, PropInfo, Value.i);
                 end;
       pkRecord,
        pkArray,
          pkSet: if (not CrystalLUA.PopSetDifficultTypeProp(Self, instance, stack_index, PropertyInfo^)) then
                 ThrowAssignValue({false});

    pkUniversal: if (not CrystalLUA.PopSetUniversalTypeProp(Self, instance, stack_index, PropertyInfo^)) then
                 ThrowAssignValue({false});
    end;    
  end;  *)
//end;

// размер TypeInfo
(*function TLua.__len(const ClassInfo: TLuaClassInfo): integer;
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
end; *)


// операторы над структурами и множествами
// neg, add, sub, mul, div, mod, pow, equal, less, less_equal
// операторы над множествами уже встроены: neg, +, -, *, <=, ==

// операторы над структурами опциональны. но есть ограничения
// при сравнении Result - это результат сравнения,
// при операторе neg - X1 и X2 равны - это исходная структура
// +,- и сравнение - делается c двумя операндами структурами
// для *,/,% и ^ - второй операнд - double
(*function TLua.__operator(const ClassInfo: TLuaClassInfo; const Kind: integer): integer;
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

    Assert('Fail operation "%s" with operand%s', [_kind, part2]);
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
      // инфертировать множество
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
end; *)



// по умолчанию сюда приходит калбек конструктора Create() для классов.
// но сюда так же перенаправляются конструкторы "на стеке" из __call(create = false)
// не только из классов, но так же из структур, массивов, множеств
//
// поэтому с ОЧЕНЬ большой вероятностью, первый аргумент является таблицей-"классом"
// для чистоты используемости как раз проверяем первый аргумент (если что - ошибка)
(*function TLua.__constructor(const ClassInfo: TLuaClassInfo; const __create: boolean): integer;
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
  Assert('Incorrect usage of %s constructor.', [ClassInfo._ClassName]);

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
  Assert('%s can not be initialized by a table.', [ClassInfo._ClassName]);


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
      if (ArgsCount < 0) or (ArgsCount > 1) then Assert('Wrong arguments count(%d) of TLuaReference() constructor', [ArgsCount]);

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
    Assert('Constructor of %s should have %d arguments', [ClassInfo._ClassName]);

    // вызов
    begin
      constructor_address := ClassInfo.constructor_address;
      if (dword(constructor_address) >= $FE000000) then constructor_address := ppointer(dword(userdata.instance^) + dword(constructor_address) and $00FFFFFF)^;

      TTClassRecallProc16(constructor_address)(userdata.instance^, FArgs, TLuaArg(nil^));
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
end;  *)

(*function TLua.__destructor(const ClassInfo: TLuaClassInfo; const __free: boolean): integer;
var
  luatype: integer;
  userdata: PLuaUserData;
begin
  Result := 0;

  luatype := lua_type(Handle, 1);
  if (luatype <> LUA_TUSERDATA) then
  Assert('Wrong destruction type "%s"', [LuaTypeName(luatype)]);

  userdata := lua_touserdata(Handle, 1);

  // не надо удалять объекты при __gc, если не стоит соответствующего флага
  if (not __free) and (not userdata .gc_destroy) then exit;

  // если уже удалён
  if (userdata.instance = nil) then
  begin
    if (__free) then
    Assert('Instance of %s type is already destroyed', [ClassInfo._ClassName]);

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
end; *)


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
(*function TLua.__call(const ClassInfo: TLuaClassInfo): integer;
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
    if (userdata = nil) or (userdata.ClassIndex <> TMETHOD_CLASS_INDEX) then  Assert('Wrong TMethod usage', []);

    Result := __tmethod_call(TMethod(userdata.instance^));
    exit;
  end;

  // если происходит инициализация по таблице
  if (lua_gettop(Handle)=2) and (lua_type(Handle,2)=LUA_TTABLE) and (LuaTableToClass(Handle,2)<0) then
  begin
    userdata := lua_touserdata(Handle, 1);
    if (userdata = nil) {но вообще быть не должно} then Assert('Unsupported operation.', []);

    // проверка на возможность инициализации по таблице
    // в будущем возможно будет инициализация по всем типам
    // todo ?
    if (userdata.kind <> ukInstance) then
    Assert('%s can not be initialized by a table.', [ClassInfo._ClassName]);

    // вызов
    __initialize_by_table(userdata, 2);
  end else
  begin
    // 100% ошибочная ситуация вызова конструктора из экземпляра
    // но я перенаправляю вызов в конструктор (там нужное сообщение покажется)
    Result := __constructor(ClassInfo, false);
  end;
end; *)


// основная функция по взятию значения глобальной переменной
// причём в двух вариантах
// native - значит нужно вернуть значение в info
// иначе - вызывается из lua, надо вернуть значение в стек
// или Exception !
(*function TLua.__global_index(const native: boolean; const info: TLuaGlobalModifyInfo): integer;
var
  Name: pchar;
  luatype, NameLen, Ind: integer;

  // вызвать ошибку
  procedure Assert(const FmtStr: string; const Args: array of const);
  begin
    if (native) then ELua.Assert(FmtStr, Args, info.CodeAddr)
    else Self.Assert(FmtStr, Args);
  end;

  // если не получилось заполнить Variant или TLuaArg
  // вызывается в native случае
  procedure AssertUnsupported();
  begin
    lua_settop(Handle, -1-1); //stack_pop(); - убрать значение из стека
    Assert('Can''t get global variable "%s" of type "%s"', [info.Name, FBufferArg.str_data]);
  end;

  // запушить глобальную переменную
  procedure PushGlobalProp(const Index: integer; const IsConst: boolean);
  var
    prop_struct: TLuaPropertyStruct;
  begin
    prop_struct.PropertyInfo := @GlobalNative.Properties[not Index];
    prop_struct.Instance := nil;
    prop_struct.IsConst := IsConst;
    prop_struct.Index := nil;
    prop_struct.ReturnAddr := nil;
    if (native) then prop_struct.ReturnAddr := info.CodeAddr;
    
    Self.__index_prop_push(GlobalNative, @prop_struct);
  end;

begin
  // количество возвращаемых аргументов
  Result := ord(not native);

  // получение имени. проверка
  if (native) then
  begin
    Name := pchar(info.Name);
    NameLen := Length(info.Name);
  end else
  begin
    luatype := lua_type(Handle, 2);
    if (luatype <> LUA_TSTRING) then Assert('Global key should be a string. Type %s is not available as global key', [LuaTypeName(luatype)]);
    Name := lua_tolstring(Handle, 2, @NameLen);
  end;

  // указатель или ошибка
  if (not GlobalVariablePos(Name, NameLen, Ind)) then
  Assert('Global variable "%s" not found', [Name]);

  // блок: если вызывается из кода lua
  if (not native) then
  begin
    with GlobalVariables[Ind] do
    begin
      if (_Kind in GLOBAL_INDEX_KINDS) then
      begin
        lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
      end else
      if (Index >= 0) then
      begin
        lua_pushcclosure(Handle, GlobalNative.Procs[Index].lua_CFunction, 0);
      end else
      begin
        // поместить значение глобальной переменной в луа-стек
        PushGlobalProp(Index, IsConst);
      end;
    end;

    exit;
  end;

  // блок: результат надо вернуть в info.Arg^
  if (not info.IsVariant) then
  begin
    with GlobalVariables[Ind] do
    begin
      if (_Kind in GLOBAL_INDEX_KINDS) then
      begin
        lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
        if (not stack_luaarg(info.Arg^, -1, false)) then AssertUnsupported();
        lua_settop(Handle, -1-1); //stack_pop();
      end else
      if (Index >= 0) then
      begin
        info.Arg.AsPointer := GlobalNative.Procs[Index].Address;
      end else
      begin
        PushGlobalProp(Index, IsConst);
        if (not stack_luaarg(info.Arg^, -1, false)) then AssertUnsupported();
        lua_settop(Handle, -1-1); //stack_pop();        
      end;
    end;

    exit;
  end;

  // блок: результат надо вернуть в info.V^: variant
  begin
    with GlobalVariables[Ind] do
    begin
      if (_Kind in GLOBAL_INDEX_KINDS) then
      begin
        lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
        if (not stack_variant(info.V^, -1)) then AssertUnsupported();
        lua_settop(Handle, -1-1); //stack_pop();
      end else
      if (Index >= 0) then
      begin
        Assert('"%s" is a method. Variant type does not support methods', [Name]);
      end else
      begin
        // прочитать проперти, но не в Arg, а в Variant
        PushGlobalProp(Index, IsConst);
        if (not stack_variant(info.V^, -1)) then AssertUnsupported();
        lua_settop(Handle, -1-1); //stack_pop();
      end;
    end;

    exit;
  end;      
end; *)

// основная функция по изменению значения глобальной переменной
// в двух вариантах
// native - значит имя и значения берутся из info
// иначе - из стека
// если глобальная переменная неизменяема, то Exception !
(*function TLua.__global_newindex(const native: boolean; const info: TLuaGlobalModifyInfo): integer;
type
  TIntToStr = procedure(const Value: integer; var Ret: string);

var
  Name: pchar;
  luatype, NameLen, Ind: integer;
  GlobalVariable: ^TLuaGlobalVariable;

  // вызвать ошибку
  procedure Assert(const FmtStr: string; const Args: array of const);
  begin
    if (native) then ELua.Assert(FmtStr, Args, info.CodeAddr)
    else Assert(FmtStr, Args);
  end;

  // push выдал ошибку, поэтому надо вызвать Exception
  procedure AssertUnsupported();
  const
    DESCRIPTION: array[boolean] of string = ('argument', 'variant');
  begin
    Assert('Unsupported %s type = "%s"', [DESCRIPTION[info.IsVariant], FBufferArg.str_data]);
  end;

  // взять значение из стека и присвоить новое значение глобальной переменной
  procedure FillGlobalProp(const Index: integer);
  var
    prop_struct: TLuaPropertyStruct;
  begin
    prop_struct.PropertyInfo := @GlobalNative.Properties[not Index];
    prop_struct.Instance := nil;
    prop_struct.StackIndex := -1;
    prop_struct.Index := nil;
    prop_struct.ReturnAddr := nil;
    if (native) then prop_struct.ReturnAddr := info.CodeAddr;

    Self.__newindex_prop_set(GlobalNative, @prop_struct);
  end;

begin
  Result := 0;

  // получение имени. проверка
  if (native) then
  begin
    Name := pchar(info.Name);
    NameLen := Length(info.Name);
  end else
  begin
    luatype := lua_type(Handle, 2);
    if (luatype <> LUA_TSTRING) then Assert('Global key should be a string. Type %s is not available as global key', [LuaTypeName(luatype)]);
    Name := lua_tolstring(Handle, 2, @NameLen);
  end;

  // блок: найти глобальную переменную
  // если не найдена, то создать новую (тип gkLuaData), заполнить значением
  if (not GlobalVariablePos(Name, NameLen, Ind, true)) then
  begin
    with GlobalVariables[Ind] do
    begin
      _Kind := gkLuaData;
      IsConst := false;
      global_alloc_ref(Ref);

      if (not native) then lua_pushvalue(Handle, 3)
      else
      if (not info.IsVariant) then
      begin
        if (not push_luaarg(info.Arg^)) then AssertUnsupported()
      end else
      if (not push_variant(info.V^)) then AssertUnsupported();

      global_fill_value(Ref);
    end;

    exit;    
  end;

  // проверка на возможность изменения
  GlobalVariable := @GlobalVariables[Ind];
  if (GlobalVariable.IsConst) then
  Assert('Global const "%s" can not be changed', [Name]);

  { на текущий момент возможны следующие переменные: }
  { gkLuaData и gkVariable. Всё ! Причём Variable 100% не константная! }


  // блок: заполнение происходит из кода lua
  if (not native) then
  begin
    with GlobalVariable^ do
    if (_Kind = gkLuaData) then
    begin
      lua_pushvalue(Handle, 3);
      lua_rawseti(Handle, LUA_GLOBALSINDEX, Ref); //global_fill_value(Ref);
    end else
    begin
      // взять зачение из стека и положить в глобальную переменную
      FillGlobalProp(Index);
    end;

    exit;
  end;

  // блок: заполнение происходит из info.Arg^
  if (not info.IsVariant) then
  begin
    with GlobalVariable^ do
    if (_Kind = gkLuaData) then
    begin
      if (not push_luaarg(info.Arg^)) then AssertUnsupported();
      lua_rawseti(Handle, LUA_GLOBALSINDEX, Ref); //global_fill_value(Ref);
    end else
    begin
      if (not push_luaarg(info.Arg^)) then AssertUnsupported();
      FillGlobalProp(Index);
      lua_settop(Handle, -1-1); //stack_pop();
    end;

    exit;
  end;

  // блок: заполнение происходит из info.V^: Variant
  begin
    with GlobalVariable^ do
    if (_Kind = gkLuaData) then
    begin
      if (not push_variant(info.V^)) then AssertUnsupported();
      lua_rawseti(Handle, LUA_GLOBALSINDEX, Ref); //global_fill_value(Ref);
    end else
    begin
      if (not push_variant(info.V^)) then AssertUnsupported();
      FillGlobalProp(Index);
      lua_settop(Handle, -1-1); //stack_pop();
    end;

    exit;       
  end;
end; *)

// описание вида Prefix[...][...][Value][...][...][...]
// или Prefix[...][...].Value
(*function __arrayindex_description(const Prefix, Value: string; const Index, Dimention: integer): string;
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
end; *)

// прочитать значение из lua-стека по индексу 2 и занести в очередной параметр (для сложного свойства)
// если массив - то проверить индекс !
// возвращает true если нужно брать конкретные данные
// index = (property: список параметров свойства), (array: указатель на конечные данные)
(*function __read_array_parameter(const Lua: TLua; const userdata: PLuaUserData; var index: pointer): boolean;
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
    
    Lua.Assert('Wrong bounds in array %s. Array pointer = %p. Available bounds = %d..%d', [S, array_instance, low, high]);
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
end; *)

// вызвать ошибку, связанную с некорректным индексом массива или свойства
(*procedure __throw_array_index(const Lua: TLua; const ClassInfo: TLuaClassInfo; const userdata: PLuaUserData; const is_getter: boolean);
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
  Lua.Assert(Error, []);
end; *)


// операции по промежуточным user-data массивам
(*function TLua.__array_index(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
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
    Assert('%s() method can''t be called, because %s instance is const', [S, ClassInfo._ClassName]);
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
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, ClassInfo.Ref); // global_push_value(Ref);
    lua_setmetatable(Handle, -2);
  end;
  
end; *)

// изменить userdata-массив
(*function TLua.__array_newindex(const ClassInfo: TLuaClassInfo; const is_property: boolean): integer;
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

    Assert('Can not change %s %s: not anought parameters',
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

    Assert(Err, []);
  end;


  // присваивается новое значение длинны динамического массива
  // и значение некорректно - (-1) или вообще не integer
  procedure ThrowLengthValue();
  var
    Err: string;
  begin
    Err := __arrayindex_description(ClassInfo._ClassName, 'Length', userdata.array_params and $F, 0);

    Assert('Can''t change %s property, because "%s" is not correct length value', [Err, StackArgument(3)]);
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
end; *)


// получить базовые понятия о стеке (количество аргументов, смещение)
// и userdata - объект над которым производятся действия
// в основном это нужно для функций инициализаторов/конструкторов
// если возвращается false, то userdata не найден (только в случае прямого вызова, не конструктор)
(*function __inspect_proc_stack(const is_construct: boolean; const Handle: pointer;
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
end; *)                             


// изменить размер динамического массива (userdata - первый параметр)
// 2 режима: мягкий (is_construct) и жёсткий (.Resize(...))
(*function TLua.__array_dynamic_resize(): integer;
var
  userdata: PLuaUserData;
  tpinfo: ptypeinfo;
  ArgsCount, Offset, Dimention, i: integer;
  Arguments: TIntegerDynArray;


  // если i-й аргумент не подходит для того чтобы
  // использовать в изменения размерности
  procedure ThrowArgument(const N: integer);
  begin
    Assert('Wrong argument №%d of Resize() method = "%s"', [N, FBufferArg.ForceString]);
  end;
begin
  Result := 0;

  if (not __inspect_proc_stack(false, Handle, userdata, ArgsCount, Offset)) then
  Assert('The first operand of Resize() method should be a dynamic array', []);

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
  if (ArgsCount = 0) then Assert('Resize() method has no arguments', []);
  if (ArgsCount > Dimention) then Assert('Resize(%d arguments) method can''t be called, because maximum array dimention is %d', [ArgsCount, Dimention]);

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
end; *)


// функция имеет 3 назначения.
// И всегда работает на заполнение одномерной (конечной) части массива
// в статических массивах происходит наполнение "с нуля"
// в динамических - добавляется в конец
// операнды - (конечные) элементы массива или (конечные) массивы, чьи элементы совпадают
(*function TLua.__array_include(const mode: integer{constructor, include, concat}): integer;
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

  ItemKind: __TLuaFieldKind;
  ItemInfomation: pointer; // typeinfo или __lua_difficult_type__.Info - сверки совместимости массивов
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

    Assert('Wrong argument №%d of %s = "%s"%s', [N, MethodName, Description, Bounds]);
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
  Assert('The first operand of %s should be an array', [MethodName]);

  if (ArgsCount = 0) then
  begin
    if (mode = 1{Include()}) then Assert('%s has no arguments', [MethodName]);
    exit;
  end;

  // сбор информации
  Instance := userdata.instance;
  Len := userdata.array_params and $F;
  CurLen := 0;
  with userdata.ArrayInfo^ do
  begin
    if (mode = 0) and (Dimention > 1) then Assert('%s should have no arguments, because array dimention = %d', [MethodName, Dimention]);

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
        Move(SrcInstance^, DestInstance^, CopyCount*ItemSize);
      end else
      begin
        CopyArray(DestInstance, SrcInstance, ItemTypeInfo, CopyCount*ItemsCount);
      end;

      Len := CurLen;
    end;
  end;      
end; *)


// враппер для определения входит ли операнд в множество Dest
function  _SetLe_Wrap(const Dest, X1, X2: pointer; const Size: integer): boolean;
begin _SetLe_Wrap := _SetLe(X2, Dest, Size); end;


// включить или исключить числа/множества из множества (userdata) в стеке
// метод так же вызывается в конструкторе множеств
(*function TLua.__set_method(const is_construct: boolean; const method: integer{0..2}): integer;
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
    Assert('Wrong argument №%d of %s = "%s"', [N, MethodName, FBufferArg.ForceString]);
  end;
begin
  Result := ord(method=2); // если метод "Contains", то надо возвращать boolean

  if (not __inspect_proc_stack(is_construct, Handle, userdata, ArgsCount, Offset)) then
  Assert('The first operand of %s() method should be a set', [MethodName]);

  // если нет аргументов
  if (ArgsCount = 0) then
  begin
    if (not is_construct) then Assert('%s() method has no arguments', [MethodName]);
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
end; *)


(*function TLua.ProcCallback(const ClassInfo: TLuaClassInfo; const ProcInfo: TLuaProcInfo): integer;
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
      Assert('Can''t call proc "%s.%s" because the instance is unknown (%s)', [ClassInfo._ClassName, ProcInfo.ProcName, LuaTypeName(luatype)]);
    end;

    // классовость функции
    if (userdata = nil) and (not ProcInfo.with_class) then
    Assert('%s.%s is not class method', [ClassInfo._ClassName, ProcInfo.ProcName]);

    // если инстенс уже удалён
    if (userdata <> nil) and (userdata.instance = nil) then
    Assert('%s instance is already destroyed. Proc "%s"', [ClassInfo._ClassName, ProcInfo.ProcName]);

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
      TLuaProc2(ProcInfo.Address)(FArgs, FBufferArg) // глобальная
    end else
    begin
      if (userdata = nil) then proc_class := TClass(ClassesInfo[class_index]._Class) else proc_class := TClass(userdata.instance^);
      if (dword(proc_address) >= $FE000000) then proc_address := ppointer(dword(proc_class) + dword(proc_address) and $00FFFFFF)^;

      if (userdata = nil) then
        TTClassRecallProc23(proc_address)(proc_class, FArgs, FBufferArg) // классовая
      else
      if (ProcInfo.with_class) then
        TTClassRecallProc23(proc_address)(proc_class, FArgs, FBufferArg) // классовая, но через объект
      else
        TTClassRecallProc9(proc_address)(TObject(userdata.instance), FArgs, FBufferArg) // обычная с объектом класса
    end;
  end;

  // результат
  FArgsCount := 0; // FArgs не обнуляется, чтобы избежать дополнительных реаллоков и финализаторов
  if (not push_luaarg(FBufferArg)) then ELua.Assert('Can''t return value type "%s"', [FBufferArg.str_data], proc_address);
  Result := 1; // т.е. результат всегда есть, даже если nil
end; *)

(*function TLua.VariableExists(const Name: string): boolean;
var
  Ind: integer;
  luatype: integer;
begin
  if (not FNameSpaceInitalized) then INITIALIZE_NAME_SPACE();

  if (GlobalVariablePos(pchar(Name), Length(Name), Ind)) then
  with GlobalVariables[Ind] do
  if (_Kind in [gkType, gkVariable, gkConst]) then
  begin
    // тип (класс/структура), нативная переменная или Enum
    Result := true;
    exit;
  end else
  if (_Kind = gkLuaData) then
  begin
    // переменная в глобальной таблице
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
    luatype := (lua_type(Handle, -1));
    lua_settop(Handle, -1-1); //stack_pop();

    Result := (luatype <> LUA_TNONE) and (luatype <> LUA_TFUNCTION);
    exit;
  end;

  Result := false;
end; *)

(*function TLua.ProcExists(const ProcName: string): boolean;
var
  Ind: integer;
begin
  if (not FNameSpaceInitalized) then INITIALIZE_NAME_SPACE();

  if (GlobalVariablePos(pchar(ProcName), Length(ProcName), Ind)) then
  with GlobalVariables[Ind] do
  if (_Kind = gkProc) then
  begin
    // нативная глобальная процедура
    Result := true;
    exit;
  end else
  if (_Kind = gkLuaData) then
  begin
    // переменная в глобальной таблице
    lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
    Result := (lua_type(Handle, -1) = LUA_TFUNCTION);
    lua_settop(Handle, -1-1); //stack_pop();
    exit;
  end;

  Result := false;
end; *)

(*procedure __TLuaCall_luaargs(const Self: TLua; const ProcName: string; const _Args: TLuaArgs;
                            var Result: TLuaArg; const ReturnAddr: pointer);
var
  Found: boolean;
  i, Ind, RetCount: integer;    
begin
  with Self do
  begin
    if (not FNameSpaceInitalized) then INITIALIZE_NAME_SPACE();

    // запушить соответствующую cfunction если найден
    Found := GlobalVariablePos(pchar(ProcName), Length(ProcName), Ind);
    if (Found) then
    with GlobalVariables[Ind] do
    if (_Kind = gkLuaData) then
    begin
      lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
      if (lua_type(Handle, -1) <> LUA_TFUNCTION) then
      begin
        Found := false;
        stack_pop();
      end;
    end else
    begin
      Found := (_Kind = gkProc);
      if (Found) then lua_pushcclosure(Handle, GlobalNative.Procs[Index].lua_CFunction, 0);
    end;

    // если функция не найдена
    if (not Found) then
    ELua.Assert('Lua function "%s" not found', [ProcName], ReturnAddr);

    // загрузить параметры
    for i := 0 to Length(_Args)-1 do
    if (not push_luaarg(_Args[i])) then
    begin
      lua_settop(Handle, 0);
      ELua.Assert('Unknown argument type "%s" (arg №%d)', [FBufferArg.str_data, i+1], ReturnAddr)
    end;

    // вызов
    Self.ScriptCall(Length(_Args));

    // результат
    Result.FLuaType := ltEmpty;
    RetCount := lua_gettop(Handle);
    if (RetCount <> 0) then
    begin
      stack_luaarg(Result, 1, false);
      lua_settop(Handle, 0);
    end;
  end;
end;

function TLua.Call(const ProcName: string; const Args: TLuaArgs): TLuaArg;
asm
  pop ebp
  push [esp]
  jmp __TLuaCall_luaargs
end;  *)


(*procedure __TLuaCall__arguments(const Self: TLua; const ProcName: string; const _Args: array of const;
                                var Result: TLuaArg; const ReturnAddr: pointer);
var
  Found: boolean;
  i, Ind, RetCount: integer;
begin
  with Self do
  begin
    if (not FNameSpaceInitalized) then INITIALIZE_NAME_SPACE();

    // запушить соответствующую cfunction если найден
    Found := GlobalVariablePos(pchar(ProcName), Length(ProcName), Ind);
    if (Found) then
    with GlobalVariables[Ind] do
    if (_Kind = gkLuaData) then
    begin
      lua_rawgeti(Handle, LUA_GLOBALSINDEX, Ref); //global_push_value(Ref);
      if (lua_type(Handle, -1) <> LUA_TFUNCTION) then
      begin
        Found := false;
        stack_pop();
      end;
    end else
    begin
      Found := (_Kind = gkProc);
      if (Found) then lua_pushcclosure(Handle, GlobalNative.Procs[Index].lua_CFunction, 0);
    end;

    // если функция не найдена
    if (not Found) then
    ELua.Assert('Lua function "%s" not found', [ProcName], ReturnAddr);

    // загрузить параметры
    for i := 0 to Length(_Args)-1 do
    if (not push_argument(_Args[i])) then
    begin
      lua_settop(Handle, 0);
      ELua.Assert('Unknown argument type "%s" (arg №%d)', [FBufferArg.str_data, i+1], ReturnAddr)
    end;

    // вызов
    Self.ScriptCall(Length(_Args));

    // результат
    Result.FLuaType := ltEmpty;
    RetCount := lua_gettop(Handle);
    if (RetCount <> 0) then
    begin
      stack_luaarg(Result, 1, false);
      lua_settop(Handle, 0);
    end;
  end;
end;


function TLua.Call(const ProcName: string; const Args: array of const): TLuaArg;
asm
  pop ebp
  push [esp]
  jmp __TLuaCall__arguments
end;  *)


// проверить количество аргуметов в стеке на
// соответствие ожидаемому количеству аргументов
(*function __TLuaCheckArgsCount__1(const Self: TLua; const ArgsCount: TIntegerDynArray;
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
end;

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
end;

procedure TLua.CheckArgsCount(const ArgsCount: integer; const ProcName: string=''; const AClass: TClass=nil);
begin
  InternalCheckArgsCount(@ArgsCount, 1, ProcName, AClass);
end;  *)

// универсальная функция для создания, инициализации сложного типа и урегулировании конфликтов
// прошлый аналог - internal_add_class_info/internal_register_metatable
function __TLuaInternalAddType(Self: TLua; Kind: integer; Identifier: __luaname; var added: boolean): __PLuaType; overload;
var
  TypeId: __PLuaHashItem;

  procedure ThrowAlreadyRegistered();
  begin
    Self.Assert('Type "%s" is already registered (%s)', [UnpackString(Identifier), LUATYPE_NAMES[Result.kind]]);
  end;
begin
  if (Identifier = nil) then
  begin
    // глобальное пространство
    Result := Self.FStorage.Types.alloc();
    added := true;
  end else
  begin
    // иной тип
    Result := Self.FStorage.RegisteredTypes.add_ptr(Identifier, added);
  end;

  // проверка коллизии
  if (not added) then
  begin
    if (Result.kind <> Kind) then ThrowAlreadyRegistered();
    exit;
  end;

  // добавлен новый тип (added = true)
  ZeroMemory(Result, sizeof(__TLuaUniversalType));
  Result.kind := Kind;
  Result.name := Identifier;
  __TLuaHashArray(Result.namespace).Initialize(Self, @Self.FStorage.Identifiers);
  Result.Lua := Self;

  // метатаблица
  Self.global_alloc_ref(Result.metatable);
  lua_createtable(Self.Handle, 0, 0);
  if (Kind <> LUATYPE_GLOBAL) then
  begin
    // для всех сложных типов (и difficult property)
    // надо создать "указатель" на себя в метатаблице
    // гарантом типа является lightuserdata запись на основе __ЕLuaHashItem
    TypeId := Self.FStorage.HashItems.alloc();
    TypeId.next := pointer(typeinfoTClass);
    TypeId.Key := integer(typeinfoTClass);
    TypeId.Value := Result;
    lua_pushlightuserdata(Self.Handle, TypeId);
    lua_rawseti(Self.Handle, -2, 0);
  end;
  Self.global_fill_value(Result.metatable);
  if (Kind = LUATYPE_GLOBAL) then
  begin
    Self.global_push_value(Result.metatable);
    lua_setmetatable(Self.Handle, LUA_GLOBALSINDEX);
  end else
  begin
    Self.global_push_value(Result.metatable);
    lua_pushvalue(Self.Handle, 1);
    lua_setmetatable(Self.Handle, -2);
    Self.stack_pop();
  end;
  if (Kind < LUATYPE_CLASS) then exit;

  // соответствующая запись в глобальном пространстве (+ обработка коллизий)
  // todo

  // необходимо проинициализировать стандартные идентификаторы (в зависимости от типа)
  // todo
end;

function __TLuaInternalAddType(Self: TLua; Kind: integer; Identifier: __luaname): __PLuaType; overload;
var
  fake: boolean;
begin
  Result := __TLuaInternalAddType(Self, Kind, Identifier, fake);
end;

// добавить идентификатор в namespace. грамотно всё обработать,
// в случае чего вызвать ошибку 
function __TLuaInternalAddIdentifier(Self: TLua; LuaType: __PLuaType; Identifier: __luaname;
                                     Mode: __TLuaIdentifierMode; var added: boolean): __PLuaIdentifier;
var
  HashArray: __PLuaHashArray;
  stdindex: integer;
begin
  HashArray := __PLuaHashArray(@LuaType.namespace);
  Result := HashArray.find_ptr(Identifier);
  if (Result <> nil) then
  begin
    // разруливаем коллизии если нужно
    added := false;

    // проверка стандартного имени
    if (integer(Result) < 0) then
    begin
      stdindex := not integer(Result);
      // todo

      exit;
    end;

    // сверка коллизий режима
    if (Result.Mode <> Mode) then
    begin
      // todo
    end;
  end else
  begin
    // добавляем новый идентификатор
    added := true;
    Result := Self.FStorage.Identifiers.alloc();
    FillChar(Result^, sizeof(__TLuaIdentifier), #0);
    Result.Name := Identifier;
    Result.Mode := Mode;
    HashArray.add_ptr(Identifier, false, Result);
  end;


end;

// __TLuaIdentifierMode

function __TLuaInternalAddProc(Self: TLua; LuaType: __PLuaType; Identifier: __luaname; Address: pointer): __PLuaProcInfo;
var
  added: boolean;
begin
  // todo проверка адреса


  Result := __PLuaProcInfo(__TLuaInternalAddIdentifier(Self, LuaType, Identifier, imProcedure, added));


  // todo
//  Result := nil;
end;

function __TLuaInternalAddProperty(Self: TLua; LuaType: __PLuaType; Identifier: __luaname {...}): __PLuaPropertyInfo;
var
  added: boolean;
begin

  // todo

  Result := __PLuaPropertyInfo(__TLuaInternalAddIdentifier(Self, LuaType, Identifier, imProperty, added));

//  Result := nil;
end;


// добавить класс, если такого нет
// если UsePublished, то прописать ему так же всё связанное с published
// если это регистратор, то зарегистрировать всё для подрегистрируемого класса
function __TLuaRegClass(Self: TLua; AClass: TClass; UsePublished: boolean): __PLuaClassInfo;
var
  Identifier: __luaname;
  ClassRegistrator: TClass;
  ClassRegistratorName: PShortString;
  NameBuffer: ShortString;
  InstanceSize: integer;

  // строка начинается с lua (в разном регистре)
  function is_lua(name: PShortString): boolean;
  begin
    Result := (name <> nil) and (pbyte(name)^ > 3) and
              (name^[1] in ['l','L']) and (name^[2] in ['u','U']) and (name^[3] in ['a','A']);
  end;

  // является ли класс регистратором lua
  function IsRegistrator(const _Class: TClass): boolean;
  begin
    Result := (_Class <> nil) and is_lua(PShortString(ppointer(integer(_Class) + vmtClassName)^));
  end;

  // в случае если в авторегистраторе обнаружено собственное поле
  procedure ThrowOwnFields(const Name: PShortString; const is_property: boolean);
  const
    FIELD_PROPERTY: array[boolean] of AnsiString = ('Field', 'Property');
  var
    Error: LuaString;
  begin
    Error := LuaStringFormat('Class registrator "%s" can''t have own fields', [UnpackString(ClassRegistratorName^)]);
    if (Name <> nil) then Error := LuaStringFormat('%s. %s "%s"', [Error, FIELD_PROPERTY[is_property], UnpackString(Name^)]);
    Self.Assert(Error);
  end;

  // скопировать имя без символов "lua" в буфер, вернуть указатель на NameBuffer
  function Unpack_luaName(const Name: ShortString): PShortString;
  var
    Len: integer;
  begin
    Len := Length(Name)-3;
    Result := @NameBuffer;
    pbyte(Result)^ := Len;
    Move(Name[4], Result^[1], Len);
  end;  

  // добавить методы из глобального списка методов
  procedure AddPublishedMethods(const _Class: TClass);
  type
    TMethodEntry = packed record
      len: Word;
      adr: Pointer;
      name: ShortString;
    end;
  var
    i: word;
    MC: pword;
    MethodEntry: ^TMethodEntry;
    MethodIdentifier: __luaname;
  begin
    // Registrator mode
    if (ClassRegistrator <> AClass) and (_Class.ClassParent <> AClass) then AddPublishedMethods(_Class.ClassParent);

    MC := pword(ppointer(integer(_Class)+vmtMethodtable)^);
    if (MC = nil) then exit;
    MethodEntry := pointer(integer(MC)+2);

    for i := 1 to integer(MC^) do
    begin
      if (is_lua(@MethodEntry.name)) then
      begin
        MethodIdentifier := Self.FStorage.Names.Identifier(Unpack_luaName(MethodEntry.name));
        __TLuaInternalAddProc(Self, @Result.FType, MethodIdentifier, MethodEntry.adr);
      end;

      inc(integer(MethodEntry), integer(MethodEntry.len));
    end;
  end;

  // добавить published-свойства
  procedure AddPublishedProperties(const _Class: TClass);
  var
    PropCount, i: integer;
    tpinfo: TypInfo.PTypeInfo;
    PropInfo: TypInfo.PPropInfo;
  begin
    tpinfo := _Class.ClassInfo;
    if (tpinfo = nil) then PropCount := 0 else PropCount := GetTypeData(tpinfo).PropCount;//GetPropList(tpinfo, PropList);
    if (PropCount <> 0) then
    begin
      PropInfo := Self.FStorage.DataBuffer.alloc(PropCount * sizeof(Pointer));
      Self.FStorage.DataBuffer.MemoryOffset := 0;
      TypInfo.GetPropInfos(tpinfo, PPropList(PropInfo));

      for i := 0 to PropCount-1 do
      begin
        if (ClassRegistrator <> AClass) then
        with PropInfo^ do
        begin
          if ((dword(GetProc) >= $FF000000) and (integer(GetProc) and $00FFFFFF >= InstanceSize))
          or ((dword(SetProc) >= $FF000000) and (integer(SetProc) and $00FFFFFF >= InstanceSize)) then
          ThrowOwnFields(@PropInfo.Name, true);
        end;

        // регистрация
        (*tpinfo := PropInfo.PropType{$ifndef fpc}^{$endif};
        PropBase := GetLuaPropertyBase(Self, Prefix, PropName, tpinfo, true);
        PropIndex := ClassesInfo[Result].InternalAddName(PropName, false, {FInitialized, }nil{CodeAddr});
        ClassesInfo[Result].Properties[{InvertIndex} not (PropIndex)].Fill(PropInfo, PropBase);
        *)

        inc(integer(PropInfo), sizeof(Pointer));
      end;
    end;
  end;

  // все published поля. классы
  procedure AddPublishedFields(const _Class: TClass);
  type
    TUsedClassesTable = packed record
      Count: word;
      Classes: array[0..8191] of ^TClass;
    end;
    PClassFieldInfo = ^TClassFieldInfo;
    TClassFieldInfo = packed record
      Offset: integer;
      TypeIndex: Word;
      Name: ShortString;
    end;
    PClassFieldTable = ^TClassFieldTable;
    TClassFieldTable = packed record
      Count: word;
      UsedClasses: ^TUsedClassesTable;
      Fields: array[0..0] of TClassFieldInfo;
    end;
  var
    i: integer;
    Table: PClassFieldTable;
    Field: PClassFieldInfo;
  begin
    if (_Class = nil) then exit
    else AddPublishedFields(_Class.ClassParent);

    Table := PClassFieldTable(pointer(integer(_Class)+vmtFieldTable)^);
    if (Table = nil) then exit;

    // регистрация классов
    if (Table.UsedClasses <> nil) then
    for i := 0 to Table.UsedClasses.Count-1 do
    __TLuaRegClass(Self, Table.UsedClasses.Classes[i]^, True);

    // регистрация полей
    Field := @Table.Fields[0];
    for i := 0 to Table.Count-1 do
    begin
      // проверка при ссылке на поле внутри регистратора
      if (ClassRegistrator <> AClass) and (Field.Offset >= InstanceSize) then
      ThrowOwnFields(@Field.Name, false);

      // регистрация
      //TODO InternalAddProperty(true, ClassesInfo[Result]._Class, Field.Name, typeinfo(TObject), false, false, pointer(Field.Offset), pointer(Field.Offset), nil);
      inc(integer(Field), sizeof(integer)+sizeof(word)+sizeof(byte)+pbyte(@Field.Name)^);
    end;
  end;


begin
  if (AClass = nil) then
  Self.Assert('Class not defined');

  // определяемся с классом, откуда берём published информацию (ClassRegistrator)
  if IsRegistrator(AClass) then
  begin
    Result := Self.FStorage.RegisteredTypes.find(integer(AClass));
    if (Result <> nil) then exit; {если такой авторегистратор уже был зарегистрирован}

    // запоминаем регистратор, ищем реальный класс для регистрации
    ClassRegistrator := AClass;
    while (true) do
    begin
      AClass := AClass.ClassParent;
      if (not IsRegistrator(AClass)) then break;
    end;
  end else
  begin
    ClassRegistrator := nil;
    if (UsePublished) then ClassRegistrator := AClass;
  end;

  // ищем/добавляем реальный класс в хранилище типов
  Result := Self.FStorage.RegisteredTypes.find_ptr(pointer(AClass));
  if (Result = nil) then
  begin
    // добавляем
    Identifier := Self.FStorage.Names.Identifier(PShortString(PPointer(Integer(AClass) + vmtClassName)^));
    Result := __PLuaClassInfo(__TLuaInternalAddType(Self, LUATYPE_CLASS, Identifier));

    // прописать соответствующий поисковый элемент в массив зарегистрированных типов
    Self.FStorage.RegisteredTypes.add_ptr(pointer(AClass), false, Result);

    // Parent
    if (AClass <> TObject) then Result.Parent := __TLuaRegClass(Self, AClass.ClassParent, UsePublished);
  end;

  // если не нужно регистрировать published информацию
  if (ClassRegistrator = nil) then exit;

  // регистрация
  ClassRegistratorName := PShortString(PPointer(Integer(ClassRegistrator) + vmtClassName)^);
  InstanceSize := AClass.InstanceSize;
  AddPublishedMethods(ClassRegistrator);
  AddPublishedProperties(ClassRegistrator);
  AddPublishedFields(ClassRegistrator);

  // дополнительная проверка, если регистрация прошла, но всёравно возможны поля. надо делать Exception
  if (AClass <> ClassRegistrator) and (ClassRegistrator.InstanceSize <> InstanceSize) then
  ThrowOwnFields(nil, false);
end;

procedure TLua.RegClass;
asm
  push offset __TLuaRegClass
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRegClasses(const Self: TLua; const AClasses: array of TClass; const use_published: boolean);
var
  i: integer;
begin
  for i := 0 to high(AClasses) do
  __TLuaRegClass(Self, AClasses[i], use_published);
end;

procedure TLua.RegClasses;
asm
  pop ebp
  push offset __TLuaRegClasses
  jmp TLua.InternalRegisterRecall
end;


// tpinfo может быть:
// - typeinfo(struct)
// - typeinfo(DynArray of struct)
// - sizeof(struct)
function __TLuaRegRecord(Self: TLua; Name: __luaname; tpinfo: pointer): PLuaRecordInfo;
begin
  Result := nil;
end;

(*
var
  RecordTypeInfo: ptypeinfo;
  RecordSize: integer;
  TypeData: PTypeData;
  FieldTable: PFieldTable absolute TypeData;
  RecordInfo: PLuaRecordInfo;
begin
  if (not IsValidIdent(Name)) then
  Self.Assert('Non-supported record type name ("%s")', [Name]);

  // провека tpinfo, найти sizeof и реальный typeinfo
  begin
    if (tpinfo = nil) then
    Self.Assert('TypeInfo of record "%s" is not defined', [Name]);

    RecordTypeInfo := nil;
    RecordSize := 0;

    if (integer(tpinfo) < $FFFF) then
    begin
      // sizeof()
      RecordSize := integer(tpinfo);
    end else
    begin
      TypeData := GetTypeData(tpinfo);

      // запись или динамический массив
      if (TTypeInfo(tpinfo^).Kind in RECORD_TYPES) then
      begin
        RecordSize := FieldTable.Size;
        RecordTypeInfo := tpinfo;
      end else
      if (TTypeInfo(tpinfo^).Kind = tkDynArray) then
      begin
        RecordSize := TypeData.elSize;

        if (TypeData.elType <> nil) then
        begin
          RecordTypeInfo := TypeData.elType^;
          if (RecordTypeInfo <> nil) and (not (RecordTypeInfo.Kind in RECORD_TYPES)) then
          Self.Assert('Sub dynamic type "%s" is not record type (%s)', [RecordTypeInfo.Name, TypeKindName(RecordTypeInfo.Kind)]);
        end;
      end else
      begin
        Self.Assert('Type "%s" is not record and subdynamic type (%s)', [Name, TypeKindName(ptypeinfo(tpinfo).Kind)]);
      end;
    end;
  end;

  // найти имеющийся
  if (RecordTypeInfo <> nil) then
  begin
    if (not SameStrings(RecordTypeInfo.Name, Name)) then
    Self.Assert('Mismatch of names: typeinfo "%s" and "%s" as parameter "Name"', [RecordTypeInfo.Name, Name]);

    Result := internal_class_index(RecordTypeInfo);
  end else
  begin
    Result := -1;
  end;

  if (Result < 0) then Result := internal_class_index_by_name(Name);
  if (Result >= 0) then
  with ClassesInfo[Result] do
  begin
    if (_ClassKind <> ckRecord) then
    Self.Assert('Type "%s" is already registered', [Name]);

    // проверка на соответствие
    with PLuaRecordInfo(_Class)^ do
    begin
      if (Size <> RecordSize) then
      Self.Assert('Size of %s (%d) differs from the previous value (%d)', [Name, RecordSize, Size]);

      if (FTypeInfo = nil) then FTypeInfo := RecordTypeInfo
      else
      if (FTypeInfo <> RecordTypeInfo) then
      Self.Assert('TypeInfo of "%s" differs from the previous value', [Name]);
    end;

    exit;
  end;

  // проициализировать RecordInfo
  new(RecordInfo);
  with RecordInfo^ do
  begin
    FType.Lua := Self;
    FTypeInfo := RecordTypeInfo;
    FSize := RecordSize;
    FOperators := [];
    FOperatorCallback := nil;
  end;

  // добавить в список ClassesInfo
  Result := internal_add_class_info();
  with ClassesInfo[Result] do
  begin
    _Class := RecordInfo;
    _ClassKind := ckRecord;
    _ClassName := Name;
    Ref := internal_register_metatable(nil{CodeAddr}, _ClassName, Result);

    RecordInfo.FName := Name;
    RecordInfo.FClassIndex := Result;
  end;

  // добавить в список быстрого поиска
  internal_add_class_index(RecordInfo, Result);
  internal_add_class_index_by_name(Name, Result);
  if (RecordTypeInfo <> nil) then internal_add_class_index(RecordTypeInfo, Result);
end; *)

function __TLuaRegRecordByString(Self: TLua; const Name: LuaString; tpinfo: pointer): PLuaRecordInfo;
begin
  __TLuaRegRecordByString := __TLuaRegRecord(Self,Self.CheckIdentifier(Name),tpinfo);
end;

function  TLua.RegRecord;
asm
  push offset __TLuaRegRecordByString
  jmp TLua.InternalRegisterRecall
end;

// itemtypeinfo - обычный тип или recordinfo или arrayinfo
function __TLuaRegArray(Self: TLua; Identifier, itemtypeinfo: pointer; const ABounds: array of integer): PLuaArrayInfo;
begin
  Result := nil;
end;
(*const
  STATIC_DYNAMIC: array[boolean] of string = ('static', 'dynamic');
var
  i: integer;
  Dest: TLuaArrayInfo;

  arraytypeinfo: ptypeinfo;
  PropertyInfo: PLuaPropertyInfo;

  elType: PPTypeInfo;
  FBufSize: integer;
  buftypeinfo: ptypeinfo;
  buftypekind: TTypeKind;
begin
  ZeroMemory(@Dest, sizeof(Dest));
  PropertyInfo := PLuaPropertyInfo(@Dest.ItemInfo);

  // идентификатор: Имя, typeinfo
  if (Identifier = nil) then
  Self.Assert('Array identifier is not defined');
  try
    if (TTypeKind(Identifier^) in [tkArray, tkDynArray]) then
    begin
      arraytypeinfo := ptypeinfo(Identifier);
      Dest.FName := arraytypeinfo.Name;
    end else
    begin
      Dest.FName := pchar(Identifier); // todo Unicode ?
      arraytypeinfo := nil;
    end;
  except
    Self.Assert('Array identifier is not correct');
    Result := -1;
    exit;
  end;
  if (not IsValidIdent(Dest.Name)) then
  Self.Assert('Non-supported array type name ("%s")', [Dest.Name]);

  // сбор информации
  with Dest do
  begin
      // проверка имеющегося
      Result := internal_class_index_by_name(Name);
      if (Result < 0) and (arraytypeinfo <> nil) then Result := internal_class_index(arraytypeinfo);
      if (Result >= 0) and (ClassesInfo[Result]._ClassKind <> ckArray) then Self.Assert('Type "%s" is already registered', [Name]);

      // IsDymanic
      FIsDynamic := (arraytypeinfo <> nil) and (arraytypeinfo.Kind = tkDynArray);
      FBoundsData := IntegerDynArray(ABounds);
      FBounds := pointer(FBoundsData);
      if (IsDynamic <> (Bounds=nil)) then
      begin
        if (IsDynamic) then Self.Assert('Dynamic array "%s" has no bounds', [Name])
        else Self.Assert('Array information of "%s" is not defined', [Name]);
      end;

      // проверка itemtypeinfo, Kind, размер
      if (itemtypeinfo = nil) then
      Self.Assert('"%s" registering... The typeinfo of %s array item is not defined', [Name, STATIC_DYNAMIC[IsDynamic]]);
      PropertyInfo.Base := GetLuaPropertyBase(Self, '', Name, ptypeinfo(itemtypeinfo));
      itemtypeinfo := PropertyInfo.Base.Information;
      FItemSize := GetLuaItemSize(PropertyInfo.Base);
      if (FItemSize = 0) then Self.Assert('"%s" registering... The size of %s array item is not defined', [Name, STATIC_DYNAMIC[IsDynamic]]);


      // подсчёт Dimention, проверка Bounds, Multiplies
      if (IsDynamic) then
      begin
          buftypeinfo := arraytypeinfo;
          while (buftypeinfo <> nil) do
          begin
            inc(FDimention);
            elType := GetTypeData(buftypeinfo).elType;

            if (elType = nil) then
            begin
              if (FItemSize = GetTypeData(buftypeinfo).elSize) then break;
            end else
            begin
              buftypeinfo := elType^;
              if (buftypeinfo = itemtypeinfo) then break;

              buftypekind := ptypeinfo(buftypeinfo).kind;
              if (buftypekind = tkDynArray) then
              begin
                if (PLuaArrayInfo(itemtypeinfo).FTypeInfo = buftypeinfo) then break;
                continue;
              end else
              if (buftypekind = tkArray) then
              begin
                if (PLuaArrayInfo(itemtypeinfo).FTypeInfo = buftypeinfo) or
                   (PLuaArrayInfo(itemtypeinfo).FSize = integer(PFieldTable(GetTypeData(buftypeinfo)).Size)) then break;
              end else
              if (buftypekind in RECORD_TYPES) then
              begin
                if (PLuaRecordInfo(itemtypeinfo).FTypeInfo = buftypeinfo) then break;
              end;
            end;

            Self.Assert('Incorrect itemtypeinfo of dynamic array "%s"', [Name]);
          end;

          buftypeinfo := arraytypeinfo;
          SetLength(FMultiplies, Dimention);
          ptypeinfo(FMultiplies[0]) := buftypeinfo;
          for i := 1 to Dimention-1 do
          begin
            buftypeinfo := GetTypeData(buftypeinfo).elType^;
            ptypeinfo(FMultiplies[i]) := buftypeinfo;
          end;                       
      end else
      begin
        FDimention := Length(FBoundsData);
        if (Dimention and 1 = 1) then Self.Assert('"%s" registering... Bounds size should be even. %d is an incorrect size', [Name, Dimention]);
        FDimention := FDimention div 2;

        for i := 0 to Dimention-1 do
        if (ABounds[i*2] > ABounds[i*2+1]) then
        Self.Assert('"%s" registering... Incorrect bounds: "%d..%d"', [Name, ABounds[i*2], ABounds[i*2+1]]);

        SetLength(FMultiplies, Dimention);
        FMultiplies[Dimention-1] := FItemSize;
        for i := Dimention-2 downto 0 do
        FMultiplies[i] := FMultiplies[i+1]*(ABounds[(i+1)*2+1] - ABounds[(i+1)*2] + 1);
      end;


      // заполнение информации для финализации
      if (arraytypeinfo <> nil) then
      begin
        FTypeInfo := arraytypeinfo;
        FItemsCount := 1;

        if (IsDynamic) then
        begin
          FSize := sizeof(pointer);
        end else
        begin
          FSize := PFieldTable(GetTypeData(arraytypeinfo)).Size;

          // дополнительная проверка размера
          FBufSize := FItemSize;
          for i := 0 to Dimention-1 do
          FBufSize := FBufSize*(ABounds[i*2+1]-ABounds[i*2]+1);

          if (FSize <> FBufSize) then
          Self.Assert('Incorrect bounds of static array "%s"', [Name]);
        end;
      end else
      begin
        // 100% статический массив
        // элементов itemtypeinfo.

        // конечный элемент финализации - FTypeInfo: ptypeinfo
        PropertyInfo.Base.Information := itemtypeinfo;
        FTypeInfo := GetLuaDifficultTypeInfo(PropertyInfo.Base);

        // определить общее количество элементов по Bounds
        FItemsCount := 1;
        if (PropertyInfo.Base.Kind = pkArray) then FItemsCount := PLuaArrayInfo(itemtypeinfo).FItemsCount;

        for i := 0 to Dimention-1 do
        FItemsCount := FItemsCount*(ABounds[i*2+1]-ABounds[i*2]+1);

        // размер статического массива
        FSize := FItemSize*FItemsCount;
      end;                   
  end;

  // если найден, то очистить поле ItemInfo, а если не найден, то создать
  if (Result >= 0) then
  begin
    Dest.FClassIndex := Result;
    TLuaPropertyInfo(PLuaArrayInfo(ClassesInfo[Result]._Class).ItemInfo).Cleanup();
  end else
  begin
    Result := internal_add_class_info();
    Dest.FClassIndex := Result;    
//    FInitialized := false;

    with ClassesInfo[Result] do
    begin
      new(PLuaArrayInfo(_Class));
      _ClassKind := ckArray;
      _ClassName := Dest.Name;
      Ref := internal_register_metatable(nil{CodeAddr}, _ClassName, Result);

      // добавить в список быстрого поиска
      internal_add_class_index(_Class, Result);
      internal_add_class_index_by_name(_ClassName, Result);
      if (arraytypeinfo <> nil) then internal_add_class_index(arraytypeinfo, Result);
    end;
  end;

  // в любом случае заполнить ArrayInfo
  PLuaArrayInfo(ClassesInfo[Result]._Class)^ := Dest;

  // заполнение PropertyInfo элемента массива
  TLuaPropertyInfo(PLuaArrayInfo(ClassesInfo[Result]._Class).ItemInfo).Fill(
                   ClassesInfo[Result], PropertyInfo.Base, nil, nil, nil);
end; *)


function  TLua.RegArray;
asm
  pop ebp
  push offset __TLuaRegArray
  jmp TLua.InternalRegisterRecall
end;

// tpinfo - только typeinfo(Set)
function __TLuaRegSet(Self: TLua; SetTypeInfo: ptypeinfo): PLuaSetInfo;
begin
  Result := nil;
end; (*
const
  MASK_3 = $FF shl 3;
var
  Name: string;
  TypeData: PTypeData;  
  SetInfo: PLuaSetInfo;
begin
  // проверка tpinfo
  if (tpinfo = nil) then
  Self.Assert('TypeInfo of set is not defined');
  if (ptypeinfo(tpinfo).Kind <> tkSet) then Self.Assert('TypeInfo of set is not correct: TypeKind = %s', [TypeKindName(ptypeinfo(tpinfo).Kind)]);

  // имя
  Name := ptypeinfo(tpinfo).Name;

  // поиск имеющегося
  Result := internal_class_index(tpinfo);
  if (Result < 0) then Result := internal_class_index_by_name(Name); 
  if (Result >= 0) and (ClassesInfo[Result]._ClassKind <> ckSet) then Self.Assert('Type "%s" is already registered', [Name]);

  // добавление
  if (Result < 0) then
  begin
    Result := internal_add_class_info();
//    FInitialized := false;

    new(SetInfo);
    with ClassesInfo[Result] do
    begin
      _Class := SetInfo;
      _ClassKind := ckSet;
      _ClassName := Name;
      Ref := internal_register_metatable(nil{CodeAddr}, _ClassName, Result);

      // добавить в список быстрого поиска
      internal_add_class_index(_Class, Result);
      internal_add_class_index_by_name(_ClassName, Result);
      internal_add_class_index(tpinfo, Result);
    end;

    // заполнить поля
    SetInfo.FName := Name;
    SetInfo.FClassIndex := Result;
    SetInfo.FTypeInfo := GetTypeData(tpinfo).{$ifdef fpc}CompType{$else}CompType^{$endif};
    TypeData := GetTypeData(SetInfo.FTypeInfo);
    SetInfo.FLow := TypeData.MinValue;
    SetInfo.FHigh := TypeData.MaxValue;
    if (SetInfo.FTypeInfo.Kind = tkEnumeration) and (not IsTypeInfo_Boolean(SetInfo.FTypeInfo)) then __TLuaRegEnum(Self, SetInfo.FTypeInfo);// Self.RegEnum(SetInfo.FTypeInfo);

    // расчёт размера множества
    with SetInfo^ do
    begin
      {$ifdef fpc}
         if (FHigh > 31) then FSize := 32 else FSize := 4;
         FRealSize := FSize;
         FCorrection := 0;
         FAndMasks := $0000FFFF;
      {$else}
         FSize := (((FHigh+7+1)and MASK_3)-(FLow and MASK_3))shr 3;
         FRealSize := FSize;
         if (FSize = 3) then FSize := 4;
         FCorrection := (FLow and MASK_3);
         pchar(@FAndMasks)[0] := char($FF shr (7 - (FHigh and 7)));
         pchar(@FAndMasks)[1] := char($FF shl (FLow - FCorrection));
      {$endif}
    end;
  end;
end;  *)

function  TLua.RegSet;
asm
  push offset __TLuaRegSet
  jmp TLua.InternalRegisterRecall
end;

// зарегистрировать глобальный метод по его сигнатуре
procedure __TLuaRegGlobalSignatureProc(Lua: TLua; const Signature: LuaString; Address: pointer);
begin

end;

procedure TLua.RegGlobalProc;
asm
  push offset __TLuaRegGlobalSignatureProc
  jmp TLua.InternalRegisterRecall
end;

// зарегистрировать глобальный метод с ручной обработкой
procedure __TLuaRegGlobalRecallProc(Self: TLua; const ProcName: LuaString; RecallProc: TGlobalRecallProc);
begin

  // GlobalNameSpace!!!!
//  __TLuaInternalAddProc(Self, @ClassInfo.FType, Self.CheckIdentifier(ProcName), Address);
end;

procedure TLua.RegGlobalRecallProc;
asm
  push offset __TLuaRegGlobalRecallProc
  jmp TLua.InternalRegisterRecall
end;


// зарегистрировать классовую функцию, используя сигнатуру
procedure __TLuaRegClassSignatureProc(Self: TLua; AClass: TClass; const Signature: LuaString; Address: pointer);
begin

end;

procedure TLua.RegClassProc;
asm
  pop ebp
  push offset __TLuaRegClassSignatureProc
  jmp TLua.InternalRegisterRecall
end;

// зарегистрировать классовую функцию с ручной обработкой
procedure __TLuaRegClassRecallProc(Self: TLua; AClass: TClass; const ProcName: LuaString; const RecallProc: TClassRecallProc; is_class: boolean);
begin

end;

procedure TLua.RegClassRecallProc;
asm
  pop ebp
  push offset __TLuaRegClassRecallProc
  jmp TLua.InternalRegisterRecall
end;


procedure __TLuaRegProperty(const Self: TLua; const AClass: TClass;
                            const PropertyName: LuaString; const tpinfo: pointer;
                            const PGet, PSet: pointer; const is_class: boolean;
                            const parameters: LuaString='');
begin

end;

procedure TLua.RegProperty;
asm
  pop ebp
  push offset __TLuaRegProperty
  jmp TLua.InternalRegisterRecall
end;



(*procedure __TLuaRegProc_global(const Self: TLua; const ProcName: string; const Proc: TLuaProc;
                               const ArgsCount: integer);
begin
  Self.InternalAddProc(true, nil, ProcName, ArgsCount, false, @Proc);
end;

procedure TLua.RegProc(const ProcName: string; const Proc: TLuaProc; const ArgsCount: integer);
asm
  pop ebp
  push offset __TLuaRegProc_global
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRegProc_class(const Self: TLua; const AClass: TClass; const ProcName: string;
          const Proc: TTClassRecallProc; const ArgsCount: integer; const with_class: boolean; const ReturnAddr: pointer);
begin
  if (AClass = nil) then
  ELua.Assert('AClass is not defined', [], ReturnAddr);

  Self.InternalAddProc(true, AClass, ProcName, ArgsCount, with_class, TMethod(Proc).Code);
end;

procedure TLua.RegProc(const AClass: TClass; const ProcName: string; const Proc: TTClassRecallProc; const ArgsCount: integer; const with_class: boolean);
asm
  pop ebp
  push offset __TLuaRegProc_class
  jmp TLua.InternalRegisterRecall
end;
     *)

// зарегистрировать свойство
(*procedure __TLuaRegProperty(const Self: TLua; const AClass: TClass; const PropertyName: string; const tpinfo: pointer;
          const PGet, PSet: pointer; const parameters: PLuaRecordInfo; const default: boolean{; const ReturnAddr: pointer});
begin
  // базовые проверки
  if (AClass = nil) then
  Self.Assert('AClass is not defined');

  if (PGet = nil) and (PSet = nil) then
  Self.Assert('The %s.%s property has no setter and getter', [AClass.ClassName, PropertyName]);

  // регистрация
  Self.InternalAddProperty(true, AClass, PropertyName, tpinfo, false, default, PGet, PSet, parameters);
end;

procedure TLua.RegProperty(const AClass: TClass; const PropertyName: string; const tpinfo: pointer; const PGet, PSet: pointer; const parameters: PLuaRecordInfo; const default: boolean);
asm
  pop ebp
  push offset __TLuaRegProperty
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRegVariable(const Self: TLua; const VariableName: string; const X; const tpinfo: pointer; const IsConst: boolean);
var
  P: pointer;

begin
  P := @X;
  if (P = nil) then
  Self.Assert('Pointer to variable "%s" is not defined', [VariableName]);

  // регистрация
  Self.InternalAddProperty(true, GLOBAL_NAME_SPACE, VariableName, tpinfo, IsConst, false, P, P, nil);
end;

procedure TLua.RegVariable(const VariableName: string; const X; const tpinfo: pointer; const IsConst: boolean);
asm
  pop ebp
  push offset __TLuaRegVariable
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRegConst_variant(const Self: TLua; const ConstName: string; const Value: Variant);
var
  Ref: integer;
  VarData: TVarData absolute Value;
begin
  // проверка
  if (not IsValidIdent(ConstName)) then
  Self.Assert('Invalid constant name "%s"', [ConstName]);

  // доступный ли Variant
  if (VarData.VType <> varString) and (not(VarData.VType in VARIANT_SUPPORT)) then
  Self.Assert('Not supported variant value');

  // регистрация
  Ref := Self.internal_register_global(ConstName, gkConst, nil{ReturnAddr}).Ref;

  // пуш
  if (not Self.push_variant(Value)) then
  Self.Assert('Not supported variant value "%s"', [Self.FBufferArg.str_data]);

  // присвоение
  Self.global_fill_value(Ref);
end;


procedure TLua.RegConst(const ConstName: string; const Value: Variant);
asm
  push offset __TLuaRegConst_variant
  jmp TLua.InternalRegisterRecall
end;

procedure __TLuaRegConst_luaarg(const Self: TLua; const ConstName: string; const Value: TLuaArg);
var
  Ref: integer;
begin
  // проверка
  if (not IsValidIdent(ConstName)) then
  Self.Assert('Invalid constant name "%s"', [ConstName]);

  // доступный ли Value
  if (byte(Value.LuaType) >= byte(ltTable)) then
  Self.Assert('Not supported argument value');

  // регистрация
  Ref := Self.internal_register_global(ConstName, gkConst, nil{ReturnAddr}).Ref;

  // пуш
  if (not Self.push_luaarg(Value)) then
  Self.Assert('Not supported argument value "%s"', [Self.FBufferArg.str_data]);

  // присвоение
  Self.global_fill_value(Ref);
end;

procedure TLua.RegConst(const ConstName: string; const Value: TLuaArg);
asm
  push offset __TLuaRegConst_luaarg
  jmp TLua.InternalRegisterRecall
end;    *)

procedure __TLuaRegEnum(Self: TLua; EnumTypeInfo: ptypeinfo);
begin

end;
(*var
  i, Ref: integer;
  S: string;
begin
  with Self do
  begin
    // если в списке такой уже имеется
    if (InsortedPos4(integer(EnumTypeInfo), EnumerationList) >= 0) then exit;

    // проверка
    if (EnumTypeInfo = nil) then
    Self.Assert('EnumTypeInfo is not defined');

    if (EnumTypeInfo.Kind <> tkEnumeration) or (IsTypeInfo_Boolean(EnumTypeInfo)) then
    Self.Assert('Type "%s" (kind: %s) is not enumeration',
               [EnumTypeInfo.Name, TypeKindName(EnumTypeInfo.Kind)]);


    // добавить в список EnumerationList
    i := InsortedPlace4(integer(EnumTypeInfo), pointer(EnumerationList), Length(EnumerationList));
    ptypeinfo(DynArrayInsert(EnumerationList, typeinfo(TIntegerDynArray), i)^) := EnumTypeInfo;

    // регистрация каждого enum-а
    with GetTypeData(EnumTypeInfo)^ do
    for i := MinValue to MaxValue do
    begin
      S := GetEnumName(EnumTypeInfo, byte(i));

      Ref := internal_register_global(S, gkConst, nil{ReturnAddr}).Ref;
      lua_pushinteger(Handle, i);
      global_fill_value(Ref);
    end;
  end;
end;   *)

procedure TLua.RegEnum(const EnumTypeInfo: ptypeinfo);
asm
  push offset __TLuaRegEnum
  jmp TLua.InternalRegisterRecall
end;

// внутренняя неафишируемая функция, регистрирующая (или ищущая) факт существования интерфейса.
// нужно это потому, библиотека во-первых оперирует так же интерфейсами,
// во вторых интерфейсы скорее всего будут иметь/накапливать методы. Так что хз
// todo
function __TLuaInternalRegInterface(Self: TLua; tpinfo: ptypeinfo): __PLuaInterfaceInfo;
begin
  Result := nil;
end;

// распознать параметры метода по сигнатуре
function __TLuaInternalRegMethod(Self: TLua; const SIGNATURE: LuaString; OfObject: boolean): __PLuaMethodInfo; overload;
begin
  Result := nil;
end;

// распознать параметры метода по typeinfo: событие или tkProcedure (D >= 2010)
function __TLuaInternalRegMethod(Self: TLua; tpinfo: ptypeinfo): __PLuaMethodInfo; overload;
begin
  Result := nil;
end;


{ TLuaModule }


procedure __TLuaModuleGetLine(const Self: TLuaModule; const AIndex: integer;
                              var Result: LuaString; const CodeAddr: pointer);
begin
  with Self do
  begin
    if (dword(AIndex) >= dword(FLinesCount)) then
    {$ifdef NO_CRYSTAL}TExcept{$else}EWrongParameter{$endif}.Assert('Can''t get line %d from module "%s". Lines count = %d', [AIndex, Name, FLinesCount], CodeAddr);
  end;

  with Self.FLinesInfo[AIndex] do
  __UnpackString(Chars, Result, Length);
end;

function TLuaModule.GetLine(AIndex: integer): LuaString;
asm
  push [esp]
  jmp __TLuaModuleGetLine
end;

procedure __TLuaModuleGetLineInfo(const Self: TLuaModule; const AIndex: integer;
                              var Result: TLuaModuleLineInfo; const CodeAddr: pointer);
begin
  with Self do
  begin
    if (dword(AIndex) >= dword(FLinesCount)) then
    {$ifdef NO_CRYSTAL}TExcept{$else}EWrongParameter{$endif}.Assert('Can''t get line %d from module "%s". Lines count = %d', [AIndex, Name, FLinesCount], CodeAddr);

    Result := FLinesInfo[AIndex];
  end;
end;

function TLuaModule.GetLineInfo(AIndex: integer): TLuaModuleLineInfo;
asm
  push [esp]
  jmp __TLuaModuleGetLineInfo
end;

// эта функция нужна тогда, когда нужно представить "слепок модуля"
// по нескольким строкам, с указанием конкретной строки.
// причём в качестве результата возвращается внутренняя строка string!
//
// очень важно при этом понимать, что в модулях данные хранятся во внутреннем формате: ansi или utf8
// string в свою очередь тоже может быть Ansi, а может быть Utf16 !
//
// а нужна эта функция только в двух случаях. И оба связаны с внутренним типом Exception:
// - либо во время выполнения скрипта возник Exception и его нужно детализировать
// - либо возникла ошибка скрипта и её нужно представить в виде ELuaScript
function TLuaModule.GetCodeString(const Line: integer; const AsUpdate: integer): string;
const
  LINE_POINT: __luabuffer = '-->> ';
  HEADERS: array[boolean{realcode}] of __luabuffer = ('code (another version!!!):', 'code:');
  HEADERS_SIZES: array[boolean{realcode}] of integer = (26, 5);
var
  realcode: boolean;
  MinLine, MaxLine: integer;
  NumberLength, BufferLength, i: integer;
  S: ShortString;
  buffer: __luabuffer {$if defined(LUA_ANSI) and (not defined(UNICODE))}absolute Result{$ifend};
  current: __luadata;
  __S: __luadata;

  {$ifdef LUA_UNICODE}
     ResultLength: integer;
  {$endif}

  function CleanLine(const L: integer): boolean;
  var
    i: integer;
  begin
    Result := false;

    with Self.FLinesInfo[L] do
    for i := 0 to Length-1 do
    if (Chars[i] > #32) then exit;

    Result := true;
  end;

  procedure Write(const Chars: __luadata; const Length: integer);
  begin
    Move(Chars^, current^, Length);
    inc(current, Length);
  end;
begin
  if (dword(Line) < dword(FLinesCount)) then Result := ''
  else
  begin
    Result := '(can''t retrieve code)';
    exit;
  end;

  // этот флаг отвечает за написание:
  // "code:" или "code (another version!!!):"
  realcode := (Self.FUpdate=AsUpdate);

  // определяем с какой по какую линию
  MinLine := Line-2; if (MinLine < 0) then MinLine := 0;
  while (MinLine <> Line) and (CleanLine(MinLine)) do inc(MinLine);
  MaxLine := Line+2; if (MaxLine >= Self.FLinesCount) then MaxLine := Self.FLinesCount-1;
  while (MaxLine <> Line) and (CleanLine(MaxLine)) do dec(MaxLine);

  // определяем длинну номера в символах
  Str(MaxLine, S);
  NumberLength := Length(S);

  // определяем результирующую длинну буфера
  BufferLength := HEADERS_SIZES[realcode] + {#13Number: }(MaxLine-MinLine+1)*(3+NumberLength) + Length(LINE_POINT);
  for i := MinLine to MaxLine do
  inc(BufferLength, Self.FLinesInfo[i].Length);

  // теперь заполняем буфер
  __S := __luadata(@S);
  SetLength(buffer, BufferLength);
  current := pointer(buffer);
  Write(pointer(HEADERS[realcode]), HEADERS_SIZES[realcode]);
  for i := MinLine to MaxLine do
  begin
    // '#13Number: '
    Str(i:NumberLength, S);
    __S[0] := #13;
    __S[NumberLength+1] := ':';
    __S[NumberLength+2] := #32;
    Write(__S, 3+NumberLength);

    // -->>
    if (i = Line) then Write(pointer(LINE_POINT), Length(LINE_POINT));

    // Module[i]
    with FLinesInfo[i] do
    Write(Chars, Self.FLinesInfo[i].Length);
  end;


  // теперь надо распаковать LuaString в string
  {$ifdef LUA_UNICODE}
     // buffer: Utf8String;
     ResultLength := Utf8Length(pointer(buffer), BufferLength);
     SetLength(Result, ResultLength);

     {$ifdef UNICODE}
        // string = UnicodeString
        Utf16FromUtf8(pointer(Result), ResultLength, pointer(buffer), BufferLength);
     {$else}
        // string = AnsiString
        AnsiFromUtf8(pointer(Result), ResultLength, pointer(buffer), BufferLength);
     {$endif}

  {$else .LUA_ANSI}
     // buffer: AnsiString;
     {$ifdef UNICODE}
        // string = UnicodeString
        SetLength(Result, BufferLength);
        Utf16FromAnsi(pointer(Result), pointer(buffer), BufferLength);
     {$else}
        // string = AnsiString
        // на этот случай уже всё сделано
     {$endif}
  {$endif}
end;

procedure TLuaModule.SaveToStream(const Stream: TStream);
{$ifdef LUA_INICODE}
var
  BOM: integer;
{$endif}
begin
{$ifdef LUA_INICODE}
  BOM := BOM_UTF8;
  Stream.Write(BOM, 3);
{$endif}

  // либо Ansi, либо Utf8 символы
  Stream.Write(pointer(integer(pointer(FBuffer))+FBufferOffset)^, Length(FBuffer)-FBufferOffset);
end;

procedure TLuaModule.SaveToFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TLuaModule.SaveToFile;
begin
  SaveToFile(FileName);
end;

// функция занимается распределением буфера на отдельные линиии
// если возвращает true - значит не пустой
function TLuaModule.InitializeLines(): boolean;
var
  i, Len: integer;
  Memory: __luadata;
  MemSize: integer;

  BufferedLinesCount: integer;
begin
  Result := false;
  Memory := pointer(FBuffer);
  MemSize := Length(FBuffer);
  inc(Memory, FBufferOffset);
  dec(MemSize, FBufferOffset);
  if (MemSize = 0) then exit;

  // тест пустого
  for i := 0 to MemSize-1 do
  if (Memory[i] > #32) then
  begin
    Result := true;
    break;
  end;

  // раскидывание по линиям
  // FLinesCount и FLinesInfo сейчас пустые
  BufferedLinesCount := 0;
  while (MemSize > 0) do
  begin
    // определение длинны линии
    Len := MemSize;
    for i := 0 to MemSize-1 do
    if (Memory[i] in [#10, #13]) then
    begin
      Len := i; // длинна
      break;
    end;

    // добавление линии
    inc(FLinesCount);
    if (FLinesCount > BufferedLinesCount) then
    begin
      inc(BufferedLinesCount, 128);
      SetLength(FLinesInfo, BufferedLinesCount);
    end;
    with FLinesInfo[FLinesCount-1] do
    begin
      if (Len = 0) then Chars := nil else Chars := Memory;
      Length := Len;
    end;

    // смещение Memory, MemSize
    inc(Memory, Len);
    dec(MemSize, Len);

    // смещение #13 #10
    if (Memory^ = #13) and (Memory[1] = #10) then Len := 2 else Len := 1;
    inc(Memory, Len);
    dec(MemSize, Len);
  end;                

  // финализация размера FLinesInfo
  if (FLinesCount <> BufferedLinesCount) then
  SetLength(FLinesInfo, FLinesCount);
end;


// (last version !!!!)
// выполнить препроцессинг скрипта
// на данный момент это только замена точек на двоеточие
//
// todo
// пропроцессинг должен быть значительно умнее !!!!!!
procedure PreprocessScript(var Memory: __luabuffer);
const
  SPACES = [#32, #9];
  ENTER = [#13, #10];
  IGNORS = SPACES + ENTER;
  STR_PREPS = ['!','?','.',',','"','''','`',':',';','#','№','$','%','&','(',')',
               '[',']','{','}','/','|','\','~','^','*','+','-','<','=','>'] - ['_'];
  STD_NAME_SPACES: array[0..7] of string = ('coroutine', 'package', 'string', 'table', 'math', 'io', 'os', 'debug');
  STD_HASHES: array[0..7] of integer = ($5C958D0E, $61FFBC58, $37EF079, $1FF4007F, $4C000063, $4000046E, $40000457, $3E6C006F);

  // определить "объект" перед точкой и сравнить на стандартный неймспейс
  function TestStdNameSpace(P: integer): boolean;
  var
    obj, i: integer;
    C: char;
    S: string;
  begin
    Result := false;

    obj := 0;
    for i := P downto 1 do
    begin
      C := Memory[i];

      if (obj <> 0) then
      begin
        if (C in (STR_PREPS + IGNORS)) then
        begin
          S := Copy(Memory, i+1, obj-i);
          P := IntPos(StringHash(S), pointer(@STD_HASHES), Length(STD_HASHES));

          Result := (P >= 0) and (SameStrings(S, STD_NAME_SPACES[P]));
          exit;
        end;
      end else
      if (not (C in IGNORS)) then
      begin
        obj := i;
        if (C in STR_PREPS) then exit;
      end;
    end;
  end;

var
  C: char;
  FuncFound, IgnoresFound: boolean;
  P, i: integer;
begin
  P := 0;
 
  while true do
  begin
    P := CharPosEx('(', Memory, P+1);
    if (P = 0) then break;

    FuncFound := false;
    IgnoresFound := false;
    for i := P-1 downto 1 do
    begin
      C := Memory[i];

      if (FuncFound) then
      begin
        if (C = '.') then
        begin
          if (i <> 1) and (Memory[i-1] <> '.'{оператор ..})
          and (not TestStdNameSpace(i-1)) then Memory[i] := ':'; {+Unique}

          break;
        end;

        if (not IgnoresFound) then
        begin
          IgnoresFound := (C in IGNORS);
          if (IgnoresFound) then continue;
        end;

        // если найден знак (и это не точка) то закончить цикл
        if (C in STR_PREPS) then break;

        // если найден не знак и не пропускаемый символ (хотя пропускаемые уже встречались) - значит это была неклассовая функция 
        if (IgnoresFound) and (not (C in IGNORS)) then break;

        continue;
      end else
      begin
        FuncFound := not(C in SPACES);
        if (FuncFound{не пробел}) and (C in (STR_PREPS + ENTER)) {операторы или другая строка} then break;
        continue;
      end;

      break;
    end;
  end;
end;


// фукнция предназначена для "компиляции" модуля
// но предварительно скрипт проходит сложную процедуру препроцессинга!
function TLuaModule.Compile(): integer;
var
  CW: word;
  buffer: __luabuffer;
begin
  // здесь должен быть крутой препроцессинг скрипта
  // сейчас пока так
  buffer := FBuffer;
  if (FBufferOffset <> 0) then Delete(buffer, 1, FBufferOffset);

  // !!!!!!!!!!!!!!!!!!!!!!
  // TODO !!!!!!!!!!!!!!!!!
  // !!!!!!!!!!!!!!!!!!!!!!
  PreprocessScript(buffer);


  // буфер сформирован, компилируем
  // с правильным указанием имени чанка
  begin
    CW := Get8087CW();
    Set8087CW($037F {default intel C++ mode});
    try
      Result := luaL_loadbuffer(Lua.Handle, pansichar(buffer), Length(buffer),
                pansichar(AnsiStringFormat('__%d_%d__', [Self.Index, Self.FUpdate])));
    finally
      Set8087CW(CW);
    end;
  end;
end;







initialization
  {$ifdef NO_CRYSTAL}
     InitializeDefaultCodePage();
  {$endif}
  {$ifdef MSWINDOWS}
     SYS_EXCEPTION_PROC := System.RaiseExceptionProc;
  {$else}
     {$MESSAGE 'maybe we need to realize Script IDE Exception System smart for current OS'}
  {$endif}
  {$ifdef LUA_INITIALIZE} Lua := CreateLua(); {$endif}

finalization
  {$ifdef LUA_INITIALIZE} FreeAndNil(Lua); {$endif}
  FreeLuaHandle();


end.
