{ ***************************ver. 25/03/2013 **************************** }
{                                                                         }
{ CrystalLUA is a small part of large casual game engine CrystalEngine,   }
{ that simplifies the interaction of LUA language and Delphi code.        }
{                                                                         }
{ Copyright: Dmitry Mozulyov (aka Devil)                                  }
{ email: softforyou@inbox.ru                                              }
{ icq: 250481638                                                          }
{ *********************************************************************** }

unit CrystalLUA;



// если выставлен этот флаг, то Lua: TLua инициализируется и удаляется автоматически
//{$define LUA_INITIALIZE}

// использовать ли Crystal-библиотеку SysUtilsEx
{$define NO_CRYSTAL}

// оптимизация и отключение лишних проверок
{$define critical_code}



// опции компилятора
{$ifdef NO_CRYSTAL}
  {$ifdef fpc}
    {$mode delphi}
    {$asmmode intel}
  {$endif}
  {$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$MINENUMSIZE 1}
  {$ifndef VER140}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$endif}
  {$ifdef critical_code}
    {$O+}{$R-}{$I-}{$Q-}
  {$endif}
  {$ifdef MSWINDOWS}{$define WINDOWS}{$endif} 
{$else}
  {$I crystal_options.inc}
{$endif}

interface
  uses SysUtils, TypInfo, {$ifdef WINDOWS}Windows,{для дампов}{$endif}
       {$ifdef fpc}Variants,{$endif}
       {$ifdef NO_CRYSTAL}Types, Classes{$else}SysUtilsEx{$endif};

type
  TLua = class;
  PLuaArg = ^TLuaArg;
  PLuaTable = ^TLuaTable;

  {$ifdef NO_CRYSTAL}
  TExcept = class(Exception)
  public
    class procedure Assert(const Message: AnsiString; const CodeAddr: pointer = nil); overload;
    class procedure Assert(const FmtStr: AnsiString; const Args: array of const; const CodeAddr: pointer = nil); overload;
  end;
  {$endif}

  // ошибки, связанные с неправильным использованием данных TLua
  ELua = class(TExcept);

  // ошибки, сгенерированные внутри скрипта
  ELuaScript = class(ELua);

  // типы, по которым идёт взаимодействие между lua и кодом
  TLuaArgType = (ltEmpty, ltBoolean, ltInteger, ltDouble, ltString, ltPointer, // <-- простые типы
                 ltClass, ltObject, ltRecord, ltArray, ltSet, ltTable {<-- сложные типы} );

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

  // взаимодействие по структурам между lua и программой
  PLuaRecordInfo = ^TLuaRecordInfo; // описание TLuaRecordInfo внизу из-за завязки на TLuaClassProc
  TLuaRecord = object(__lua_difficult_type__)
  public
    Info: PLuaRecordInfo;
  end;
  PLuaRecord = ^TLuaRecord;

  // необходимо для динамических и статических массивов
  PLuaArrayInfo = ^TLuaArrayInfo; // описание TLuaArrayInfo внизу
  TLuaArray = object(__lua_difficult_type__)
  public
    Info: PLuaArrayInfo;
  end;
  PLuaArray = ^TLuaArray;

  // множества (Set of )
  PLuaSetInfo = ^TLuaSetInfo; // описание TLuaSetInfo внизу
  TLuaSet = object(__lua_difficult_type__)
  public
    Info: PLuaSetInfo;
  end;
  PLuaSet = ^TLuaSet;

  // аргуметы вызова
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


  // интерфейс для перебора всех элементов в таблице: Key, Value
  TLuaPair = object
  private
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

  
  // интерфейс для работы с таблицами внутри lua
  TLuaTable = object
  private
    {$hints off}none: byte; {TLuaArgType = ltTable} {$hints on}
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


  // ссылка
  // создана для быстрого оперирования глобальными объектами, заточенными под Lua
  TLuaReference = class
  private
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



  { альтернативные калбеки, которые можно преобразовать функциями LuaProc и LuaClassProc}
  TLuaProc = function(const Args: TLuaArgs): TLuaArg;
  TLuaProc0 = TLuaProc;
  TLuaProc1 = function (const Arg: TLuaArg): TLuaArg;
  TLuaProc2 = procedure(const Args: TLuaArgs; var Result: TLuaArg);
  TLuaProc3 = procedure(const Arg: TLuaArg; var Result: TLuaArg);
  TLuaProc4 = procedure(const Args: TLuaArgs);
  TLuaProc5 = procedure(const Arg: TLuaArg);
  TLuaProc6 = procedure();
  TLuaClassProc = function(const Args: TLuaArgs): TLuaArg of object;
  TLuaClassProc0 = TLuaClassProc;
  TLuaClassProc1 = function (const Arg: TLuaArg): TLuaArg of object;
  TLuaClassProc2 = procedure(const Args: TLuaArgs; var Result: TLuaArg) of object;
  TLuaClassProc3 = procedure(const Arg: TLuaArg; var Result: TLuaArg) of object;
  TLuaClassProc4 = procedure(const Args: TLuaArgs) of object;
  TLuaClassProc5 = procedure(const Arg: TLuaArg) of object;
  TLuaClassProc6 = procedure() of object;
  TLuaClassProc7 = function (const AObject: TObject; const Args: TLuaArgs): TLuaArg;  
  TLuaClassProc8 = function (const AObject: TObject; const Arg: TLuaArg): TLuaArg;
  TLuaClassProc9 = procedure(const AObject: TObject; const Args: TLuaArgs; var Result: TLuaArg);
  TLuaClassProc10 = procedure(const AObject: TObject; const Arg: TLuaArg; var Result: TLuaArg);
  TLuaClassProc11 = procedure(const AObject: TObject; const Args: TLuaArgs);
  TLuaClassProc12 = procedure(const AObject: TObject; const Arg: TLuaArg);
  TLuaClassProc13 = procedure(const AObject: TObject);
  TLuaClassProc14 = function (var X; const Args: TLuaArgs): TLuaArg;
  TLuaClassProc15 = function (var X; const Arg: TLuaArg): TLuaArg;
  TLuaClassProc16 = procedure(var X; const Args: TLuaArgs; var Result: TLuaArg);
  TLuaClassProc17 = procedure(var X; const Arg: TLuaArg; var Result: TLuaArg);
  TLuaClassProc18 = procedure(var X; const Args: TLuaArgs);
  TLuaClassProc19 = procedure(var X; const Arg: TLuaArg);
  TLuaClassProc20 = procedure(var X);
  TLuaClassProc21 = function (const AClass: TClass; const Args: TLuaArgs): TLuaArg;
  TLuaClassProc22 = function (const AClass: TClass; const Arg: TLuaArg): TLuaArg;
  TLuaClassProc23 = procedure(const AClass: TClass; const Args: TLuaArgs; var Result: TLuaArg);
  TLuaClassProc24 = procedure(const AClass: TClass; const Arg: TLuaArg; var Result: TLuaArg);
  TLuaClassProc25 = procedure(const AClass: TClass; const Args: TLuaArgs);
  TLuaClassProc26 = procedure(const AClass: TClass; const Arg: TLuaArg);
  TLuaClassProc27 = procedure(const AClass: TClass);

  // операторы
  TLuaOperator = (loNeg, loAdd, loSub, loMul, loDiv, loMod, loPow, loCompare);
  TLuaOperators = set of TLuaOperator;
  TLuaOperatorCallback = procedure(var _Result, _X1, _X2; const Kind: TLuaOperator);
  { <<-- call-back функции. + вспомогательные эквиваленты ----------- ------------}

  
  // информация о структуре и её полях
  // описание здесь, а не наверху - из-за завязки на TLuaClassProc
  TLuaRecordInfo = object
  private
    FLua: TLua;
    FClassIndex: integer;
    FTypeInfo: ptypeinfo;
    FName: string;
    FSize: integer;
    FOperators: TLuaOperators;
    FOperatorCallback: TLuaOperatorCallback;

    function  GetFieldsCount: integer;
    procedure InternalRegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer; const CodeAddr: pointer);
    procedure SetOperators(const Value: TLuaOperators);
    procedure SetOperatorCallback(const Value: TLuaOperatorCallback);
  public
    procedure RegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer); overload;
    procedure RegField(const FieldName: string; const FieldPointer: pointer; const tpinfo: pointer; const pRecord: pointer = nil); overload;
    procedure RegProc(const ProcName: string; const Proc: TLuaClassProc; const ArgsCount: integer=-1);

    property Name: string read FName;
    property Size: integer read FSize;
    property FieldsCount: integer read GetFieldsCount;
    property Operators: TLuaOperators read FOperators write SetOperators;
    property OperatorCallback: TLuaOperatorCallback read FOperatorCallback write SetOperatorCallback;
  end;

  // информация о массиве
  TLuaArrayInfo = object
  private
    // основные
    FName: string;
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
    FSize: integer; // размер такого массива целиком. для динамических - 4
  public
    property Name: string read FName;
    property IsDynamic: boolean read FIsDynamic;
    property Bounds: pinteger read FBounds;
    property Dimention: integer read FDimention;
  end;

  // информация о множестве
  TLuaSetInfo = object
  private
    FName: string;
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
  public
    property Name: string read FName;
    property Size: integer read FSize;
    property Low: integer read FLow;
    property High: integer read FHigh;
  end;


  // внутренняя рутина для калбеков ------------------------------------------
  {|}   { информация по процедуре }
  {|}   TLuaProcInfo = record
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
  {|}   TLuaPropertyKind = (pkUnknown, pkBoolean, pkInteger, pkInt64, pkFloat,
  {|}                       pkObject, pkString, pkVariant, pkInterface,
  {|}                       pkPointer, pkClass, pkRecord, pkArray, pkSet, pkUniversal);
  {|}
  {|}   // разновидности типов булеанов
  {|}   TLuaPropBoolType = (btBoolean, btByteBool, btWordBool, btLongBool);
  {|}
  {|}   // разновидности "строк"
  {|}   TLuaPropStringType = (stShortString, stAnsiString, stWideString, {todo UnicodeString?,} stAnsiChar, stWideChar);
  {|}
  {|}   // базовая информация
  {|}   TLuaPropertyInfoBase = packed record
  {|}     Information: pointer; // typeinfo или вспомогательная информация: по структурами, массивами и множествами
  {|}     Kind: TLuaPropertyKind; // тип свойства
  {|}     case Integer of
  {|}       0: (OrdType: TypInfo.TOrdType);
  {|}       1: (FloatType: TypInfo.TFloatType);
  {|}       2: (StringType: TLuaPropStringType; str_max_len: byte {для shortstring-ов});
  {|}       3: (BoolType: TLuaPropBoolType);
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
  {|}   TLuaGlobalKind = (gkType, gkVariable, gkProc, gkConst, gkLuaData);
  {|}   TLuaGlobalVariable  = packed record
  {|}     _Name: string;
  {|}     _Kind: TLuaGlobalKind;
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
  {|}     function  InternalAddName(const Name: string; const AsProc: boolean; var Initialized: boolean; const CodeAddr: pointer): integer;
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
  {|}   end;
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
  {|}       3: (PropertyInfo: PLuaPropertyInfo); // временный. поэтому используется указатель
  {|}   end;
  {|}   PLuaUserData = ^TLuaUserData;
  {|}
  {|}   TLuaResultBuffer = object
  {|}   private
  {|}     Memory: pointer;
  {|}     Size: integer;
  {|}     items_count: integer;
  {|}     tpinfo: ptypeinfo;
  {|}
  {|}     procedure Finalize(const free_mem: boolean=false);
  {|}   public
  {|}     function  AllocRecord(const RecordInfo: PLuaRecordInfo): pointer;
  {|}     function  AllocArray(const ArrayInfo: PLuaArrayInfo): pointer;
  {|}     function  AllocSet(const SetInfo: PLuaSetInfo): pointer;
  {|}   end;
  // <<-- внутренняя рутина для калбеков -------------------------------------


  TLuaUnitLineInfo = record
    Str: pchar;
    Length: integer;
  end;
  TLuaUnitLineInfoDynArray = array of TLuaUnitLineInfo;

  
  TLuaUnit = class(TObject)
  private
    FName: string;
    FFileName: string;
    FText: string;
    FLinesCount: integer;
    FLinesInfo: TLuaUnitLineInfoDynArray;

    procedure InitializeLinesInfo();
    function GetLine(index: integer): string;
    function GetLineInfo(index: integer): TLuaUnitLineInfo;
  public
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const FileName: string); overload;
    procedure SaveToFile(); overload;

    property Name: string read FName;
    property FileName: string read FFileName;
    property Text: string read FText;
    property LinesCount: integer read FLinesCount;
    property Lines[index: integer]: string read GetLine; default;
    property LinesInfo[index: integer]: TLuaUnitLineInfo read GetLineInfo; 
  end;
  TLuaUnitDynArray = array of TLuaUnit;



  TLua = class(TObject)
  private
    // низкоуровневый блок
    FHandle: pointer;
    FPreprocess: boolean;
    FBufferArg: TLuaArg;
    FResultBuffer: TLuaResultBuffer;
    FReferences: TLuaReferenceDynArray; // список ссылок (LUA_REGISTRYINDEX)
    FUnitsCount: integer;
    FUnits: TLuaUnitDynArray;
    procedure Check(const ret: integer; const CodeAddr: pointer; AUnit: TLuaUnit=nil); // проверить на ошибки
    procedure InternalLoadScript(var Memory: string; const UnitName, FileName: string; CodeAddr: pointer);
    function  InternalCheckArgsCount(PArgs: pinteger; ArgsCount: integer; const ProcName: string; const AClass: TClass): integer;
    function  StackArgument(const Index: integer): string;
    function  GetUnit(const index: integer): TLuaUnit;
    function  GetUnitByName(const Name: string): TLuaUnit;

    // сложные пуши
    function  push_userdata(const ClassInfo: TLuaClassInfo; const gc_destroy: boolean; const Data: pointer): PLuaUserData;
    function  push_difficult_property(const Instance: pointer; const PropertyInfo: TLuaPropertyInfo): PLuaUserData;
    function  push_variant(const Value: Variant): boolean;
    function  push_luaarg(const LuaArg: TLuaArg): boolean;
    function  push_argument(const Value: TVarRec): boolean;

    // взаимодействие со стеком
    procedure stack_pop(const count: integer=1);
    function  stack_variant(var Ret: Variant; const StackIndex: integer): boolean;
    function  stack_luaarg(var Ret: TLuaArg; const StackIndex: integer; const lua_table_available: boolean): boolean;
  private
    // глобальное пространство
    // глобальные процедуры, переменные, калбеки глобальных переменных из Lua
    FRef: integer;
    GlobalNative: TLuaClassInfo; // нативные: методы и перменные
    GlobalVariables: TLuaGlobalVariableDynArray; // полный список включая Lua-переменные
    property  NameSpaceHash: TLuaHashIndexDynArray read GlobalNative.NameSpace; // Hash по всем глобальным переменным и функциям

    // работа с глобальной луа-таблицей
    procedure global_alloc_ref(var ref: integer);
    procedure global_free_ref(var ref: integer);
    procedure global_fill_value(const ref: integer);
    procedure global_push_value(const ref: integer);

    // найти глобальную переменную. если false, то Index - place в hash списке глобальных имён
    function  GlobalVariablePos(const Name: pchar; const NameLength: integer; var Index: integer; const auto_create: boolean=false): boolean;
  private
    // инициализация, информация по классам
    FInitialized: boolean;
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

    procedure INITIALIZE_NAME_SPACE();
    function  internal_class_index(AClass: pointer; const look_class_parents: boolean = false): integer;
    function  internal_class_index_by_name(const AName: string): integer;
    function  internal_add_class_info(const is_global_space: boolean = false): integer;
    function  internal_add_class_index(const AClass: pointer; const AIndex: integer): integer;
    function  internal_add_class_index_by_name(const AName: string; const AIndex: integer): integer;
    function  internal_register_global(const Name: string; const Kind: TLuaGlobalKind; const CodeAddr: pointer): PLuaGlobalVariable;
    function  internal_register_metatable(const CodeAddr: pointer; const GlobalName: string=''; const ClassIndex: integer = -1; const is_global_space: boolean = false): integer;
    function  InternalAddClass(AClass: TClass; UsePublished: boolean; const CodeAddr: pointer): integer;
    function  InternalAddRecord(const Name: string; tpinfo, CodeAddr: pointer): integer;
    function  InternalAddArray(Identifier, itemtypeinfo, CodeAddr: pointer; const ABounds: array of integer): integer;
    function  InternalAddSet(tpinfo, CodeAddr: pointer): integer;
    function  InternalAddProc(const IsClass: boolean; AClass: pointer; const ProcName: string; ArgsCount: integer; const with_class: boolean; Address, CodeAddr: pointer): integer;
    function  InternalAddProperty(const IsClass: boolean; AClass: pointer; const PropertyName: string; tpinfo: ptypeinfo; const IsConst, IsDefault: boolean; const PGet, PSet, Parameters, CodeAddr: pointer): integer;

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
    function __array_dynamic_resize(): integer;
    function __array_include(const mode: integer{constructor, include, concat}): integer;
    function __set_method(const is_construct: boolean; const method: integer{0..2}): integer;
    function  ProcCallback(const ClassInfo: TLuaClassInfo; const ProcInfo: TLuaProcInfo): integer;
  private
    FArgs: TLuaArgs;
    FArgsCount: integer;

    function  GetRecordInfo(const Name: string): PLuaRecordInfo;
    function  GetArrayInfo(const Name: string): PLuaArrayInfo;
    function  GetSetInfo(const Name: string): PLuaSetInfo;
    function  GetVariable(const Name: string): Variant;
    procedure SetVariable(const Name: string; const Value: Variant);
    function  GetVariableEx(const Name: string): TLuaArg;
    procedure SetVariableEx(const Name: string; const Value: TLuaArg);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GarbageCollection();
    procedure SaveNameSpace(const FileName: string); dynamic;
    function CreateReference(const global_name: string=''): TLuaReference;
    class function GetProcAddress(const ProcName: pchar; const throw_exception: boolean = false): pointer; // низкий уровень. адрес функции lua.dll

    // загрузка и запуск скриптов
    procedure RunScript(const Script: string);
    procedure LoadScript(const FileName: string); overload;
    procedure LoadScript(const ScriptBuffer: pointer; const ScriptBufferSize: integer; const UnitName: string=''); overload;

    // проверка при калбеке
    procedure ScriptAssert(const FmtStr: string; const Args: array of const); // вызвать Exception из Lua
    function  CheckArgsCount(const ArgsCount: array of integer; const ProcName: string=''; const AClass: TClass=nil): integer; overload;
    function  CheckArgsCount(const ArgsCount: TIntegerDynArray; const ProcName: string=''; const AClass: TClass=nil): integer; overload;
    procedure CheckArgsCount(const ArgsCount: integer; const ProcName: string=''; const AClass: TClass=nil); overload;

    // вызовы
    function VariableExists(const Name: string): boolean;
    function ProcExists(const ProcName: string): boolean;
    function Call(const ProcName: string; const Args: TLuaArgs): TLuaArg; overload;
    function Call(const ProcName: string; const Args: array of const): TLuaArg;  overload;

    // регистрация
    procedure RegClass(const AClass: TClass; const use_published: boolean = true);
    procedure RegClasses(const AClasses: array of TClass; const use_published: boolean = true);
    function  RegRecord(const Name: string; const tpinfo: ptypeinfo): PLuaRecordInfo;
    function  RegArray(const Identifier: pointer; const itemtypeinfo: pointer; const Bounds: array of integer): PLuaArrayInfo;
    function  RegSet(const tpinfo: ptypeinfo): PLuaSetInfo;
    procedure RegProc(const ProcName: string; const Proc: TLuaProc; const ArgsCount: integer=-1); overload;
    procedure RegProc(const AClass: TClass; const ProcName: string; const Proc: TLuaClassProc; const ArgsCount: integer=-1; const with_class: boolean=false); overload;
    procedure RegProperty(const AClass: TClass; const PropertyName: string; const tpinfo: pointer; const PGet, PSet: pointer; const parameters: PLuaRecordInfo=nil; const default: boolean=false);
    procedure RegVariable(const VariableName: string; const X; const tpinfo: pointer; const IsConst: boolean = false);
    procedure RegConst(const ConstName: string; const Value: Variant); overload;
    procedure RegConst(const ConstName: string; const Value: TLuaArg); overload;
    procedure RegEnum(const EnumTypeInfo: ptypeinfo); 

    // вспомогательные свойства
    property ResultBuffer: TLuaResultBuffer read FResultBuffer;
    property Variable[const Name: string]: Variant read GetVariable write SetVariable;
    property VariableEx[const Name: string]: TLuaArg read GetVariableEx write SetVariableEx;
    property RecordInfo[const Name: string]: PLuaRecordInfo read GetRecordInfo;
    property ArrayInfo[const Name: string]: PLuaArrayInfo read GetArrayInfo;
    property SetInfo[const Name: string]: PLuaSetInfo read GetSetInfo;

    // основные свойства
    property Handle: pointer read FHandle;
    property Args: TLuaArgs read FArgs;
    property ArgsCount: integer read FArgsCount;

    // загруженные модули
    property UnitsCount: integer read FUnitsCount;
    property Units[const index: integer]: TLuaUnit read GetUnit;
    property UnitByName[const Name: string]: TLuaUnit read GetUnitByName;
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

function LuaProc(const Proc: TLuaProc0): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc1): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc2): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc3): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc4): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc5): TLuaProc; overload;
function LuaProc(const Proc: TLuaProc6): TLuaProc; overload;
function LuaClassProc(const Proc: TLuaClassProc0): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc1): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc2): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc3): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc4): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc5): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc6): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc7): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc8): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc9): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc10): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc11): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc12): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc13): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc14): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc15): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc16): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc17): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc18): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc19): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc20): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc21): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc22): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc23): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc24): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc25): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc26): TLuaClassProc; overload;
function LuaClassProc(const Proc: TLuaClassProc27): TLuaClassProc; overload;
function LuaClassProcPtr(const Proc: pointer): TLuaClassProc;



{$ifdef LUA_INITIALIZE}
var
  Lua: TLua;
{$endif}


implementation



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


const LowerChars : array[char] of char = (
#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
#$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
#$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
#$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
#$40,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
#$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$5B,#$5C,#$5D,#$5E,#$5F,
#$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
#$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$7B,#$7C,#$7D,#$7E,#$7F,
#$90,#$83,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$9A,#$8B,#$9C,#$9D,#$9E,#$9F,
#$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9A,#$9B,#$9C,#$9D,#$9E,#$9F,
#$A0,#$A2,#$A2,#$BC,#$A4,#$B4,#$A6,#$A7,#$B8,#$A9,#$BA,#$AB,#$AC,#$AD,#$AE,#$BF,
#$B0,#$B1,#$B3,#$B3,#$B4,#$B5,#$B6,#$B7,#$B8,#$B9,#$BA,#$BB,#$BC,#$BE,#$BE,#$BF,
#$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
#$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF,
#$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
#$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF);


type
  TExceptClass = class of TExcept;

procedure __TExceptAssert_1(const Self: TExceptClass; const Message: AnsiString; const CodeAddr: pointer);
begin
  raise Self.Create(Message) at CodeAddr;
end;

class procedure TExcept.Assert(const Message: AnsiString; const CodeAddr : pointer);
asm
  test ecx, ecx
  jnz __TExceptAssert_1
  mov ecx, [esp]
  jmp __TExceptAssert_1
end;

procedure __TExceptAssert_2(const Self: TExceptClass; const FmtStr: AnsiString; const Args: array of const; const CodeAddr : pointer);
begin
  raise Self.CreateFmt(FmtStr, Args) at CodeAddr;
end;

class procedure TExcept.Assert(const FmtStr: AnsiString; const Args: array of const; const CodeAddr : pointer);
asm
  cmp [esp+8], 0
  jnz @jmp

  mov ebp, [esp+4]
  mov [esp+8], ebp
@jmp:
  pop ebp
  jmp __TExceptAssert_2
end;


function InstancePath(): string;
var
  PATH: array[0..MAX_PATH] of char;
begin
  if (System.IsLibrary) then Result := '' else Result := paramstr(0);

  if (System.IsLibrary) then
  begin
    GetModuleFileName(hInstance, PATH, MAX_PATH);
    Result := PATH;
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
    movzx esi, byte ptr [LowerChars + esi]
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
    movzx esi, byte ptr [LowerChars + esi]
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




function EqualStrings(const S1, S2 : string) : boolean; overload;
asm
   cmp eax, edx
   je @exit1

   test eax, eax
   jz @exit0

   test edx, edx
   jz @exit0

   mov ecx, [eax-4]
   cmp [edx-4], ecx
   jne @exit0

   push esi
   push edi
   mov esi, eax
   mov edi, edx
   dec esi
   dec edi

   // сам цикл
   @loop:
       movzx eax, byte ptr [esi + ecx]
       cmp byte ptr [edi + ecx], al
       je @next_iteration

       movzx edx, byte ptr [edi + ecx]
       movzx eax, byte ptr [LowerChars + eax]
       cmp byte ptr [LowerChars + edx], al
       je @next_iteration

       xor eax, eax
       pop edi
       pop esi
       ret

   @next_iteration:
   dec ecx
   jnz @loop

   pop edi
   pop esi

   @exit1:
      mov eax, 1
      ret
   @exit0:
      xor eax, eax
end;

function EqualStrings (const S1: string; const S2: pchar; const S2Length: integer) : boolean; overload;
asm
   cmp eax, edx
   je @exit1

   test eax, eax
   jz @exit0

   test edx, edx
   jz @exit0

   test ecx, ecx
   jle @exit0

   cmp ecx, [eax-4]
   jne @exit0

   push esi
   push edi
   mov esi, eax
   mov edi, edx
   dec esi
   dec edi

   // сам цикл
   @loop:
       movzx eax, byte ptr [esi + ecx]
       cmp byte ptr [edi + ecx], al
       je @next_iteration

       movzx edx, byte ptr [edi + ecx]
       movzx eax, byte ptr [LowerChars + eax]
       cmp byte ptr [LowerChars + edx], al
       je @next_iteration

       xor eax, eax
       pop edi
       pop esi
       ret

   @next_iteration:
   dec ecx
   jnz @loop

   pop edi
   pop esi

   @exit1:
      mov eax, 1
      ret
   @exit0:
      xor eax, eax
end;

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

// ------------   Блок для дампов CFunction  ----------------------------------
const
  DUMP_SIZE = 32;
  DUMPS_BLOCK_SIZE = 1024*4; // PageSize
  DUMPS_IN_BLOCK = DUMPS_BLOCK_SIZE div DUMP_SIZE;
  CHECK_BUFFER_SIZE = ((DUMPS_IN_BLOCK+31)and -32) div 8;

type              
  TDumpsBlock = object
  private
    {$ifdef WINDOWS}Handle: THandle;{$endif}
    FMemory: pointer;
    FCount: integer;
    CheckBuffer: array[0..CHECK_BUFFER_SIZE-1] of byte;
  public
    procedure Initalize();

    function Alloc(): pointer;
    function Dispose(const Number: integer): boolean;

    property Memory: pointer read FMemory;
    property Count: integer read FCount;
  end;

  TDumpManager = object
  private
    Blocks: array of TDumpsBlock;
    FCount: integer;
  public
    function  Alloc(): pointer;
    procedure Dispose(const Value: pointer);

    property Count: integer read FCount;
  end;

{ TDumpsBlock }

procedure TDumpsBlock.Initalize;
begin
  FCount := 0;

  {$ifdef WINDOWS}
    Handle := HeapCreate($00040000{HEAP_CREATE_ENABLE_EXECUTE}, 0, 0);
    FMemory := HeapAlloc(Handle, 0, DUMPS_BLOCK_SIZE);
  {$else}
    GetMem(FMemory, DUMPS_BLOCK_SIZE);
  {$endif}

  ZeroMemory(@CheckBuffer, sizeof(CheckBuffer));
end;

function TDumpsBlock.Alloc: pointer;
const
  DUMPS_32 = DUMP_SIZE*32;
asm
  mov edx, [eax+TDumpsBlock.FCount]
  inc dword ptr [eax+TDumpsBlock.FCount]
  bts [eax+TDumpsBlock.CheckBuffer], edx
  jc @find

  mov eax, [eax+TDumpsBlock.FMemory]
  shl edx, 5 //*32 {DUMP_SIZE}
  add eax, edx
  ret

@find:
  push ebx

  // ebx - number
  // edx - CheckBuffer
  // eax - результат
  lea edx, [eax+TDumpsBlock.CheckBuffer]
  mov eax, [eax+TDumpsBlock.FMemory]
  mov ecx, [edx]

  @loop:
    not  ecx // проверка на $FFFFFF
    test ecx, ecx
    jnz @after_loop

    add edx, 4
    add eax, DUMPS_32 // 32 дампа
  mov ecx, [edx]
  jmp @loop
  @after_loop:

  // все нулевые биты стали единичными. занести в ebx номер бита
  bsf ebx, ecx

  // смещения, результат, установить бит
  btr ecx, ebx
  not ecx
  shl ebx, 5 //*32 {DUMP_SIZE}
  mov [edx], ecx
  add eax, ebx

  pop ebx
end;

procedure UncheckBit(const Memory: pointer; const Number: integer);
asm
  btr [eax], edx
end;

function TDumpsBlock.Dispose(const Number: integer): boolean;
begin
  UncheckBit(@CheckBuffer, Number);

  dec(FCount);
  Result := (FCount=0);

  if (Result) then
  begin
    {$ifdef WINDOWS}
      HeapFree(Handle, 0, FMemory);
      HeapDestroy(Handle);
    {$else}
      FreeMem(FMemory);
    {$endif}
  end;
end;


{ TDumpManager }

function TDumpManager.Alloc: pointer;
var
  i: integer;
  Block: ^TDumpsBlock;
begin
  Block := pointer(Blocks);
  for i := 0 to Count-1 do
  begin
    if (Block^.FCount <> DUMPS_IN_BLOCK) then
    begin
      Alloc := Block.Alloc;
      exit;
    end;

    inc(Block);
  end;

  // добавление нового блока
  inc(FCount);
  SetLength(Blocks, FCount);
  Block := @Blocks[FCount-1];
  Block.Initalize();
  Alloc := Block.Alloc;
end;

procedure TDumpManager.Dispose(const Value: pointer);
var
  i, Number: integer;
  Block: ^TDumpsBlock;
begin
  if (Count <> 0) then
  begin
    Block := @Blocks[Count-1];
    
    for i := Count-1 downto 0 do
    begin
      if (dword(Value) >= dword(Block.Memory)) then
      begin
        Number := (integer(Value)-integer(Block.Memory)) shr 5; // div 32
        if (Number < DUMPS_IN_BLOCK) then
        begin
          if (Block.Dispose(Number)) then
          begin
            dec(FCount);
            if (i <> FCount) then Block^ := Blocks[FCount];
            SetLength(Blocks, FCount);
          end;

          exit;
        end;
      end;

      dec(Block);
    end;
  end;
end;

var
  CFunctionDumpManager: TDumpManager;


// <<<---------   Блок для дампов CFunction  ----------------------------------

// ------------   LUA-рутина  -------------------------------------------------
var
  LuaHandle: THandle;
  LuaPath: string;
  LuaInitialized: boolean;

type
  Plua_State = pointer;
  lua_CFunction = function(L: Plua_State): integer; cdecl;
  size_t = cardinal;
  lua_Number = Double;
  lua_Integer = Integer;

  lua_Debug = record           (* activation record *)
    event: Integer;
    name: PChar;               (* (n) *)
    namewhat: PChar;           (* (n) `global', `local', `field', `method' *)
    what: PChar;               (* (S) `Lua', `C', `main', `tail'*)
    source: PChar;             (* (S) *)
    currentline: Integer;      (* (l) *)
    nups: Integer;             (* (u) number of upvalues *)
    linedefined: Integer;      (* (S) *)
    short_src: array[0..60{LUA_IDSIZE} - 1] of Char; (* (S) *)
    (* private part *)
    i_ci: Integer;              (* active function *)

    // это сделано во избежание. lua_getstack портит первый байт MANY_FIELDS точно!
    MANY_FIELDS: array[0..3] of byte;
  end;
  Plua_Debug = ^lua_Debug;

var
  LUA_VERSION_52: boolean = false;
  LUA_REGISTRYINDEX: integer = -10000;

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

var
  lua_open: function(): Plua_State;
  luaL_openlibs: procedure(L: Plua_State);cdecl;
  lua_close: procedure(L: Plua_State); cdecl;
  lua_gc: function(L: Plua_State; what: Integer; data: Integer):Integer;cdecl;
  luaL_loadbuffer: function(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl;
  luaL_loadbufferx: function(L: Plua_State; const buff: PChar; size: Integer; const name, mode: PChar): Integer; cdecl;
  lua_pcall: function(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
  lua_pcallk: function(L: Plua_State; nargs, nresults, errf, ctx: Integer; k: lua_CFunction): Integer; cdecl;
  lua_error: function(L: Plua_State): Integer; cdecl;
  lua_next: function(L: Plua_State; idx: Integer): Integer; cdecl;
  lua_getstack: function(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
  lua_getinfo: function(L: Plua_State; const what: PChar; ar: Plua_Debug): Integer; cdecl;

  lua_type: function(L: Plua_State; idx: Integer): Integer; cdecl;
  lua_gettop: function(L: Plua_State): Integer; cdecl;
  lua_settop: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_remove: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_insert: procedure(L: Plua_State; idx: Integer); cdecl;

  lua_pushnil: procedure(L: Plua_State); cdecl;
  lua_pushboolean: procedure(L: Plua_State; b: LongBool); cdecl;
  lua_pushinteger: procedure(L: Plua_State; n: lua_Integer); cdecl;
  lua_pushnumber: procedure(L: Plua_State; n: lua_Number); cdecl;
  lua_pushlstring: procedure(L: Plua_State; const s: PChar; l_: size_t); cdecl;
  lua_pushcclosure: procedure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
  lua_pushlightuserdata: procedure(L: Plua_State; p: Pointer); cdecl;
  lua_newuserdata: function(L: Plua_State; sz: Integer): Pointer; cdecl;
  lua_pushvalue: procedure(L: Plua_State; Idx: Integer); cdecl;
  lua_toboolean: function(L: Plua_State; idx: Integer): LongBool; cdecl;
  lua_tonumber: function(L: Plua_State; idx: Integer): lua_Number; cdecl;
  lua_tonumberx: function(L: Plua_State; idx: Integer; isnum: pinteger): lua_Number; cdecl;
  lua_tolstring: function(L: Plua_State; idx: Integer; len: pinteger): PChar; cdecl;
  lua_tocfunction: function(L: Plua_State; idx: Integer): lua_CFunction; cdecl;
  lua_touserdata: function(L: Plua_State; idx: Integer): Pointer; cdecl;
  lua_objlen: function(L: Plua_State; idx: Integer): size_t; cdecl;

  lua_rawgeti: procedure(L: Plua_State; idx, n: Integer); cdecl;
  lua_rawseti: procedure(L: Plua_State; idx, n: Integer); cdecl;
  lua_rawget: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_rawset: procedure(L: Plua_State; idx: Integer); cdecl;
  lua_createtable: procedure(L: Plua_State; narr: Integer; nrec: Integer); cdecl; (* old newtable *)
  lua_setmetatable: function(L: Plua_State; objindex: Integer): Integer; cdecl;

// для 5.2
function luaL_loadbuffer_52(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl;
begin
  Result := luaL_loadbufferx(L, buff, size, name, nil);
end;

function lua_pcall_52(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
begin
  Result := lua_pcallk(L, nargs, nresults, errf, 0, nil);
end;

function lua_tonumber_52(L: Plua_State; idx: Integer): lua_Number; cdecl;
begin
  Result := lua_tonumberx(L, idx, nil);
end;



// коррекция пустой строки
const NULL_CHAR: char = #0;

procedure lua_push_pchar(const L: Plua_State; const S: PChar; const any4bytes: integer=0); cdecl;
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


procedure lua_push_pascalstring(const L: Plua_State; const S: string; const any4bytes: integer=0); cdecl;
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


procedure lua_to_pascalstring(var Dest: AnsiString; L: Plua_State; const Index: integer);
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
  // lua_tolstring: function(L: Plua_State; idx: Integer; len: pinteger=nil): PChar; cdecl;
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


function lua_toint64(L: Plua_State; idx: Integer): int64; register;
asm
  push edx
  push eax
  call lua_tonumber

  // значение
  fistp qword ptr [esp]
  pop eax
  pop edx
end;




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
    LuaHandle := LoadLibrary(pchar(LuaPath));
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
var
  Buf: pointer;

  function FailLoad(var Proc; const ProcName: pchar): boolean;
  begin
    pointer(Proc) := GetProcAddress(LuaHandle, ProcName);
    Result := (pointer(Proc) = nil);
  end;

begin
  Result := false;
  if (not LuaInitialized) then
  begin
    if (LoadLuaHandle() = 0) then exit;
    LUA_VERSION_52 := not FailLoad(Buf, 'lua_tounsignedx');
    if (LUA_VERSION_52) then LUA_REGISTRYINDEX := (-1000000 - 1000);

    if FailLoad(@lua_open, 'luaL_newstate') then exit;
    if FailLoad(@luaL_openlibs, 'luaL_openlibs') then exit;
    if FailLoad(@lua_close, 'lua_close') then exit;
    if FailLoad(@lua_gc, 'lua_gc') then exit;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@luaL_loadbufferx, 'luaL_loadbufferx') then exit;
      luaL_loadbuffer := luaL_loadbuffer_52;
    end else
    begin
      if FailLoad(@luaL_loadbuffer, 'luaL_loadbuffer') then exit;
    end;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@lua_pcallk, 'lua_pcallk') then exit;
      lua_pcall := lua_pcall_52;      
    end else
    begin
      if FailLoad(@lua_pcall, 'lua_pcall') then exit;
    end;
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
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@lua_tonumberx, 'lua_tonumberx') then exit;
      lua_tonumber := lua_tonumber_52;
    end else
    begin
      if FailLoad(@lua_tonumber, 'lua_tonumber') then exit;
    end;
    if FailLoad(@lua_tolstring, 'lua_tolstring') then exit;
    if FailLoad(@lua_tocfunction, 'lua_tocfunction') then exit;
    if FailLoad(@lua_touserdata, 'lua_touserdata') then exit;
    if (LUA_VERSION_52) then
    begin
      if FailLoad(@lua_objlen, 'lua_rawlen') then exit;
    end else
    begin
      if FailLoad(@lua_objlen, 'lua_objlen') then exit;
    end;

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
procedure GetUserDataType(var Result: string; const Lua: TLua; const userdata: PLuaUserData);
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
end;


var
  // глобальный массив "Lua_CFunction"-дампов
  CFunctionDumps: array of pointer;

// создать дамп перевызова TLua.CallbackProc с параметрами
function CreateCFunctionDump(const Lua: TLua; const P1, P2, CallbackProc: pointer): pointer;
var
  Dump: pchar absolute Result;
begin
  Result := CFunctionDumpManager.Alloc();

  // mov eax, Lua
  byte(Dump[0]) := $B8;
  pinteger(@Dump[1])^ := integer(Lua);
  // mov edx, ClassIndex
  byte(Dump[5]) := $BA;
  ppointer(@Dump[6])^ := P1;
  // mov ecx, ProcIndex
  byte(Dump[10]) := $B9;
  ppointer(@Dump[11])^ := P2;
  // jmp dword ptr [Dump + 21]
  byte(Dump[15]) := $FF;
  byte(Dump[16]) := $25;
  ppointer(@Dump[17])^ := @Dump[21];
  // [Dump + 21] := CallbackProc
  ppointer(@Dump[21])^ := CallbackProc;
end;

// создать дамп "Lua_CFunction" с параметрами и добавить в массив
function AddLuaCallbackProc(const Lua: TLua; const P1, P2, CallbackProc: pointer): pointer{Lua_CFunction};
var
  Len: integer;
begin
  Len := Length(CFunctionDumps);
  SetLength(CFunctionDumps, Len+1);
  CFunctionDumps[Len] := CreateCFunctionDump(Lua, P1, P2, CallbackProc);

  Result := {Lua_CFunction}(pointer(CFunctionDumps[Len]));
end;

// удалить все дампы связанные с конкретным Lua
procedure DeleteCFunctionDumps(const Lua: TLua);
var
  i, Len: integer;
begin
  i := 0;
  Len := Length(CFunctionDumps);

  while (i < Len) do
  begin
    if (pinteger(integer(CFunctionDumps[i])+1)^ = integer(Lua)) then
    begin
      CFunctionDumpManager.Dispose(CFunctionDumps[i]);
      CFunctionDumps[i] := CFunctionDumps[Len-1];
      dec(Len);
    end else
    begin
      inc(i);
    end;
  end;

  SetLength(CFunctionDumps, Len);  
end;

// найти указатель на конечную калбек-функцию, имея исходную CFunction
// нужно при анализе луа-аргумента.
//
// причём если фунцкия зарегистрирована, то возвращается конечная функция
// а если функция внутри lua, то возвращается она сама
function CFunctionPtr(CFunction: Lua_CFunction): pointer;
var
  ProcInfo: ^TLuaProcInfo;
begin
  Result := @CFunction;

  if (InsortedPos4(integer(@CFunction), CFunctionDumps) >= 0) then
  begin
    ProcInfo := ppointer(integer(@CFunction) + 11)^;
    if (ProcInfo <> nil) and (ProcInfo.Address <> nil) then Result := ProcInfo.Address;
  end;
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

const
  GLOBAL_INDEX_KINDS: set of TLuaGlobalKind = [gkType, gkConst, gkLuaData];
  CONST_GLOBAL_KINDS: set of TLuaGlobalKind = [gkType, gkProc, gkConst];
  NATIVE_GLOBAL_KINDS: set of TLuaGlobalKind = [gkVariable, gkProc];
  RECORD_TYPES: set of TTypeKind = [tkRecord{$ifdef fpc},tkObject{$endif}];
  VARIANT_SUPPORT = [varEmpty, varNull, varSmallint, varInteger, varSingle,
                     varDouble, varCurrency, varDate, varOleStr, varBoolean, varError{as Empty},
                     varShortInt, varByte, varWord, varLongWord, varInt64{, почему-то не умещается varString}];
  VARIANT_SIMPLE = VARIANT_SUPPORT - [varOleStr];

  

function LuaProc(const Proc: TLuaProc0): TLuaProc;
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
function LuaClassProc(const Proc: TLuaClassProc0): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc1): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc2): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc3): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc4): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc5): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc6): TLuaClassProc;
begin Result := TLuaClassProc(Proc); end;
function LuaClassProc(const Proc: TLuaClassProc7): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc8): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc9): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc10): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc11): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc12): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc13): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc14): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc15): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc16): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc17): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc18): TLuaClassProc;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc19): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc20): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc21): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc22): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc23): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc24): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc25): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc26): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProc(const Proc: TLuaClassProc27): TLuaClassProc; overload;
begin TMethod(Result).Code := @Proc; end;
function LuaClassProcPtr(const Proc: pointer): TLuaClassProc;
begin TMethod(Result).Code := Proc; end;


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

procedure __TLuaArgSetVariant(var Self: TLuaArg; const Value: Variant; const ReturnAddr: pointer);
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
  jmp __TLuaArgSetVariant
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


procedure __TLuaArgSetRecord(var Self: TLuaArg; const Value: TLuaRecord; const ReturnAddr: pointer);
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
  jmp __TLuaArgSetRecord
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

procedure __TLuaArgSetArray(var Self: TLuaArg; const Value: TLuaArray; const ReturnAddr: pointer);
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
  jmp __TLuaArgSetArray
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

procedure __TLuaArgSetSet(var Self: TLuaArg; const Value: TLuaSet; const ReturnAddr: pointer);
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
  jmp __TLuaArgSetSet
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



{ TLuaTable }

procedure TLuaTable.ThrowValueType(const CodeAddr: pointer; const pop: boolean);
begin
  if (pop) then Lua.stack_pop();
  ELua.Assert('Unsupported value type = "%s"', [Lua.FBufferArg.str_data], CodeAddr);
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



{ TLuaRecordInfo }

function TLuaRecordInfo.GetFieldsCount: integer;
begin
  Result := Length(FLua.ClassesInfo[FClassIndex].Properties);
end;

// tpinfo может быть:
// - typeinfo(type)
// - PLuaRecordInfo
procedure TLuaRecordInfo.InternalRegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer; const CodeAddr: pointer);
type
  TDataBuffer = array[0..sizeof(TLuaPropertyInfo)-1] of byte;
var
  i, j: integer;
  Buffer: TDataBuffer;
begin
  // провеки ?

  // регистрация
  FLua.InternalAddProperty(false, @Self, FieldName, tpinfo, false, false,
       pointer(FieldOffset), pointer(FieldOffset), nil, CodeAddr);

  // сортировка полей по возрастанию
  with FLua.ClassesInfo[FClassIndex] do
  for i := 0 to Length(Properties)-2 do
  for j := i+1 to Length(Properties)-1 do
  if (integer(Properties[i].read_mode) > integer(Properties[j].read_mode)) then
  begin
    // swap i <--> j
    Buffer := TDataBuffer(Properties[i]);
    TDataBuffer(Properties[i]) := TDataBuffer(Properties[j]);
    TDataBuffer(Properties[j]) := Buffer;
  end;
end;


procedure __TLuaRecordInfoRegField_1(const Self: TLuaRecordInfo; const FieldName: string;
          const FieldOffset: integer; const tpinfo: pointer; const ReturnAddr: pointer);
begin
  Self.InternalRegField(FieldName, FieldOffset, tpinfo, ReturnAddr);
end;

procedure TLuaRecordInfo.RegField(const FieldName: string; const FieldOffset: integer; const tpinfo: pointer);
asm
  pop ebp
  push [esp]
  jmp __TLuaRecordInfoRegField_1
end;

procedure __TLuaRecordInfoRegField_2(const Self: TLuaRecordInfo; const FieldName: string;
          const FieldPointer: pointer; const tpinfo: pointer; const pRecord: pointer; const ReturnAddr: pointer);
begin
  if (integer(pRecord) > integer(FieldPointer)) then
  ELua.Assert('Illegal parameters using: FieldPointer and pRecord', [ReturnAddr]);

  Self.InternalRegField(FieldName, integer(FieldPointer)-integer(pRecord), tpinfo, ReturnAddr);
end;

procedure TLuaRecordInfo.RegField(const FieldName: string; const FieldPointer: pointer; const tpinfo: pointer; const pRecord: pointer = nil);
asm
  pop ebp
  push [esp]
  jmp __TLuaRecordInfoRegField_2
end;


procedure __TLuaRecordInfoRegProc(const Self: TLuaRecordInfo; const ProcName: string; const Proc: TLuaClassProc;
                                  const ArgsCount: integer; const ReturnAddr: pointer);
begin
  Self.FLua.InternalAddProc(false, @Self, ProcName, ArgsCount, false, TMethod(Proc).Code, ReturnAddr);
end;

procedure TLuaRecordInfo.RegProc(const ProcName: string; const Proc: TLuaClassProc; const ArgsCount: integer=-1);
asm
  pop ebp
  push [esp]
  jmp __TLuaRecordInfoRegProc
end;

procedure TLuaRecordInfo.SetOperators(const Value: TLuaOperators);
begin
  if (FOperators <> Value) then
  begin
    FOperators := Value;
    FLua.FInitialized := false;
  end;  
end;

procedure TLuaRecordInfo.SetOperatorCallback(const Value: TLuaOperatorCallback);
begin
  if (@FOperatorCallback <> @Value) then
  begin
    FOperatorCallback := Value;
    FLua.FInitialized := false;
  end;
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

// изначально функция задумалась как аналог TypInfo-функций для работы со сложными свойствами:
// структуры, массивы, множества
//
// сейчас функция берёт значение из стека и присваивает "значение" 
function PopSetDifficultTypeProp(const Lua: TLua; const instance: pointer; const stack_index: integer; const Info: TLuaPropertyInfo): boolean;
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
procedure PackPointer(const ClassInfo: TLuaClassInfo; var Dest: pointer; const P: pointer);
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
end;

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
var
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
end;



// подчистить динамические данные - PropInfo и
procedure TLuaPropertyInfo.Cleanup();
begin
  if (not IsRTTI) and (PropInfo <> nil) then
  begin
    FreeMem(PropInfo);
    PropInfo := nil;
  end;

  PropertyName := '';
end;


// заполнение информации основываясь на PropInfo, созданном RTTI
procedure TLuaPropertyInfo.Fill(const RTTIPropInfo: PPropInfo; const PropBase: TLuaPropertyInfoBase);
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
end;


// искуственная функция, создающая PropInfo (если нужно)
// и заполняющая все необходимые поля
procedure TLuaPropertyInfo.Fill(const class_info; const PropBase: TLuaPropertyInfoBase;
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
end;


{ TLuaClassInfo }

// возвращает индекс. отрицательный (для свойств) или положительный (для методов)
function TLuaClassInfo.InternalAddName(const Name: string; const AsProc: boolean; var Initialized: boolean; const CodeAddr: pointer): integer;
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
  Initialized := false;


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
end;

// быстрый поиск свойства (в простом "классе")
// сделано это в основном для простых структур
procedure FastFindProperty(const Properties: TLuaPropertyInfoDynArray; const Name: pchar; const NameLength: integer; var PropertyInfo: pointer);
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
end;

// найти индекс в списке глобальных имён
function TLuaClassInfo.NameSpacePlace(const Lua: TLua; const Name: pchar; const NameLength: integer; var ProcInfo, PropertyInfo: pointer): integer;
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
end;

function  TLuaClassInfo.PropertyIdentifier(const Name: string = ''): string;
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
end;

procedure TLuaClassInfo.Cleanup();
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
end;


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

// низкий уровень. адрес функции lua.dll
function __TLuaGetProcAddress(const Self: TClass; const ProcName: pchar;
         const throw_exception: boolean; const ReturnAddr: pointer): pointer;
begin
  if (LoadLuaHandle = 0) and (throw_exception) then
  ELua.Assert('Lua library not found'#13'"%s"', [LuaPath], ReturnAddr);

  // загрузить функции
  if (LuaHandle = 0) then Result := nil
  else Result := {$ifdef NO_CRYSTAL}Windows{$else}SysUtilsEx{$endif}.GetProcAddress(LuaHandle, ProcName);

  // если не найдена
  if (Result = nil) and (throw_exception) then
  ELua.Assert('Proc "%s" not found in library'#13'"%s"', [ProcName, LuaPath], ReturnAddr);
end;

class function TLua.GetProcAddress(const ProcName: pchar; const throw_exception: boolean = false): pointer;
asm
  push [esp]
  jmp __TLuaGetProcAddress
end;


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


// конструктор
constructor TLua.Create();
begin
  if (not InitializeLUA) then
  ELua.Assert('Lua library was not initialized:'#13'"%s"', [LuaPath]);

  FHandle := lua_open();
  luaL_openlibs(Handle);

  // флаг препроцессинга (замены '.' на ':')
  FPreprocess := true;

  // метатаблица для сложных свойств
  mt_properties := internal_register_metatable(nil);

  // базовая инициализация глобального пространства
  internal_add_class_info(TRUE);
  GlobalNative._Class := GLOBAL_NAME_SPACE;
  GlobalNative._ClassName := 'GLOBAL_NAME_SPACE';
  GlobalNative.Ref := internal_register_metatable(nil, '', -1, TRUE);

  // TObject
  InternalAddClass(TObject, false, nil);

  // TMethod: структура для хранения событий
  with RegRecord('TMethod', pointer(sizeof(TMethod)))^, TMethod(nil^) do
  begin
    RegField('Code', @Code, typeinfoPointer);
    RegField('Data', @Data, typeinfoPointer);
    RegProc(LUA_CONSTRUCTOR, LuaClassProc(TMethodConstructor));
    Operators := [loCompare];
    OperatorCallback := TMethodOperator;
  end;

  // TLuaReference
  InternalAddClass(TLuaReference, false, nil);
end;

// деструктор
destructor TLua.Destroy();
var
  i: integer;
begin
  // внутренние данные
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
  FUnitsCount := 0;
  

  inherited;
end;

procedure __TLuaGarbageCollection(const Self: TLua; const ReturnAddr: pointer);
var
  ret: integer;
begin
  ret := lua_gc(Self.Handle, 2{LUA_GCCOLLECT}, 0);
  if (ret <> 0) then Self.Check(ret, ReturnAddr);
end;

procedure TLua.GarbageCollection();
asm
  mov edx, [esp]
  jmp __TLuaGarbageCollection
end;


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
end;


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
end;

function TLua.CreateReference(const global_name: string=''): TLuaReference;
asm
  mov ecx, [esp]
  jmp __TLuaCreateReference
end;



// проверить выполнение на ошибку
procedure TLua.Check(const ret: integer; const CodeAddr: pointer; AUnit: TLuaUnit=nil);

  procedure ThrowAssertation();
  var
    Err: string;
    err_str: pchar;
    P1, P2, i: integer;
    UnitName, line_fmt: string;
    UnitLine: integer;
    MinLine, MaxLine: integer;
  begin
    lua_to_pascalstring(Err, Handle, -1);
    stack_pop();
    if (Err = '') then exit;

    // изменить Err - узнать имя чанка и номер строки
    UnitLine := 0;
    if (Err[1] = '[') then
    begin
      P1 := CharPos('"', Err);
      P2 := CharPos(']', Err);
      if (P1 <> 0) and (P2 <> 0) then
      begin
        UnitName := Copy(Err, P1+1, P2-P1-2);
        Delete(Err, 1, P2);

        if (Err <> '') and (Err[1] = ':') then
        begin
          P1 := 1;
          P2 := CharPosEx(':', Err, 2);
          if (P2 <> 0) then
          begin
            UnitLine := StrToIntDef(Copy(Err, P1+1, P2-P1-1), 0);
            if (UnitLine > 0) then dec(UnitLine);
            Delete(Err, 1, P2);
            if (Err <> '') and (Err[1] = #32) then Delete(Err, 1, 1);
          end;
        end;
      end;
    end;

    // определиться с чанком
    if (UnitName = '') then
    begin
      AUnit := nil;
      UnitName := 'GLOBAL_NAME_SPACE';
    end else
    begin
      if (AUnit = nil) then AUnit := Self.UnitByName[UnitName];
      if (AUnit <> nil) and (dword(UnitLine) >= dword(AUnit.FLinesCount)) then AUnit := nil;
    end;

    // определиться с текстом сообщения
    Err := Format('unit "%s", line %d.'#13'%s', [UnitName, UnitLine, Err]);

    if (AUnit <> nil) then
    begin
      MinLine := UnitLine-2; if (MinLine < 0) then MinLine := 0;
      if (MinLine <> UnitLine) and (Trim(AUnit[MinLine]) = '') then inc(MinLine);
      MaxLine := UnitLine+2; if (MaxLine >= AUnit.FLinesCount) then MaxLine := AUnit.FLinesCount-1;
      if (MaxLine <> UnitLine) and (Trim(AUnit[MaxLine]) = '') then dec(MaxLine);
      line_fmt := Format(#13'%%%dd:  ', [Length(IntToStr(MaxLine))]);
      Err := Err + #13#13'Code:';

      for i := MinLine to MaxLine do
      begin
        Err := Err + Format(line_fmt, [i]);
        if (i = UnitLine) then Err := Err + '-->> ';
        Err := Err + AUnit[i];
      end;
    end;

    // откорректировать #13 --> #10 (для случаев консоли)
    err_str := pointer(Err);
    for i := 0 to Length(Err)-1 do
    if (err_str[i] = #13) then err_str[i] := #10;  

    // exception
    ELuaScript.Assert(Err, CodeAddr);
  end;

begin
  if (ret <> 0) then ThrowAssertation();
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
end;

// вызвать Exception из Lua
procedure __TLuaScriptAssert(const Self: TLua; const FmtStr: string; const Args: array of const; const ReturnAddr: pointer);
var
  S: string;
  DebugInfo: lua_Debug;
begin
  // получить Debug-информацию
  ZeroMemory(@DebugInfo, sizeof(DebugInfo));
  lua_getstack(Self.Handle, 1, @DebugInfo);
  lua_getinfo(Self.Handle, 'Sln', @DebugInfo);

  if (DebugInfo.currentline < 0) then
  ELua.Assert(FmtStr, Args, ReturnAddr); // ошибочный случай вызова TLua.ScriptAssert

  // вывод сообщения (стандартный вид)
  S := Format('%s:%d: ', [pchar(@DebugInfo.short_src[4]), DebugInfo.currentline]);
  lua_push_pascalstring(Self.Handle, S + Format(FmtStr, Args));
  lua_error(Self.Handle);
end;

procedure TLua.ScriptAssert(const FmtStr: string; const Args: array of const);
asm
  pop ebp
  push [esp]
  jmp __TLuaScriptAssert
end;


// выполнить препроцессинг скрипта
// на данный момент это только замена точек на двоеточие
procedure PreprocessScript(var Memory: string);
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

// загрузка скрипта
procedure TLua.InternalLoadScript(var Memory: string; const UnitName, FileName: string; CodeAddr: pointer);
var
  ret, unit_index: integer;
  internal_exception: Exception;
  AUnit, LastUnit: TLuaUnit;
  CW: word;

  // в случае ошибки пытаемся восстановить старый чанк в Lua
  // при этом сохраняем Exception - потом он будет использован
  procedure OnExceptionRetrieve(const E: Exception);
  begin
    internal_exception := Exception(E.ClassType.NewInstance);
    CopyObject(internal_exception, E);
    AUnit.Free;

    if (LastUnit <> nil) then
    begin
      Memory := LastUnit.Text;
      PreprocessScript(Memory);
      try
        ret := luaL_loadbuffer(Handle, pchar(Memory), Length(Memory), pchar(LastUnit.Name));
        if (ret = 0) then ret := lua_pcall(Handle, 0, 0, 0);
        if (ret = 0) then {ret := }lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
      except
      end;
    end;
  end;
begin
  // определиться с чанками
  if (UnitName = '') then
  begin
    AUnit := nil;
    LastUnit := nil;
    unit_index := -1;
  end else
  begin
    if (UnitName[1] = #32) or (UnitName[Length(UnitName)] = #32) then
    ELua.Assert('Unit name "%s" contains left or/and right spaces', [UnitName], CodeAddr);
    //UnitName := StringLower(UnitName);

    // предыдущий чанк
    LastUnit := Self.UnitByName[UnitName];
    unit_index := IntPos(integer(LastUnit), pinteger(FUnits), Length(FUnits));

    // либо текущий чанк - предыдущий, либо создаю новый и инициализирую
    if (LastUnit <> nil) and (SameStrings(LastUnit.Text, Memory)) then
    begin
      AUnit := LastUnit;
      LastUnit := nil;
    end else
    begin
      AUnit := TLuaUnit.Create;
      AUnit.FName := UnitName;
      AUnit.FFileName := FileName;
      AUnit.FText := Memory;
      AUnit.InitializeLinesInfo();
    end;
  end;


  // загрузить чанк
  // если всё прошло отлично, то добавить/заменить чанк в архиве
  // если конечно это не RunScript вызов
  internal_exception := nil;
  try
    // выполнить препроцессинг
    PreprocessScript(Memory);

    // выполнить скрипт
    if (not FInitialized) then INITIALIZE_NAME_SPACE();

    // загрузить буфер
    begin
      CW := Get8087CW();
      Set8087CW($037F {default intel C++ mode});
      try
        ret := luaL_loadbuffer(Handle, pansichar(Memory), Length(Memory), pansichar(UnitName));
      finally
        Set8087CW(CW);
      end;
    end;

    // вызов, чистка, проверка
    if (ret = 0) then ret := lua_pcall(Handle, 0, 0, 0);
    if (ret = 0) then ret := lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
    if (ret <> 0) then Check(ret, CodeAddr, AUnit);

    // инициализация чанка прошла успешно, занести в массив чанков
    if (unit_index{чанк с таким именем уже был} >= 0) then
    begin
      if (LastUnit <> nil) then LastUnit.Free;
      FUnits[unit_index] := AUnit;
    end else
    if (AUnit <> nil) then
    begin
      unit_index := FUnitsCount;
      inc(FUnitsCount);
      SetLength(FUnits, FUnitsCount);
      FUnits[unit_index] := AUnit;
    end;
  except
    on E: Exception do OnExceptionRetrieve(E);
  end;
  
  if (internal_exception <> nil) then raise internal_exception at CodeAddr;
end;

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
end;


function TLua.push_variant(const Value: Variant): boolean;
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
end;

function TLua.push_luaarg(const LuaArg: TLuaArg): boolean;
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
      ltClass: lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassesInfo[internal_class_index(pointer(Data[0]), true)].Ref);
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
end;


function TLua.push_argument(const Value: TVarRec): boolean;
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
                   if (TClass(pointer(VObject)^) = TLuaReference) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaReference(VObject).Index)
                   else
                   push_userdata(ClassesInfo[internal_class_index(TClass(pointer(VObject)^), true)], false, pointer(VObject));
                 end;  
    vtClass:     if (VClass = nil) then lua_pushnil(Handle) else lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassesInfo[internal_class_index(pointer(VClass), true)].Ref);
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
end;

{procedure TLua.stack_pop(const count: integer);
begin
  lua_settop(Handle, -count - 1);//lua_pop(Handle, count);
end; }
procedure TLua.stack_pop(const count: integer);
asm
  not edx
  mov eax, [eax + TLua.FHandle]
  push edx
  push eax
  call lua_settop
  add esp, 8
end;

function TLua.stack_variant(var Ret: Variant; const StackIndex: integer): boolean;
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
end;

function TLua.stack_luaarg(var Ret: TLuaArg; const StackIndex: integer; const lua_table_available: boolean): boolean;
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
end;

procedure TLua.global_alloc_ref(var ref: integer);
begin
  if (ref = 0) then
  begin
    dec(FRef); 
    ref := FRef;
  end;
end;

procedure TLua.global_free_ref(var ref: integer);
begin
  if (ref < 0) then
  begin
    lua_pushnil(Handle);
    lua_rawseti(Handle, LUA_REGISTRYINDEX, ref);
    ref := 0;
  end;
end;

procedure TLua.global_fill_value(const ref: integer);
{begin
  if (ref <= 0) then stack_pop()
  else lua_rawseti(Handle, LUA_REGISTRYINDEX, ref);
end;}
asm
  mov ecx, [eax + TLua.FHandle]
  test edx, edx
  jl @rawset
    mov edx, 1
    jmp TLua.stack_pop
@rawset:
  push edx
  push LUA_REGISTRYINDEX
  push ecx
  call [lua_rawseti]
  add esp, 12
end;

procedure TLua.global_push_value(const ref: integer);
{begin
  if (ref <= 0) then lua_pushnil(Handle)
  else lua_rawgeti(Handle, LUA_REGISTRYINDEX, ref);
end;}
asm
  mov ecx, [eax + TLua.FHandle]
  test edx, edx
  jl @rawget
    push ecx
    call [lua_pushnil]
    pop eax
    ret
@rawget:
  push edx
  push LUA_REGISTRYINDEX
  push ecx
  call [lua_rawgeti]
  add esp, 12
end;

// Index - позиция переменной в глобальном списке GlobalVariables если результат = true
// если false, то Index = place в массиве NameSpaceHash
// если выставлен флаг auto_create, то переменная создаётся, но результат всёравно False
function  TLua.GlobalVariablePos(const Name: pchar; const NameLength: integer; var Index: integer; const auto_create: boolean): boolean;
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
      _Kind := low(TLuaGlobalKind);
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
end;


procedure __TLuaRunScript(const Self: TLua; const Script: string; const ReturnAddr: pointer);
var
  Memory: string;
begin
  Memory := Script;
  Self.InternalLoadScript(Memory, '', '', ReturnAddr);
end;

procedure TLua.RunScript(const Script: string);
asm
  mov ecx, [esp]
  jmp __TLuaRunScript
end;

procedure __TLuaLoadScript_file(const Self: TLua; const FileName: string; const ReturnAddr: pointer);
var
  F: TFileStream;
  Size: integer;
  Memory: string;
begin
  if (not FileExists(FileName)) then
  begin
    ELua.Assert('File "%s" not found', [FileName], ReturnAddr);
  end;

  F := SharedFileStream(FileName);
  try
    Size := F.Size;
    if (Size <> 0) then
    begin
      SetLength(Memory, Size);
      F.Read(pointer(Memory)^, Size);
    end;
  finally
    F.Free;
  end;

  Self.InternalLoadScript(Memory, ExtractFileName(FileName), FileName, ReturnAddr);
end;

procedure TLua.LoadScript(const FileName: string);
asm
  mov ecx, [esp]
  jmp __TLuaLoadScript_file
end;

procedure __TLuaLoadScript_buffer(const Self: TLua; const ScriptBuffer: pointer;
          const ScriptBufferSize: integer; const UnitName: string; const ReturnAddr: pointer);
var
  Memory: string;
begin
  if (ScriptBufferSize >= 0) then
  begin
    SetLength(Memory, ScriptBufferSize);
    Move(ScriptBuffer^, pointer(Memory)^, ScriptBufferSize);
  end;

  Self.InternalLoadScript(Memory, UnitName, '', ReturnAddr);
end;

procedure TLua.LoadScript(const ScriptBuffer: pointer; const ScriptBufferSize: integer; const UnitName: string);
asm
  pop ebp
  push [esp]
  jmp __TLuaLoadScript_buffer
end;



function TLua.internal_class_index_by_name(const AName: string): integer;
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
end;

function TLua.GetRecordInfo(const Name: string): PLuaRecordInfo;
var
  Index: integer;
begin
  Index := internal_class_index_by_name(Name);

  if (Index < 0) then GetRecordInfo := nil
  else
  with ClassesInfo[Index] do
  if (_ClassKind <> ckRecord) then GetRecordInfo := nil
  else
  GetRecordInfo := _Class;
end;

function TLua.GetArrayInfo(const Name: string): PLuaArrayInfo;
var
  Index: integer;
begin
  Index := internal_class_index_by_name(Name);

  if (Index < 0) then GetArrayInfo := nil
  else
  with ClassesInfo[Index] do
  if (_ClassKind <> ckArray) then GetArrayInfo := nil
  else
  GetArrayInfo := _Class;
end;

function TLua.GetSetInfo(const Name: string): PLuaSetInfo;
var
  Index: integer;
begin
  Index := internal_class_index_by_name(Name);

  if (Index < 0) then GetSetInfo := nil
  else
  with ClassesInfo[Index] do
  if (_ClassKind <> ckSet) then GetSetInfo := nil
  else
  GetSetInfo := _Class;
end;

procedure __TLuaGetVariable(const Self: TLua; const Name: string; var Result: Variant; const ReturnAddr: pointer);
var
  modify_info: TLuaGlobalModifyInfo;
begin
  modify_info.Name := Name;
  modify_info.CodeAddr := ReturnAddr;
  modify_info.IsVariant := true;
  modify_info.V := @Result;

  Self.__global_index(true, modify_info);
end;

function TLua.GetVariable(const Name: string): Variant;
asm
  push [esp]
  jmp __TLuaGetVariable
end;


procedure __TLuaSetVariable(const Self: TLua; const Name: string; const Value: Variant; const ReturnAddr: pointer);
var
  modify_info: TLuaGlobalModifyInfo;
begin
  modify_info.Name := Name;
  modify_info.CodeAddr := ReturnAddr;
  modify_info.IsVariant := true;
  modify_info.V := @Value;

  Self.__global_newindex(true, modify_info);
end;

procedure TLua.SetVariable(const Name: string; const Value: Variant);
asm
  push [esp]
  jmp __TLuaSetVariable
end;

procedure __TLuaGetVariableEx(const Self: TLua; const Name: string; var Result: TLuaArg; const ReturnAddr: pointer);
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
end;

procedure __TLuaSetVariableEx(const Self: TLua; const Name: string; const Value: TLuaArg; const ReturnAddr: pointer);
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
end;


// зарегистрировать глобальную переменную
// при необходимости создать/удалить Ref и Index
// урегулировать конфликты или вызвать exception
// Kind - Type (Class или Record), Variable, Proc или Enum
// инициализация gkLuaData происходит в global_newindex если переменная не найдена
function TLua.internal_register_global(const Name: string; const Kind: TLuaGlobalKind; const CodeAddr: pointer): PLuaGlobalVariable;
const
  KIND_NAMES: array[TLuaGlobalKind] of string = ('type', 'variable', 'method', 'enum', '');
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
    Result.Index := GlobalNative.InternalAddName(Name, (Kind = gkProc), FInitialized, CodeAddr);
  end else
  begin
    global_alloc_ref(Result.Ref);
  end;

  // меняем флаг инициализации глобального пространства имён
  if (Kind <> gkConst) then FInitialized := false;
end;

// создать и проинициализировать метатаблицу
function  TLua.internal_register_metatable(const CodeAddr: pointer; const GlobalName: string=''; const ClassIndex: integer = -1; const is_global_space: boolean = false): integer;
const
  LUA_GLOBALSINDEX = -10002;
  LUA_RIDX_GLOBALS = 2;
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
    if (LUA_VERSION_52) then
    begin
      lua_rawgeti(Handle, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
      global_push_value(Result);
      lua_setmetatable(Handle, -2);
      stack_pop();
    end else
    begin
      global_push_value(Result);
      lua_setmetatable(Handle, LUA_GLOBALSINDEX);
    end;
  end else
  begin
    global_push_value(Result);
    lua_pushvalue(Handle, 1);
    lua_setmetatable(Handle, -2);
    stack_pop();
  end;
end;

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
end;

function  TLua.internal_add_class_index(const AClass: pointer; const AIndex: integer): integer;
begin
  Result := InsortedPlace8(integer(AClass), pointer(ClassesIndexes), Length(ClassesIndexes));
  with TLuaClassIndex(DynArrayInsert(ClassesIndexes, typeinfo(TLuaClassIndexDynArray), Result)^) do
  begin
    _Class := AClass;
    Index := AIndex;
  end;
end;

function  TLua.internal_add_class_index_by_name(const AName: string; const AIndex: integer): integer;
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
end;

// быстро найти индекс класса в массиве ClassesInfo
function TLua.internal_class_index(AClass: pointer; const look_class_parents: boolean): integer;
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
asm
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
end;


// добавить класс, если такого нет
// если UsePublished, то прописать ему так же всё связанное с published
// если это регистратор, то зарегистрировать всё для подрегистрируемого класса
function TLua.InternalAddClass(AClass: TClass; UsePublished: boolean; const CodeAddr: pointer): integer;
var
  InstanceSize: integer;
  ClassRegistrator: TClass;
  ClassParentIndex: integer;

  // является ли класс регистратором lua
  function IsRegistrator(const _Class: TClass): boolean;
  begin
    Result := (_Class <> nil) and (EqualStrings('lua', Copy(_Class.ClassName, 1, 3)));
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
    MethodName: string;
  begin
    // Registrator mode
    if (ClassRegistrator <> nil) and (_Class.ClassParent <> AClass) then AddPublishedMethods(_Class.ClassParent);

    MC := pword(ppointer(integer(_Class)+vmtMethodtable)^);
    if (MC = nil) then exit;
    MethodEntry := pointer(integer(MC)+2);

    for i := 1 to MC^ do
    begin
      MethodName := MethodEntry.name;

      if (EqualStrings(Copy(MethodName, 1, 3), 'lua')) then
      begin
        Delete(MethodName, 1, 3);
        InternalAddProc(true, AClass, MethodName, -1, false, MethodEntry.adr, CodeAddr);
      end;

      inc(integer(MethodEntry), integer(MethodEntry.len));
    end;
  end;

  // добавить published-свойства
  procedure AddPublishedProperties(const _Class: TClass);
  var
    PropCount, i, PropIndex: integer;
    tpinfo: TypInfo.PTypeInfo;
    PropList: TypInfo.PPropList;
    PropInfo: TypInfo.PPropInfo;
    PropName, Prefix: string;
    PropBase: TLuaPropertyInfoBase;
  begin
    tpinfo := _Class.ClassInfo;
    if (tpinfo = nil) then exit;

    PropCount := GetPropList(tpinfo, PropList);
    if (PropCount <> 0) then
    try
      Prefix := _Class.ClassName + '.';

      for i := 0 to PropCount-1 do
      begin
        PropInfo := PropList[i];
        PropName := PropInfo.Name;

        // проверка при ссылке на поле внутри регистратора
        if (ClassRegistrator <> nil) then
        with PropInfo^ do
        begin
          if ((dword(GetProc) >= $FF000000) and (integer(GetProc) and $00FFFFFF >= InstanceSize))
          or ((dword(SetProc) >= $FF000000) and (integer(SetProc) and $00FFFFFF >= InstanceSize)) then
          ELua.Assert('Class registrator "%s" can''t have own fields. Property "%s"', [ClassRegistrator.ClassName, PropName], CodeAddr);
        end;

        // регистрация
        tpinfo := PropInfo.PropType{$ifndef fpc}^{$endif};
        PropBase := GetLuaPropertyBase(Self, Prefix, PropName, tpinfo, CodeAddr, true);
        PropIndex := ClassesInfo[Result].InternalAddName(PropName, false, FInitialized, CodeAddr);
        ClassesInfo[Result].Properties[{InvertIndex} not (PropIndex)].Fill(PropInfo, PropBase);
      end;
    finally
      FreeMem(PropList);
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
    InternalAddClass(Table.UsedClasses.Classes[i]^, True, CodeAddr);

    // регистрация полей
    Field := @Table.Fields[0];
    for i := 0 to Table.Count-1 do
    begin
      // проверка при ссылке на поле внутри регистратора
      if (ClassRegistrator <> nil) and (Field.Offset >= InstanceSize) then
      ELua.Assert('Class registrator "%s" can''t have own fields. Field "%s"', [ClassRegistrator.ClassName, Field.Name], CodeAddr);

      // регистрация
      InternalAddProperty(true, ClassesInfo[Result]._Class, Field.Name, typeinfo(TObject), false, false, pointer(Field.Offset), pointer(Field.Offset), nil, CodeAddr);
      inc(integer(Field), sizeof(integer)+sizeof(word)+sizeof(byte)+pbyte(@Field.Name)^);
    end;
  end;

begin
  if (AClass = nil) then
  ELua.Assert('AClass is not defined', [], CodeAddr);

  // найти имеющийся
  Result := internal_class_index(AClass, false);
  if (Result >= 0) and (not UsePublished) then exit;

  // проверка на класс регистратор
  if (not IsRegistrator(AClass)) then
  begin
    ClassRegistrator := nil;
  end else
  begin
    ClassRegistrator := AClass;

    while (AClass <> nil) do
    begin
      AClass := AClass.ClassParent;
      if (not IsRegistrator(AClass)) then break;
    end;

    if (AClass = nil) then
    ELua.Assert('ClassRegistrator is defined, but really Class not found', [], CodeAddr);

    // найти имеющийся
    Result := internal_class_index(AClass, false);
  end;

  // если не зарегистрирован, то зарегистрировать, зарегистрировав при этом предка
  if (Result < 0) then
  begin
    // проверка на имеющийся RecordInfo или другие типы
    if (internal_class_index_by_name(AClass.ClassName) >= 0) then
    ELua.Assert('Type "%s" is already registered', [AClass.ClassName]);

    // зарегистрировать предков
    if (AClass.ClassParent = nil) then ClassParentIndex := -1
    else ClassParentIndex := InternalAddClass(AClass.ClassParent, UsePublished, CodeAddr);

    // добавление в массив, информация о классе, регистрация метатаблицы
    Result := internal_add_class_info();
    with ClassesInfo[Result] do
    begin
      _Class := AClass;
      _ClassKind := ckClass;      
      _ClassName := AClass.ClassName;
      ParentIndex := ClassParentIndex;
      Ref := internal_register_metatable(CodeAddr, _ClassName, Result);

      // конструктор, дефолтное свойство
      if (ClassParentIndex >= 0) then
      begin
        constructor_address := ClassesInfo[ClassParentIndex].constructor_address;
        constructor_args_count := ClassesInfo[ClassParentIndex].constructor_args_count;
        _DefaultProperty := ClassesInfo[ClassParentIndex]._DefaultProperty;
      end;
    end;

    // добавить в список быстрого поиска  
    internal_add_class_index(AClass, Result);
    internal_add_class_index_by_name(ClassesInfo[Result]._ClassName, Result);
  end;

  // из регистратора взять методы и свойства
  InstanceSize := AClass.InstanceSize;
  if (ClassRegistrator <> nil) then
  begin
    // published-методы
    AddPublishedMethods(ClassRegistrator);

    // published-свойства
    AddPublishedProperties(ClassRegistrator);

    // published поля и их классы (это типа published полей в форме - кнопки и т.д. - компоненты)
    AddPublishedFields(ClassRegistrator);
  end else
  if (UsePublished) then
  begin
    // зарегистрировать published-информацию прям из этого класса
    AddPublishedMethods(AClass);
    AddPublishedProperties(AClass);
    AddPublishedFields(AClass);
  end;
end;

// tpinfo может быть:
// - typeinfo(struct)
// - typeinfo(DynArray of struct)
// - sizeof(struct)
function TLua.InternalAddRecord(const Name: string; tpinfo, CodeAddr: pointer): integer;
var
  RecordTypeInfo: ptypeinfo;
  RecordSize: integer;
  TypeData: PTypeData;
  FieldTable: PFieldTable absolute TypeData;
  RecordInfo: PLuaRecordInfo;
begin
  if (not IsValidIdent(Name)) then
  ELua.Assert('Non-supported record type name ("%s")', [Name], CodeAddr);

  // провека tpinfo, найти sizeof и реальный typeinfo
  begin
    if (tpinfo = nil) then
    ELua.Assert('TypeInfo of record "%s" is not defined', [Name], CodeAddr);

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
          ELua.Assert('Sub dynamic type "%s" is not record type (%s)', [RecordTypeInfo.Name, TypeKindName(RecordTypeInfo.Kind)], CodeAddr);
        end;
      end else
      begin
        ELua.Assert('Type "%s" is not record and subdynamic type (%s)', [Name, TypeKindName(ptypeinfo(tpinfo).Kind)], CodeAddr);
      end;
    end;
  end;

  // найти имеющийся
  if (RecordTypeInfo <> nil) then
  begin
    if (not SameStrings(RecordTypeInfo.Name, Name)) then
    ELua.Assert('Mismatch of names: typeinfo "%s" and "%s" as parameter "Name"', [RecordTypeInfo.Name, Name], CodeAddr);

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
    ELua.Assert('Type "%s" is already registered', [Name], CodeAddr);

    // проверка на соответствие
    with PLuaRecordInfo(_Class)^ do
    begin
      if (Size <> RecordSize) then
      ELua.Assert('Size of %s (%d) differs from the previous value (%d)', [Name, RecordSize, Size], CodeAddr);

      if (FTypeInfo = nil) then FTypeInfo := RecordTypeInfo
      else
      if (FTypeInfo <> RecordTypeInfo) then
      ELua.Assert('TypeInfo of "%s" differs from the previous value', [Name], CodeAddr);
    end;

    exit;
  end;

  // проициализировать RecordInfo
  new(RecordInfo);
  with RecordInfo^ do
  begin
    FLua := Self;
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
    Ref := internal_register_metatable(CodeAddr, _ClassName, Result);

    RecordInfo.FName := Name;
    RecordInfo.FClassIndex := Result;
  end;

  // добавить в список быстрого поиска
  internal_add_class_index(RecordInfo, Result);
  internal_add_class_index_by_name(Name, Result);
  if (RecordTypeInfo <> nil) then internal_add_class_index(RecordTypeInfo, Result);
end;


// itemtypeinfo - обычный тип или recordinfo или arrayinfo
function TLua.InternalAddArray(Identifier, itemtypeinfo, CodeAddr: pointer; const ABounds: array of integer): integer;
const
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
  ELua.Assert('Array identifier is not defined', CodeAddr);
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
    ELua.Assert('Array identifier is not correct', CodeAddr);
    Result := -1;
    exit;
  end;
  if (not IsValidIdent(Dest.Name)) then
  ELua.Assert('Non-supported array type name ("%s")', [Dest.Name], CodeAddr);

  // сбор информации
  with Dest do
  begin
      // проверка имеющегося
      Result := internal_class_index_by_name(Name);
      if (Result < 0) and (arraytypeinfo <> nil) then Result := internal_class_index(arraytypeinfo);
      if (Result >= 0) and (ClassesInfo[Result]._ClassKind <> ckArray) then ELua.Assert('Type "%s" is already registered', [Name], CodeAddr);

      // IsDymanic
      FIsDynamic := (arraytypeinfo <> nil) and (arraytypeinfo.Kind = tkDynArray);
      FBoundsData := IntegerDynArray(ABounds);
      FBounds := pointer(FBoundsData);
      if (IsDynamic <> (Bounds=nil)) then
      begin
        if (IsDynamic) then ELua.Assert('Dynamic array "%s" has no bounds', [Name], CodeAddr)
        else ELua.Assert('Array information of "%s" is not defined', [Name], CodeAddr);
      end;

      // проверка itemtypeinfo, Kind, размер
      if (itemtypeinfo = nil) then
      ELua.Assert('"%s" registering... The typeinfo of %s array item is not defined', [Name, STATIC_DYNAMIC[IsDynamic]], CodeAddr);
      PropertyInfo.Base := GetLuaPropertyBase(Self, '', Name, ptypeinfo(itemtypeinfo), CodeAddr);
      itemtypeinfo := PropertyInfo.Base.Information;
      FItemSize := GetLuaItemSize(PropertyInfo.Base);
      if (FItemSize = 0) then ELua.Assert('"%s" registering... The size of %s array item is not defined', [Name, STATIC_DYNAMIC[IsDynamic]], CodeAddr);


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

            ELua.Assert('Incorrect itemtypeinfo of dynamic array "%s"', [Name], CodeAddr);
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
        if (Dimention and 1 = 1) then ELua.Assert('"%s" registering... Bounds size should be even. %d is an incorrect size', [Name, Dimention], CodeAddr);
        FDimention := FDimention div 2;

        for i := 0 to Dimention-1 do
        if (ABounds[i*2] > ABounds[i*2+1]) then
        ELua.Assert('"%s" registering... Incorrect bounds: "%d..%d"', [Name, ABounds[i*2], ABounds[i*2+1]], CodeAddr);

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
          ELua.Assert('Incorrect bounds of static array "%s"', [Name], CodeAddr);
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
    FInitialized := false;

    with ClassesInfo[Result] do
    begin
      new(PLuaArrayInfo(_Class));
      _ClassKind := ckArray;
      _ClassName := Dest.Name;
      Ref := internal_register_metatable(CodeAddr, _ClassName, Result);

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
end;


// tpinfo - только typeinfo(Set)
function TLua.InternalAddSet(tpinfo, CodeAddr: pointer): integer;
const
  MASK_3 = $FF shl 3;
var
  Name: string;
  TypeData: PTypeData;  
  SetInfo: PLuaSetInfo;
begin
  // проверка tpinfo
  if (tpinfo = nil) then
  ELua.Assert('TypeInfo of set is not defined', [], CodeAddr);
  if (ptypeinfo(tpinfo).Kind <> tkSet) then ELua.Assert('TypeInfo of set is not correct: TypeKind = %s', [TypeKindName(ptypeinfo(tpinfo).Kind)], CodeAddr);

  // имя
  Name := ptypeinfo(tpinfo).Name;

  // поиск имеющегося
  Result := internal_class_index(tpinfo);
  if (Result < 0) then Result := internal_class_index_by_name(Name); 
  if (Result >= 0) and (ClassesInfo[Result]._ClassKind <> ckSet) then ELua.Assert('Type "%s" is already registered', [Name], CodeAddr);

  // добавление
  if (Result < 0) then
  begin
    Result := internal_add_class_info();
    FInitialized := false;

    new(SetInfo);
    with ClassesInfo[Result] do
    begin
      _Class := SetInfo;
      _ClassKind := ckSet;
      _ClassName := Name;
      Ref := internal_register_metatable(CodeAddr, _ClassName, Result);

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
    if (SetInfo.FTypeInfo.Kind = tkEnumeration) and (not IsTypeInfo_Boolean(SetInfo.FTypeInfo)) then Self.RegEnum(SetInfo.FTypeInfo);

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
end;

function TLua.InternalAddProc(const IsClass: boolean; AClass: pointer; const ProcName: string; ArgsCount: integer; const with_class: boolean; Address, CodeAddr: pointer): integer;
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
end;

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
  ELua.Assert('TypeInfo of %s "%s" is not defined', [ClassInfo.PropertyIdentifier, PropertyName], CodeAddr);

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
end;


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
end;

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
end;


// вызов метода + корректное восстановление стека
procedure TMethod_Call(const ASelf, P1, P2: integer; const Code: pointer);
asm
  push esi
  mov esi, esp
    CALL [EBP+8]
  mov esp, esi
  pop esi
end;

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
end;


// проверка на допустимость вызова дефолтного свойства
function __can_jump_default_property(const Lua: TLua; const luatype: integer; const PropInfo: TLuaPropertyInfo): boolean;
begin
  Result := (luatype <> LUA_TSTRING);

  // проверка, если аргумент - строка
  if (not Result) then
  with PropInfo, PLuaRecordInfo(Parameters)^ do
  if (Parameters <> INDEXED_PROPERTY) then                                       {todo потом посмотреть более детально}
  Result := (Parameters = NAMED_PROPERTY) or (Lua.ClassesInfo[FClassIndex].Properties[0].Base.Kind = pkString);
end;

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


// самый распространённый метод
// когда у класса/структуры нужно найти свойство или метод. В этом случае prop_struct = nil
//
// но так как концепция свойств основополагающая в CrystalLUA, то метод по сути является
// базовым для чтения данных так же из массивов, глобального пространства, при создании референсов.
// в этом случае prop_struct заполнен (<> nil)
function TLua.__index_prop_push(const ClassInfo: TLuaClassInfo; const prop_struct: PLuaPropertyStruct): integer;
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
  instance: pointer absolute userdata;
  Obj: TObject absolute instance;
  Value: TPackedValue;

  // ошибка: ничего не нaйдено
  procedure ThrowFoundNothing();
  const
    instance_type: array[boolean] of string = ('instance', 'type');
  begin
    TStackArgument(@TLua.StackArgument)(Self, 2, FBufferArg.str_data);
    ScriptAssert('"%s" not found in %s %s', [FBufferArg.str_data, ClassInfo._ClassName, instance_type[userdata = nil]]);
  end;

  // если что-то не так в user-data
  procedure ThrowFailInstance();
  begin
    // анализ userdata
    if (userdata = nil) then
    ScriptAssert('%s.%s property is not class property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);

    // instance empty ?
    if (userdata.instance = nil) then
    ScriptAssert('Instance (%s) is already destroyed', [ClassInfo._ClassName]);

    // writeonly
    if (PropertyInfo.read_mode = MODE_NONE_USE) then
    ScriptAssert('%s.%s property is writeonly property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);
  end;

begin
  Result := 1;

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
  // основной код функции - кладём в стек значение свойства
  with PropertyInfo^ do
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
                     if (Base.Kind = pkClass) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, ClassesInfo[internal_class_index(Value.p, true)].Ref)
                     else
                     // pkObject
                     if (TClass(Value.p^) = TLuaReference) then lua_rawgeti(Handle, LUA_REGISTRYINDEX, TLuaReference(Value.p).Index)
                     else
                     push_userdata(ClassesInfo[internal_class_index(TClass(Value.p^), true)], false, Value.p);
                   end;
         pkRecord,
          pkArray,
            pkSet: CrystalLUA.GetPushDifficultTypeProp(Self, instance, is_const, PropertyInfo^);

      pkUniversal: CrystalLUA.GetPushUniversalTypeProp(Self, instance, is_const, PropertyInfo^);
      end;
  end;
end;

// изменить свойство
// изменить функцию нельзя !
function TLua.__newindex_prop_set(const ClassInfo: TLuaClassInfo; const prop_struct: PLuaPropertyStruct): integer;
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
  ProcInfo: ^TLuaProcInfo;
  PropertyInfo: ^TLuaPropertyInfo;

  stack_index: integer; // откуда брать параметр (для присвоения)
  instance: pointer absolute userdata;
  Obj: TObject absolute instance;
  PV: PVarData absolute instance;
  ClassIndex: integer absolute SLength;
  Value: TPackedValue;

  // ошибка: ничего не нaйдено
  procedure ThrowFoundNothing();
  begin
    TStackArgument(@TLua.StackArgument)(Self, 2, FBufferArg.str_data);
    ScriptAssert('"%s" not found in %s %s', [FBufferArg.str_data, ClassInfo._ClassName, instance_type[userdata = nil]]);
  end;

  // если что-то не так в user-data
  procedure ThrowFailInstance();
  begin
    // анализ userdata
    if (userdata = nil) then
    ScriptAssert('%s.%s property is not class property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);

    // instance empty ?
    if (userdata.instance = nil) then
    ScriptAssert('Instance (%s) is already destroyed', [ClassInfo._ClassName]);

    // неизменяемая структура
    if (UserData.is_const) then
    ScriptAssert('Field "%s" is a constant field', [PropertyInfo.PropertyName]);

    // readonly
    if (PropertyInfo.write_mode = MODE_NONE_USE) then
    ScriptAssert('%s.%s property is readonly property', [ClassInfo._ClassName, PropertyInfo.PropertyName]);
  end;

  // если не получается присвоить ("тип" в FBufferArg.str_data)
  procedure ThrowAssignValue(const look_luatype: boolean=false);
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
    else ScriptAssert(Desc, []);
  end;

  // преобразовать значение в стеке в число
  // или вызывать ошибку
  function CastValueAsNumber(): extended;
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
  end;

  // проверить datauser, находящийся в Value.p на экземпляр класса
  // если всё нет - вызывается ошибка
  // если всё хорошо - то в Value.p заносится instance (TObject)
  procedure CastUserDataAsTObject();
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
  end;


  // в случае изменения "стандартного свойства"
  // true если "прокатило"
  function change_std_prop(): boolean;
  begin
    Result := false;

    // изменение значения TLuaReference
    if (ClassInfo._ClassIndex = TLUA_REFERENCE_CLASS_INDEX) and (stdindex = STD_VALUE) and
       (userdata <> nil) and (userdata.instance <> nil) then
    begin
      lua_pushvalue(Handle, 3);
      lua_rawseti(Handle, LUA_REGISTRYINDEX, TLuaReference(userdata.instance).Index);
      Result := true;
      exit;
    end;

    // проверка на изменение стандартного поля
    if (stdindex in [STD_TYPE..STD_IS_EMPTY]) or ((ClassInfo._ClassKind=ckClass)=(stdindex in [STD_CREATE, STD_FREE]))
    or ((ClassInfo._ClassKind=ckSet)=(stdindex in [STD_LOW, STD_HIGH, STD_INCLUDE..STD_CONTAINS]))
    then
    ScriptAssert('Standard field "%s" can not be changed in %s %s', [S, ClassInfo._ClassName, instance_type[userdata = nil]]);
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
  ScriptAssert('Method %s.%s can not be changed', [ClassInfo._ClassName, S]);

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
      ScriptAssert('%s.%s property should have parameters', [ClassInfo._ClassName, PropertyInfo.PropertyName])
    end;

    exit;
  end;

  // доводим параметры
  instance := userdata.instance;
  stack_index := 3;


PROP_POPSET:
  // очень важно, какой тип пытаемся присвоить
  // если не получается - надо вызывать ошибку
  luatype := lua_type(Handle, stack_index);

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
  end;
end;

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
end;



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
end;

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


// основная функция по взятию значения глобальной переменной
// причём в двух вариантах
// native - значит нужно вернуть значение в info
// иначе - вызывается из lua, надо вернуть значение в стек
// или Exception !
function TLua.__global_index(const native: boolean; const info: TLuaGlobalModifyInfo): integer;
var
  Name: pchar;
  luatype, NameLen, Ind: integer;

  // вызвать ошибку
  procedure Assert(const FmtStr: string; const Args: array of const);
  begin
    if (native) then ELua.Assert(FmtStr, Args, info.CodeAddr)
    else ScriptAssert(FmtStr, Args);
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
        lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
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
        lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
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
        lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
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
end;

// основная функция по изменению значения глобальной переменной
// в двух вариантах
// native - значит имя и значения берутся из info
// иначе - из стека
// если глобальная переменная неизменяема, то Exception !
function TLua.__global_newindex(const native: boolean; const info: TLuaGlobalModifyInfo): integer;
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
    else ScriptAssert(FmtStr, Args);
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
      lua_rawseti(Handle, LUA_REGISTRYINDEX, Ref); //global_fill_value(Ref);
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
      lua_rawseti(Handle, LUA_REGISTRYINDEX, Ref); //global_fill_value(Ref);
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
      lua_rawseti(Handle, LUA_REGISTRYINDEX, Ref); //global_fill_value(Ref);
    end else
    begin
      if (not push_variant(info.V^)) then AssertUnsupported();
      FillGlobalProp(Index);
      lua_settop(Handle, -1-1); //stack_pop();
    end;

    exit;       
  end;
end;

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
end;

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
end;

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
  
end;

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
end;


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
        Move(SrcInstance^, DestInstance^, CopyCount*ItemSize);
      end else
      begin
        CopyArray(DestInstance, SrcInstance, ItemTypeInfo, CopyCount*ItemsCount);
      end;

      Len := CurLen;
    end;
  end;      
end;


// враппер для определения входит ли операнд в множество Dest
function  _SetLe_Wrap(const Dest, X1, X2: pointer; const Size: integer): boolean;
begin _SetLe_Wrap := _SetLe(X2, Dest, Size); end;


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
end;


function TLua.ProcCallback(const ClassInfo: TLuaClassInfo; const ProcInfo: TLuaProcInfo): integer;
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
      TLuaProc2(ProcInfo.Address)(FArgs, FBufferArg) // глобальная
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
end;

function TLua.VariableExists(const Name: string): boolean;
var
  Ind: integer;
  luatype: integer;
begin
  if (not FInitialized) then INITIALIZE_NAME_SPACE();

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
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
    luatype := (lua_type(Handle, -1));
    lua_settop(Handle, -1-1); //stack_pop();

    Result := (luatype <> LUA_TNONE) and (luatype <> LUA_TFUNCTION);
    exit;
  end;

  Result := false;
end;

function TLua.ProcExists(const ProcName: string): boolean;
var
  Ind: integer;
begin
  if (not FInitialized) then INITIALIZE_NAME_SPACE();

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
    lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
    Result := (lua_type(Handle, -1) = LUA_TFUNCTION);
    lua_settop(Handle, -1-1); //stack_pop();
    exit;
  end;

  Result := false;
end;

procedure __TLuaCall_luaargs(const Self: TLua; const ProcName: string; const _Args: TLuaArgs;
                            var Result: TLuaArg; const ReturnAddr: pointer);
var
  Found: boolean;
  i, Ind, ret,RetCount: integer;    
begin
  with Self do
  begin
    if (not FInitialized) then INITIALIZE_NAME_SPACE();

    // запушить соответствующую cfunction если найден
    Found := GlobalVariablePos(pchar(ProcName), Length(ProcName), Ind);
    if (Found) then
    with GlobalVariables[Ind] do
    if (_Kind = gkLuaData) then
    begin
      lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
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
    ret := lua_pcall(Handle, Length(_Args), LUA_MULTRET, 0);
    if (ret = 0) then ret := lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
    if (ret <> 0) then Check(ret, ReturnAddr);

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
end;


procedure __TLuaCall__arguments(const Self: TLua; const ProcName: string; const _Args: array of const;
                                var Result: TLuaArg; const ReturnAddr: pointer);
var
  Found: boolean;
  i, Ind, ret, RetCount: integer;
begin
  with Self do
  begin
    if (not FInitialized) then INITIALIZE_NAME_SPACE();

    // запушить соответствующую cfunction если найден
    Found := GlobalVariablePos(pchar(ProcName), Length(ProcName), Ind);
    if (Found) then
    with GlobalVariables[Ind] do
    if (_Kind = gkLuaData) then
    begin
      lua_rawgeti(Handle, LUA_REGISTRYINDEX, Ref); //global_push_value(Ref);
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
    ret := lua_pcall(Handle, Length(_Args), LUA_MULTRET, 0);
    if (ret = 0) then ret := lua_gc(Handle, 2{LUA_GCCOLLECT}, 0);
    if (ret <> 0) then Check(ret, ReturnAddr);

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
end;


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
end;

procedure __TLuaRegClass(const Self: TLua; const AClass: TClass; const use_published: boolean; const ReturnAddr: pointer);
begin
  Self.InternalAddClass(AClass, use_published, ReturnAddr);
end;

procedure TLua.RegClass(const AClass: TClass; const use_published: boolean);
asm
  push [esp]
  jmp __TLuaRegClass
end;

procedure __TLuaRegClasses(const Self: TLua; const AClasses: array of TClass; const use_published: boolean; const ReturnAddr: pointer);
var
  i: integer;
begin
  for i := 0 to high(AClasses) do
  Self.InternalAddClass(AClasses[i], use_published, ReturnAddr);
end;

procedure TLua.RegClasses(const AClasses: array of TClass; const use_published: boolean);
asm
  pop ebp
  push [esp]
  jmp __TLuaRegClasses
end;


// tpinfo может быть:
// - typeinfo(struct)
// - typeinfo(DynArray of struct)
// - sizeof(struct)
function  __TLuaRegRecord(const Self: TLua; const Name: string; const tpinfo: ptypeinfo; const ReturnAddr: pointer): PLuaRecordInfo;
begin
  Result := PLuaRecordInfo(Self.ClassesInfo[Self.InternalAddRecord(Name, tpinfo, ReturnAddr)]._Class);
end;

function  TLua.RegRecord(const Name: string; const tpinfo: ptypeinfo): PLuaRecordInfo;
asm
  push [esp]
  jmp __TLuaRegRecord
end;                 

// itemtypeinfo - обычный тип или recordinfo или arrayinfo
function  __TLuaRegArray(const Self: TLua; const Identifier: pointer; const itemtypeinfo: pointer; const Bounds: array of integer; const ReturnAddr: pointer): PLuaArrayInfo;
begin
  Result := Self.ClassesInfo[Self.InternalAddArray(Identifier, itemtypeinfo, ReturnAddr, Bounds)]._Class;
end;

function  TLua.RegArray(const Identifier: pointer; const itemtypeinfo: pointer; const Bounds: array of integer): PLuaArrayInfo;
asm
  pop ebp
  push [esp]
  jmp __TLuaRegArray
end;


function  __TLuaRegSet(const Self: TLua; const tpinfo: ptypeinfo; const ReturnAddr: pointer): PLuaSetInfo;
begin
  Result := Self.ClassesInfo[Self.InternalAddSet(tpinfo, ReturnAddr)]._Class;
end;

function  TLua.RegSet(const tpinfo: ptypeinfo): PLuaSetInfo;
asm
  mov ecx, [esp]
  jmp __TLuaRegSet
end;

procedure __TLuaRegProc_global(const Self: TLua; const ProcName: string; const Proc: TLuaProc;
                               const ArgsCount: integer; const ReturnAddr: pointer);
begin
  Self.InternalAddProc(true, nil, ProcName, ArgsCount, false, @Proc, ReturnAddr);
end;

procedure TLua.RegProc(const ProcName: string; const Proc: TLuaProc; const ArgsCount: integer);
asm
  pop ebp
  push [esp]
  jmp __TLuaRegProc_global
end;


procedure __TLuaRegProc_class(const Self: TLua; const AClass: TClass; const ProcName: string;
          const Proc: TLuaClassProc; const ArgsCount: integer; const with_class: boolean; const ReturnAddr: pointer);
begin
  if (AClass = nil) then
  ELua.Assert('AClass is not defined', [], ReturnAddr);

  Self.InternalAddProc(true, AClass, ProcName, ArgsCount, with_class, TMethod(Proc).Code, ReturnAddr);
end;

procedure TLua.RegProc(const AClass: TClass; const ProcName: string; const Proc: TLuaClassProc; const ArgsCount: integer; const with_class: boolean);
asm
  pop ebp
  push [esp]
  jmp __TLuaRegProc_class
end;


// зарегистрировать свойство
procedure __TLuaRegProperty(const Self: TLua; const AClass: TClass; const PropertyName: string; const tpinfo: pointer;
          const PGet, PSet: pointer; const parameters: PLuaRecordInfo; const default: boolean; const ReturnAddr: pointer);
begin
  // базовые проверки
  if (AClass = nil) then
  ELua.Assert('AClass is not defined', [], ReturnAddr);

  if (PGet = nil) and (PSet = nil) then
  ELua.Assert('The %s.%s property has no setter and getter', [AClass.ClassName, PropertyName], ReturnAddr);

  // регистрация
  Self.InternalAddProperty(true, AClass, PropertyName, tpinfo, false, default, PGet, PSet, parameters, ReturnAddr);
end;

procedure TLua.RegProperty(const AClass: TClass; const PropertyName: string; const tpinfo: pointer; const PGet, PSet: pointer; const parameters: PLuaRecordInfo; const default: boolean);
asm
  pop ebp
  push [esp]
  jmp __TLuaRegProperty
end;

procedure __TLuaRegVariable(const Self: TLua; const VariableName: string; const X; const tpinfo: pointer; const IsConst: boolean; const ReturnAddr: pointer);
var
  P: pointer;

begin
  P := @X;
  if (P = nil) then
  ELua.Assert('Pointer to variable "%s" is not defined', [VariableName], ReturnAddr);

  // регистрация
  Self.InternalAddProperty(true, GLOBAL_NAME_SPACE, VariableName, tpinfo, IsConst, false, P, P, nil, ReturnAddr);
end;

procedure TLua.RegVariable(const VariableName: string; const X; const tpinfo: pointer; const IsConst: boolean);
asm
  pop ebp
  push [esp]
  jmp __TLuaRegVariable
end;

procedure __TLuaRegConst_variant(const Self: TLua; const ConstName: string; const Value: Variant; const ReturnAddr: pointer);
var
  Ref: integer;
  VarData: TVarData absolute Value;
begin
  // проверка
  if (not IsValidIdent(ConstName)) then
  ELua.Assert('Invalid constant name "%s"', [ConstName], ReturnAddr);

  // доступный ли Variant
  if (VarData.VType <> varString) and (not(VarData.VType in VARIANT_SUPPORT)) then
  ELua.Assert('Not supported variant value', [], ReturnAddr);

  // регистрация
  Ref := Self.internal_register_global(ConstName, gkConst, ReturnAddr).Ref;

  // пуш
  if (not Self.push_variant(Value)) then
  ELua.Assert('Not supported variant value "%s"', [Self.FBufferArg.str_data], ReturnAddr);

  // присвоение
  Self.global_fill_value(Ref);
end;


procedure TLua.RegConst(const ConstName: string; const Value: Variant);
asm
  push [esp]
  jmp __TLuaRegConst_variant
end;

procedure __TLuaRegConst_luaarg(const Self: TLua; const ConstName: string; const Value: TLuaArg; const ReturnAddr: pointer);
var
  Ref: integer;
begin
  // проверка
  if (not IsValidIdent(ConstName)) then
  ELua.Assert('Invalid constant name "%s"', [ConstName], ReturnAddr);

  // доступный ли Value
  if (byte(Value.LuaType) >= byte(ltTable)) then
  ELua.Assert('Not supported argument value', [], ReturnAddr);

  // регистрация
  Ref := Self.internal_register_global(ConstName, gkConst, ReturnAddr).Ref;

  // пуш
  if (not Self.push_luaarg(Value)) then
  ELua.Assert('Not supported argument value "%s"', [Self.FBufferArg.str_data], ReturnAddr);

  // присвоение
  Self.global_fill_value(Ref);
end;

procedure TLua.RegConst(const ConstName: string; const Value: TLuaArg);
asm
  push [esp]
  jmp __TLuaRegConst_luaarg
end;

procedure __TLuaRegEnum(const Self: TLua; const EnumTypeInfo: ptypeinfo; const ReturnAddr: pointer);
var
  i, Ref: integer;
  S: string;
begin
  with Self do
  begin
    // если в списке такой уже имеется
    if (InsortedPos4(integer(EnumTypeInfo), EnumerationList) >= 0) then exit;

    // проверка
    if (EnumTypeInfo = nil) then
    ELua.Assert('EnumTypeInfo is not defined', ReturnAddr);

    if (EnumTypeInfo.Kind <> tkEnumeration) or (IsTypeInfo_Boolean(EnumTypeInfo)) then
    ELua.Assert('Type "%s" (kind: %s) is not enumeration',
               [EnumTypeInfo.Name, TypeKindName(EnumTypeInfo.Kind)], ReturnAddr);


    // добавить в список EnumerationList
    i := InsortedPlace4(integer(EnumTypeInfo), pointer(EnumerationList), Length(EnumerationList));
    ptypeinfo(DynArrayInsert(EnumerationList, typeinfo(TIntegerDynArray), i)^) := EnumTypeInfo;

    // регистрация каждого enum-а
    with GetTypeData(EnumTypeInfo)^ do
    for i := MinValue to MaxValue do
    begin
      S := GetEnumName(EnumTypeInfo, byte(i));

      Ref := internal_register_global(S, gkConst, ReturnAddr).Ref;
      lua_pushinteger(Handle, i);
      global_fill_value(Ref);
    end;
  end;
end;

procedure TLua.RegEnum(const EnumTypeInfo: ptypeinfo);
asm
  mov ecx, [esp]
  jmp __TLuaRegEnum
end;


function TLua.GetUnit(const index: integer): TLuaUnit;
begin
  if (dword(index) >= dword(FUnitsCount)) then
  {$ifdef NO_CRYSTAL}TExcept{$else}EWrongParameter{$endif}.Assert('Can''t get unit[%d]. Units count = %d', [index, FUnitsCount]);

  GetUnit := FUnits[index];
end;

function TLua.GetUnitByName(const Name: string): TLuaUnit;
var
  i: integer;
begin
  for i := 0 to FUnitsCount-1 do
  begin
    Result := FUnits[i];
    if (EqualStrings(Result.FName, Name)) then exit;
  end;

  Result := nil;
end;











{ TLuaUnit }

// анализировать Text, собрать информацию по строкам
procedure TLuaUnit.InitializeLinesInfo();
var
  Last, Current, Max: pchar;

  procedure Add();
  begin
    inc(FLinesCount);
    SetLength(FLinesInfo, FLinesCount);
    with FLinesInfo[FLinesCount-1] do
    begin
      Str := Last;
      Length := integer(Current)-integer(Last);
    end;

    // inc Current
    if (Current^ = #13) and (Current <> Max) and (Current[1] = #10) then inc(Current, 2)
    else inc(Current, 1);

    // Last
    Last := Current;
  end;
begin
  FLinesCount := 0;
  FLinesInfo := nil;
  if (FText = '') then exit;

  Last := pchar(pointer(FText));
  Current := Last;
  Max := pchar(@Last[Length(FText)-1]);

  while (integer(Current) <= integer(Max)) do
  begin
    if (Current^ in [#13, #10]) then Add()
    else
    inc(Current);
  end;

  if (Last <> Current) then Add();
end;

procedure TLuaUnit.SaveToStream(const Stream: TStream);
begin
  Stream.WriteBuffer(pointer(FText)^, Length(FText));
end;

procedure TLuaUnit.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TLuaUnit.SaveToFile();
begin
  if (FileName <> '') then SaveToFile(FileName)
  else SaveToFile(Name);
end;

function TLuaUnit.GetLine(index: integer): string;
begin
  if (dword(index) >= dword(FLinesCount)) then
  {$ifdef NO_CRYSTAL}TExcept{$else}EWrongParameter{$endif}.Assert('Can''t get line %d from unit "%s". Lines count = %d', [index, Name, FLinesCount]);

  with FLinesInfo[index] do
  AnsiFromPCharLen(Result, Str, Length);
end;

function TLuaUnit.GetLineInfo(index: integer): TLuaUnitLineInfo;
begin
  if (dword(index) >= dword(FLinesCount)) then
  {$ifdef NO_CRYSTAL}TExcept{$else}EWrongParameter{$endif}.Assert('Can''t get line info %d from unit "%s". Lines count = %d', [index, Name, FLinesCount]);

  GetLineInfo := FLinesInfo[index];
end;



initialization
  InitTypInfoProcs();
  {$ifdef LUA_INITIALIZE} Lua := CreateLua(); {$endif}

finalization
  {$ifdef LUA_INITIALIZE} FreeAndNil(Lua); {$endif}
  FreeLuaHandle();
  CFunctionDumps := nil;


end.
