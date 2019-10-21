# CrystalLUA

<img align="right" src="data/logo.png">

[Lua](https://www.lua.org/start.html) is a lightweight, high-level, multi-paradigm programming language designed primarily for embedded use in applications. CrystalLUA is a high-level library for binding your native Delphi code and scripts in Lua language. The difference between CrystalLUA and other binding libraries is in high code performance and convenient registration based on RTTI. This project was developed many years ago, you can use an [outdated version](https://github.com/d-mozulyov/CrystalLUA/archive/0.1.zip) (Delphi 6-2007) of the library.

Now the library is actively developing, despite the fact that not all the features declared in the old versions have been implemented, many others are already supported: Unicode, 64 bits, extended RTTI, executable types (functions, events, references). In addition, the [RLua](https://gitlab.com/d-mozulyov/rlua) fork is being developed, which over time will make CrystalLUA cross-platform and independent of dynamic libraries.

Below are a few examples that will help you quickly master CrystalLUA. Enjoy :).


### Hello, World!

`TLua` is the main class of the library, which allows you to execute scripts and register types, functions and variables. Now it uses a dynamic library, so before calling the constructor, you must specify the relative or full path `LuaLibraryPath`. There are several ways to execute a script; the simplest is the `RunScript` method, which takes a script text parameter.

```pascal
program HelloWorld;
{$APPTYPE CONSOLE}
uses
  SysUtils, CrystalLUA;
var
  Lua: TLua;
begin
  LuaLibraryPath := {$ifdef CPUX86}'lua.dll'{$else}'lua64.dll'{$endif};
  Lua := TLua.Create;
  try
    Lua.RunScript('print("Hello, World!\n")');
  finally
    Lua.Free;
  end;
  Write('Press Enter to quit');
  Readln;
end.
```

```
Hello, World!
Press Enter to quit
```

### Script functions calling

You can load a script from a file, memory or resource using the `LoadScript` method. In this example, the script is loaded from a set of strings that are converted to `TBytes`. You can call any script function and get the result.

```pascal
program ScriptFunctions;
{$APPTYPE CONSOLE}
uses
  SysUtils, CrystalLUA, Classes;
var
  Lua: TLua;
  a, b, c, r: Integer;
  Bytes: TBytes;
begin
  LuaLibraryPath := {$ifdef CPUX86}'lua.dll'{$else}'lua64.dll'{$endif};
  Lua := TLua.Create;
  try
    a := 1;
    b := 2;
    c := 3;
    Bytes := TEncoding.UTF8.GetPreamble + TEncoding.UTF8.GetBytes(
      'function myfunc(a, b, c)'#13 +
      '  return (a + b * c)'#13 +
      'end');
    Lua.LoadScript(Bytes);
    Write(a, ' + ', b, ' * ', c, ' = ');
    r := Lua.Call('myfunc', [a, b, c]).Items[0].AsInteger;
    Writeln(r);
  finally
    Lua.Free;
  end;
  Write('Press Enter to quit');
  Readln;
end.
```

```
1 + 2 * 3 = 7
Press Enter to quit
```


### Involvement of components (VCL, FMX, etc.)

The library allows you to bind your native code and scripts. The CrystalLUA namespace contains only those elements that you register in it. But by default, all the information that was found in the RTTI gets into the namespace. Therefore, by registering the global variable `Form1: TForm1`, you get access to all its public properties, methods and child components.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  LuaLibraryPath := {$ifdef CPUX86}'lua.dll'{$else}'lua64.dll'{$endif};
  FLua := TLua.Create;
  FLua.RegVariable('Form1', Form1, TForm1);
end;
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FLua.Free;
end;
procedure TForm1.btnRunClick(Sender: TObject);
begin
  // FLua.RunScript('Form1.btnRun.Left = Form1.btnRun.Left + 10');
  FLua.RunScript('Form1.Caption = "Component property changed!"');
end;
```
![](data/example_vcl.png)


### Native functions calling

Modern Delphi allows you to receive declared [methods RTTI](http://docwiki.embarcadero.com/RADStudio/Rio/en/RTTI_directive_(Delphi)). This example demonstrates how to call a native function from a script.

```pascal
  public
    { Public declarations }
    function AskQuestion(const ATitle, AQuestion: string): Boolean;
  end;
  
function TForm1.AskQuestion(const ATitle, AQuestion: string): Boolean;
begin
  Result := Application.MessageBox(PChar(AQuestion), PChar(ATitle), MB_YESNO) = IDYES;
end;
procedure TForm1.btnRunClick(Sender: TObject);
begin
  // FLua.RunScript('Form1:Close()');
  FLua.RunScript('Form1.Caption = tostring(Form1:AskQuestion("My title", "Do you like the library?"))');
end;
```
![](data/example_method.png)


### Executable types

The library allows you to read, modify and execute types such as functions, events, references. Remember that references have RTTI if `{$M+}` directive declared. The example below shows how to read and execute a variable of your type.

```pascal
type
  {$M+}
  TMyReference = reference to function(const ATitle, AQuestion: string): Boolean;
  TForm1 = class(TForm)
    btnRun: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    FLua: TLua;
    FMyReference: TMyReference;
    function AskQuestion(const ATitle, AQuestion: string): Boolean;
  public
    property MyReference: TMyReference read FMyReference write FMyReference;
  end;
  
procedure TForm1.FormCreate(Sender: TObject);
begin
  LuaLibraryPath := {$ifdef CPUX86}'lua.dll'{$else}'lua64.dll'{$endif};
  FLua := TLua.Create;
  FLua.RegVariable('Form1', Form1, TForm1);
  FMyReference := AskQuestion;  
end;
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FLua.Free;
end;
procedure TForm1.btnRunClick(Sender: TObject);
begin
  FLua.RunScript(
    'local Func = Form1.MyReference'#13 +
    'Form1.Caption = tostring(Func("My title", "Do you like the library?"))');
end;  
```  