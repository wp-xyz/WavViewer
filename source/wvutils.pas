unit wvUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

function CreateIni: TCustomIniFile;


implementation

uses
  Forms;

function CreateIni: TCustomIniFile;
var
  fn: String;
begin
  fn := ChangeFileExt(Application.ExeName, '.ini');
  Result := TIniFile.Create(fn);
end;


end.

