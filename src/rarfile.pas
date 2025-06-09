unit rarfile;

interface

uses
  SysUtils, Classes, arcbase, unrar;

type
  TRarFile = class(TBaseArcFile)
  private
    fFileName: string;
    fFiles: TStringList;
    flasterror: string;
    fRAR: TRAR;
  protected
    function GetFile(Index: Integer): string; override;
    procedure Load; override;
    procedure Clear; override;
    procedure SetFileName(const Value: string); override;
    function GetFileCount: integer; override;
    function GetFileName: string; override;
    procedure RARListFile(Sender: TObject; const aFileInformation: TRARFileItem);
    procedure RARError(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);
    procedure RARNextVolumeRequired(Sender: TObject; const aRequiredFileName: AnsiString; out oNewFileName: AnsiString; out oCancel: Boolean);
    procedure RARPasswordRequired(Sender: TObject; const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: Boolean);
    procedure RARProgress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
  public
    constructor Create(const aFileName: string); override;
    destructor Destroy; override;
    function GetRARFileData(const Index: integer; var p: pointer;
      var size: integer): boolean; overload; virtual;
    function GetRARFileData(const Name: string; var p: pointer;
      var size: integer): boolean; overload; virtual;
    function ExtractFiles(const alist: TStringList; const aalias: TStringList; const apath: string = ''): boolean; override;
    property FileName: string read GetFileName write SetFileName;
    property Files[Index: Integer]: string read GetFile;
    property FileCount: integer read GetFileCount;
    property RAR: TRAR read fRAR;
    property lasterror: string read flasterror;
  end;

implementation

uses
  tmpfiles;

constructor TRarFile.Create(const aFileName: string);
begin
  inherited Create(aFileName);

  fFileName := aFileName;
  fFiles := TStringList.Create;

  fRAR := TRAR.create(nil);

  fRAR.onError := RARError;
  fRAR.onListFile := RARListFile;

  fRAR.onPasswordRequired := RARPasswordRequired;
  fRAR.onNextVolumeRequired := RARNextVolumeRequired;
  fRAR.onProgress := RARProgress;

  Load;
end;

destructor TRarFile.Destroy;
begin
  fRAR.Free;
  fFiles.Free;
  inherited Destroy;
end;

function TRarFile.GetFile(Index: Integer): string;
begin
  Result := fFiles[Index];
end;

procedure TRarFile.Load;
begin
  Clear;
  fRAR.addFile(fFileName);
  fRAR.listArchive(fFileName);
end;

procedure TRarFile.Clear;
begin
  flasterror := '';
  fFiles.Clear;
  fRAR.clearFiles;
end;

procedure TRarFile.SetFileName(const Value: string);
begin
  if fFileName <> Value then
  begin
    fFileName := Value;
    Load;
  end;
end;

function TRarFile.GetFileCount: integer;
begin
  Result := fFiles.Count;
end;

function TRarFile.GetFileName: string;
begin
  Result := fFileName;
end;

procedure TRarFile.RARListFile(Sender: TObject; const aFileInformation: TRARFileItem);
begin
  fFiles.Add(aFileInformation.fileName);
end;

procedure TRarFile.RARError(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);
begin
  flasterror := Format('Last RAR error code: %d', [aErrorCode]);
end;

procedure TRarFile.RARNextVolumeRequired(Sender: TObject; const aRequiredFileName: AnsiString; out oNewFileName: AnsiString; out oCancel: Boolean);
begin
end;

procedure TRarFile.RARPasswordRequired(Sender: TObject; const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: Boolean);
begin
end;

procedure TRarFile.RARProgress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
end;

var
  tmpidx: Integer = 0;

function TRarFile.GetRARFileData(const Index: integer; var p: pointer;
  var size: integer): boolean;
var
  tmpfile: string;
  l1, l2: TStringList;
  fs: TFileStream;
begin
  tmpfile := I_NewTempFile('rar' + IntToStr(tmpidx));
  Inc(tmpidx);
  l1 := TStringList.Create;
  l2 := TStringList.Create;
  l1.Add(fFiles.Strings[index]);
  l2.Add(ExtractFileName(tmpfile));
  Result := fRAR.ExtractArchiveEx(fFileName, ExtractFilePath(tmpfile), l1, l2);
  if Result and FileExists(tmpfile) then
  begin
    fs := TFileStream.Create(tmpfile, fmOpenRead);
    size := fs.Size;
    GetMem(p, size);
    fs.Read(p^, size);
    fs.Free;
    Result := True;
  end
  else
    Result := False;
end;

function TRarFile.GetRARFileData(const Name: string; var p: pointer;
  var size: integer): boolean;
var
  idx: integer;
begin
  idx := fFiles.IndexOf(Name);
  if idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := GetRARFileData(idx, p, size);
end;

function TRarFile.ExtractFiles(const alist: TStringList; const aalias: TStringList; const apath: string = ''): boolean;
begin
  try
    Result := fRAR.ExtractArchiveEx(fFileName, apath, alist, aalias);
  except
    Result := False;
  end;
end;

end.
