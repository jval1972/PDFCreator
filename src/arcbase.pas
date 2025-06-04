unit arcbase;

interface

uses
  SysUtils, Classes;

type
  TBaseArcFile = class(TObject)
  protected
    function GetFile(Index: Integer): string; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure SetFileName(const Value: string); virtual; abstract;
    function GetFileCount: integer; virtual; abstract;
    function GetFileName: string; virtual; abstract;
  public
    constructor Create(const aFileName: string); virtual;
    function ExtractFiles(const alist: TStringList; const aalias: TStringList; const apath: string = ''): boolean; virtual; abstract;
    property FileName: string read GetFileName write SetFileName;
    property Files[Index: Integer]: string read GetFile;
    property FileCount: integer read GetFileCount;
  end;

implementation

constructor TBaseArcFile.Create(const aFileName: string);
begin
  inherited Create;
end;

end.
