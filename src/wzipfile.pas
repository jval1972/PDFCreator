unit wzipfile;

interface

uses
  SysUtils, Classes, Types, arcbase, zlib, Windows;

type
  TCommonFileHeader = packed record
    VersionNeededToExtract: word;
    GeneralPurposeBitFlag: word;
    CompressionMethod: word;
    LastModFileTimeDate: LongWord;
    Crc32: LongWord;
    CompressedSize: LongWord;
    UncompressedSize: LongWord;
    FilenameLength: word;
    ExtraFieldLength: word;
  end;

  TLocalFile = packed record
    LocalFileHeaderSignature: LongWord;
    CommonFileHeader: TCommonFileHeader;
    filename: AnsiString;
    extrafield: AnsiString;
    CompressedData: AnsiString;
  end;

  TFileHeader = packed record
    CentralFileHeaderSignature: LongWord; // $02014b50
    VersionMadeBy: word;
    CommonFileHeader: TCommonFileHeader;
    FileCommentLength: word;
    DiskNumberStart: word;
    InternalFileAttributes: word;
    ExternalFileAttributes: LongWord;
    RelativeOffsetOfLocalHeader: LongWord;
    filename: AnsiString;
    extrafield: AnsiString;
    fileComment: AnsiString;
  end;

  TEndOfCentralDir = packed record
    EndOfCentralDirSignature: LongWord; // $06054b50
    NumberOfThisDisk: word;
    NumberOfTheDiskWithTheStart: word;
    TotalNumberOfEntriesOnThisDisk: word;
    TotalNumberOfEntries: word;
    SizeOfTheCentralDirectory: LongWord;
    OffsetOfStartOfCentralDirectory: LongWord;
    ZipfileCommentLength: word;
  end;

  TWZipFile = class(TObject)
    Files: array of TLocalFile;
    CentralDirectory: array of TFileHeader;
    EndOfCentralDirectory: TEndOfCentralDir;
    ZipFileComment: string;
  private
    function GetUncompressed(i: integer): string;
    procedure SetUncompressed(i: integer; const Value: string);
    function GetDateTime(i: integer): TDateTime;
    procedure SetDateTime(i: integer; const Value: TDateTime);
    function GetCount: integer;
    function GetName(i: integer): string;
    procedure SetName(i: integer; const Value: string);
  public
    property Count: integer read GetCount;
    function AddFile(const name: string; FAttribute: LongWord = 0): integer;
    procedure SaveToFile(const filename: string);
    procedure SaveToStream(ZipFileStream: TStream);
    procedure LoadFromFile(const filename: string);
    procedure LoadFromStream(const ZipFileStream: TStream);
    property Uncompressed[i: integer]: string read GetUncompressed write SetUncompressed;
    property Data[i: integer]: string read GetUncompressed write SetUncompressed;
    property DateTime[i: integer]: TDateTime read GetDateTime write SetDateTime;
    property Name[i: integer]: string read GetName write SetName;
  end;

  EZipFileCRCError = class(Exception);

function ZipCRC32(const Data: string): LongWord;

procedure AddFileToZip(const pk3: TWZipFile; const fname: string; const fnamedesc: string = '');

procedure AddStreamToZip(const pk3: TWZipFile; const fname: string; const strm: TStream);

procedure AddStringToZip(const pk3: TWZipFile; const fname: string; const data: string);

procedure AddDataToZip(const pk3: TWZipFile; const fname: string; const data: pointer; const datasize: integer);

type
  TZipFile = class(TBaseArcFile)
  private
    fFileName: string;
    fFiles: TStringList;
    f: TFileStream;
  protected
    function GetFile(Index: Integer): string; override;
    procedure Load; override;
    procedure Clear; override;
    procedure SetFileName(const Value: string); override;
    function GetFileCount: integer; override;
    function GetFileName: string; override;
  public
    constructor Create(const aFileName: string); override;
    destructor Destroy; override;
    function GetZipFileData(const Index: integer; var p: pointer;
      var size: integer): boolean; overload; virtual;
    function GetZipFileData(const Name: string; var p: pointer;
      var size: integer): boolean; overload; virtual;
    function ExtractFiles(const alist: TStringList; const aalias: TStringList; const apath: string = ''): boolean; override;
    property FileName: string read GetFileName write SetFileName;
    property Files[Index: Integer]: string read GetFile;
    property FileCount: integer read GetFileCount;
  end;

implementation

uses
  options, zlibpasEx;

{ TWZipFile }

procedure TWZipFile.SaveToFile(const filename: string);
var
  ZipFileStream: TFileStream;
begin
  ZipFileStream := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(ZipFileStream);
  finally
    ZipFileStream.Free;
  end;
end;

procedure TWZipFile.SaveToStream(ZipFileStream: TStream);
var
  i: integer;
begin
    for i := 0 to High(Files) do
      with Files[i] do
      begin
        CentralDirectory[i].RelativeOffsetOfLocalHeader :=
          ZipFileStream.Position;
        ZipFileStream.Write(LocalFileHeaderSignature, 4);
        if LocalFileHeaderSignature = $04034B50 then
        begin
          ZipFileStream.Write(CommonFileHeader, SizeOf(CommonFileHeader));
          ZipFileStream.Write(PChar(filename)^,
            CommonFileHeader.FilenameLength);
          ZipFileStream.Write(PChar(extrafield)^,
            CommonFileHeader.ExtraFieldLength);
          ZipFileStream.Write(PChar(CompressedData)^,
            CommonFileHeader.CompressedSize);
        end;
      end;

    EndOfCentralDirectory.OffsetOfStartOfCentralDirectory :=
      ZipFileStream.Position;

    for i := 0 to High(CentralDirectory) do
      with CentralDirectory[i] do
      begin
        ZipFileStream.Write(CentralFileHeaderSignature, 4);
        ZipFileStream.Write(VersionMadeBy, 2);
        ZipFileStream.Write(CommonFileHeader, SizeOf(CommonFileHeader));
        ZipFileStream.Write(FileCommentLength, 2);
        ZipFileStream.Write(DiskNumberStart, 2);
        ZipFileStream.Write(InternalFileAttributes, 2);
        ZipFileStream.Write(ExternalFileAttributes, 4);
        ZipFileStream.Write(RelativeOffsetOfLocalHeader, 4);
        ZipFileStream.Write(PChar(filename)^, Length(filename));
        ZipFileStream.Write(PChar(extrafield)^, Length(extrafield));
        ZipFileStream.Write(PChar(fileComment)^, Length(fileComment));
      end;

    with EndOfCentralDirectory do
    begin
      EndOfCentralDirSignature := $06054B50;
      NumberOfThisDisk := 0;
      NumberOfTheDiskWithTheStart := 0;
      TotalNumberOfEntriesOnThisDisk := High(Files) + 1;
      TotalNumberOfEntries := High(Files) + 1;
      SizeOfTheCentralDirectory :=
        ZipFileStream.Position - OffsetOfStartOfCentralDirectory;
      ZipfileCommentLength := Length(ZipFileComment);
    end;

  ZipFileStream.Write(EndOfCentralDirectory, SizeOf(EndOfCentralDirectory));
  ZipFileStream.Write(PChar(ZipFileComment)^, Length(ZipFileComment));
end;


procedure TWZipFile.LoadFromStream(const ZipFileStream: TStream);
var
  n: integer;
  signature: LongWord;
begin
  n := 0;
  repeat
    signature := 0;
    ZipFileStream.Read(signature, 4);
    if ZipFileStream.Position =  ZipFileStream.Size then
      Exit;
  until signature = $04034B50;

  repeat
    begin
      if signature = $04034B50 then
      begin
        inc(n);
        SetLength(Files, n);
        SetLength(CentralDirectory, n);
        with Files[n - 1] do
        begin
          LocalFileHeaderSignature := signature;
          ZipFileStream.Read(CommonFileHeader, SizeOf(CommonFileHeader));
          SetLength(filename, CommonFileHeader.FilenameLength);
          ZipFileStream.Read(PChar(filename)^,
            CommonFileHeader.FilenameLength);
          SetLength(extrafield, CommonFileHeader.ExtraFieldLength);
          ZipFileStream.Read(PChar(extrafield)^,
            CommonFileHeader.ExtraFieldLength);
          SetLength(CompressedData, CommonFileHeader.CompressedSize);
          ZipFileStream.Read(PChar(CompressedData)^,
            CommonFileHeader.CompressedSize);
        end;
      end;
    end;
    signature := 0;
    ZipFileStream.Read(signature, 4);
  until signature <> $04034B50;

  n := 0;
  repeat
    begin
      if signature = $02014B50 then
      begin
        inc(n);
        with CentralDirectory[n - 1] do
        begin
          CentralFileHeaderSignature := signature;
          ZipFileStream.Read(VersionMadeBy, 2);
          ZipFileStream.Read(CommonFileHeader, SizeOf(CommonFileHeader));
          ZipFileStream.Read(FileCommentLength, 2);
          ZipFileStream.Read(DiskNumberStart, 2);
          ZipFileStream.Read(InternalFileAttributes, 2);
          ZipFileStream.Read(ExternalFileAttributes, 4);
          ZipFileStream.Read(RelativeOffsetOfLocalHeader, 4);
          SetLength(filename, CommonFileHeader.FilenameLength);
          ZipFileStream.Read(PChar(filename)^,
            CommonFileHeader.FilenameLength);
          SetLength(extrafield, CommonFileHeader.ExtraFieldLength);
          ZipFileStream.Read(PChar(extrafield)^,
            CommonFileHeader.ExtraFieldLength);
          SetLength(fileComment, FileCommentLength);
          ZipFileStream.Read(PChar(fileComment)^, FileCommentLength);
        end;
      end;
    end;
    signature := 0;
    ZipFileStream.Read(signature, 4);
  until signature <> $02014B50;

  if signature = $06054B50 then
  begin
    EndOfCentralDirectory.EndOfCentralDirSignature := Signature;
    ZipFileStream.Read(EndOfCentralDirectory.NumberOfThisDisk,
      SizeOf(EndOfCentralDirectory) - 4);
    SetLength(ZipFileComment, EndOfCentralDirectory.ZipfileCommentLength);
    ZipFileStream.Read(PChar(ZipFileComment)^,
      EndOfCentralDirectory.ZipfileCommentLength);
  end;
end;

procedure TWZipFile.LoadFromFile(const filename: string);
var
  ZipFileStream: TFileStream;
begin
  ZipFileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(ZipFileStream);
  finally
    ZipFileStream.Free;
  end;
end;

function TWZipFile.GetUncompressed(i: integer): string;
var
  Decompressor: TDecompressionStream;
  UncompressedStream: TStringStream;
  Aheader: string;
  ReadBytes: integer;
  LoadedCrc32: LongWord;
begin
  if (i < 0) or (i > High(Files)) then
    raise Exception.Create(
      'TWZipFile.GetUncompressed(): Index out of range.');

  Aheader := Chr(120) + Chr(156);
  //manufacture a 2 byte header for zlib; 4 byte footer is not required.
  UncompressedStream := TStringStream.Create(Aheader + Files[i].CompressedData);
  try {+}
    Decompressor := TDecompressionStream.Create(UncompressedStream);
    try {+}
      SetLength(Result, Files[i].CommonFileHeader.UncompressedSize);
      ReadBytes := Decompressor.Read(PChar(Result)^,
        Files[i].CommonFileHeader.UncompressedSize);
      if ReadBytes <> integer(Files[i].CommonFileHeader.UncompressedSize) then
        Result := '';
    finally
      Decompressor.Free;
    end;
  finally
    UncompressedStream.Free;
  end;

  LoadedCRC32 := ZipCRC32(Result);
  if LoadedCRC32 <> Files[i].CommonFileHeader.Crc32 then
    raise EZipFileCRCError.CreateFmt(
      'TWZipFile.GetUncompressed(): CRC Error in "%s".', [Files[i].filename]);
end;

procedure TWZipFile.SetUncompressed(i: integer; const Value: string);
var
  Compressor: TCompressionStream;
  CompressedStream: TStringStream;
begin
  if i > High(Files) then
    raise Exception.Create(
      'TWZipFile.SetUncompressed(): Index out of range.');

  CompressedStream := TStringStream.Create('');
  try {+}
    Compressor := TcompressionStream.Create(TCompressionLevel(optzipcompression), CompressedStream);
    try {+}
      Compressor.Write(PChar(Value)^, Length(Value));
    finally
      Compressor.Free;
    end;
    Files[i].CompressedData := Copy(CompressedStream.DataString, 3,
      Length(CompressedStream.DataString) - 6);
    //strip the 2 byte headers and 4 byte footers
    Files[i].LocalFileHeaderSignature := $04034B50;
    with Files[i].CommonFileHeader do
    begin
      VersionNeededToExtract := 20;
      GeneralPurposeBitFlag := 0;
      CompressionMethod := 8;
      LastModFileTimeDate := DateTimeToFileDate(Now);
      Crc32 := ZipCRC32(Value);
      CompressedSize := Length(Files[i].CompressedData);
      UncompressedSize := Length(Value);
      FilenameLength := Length(Files[i].filename);
      ExtraFieldLength := Length(Files[i].extrafield);
    end;

    with CentralDirectory[i] do
    begin
      CentralFileHeaderSignature := $02014B50;
      VersionMadeBy := 20;
      CommonFileHeader := Files[i].CommonFileHeader;
      FileCommentLength := 0;
      DiskNumberStart := 0;
      InternalFileAttributes := 0;
      //      ExternalFileAttributes := 0;
      RelativeOffsetOfLocalHeader := 0;
      filename := Files[i].filename;
      extrafield := Files[i].extrafield;
      fileComment := '';
    end;
  finally
    CompressedStream.Free;
  end;
end;

function TWZipFile.AddFile(const name: string; FAttribute: LongWord = 0): integer;
begin
  SetLength(Files, High(Files) + 2);
  SetLength(CentralDirectory, Length(Files));
  Result := High(Files);
  Files[High(Files)].filename := name;
  Files[High(Files)].CompressedData := ''; //start with an empty file
  Files[High(Files)].extrafield := '';
  Files[High(Files)].LocalFileHeaderSignature := $04034B50;
  with Files[High(Files)].CommonFileHeader do
  begin
    VersionNeededToExtract := 20;
    GeneralPurposeBitFlag := 0;
    CompressionMethod := 8;
    LastModFileTimeDate := DateTimeToFileDate(Now);
    Crc32 := 0;
    CompressedSize := 0;
    UncompressedSize := 0;
    FilenameLength := Length(Files[High(Files)].filename);
    ExtraFieldLength := Length(Files[High(Files)].extrafield);
  end;

  with CentralDirectory[High(Files)] do
  begin
    CentralFileHeaderSignature := $02014B50;
    VersionMadeBy := 20;
    CommonFileHeader := Files[High(Files)].CommonFileHeader;
    FileCommentLength := 0;
    DiskNumberStart := 0;
    InternalFileAttributes := 0;
    ExternalFileAttributes := FAttribute;
    RelativeOffsetOfLocalHeader := 0;
    filename := Files[High(Files)].filename;
    extrafield := Files[High(Files)].extrafield;
    fileComment := '';
  end;
end;

function TWZipFile.GetDateTime(i: integer): TDateTime;
begin
  if i > High(Files) then
    raise Exception.Create(
      'TWZipFile.GetDateTime(): Index out of range.');
  Result := FileDateToDateTime(Files[i].CommonFileHeader.LastModFileTimeDate);
end;

procedure TWZipFile.SetDateTime(i: integer; const Value: TDateTime);
begin
  if i > High(Files) then
    raise Exception.Create(
      'TWZipFile.SetDateTime(): Index out of range.');
  Files[i].CommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(Value);
end;

function TWZipFile.GetCount: integer;
begin
  Result := High(Files) + 1;
end;

function TWZipFile.GetName(i: integer): string;
begin
  Result := Files[i].filename;
end;

procedure TWZipFile.SetName(i: integer; const Value: string);
begin
  Files[i].filename := Value;
end;

{ ZipCRC32 }

//calculates the zipfile CRC32 value from a string

const
  CRCtable: array[0..255] of LongWord = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419,
    $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07,
    $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856,
    $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9,
    $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3,
    $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A,
    $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599,
    $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190,
    $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E,
    $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED,
    $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950,
    $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3,
    $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5,
    $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010,
    $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17,
    $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6,
    $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615,
    $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344,
    $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A,
    $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1,
    $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C,
    $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE,
    $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31,
    $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C,
    $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B,
    $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1,
    $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278,
    $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7,
    $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66,
    $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8,
    $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B,
    $2D02EF8D
  );

function ZipCRC32(const Data: string): LongWord;
var
  i: integer;
begin
  Result := $FFFFFFFF;
  for i := 0 to Length(Data) - 1 do
    Result := (Result shr 8) xor (CRCtable[byte(Result) xor Ord(Data[i + 1])]);
  Result := Result xor $FFFFFFFF;
end;

procedure AddFileToZip(const pk3: TWZipFile; const fname: string; const fnamedesc: string = '');
var
  idx: integer;
  sdata: string;
  f: TFileStream;
  i: integer;
begin
  if not FileExists(fname) then
    exit;

  if fnamedesc = '' then
    idx := pk3.AddFile(fname)
  else
    idx := pk3.AddFile(fnamedesc);
  f := TFileStream.Create(fname, fmOpenRead);
  try
    SetLength(sdata, f.Size);
    for i := 1 to Length(sdata) do
      f.Read(sdata[i], 1);
    pk3.Data[idx] := sdata;
    pk3.DateTime[idx] := Now();
    SetLength(sdata, 0);
  finally
    f.Free;
  end;
end;

procedure AddStreamToZip(const pk3: TWZipFile; const fname: string; const strm: TStream);
var
  idx: integer;
  sdata: string;
  i: integer;
  oldp: integer;
begin
  idx := pk3.AddFile(fname);
  oldp := strm.Position;
  strm.Position := 0;
  try
    SetLength(sdata, strm.Size);
    for i := 1 to Length(sdata) do
      strm.Read(sdata[i], 1);
    pk3.Data[idx] := sdata;
    pk3.DateTime[idx] := Now();
    SetLength(sdata, 0);
  finally
    strm.Position := oldp;
  end;
end;

procedure AddStringToZip(const pk3: TWZipFile; const fname: string; const data: string);
var
  idx: integer;
begin
  idx := pk3.AddFile(fname);
  pk3.Data[idx] := data;
  pk3.DateTime[idx] := Now();
end;

procedure AddDataToZip(const pk3: TWZipFile; const fname: string; const data: pointer; const datasize: integer);
var
  idx: integer;
  sdata: string;
  i: integer;
begin
  idx := pk3.AddFile(fname);
  sdata := '';
  SetLength(sdata, datasize);
  for i := 1 to datasize do
    sdata[i] := Chr(PByteArray(data)[i - 1]);
  pk3.Data[idx] := sdata;
  pk3.DateTime[idx] := Now();
  SetLength(sdata, 0);
end;

//------------------------------------------------------------------------------

{ TZipFile }

function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  Result := inflateInit2_(stream, windowBits, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

procedure ZDecompress2(const inBuffer: Pointer; const inSize: Integer;
  const outSize: Integer; out outBuffer: Pointer);
var
  zstream: TZStreamRec;

  procedure CheckErr(err: integer);
  begin
    if err < 0 then
      Raise Exception.Create(Format('ZDecompress2(): Zip file error(%d)', [err]));
  end;

begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  GetMem(outBuffer, outSize);

  CheckErr(InflateInit2(zstream, -15));

  zstream.next_in := inBuffer;
  zstream.avail_in := inSize;
  zstream.next_out := outBuffer;
  zstream.avail_out := outSize;

  CheckErr(inflate(zstream, Z_SYNC_FLUSH));

  inflateEnd(zstream);
end;

//------------------------------------------------------------------------------
type
  TZipFileEntryInfo = class
  private
    fSize: integer;
    fCompressedSize: integer;
    fPosition: integer;
    fCompressed: boolean;
  public
    constructor Create(const aSize, aCompressedSize, aPosition: integer;
      aCompressed: boolean); virtual;
    property Size: integer read fSize;
    property CompressedSize: integer read fCompressedSize;
    property Position: integer read fPosition;
    property Compressed: boolean read fCompressed;
  end;

constructor TZipFileEntryInfo.Create(const aSize, aCompressedSize, aPosition: integer;
      aCompressed: boolean);
begin
  fSize := aSize;
  fCompressedSize := aCompressedSize;
  fPosition := aPosition;
  fCompressed := aCompressed;
end;

//------------------------------------------------------------------------------
constructor TZipFile.Create(const aFileName: string);
begin
  Inherited Create(aFileName);
  fFiles := TStringList.Create;
  fFileName := aFileName;
  Load;
end;

destructor TZipFile.Destroy;
begin
  Clear;
  fFiles.Free;
  Inherited Destroy;
end;

function TZipFile.GetZipFileData(const Index: integer; var p: pointer;
  var size: integer): boolean;
var
  tmp: pointer;
  zinf: TZipFileEntryInfo;
  csize: integer;
begin
  if (Index >= 0) and (Index < fFiles.Count) then
  begin
    zinf := (fFiles.Objects[Index] as TZipFileEntryInfo);
    if zinf.Compressed then
    begin
      size := zinf.Size;
      csize := zinf.CompressedSize;
      GetMem(tmp, csize);
      try
        f.Seek(zinf.Position, soFromBeginning);
        f.Read(tmp^, csize);
        ZDecompress2(tmp, csize, size, p);
      finally
        FreeMem(tmp, csize);
      end;
      Result := True;
    end
    else
    begin
      size := zinf.Size;
      GetMem(p, size);
      f.Seek(zinf.Position, soFromBeginning);
      f.Read(p^, size);
      Result := True;
    end;
  end
  else
    Result := False;
end;

function TZipFile.GetZipFileData(const Name: string; var p: pointer;
  var size: integer): boolean;
var
  Name2: string;
  i: integer;
begin
  Name2 := UpperCase(Name);
  for i := 1 to Length(Name) do
    if Name2[i] = '/' then
      Name2[i] := '\';
  Result := GetZipFileData(fFiles.IndexOf(Name2), p, size);
end;

function TZipFile.GetFile(Index: Integer): string;
begin
  Result := fFiles[Index];
end;

const
  ZIPFILESIGNATURE = $04034b50;
  ZIPARCIEVESIGNATURE = $08064b50;

type
  TZipFileHeader = packed record
    Signature: integer; // $04034b50
    Version: word;
    BitFlag: word;
    CompressionMethod: word;
    DosDate: integer;
    crc32: integer;
    CompressedSize: integer;
    UnCompressedSize: integer;
    FileNameLen: word;
    ExtraFieldLen: word;
  end;

// This descriptor exists only if bit 3 of the general
// purpose bit flag is set (see below).
  TZipFileDescriptor = record
    crc32: integer;
    CompressedSize: integer;
    UnCompressedSize: integer;
  end;

procedure TZipFile.Load;
var
  h: TZipFileHeader;
  str: string;
  i: integer;
begin
  Clear;
  if fFileName <> '' then
  begin
    f := TFileStream.Create(fFileName, fmOpenRead or fmShareDenyWrite);
    while True do
    begin
      f.Read(h, SizeOf(h));
      if h.Signature = ZIPFILESIGNATURE then
      begin
        SetLength(str, h.FileNameLen);
        if h.FileNameLen > 0 then
        begin
          f.Read((@str[1])^, h.FileNameLen);
          str := UpperCase(str);
          for i := 1 to h.FileNameLen do
            if str[i] = '/' then
              str[i] := '\';
          fFiles.Objects[fFiles.Add(str)] :=
            TZipFileEntryInfo.Create(h.UnCompressedSize, h.CompressedSize,
              f.Position + h.ExtraFieldLen, h.CompressionMethod > 0);
          if (h.BitFlag and $4) <> 0 then
            f.Seek(h.ExtraFieldLen + h.CompressedSize + SizeOf(TZipFileDescriptor), soFromCurrent)
          else
            f.Seek(h.ExtraFieldLen + h.CompressedSize, soFromCurrent);
        end;
      end
      else
        Break;
    end;
  end;
end;

procedure TZipFile.Clear;
var
  i: integer;
begin
  for i := 0 to fFiles.Count - 1 do
    fFiles.Objects[i].Free;
  fFiles.Clear;
  f.Free;
end;

procedure TZipFile.SetFileName(const Value: string);
begin
  if fFileName <> Value then
  begin
    fFileName := Value;
    Load;
  end;
end;

function TZipFile.GetFileCount: integer;
begin
  Result := fFiles.Count;
end;

function TZipFile.GetFileName: string;
begin
  Result := fFileName;
end;

function TZipFile.ExtractFiles(const alist: TStringList; const aalias: TStringList; const apath: string = ''): boolean;
var
  i, size, cnt: integer;
  p: pointer;
  fs: TFileStream;
begin
  cnt := 0;
  for i := 0 to alist.Count - 1 do
  begin
    p := nil;
    size := 0;
    if GetZipFileData(alist.Strings[i], p, size) then
      if (p <> nil) and (size > 0) then
      begin
        fs := TFileStream.Create(apath + aalias.Strings[i], fmCreate);
        try
          fs.Write(p^, size);
        finally
          fs.Free;
        end;
        FreeMem(p, size);
        Inc(cnt);
      end;
  end;
  Result := cnt > 0;
end;

end.

