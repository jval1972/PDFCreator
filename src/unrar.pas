unit unrar;

interface

uses
  Windows, Classes, SysUtils;

const
  customDLL            = False;

  RAR_METHOD_STORE      = 48;
  RAR_METHOD_FASTEST    = 49;
  RAR_METHOD_FAST       = 50;
  RAR_METHOD_NORMAL     = 51;
  RAR_METHOD_GOOD       = 52;
  RAR_METHOD_BEST       = 53;

  RAR_SUCCESS           =  0;
  ERAR_END_ARCHIVE      = 10;
  ERAR_NO_MEMORY        = 11;
  ERAR_BAD_DATA         = 12;
  ERAR_BAD_ARCHIVE      = 13;
  ERAR_UNKNOWN_FORMAT   = 14;
  ERAR_EOPEN            = 15;
  ERAR_ECREATE          = 16;
  ERAR_ECLOSE           = 17;
  ERAR_EREAD            = 18;
  ERAR_EWRITE           = 19;
  ERAR_SMALL_BUF        = 20;
  ERAR_UNKNOWN          = 21;
  ERAR_MISSING_PASSWORD = 22;
  ERAR_EREFERENCE       = 23;
  ERAR_BAD_PASSWORD     = 24;
  ERAR_LARGE_DICT       = 25;

  RAR_HASH_NONE         =  0;
  RAR_HASH_CRC32        =  1;
  RAR_HASH_BLAKE2       =  2;

  RAR_OM_LIST           =  0;
  RAR_OM_EXTRACT        =  1;
  RAR_OM_LIST_INCSPLIT  =  2;

  RAR_SKIP              =  0;
  RAR_TEST              =  1;
  RAR_EXTRACT           =  2;

  RAR_VOL_ASK           =  0;
  RAR_VOL_NOTIFY        =  1;

  RAR_DLL_VERSION       =  3;

  ROADF_VOLUME          = $0001;
  ROADF_COMMENT         = $0002;
  ROADF_LOCK            = $0004;
  ROADF_SOLID           = $0008;
  ROADF_NEWNUMBERING    = $0010;
  ROADF_SIGNED          = $0020;
  ROADF_RECOVERY        = $0040;
  ROADF_ENCHEADERS      = $0080;
  ROADF_FIRSTVOLUME     = $0100;

  ROADOF_KEEPBROKEN     = $0001;

  RHDF_SPLITBEFORE      = $01;
  RHDF_SPLITAFTER       = $02;
  RHDF_ENCRYPTED        = $04;
  RHDF_SOLID            = $10;
  RHDF_DIRECTORY        = $20;

  UCM_CHANGEVOLUME      =  0;
  UCM_PROCESSDATA       =  1;
  UCM_NEEDPASSWORD      =  2;
  UCM_CHANGEVOLUMEW     =  3;
  UCM_NEEDPASSWORDW     =  4;
  UCM_LARGEDICT         =  5;

  RAR_MAX_COMMENT_SIZE  = 65536;
  RAR_MIN_VERSION       =  4;

  RAR_CANCEL            = -1;
  RAR_CONTINUE          =  0;

//
  RAR_COMMENT_EXISTS    =  1;
  RAR_NO_COMMENT        =  0;
  RAR_COMMENT_UNKNOWN   = 98;
  RAR_DLL_LOAD_ERROR    = 99;
  RAR_INVALID_HANDLE    =  0;

type
  TProcessDataProc  = function(Addr: PByte; Size: integer): integer;
  TChangeVolProc    = function(ArcName: PAnsiChar; Mode: integer): integer; {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  TUnRarCallBack    = function(msg: Cardinal; UserData: LPARAM; P1: LPARAM; P2: LPARAM): integer; {stdcall;} {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  TRARHeaderData = packed record
    arcName: array[0..259] of AnsiChar;
    fileName: array[0..259] of AnsiChar;
    flags: cardinal;
    packSize: cardinal;
    unpSize: cardinal;
    hostOS: cardinal;
    fileCRC: cardinal;
    fileTime: cardinal;
    unpVer: cardinal;
    method: cardinal;
    fileAttr: cardinal;
    cmtBuf: PAnsiChar;
    cmtBufSize: cardinal;
    cmtSize: cardinal;
    cmtState: cardinal;
  end;
  PRARHeaderData = ^TRARHeaderData;

  //for UniCode FileNames and 64-Bit Sizes
//  {$ALIGN 1}
  TRARHeaderDataEx = packed record
    arcName: array[0..1023] of AnsiChar;
    arcNameW: array[0..1023] of WideChar;
    fileName: array[0..1023] of AnsiChar;
    fileNameW: array[0..1023] of WideChar;
    flags: cardinal;
    packSize: cardinal;
    packSizeHigh: cardinal;
    unpSize: cardinal;
    unpSizeHigh: cardinal;
    hostOS: cardinal;
    fileCRC: cardinal;
    fileTime: cardinal;
    unpVer: cardinal;
    method: cardinal;
    fileAttr: cardinal;
    cmtBuf: PAnsiChar;
    cmtBufSize: cardinal;
    cmtSize: cardinal;
    cmtState: cardinal;
    dictSize: cardinal;
    hashType: cardinal;
    hash: array[0..31] of byte;
    redirType: cardinal;
    redirName: PWideChar;
    redirNameSize: cardinal;
    dirTarget: cardinal;
    MTimeLow: cardinal;
    MTimeHigh: cardinal;
    CTimeLow: cardinal;
    CTimeHigh: cardinal;
    ATimeLow: cardinal;
    ATimeHigh: cardinal;
    arcNameEx: PWideChar;
    arcNameExSize: cardinal;
    fileNameEx: PWideChar;
    fileNameExSize: cardinal;
    reserved: array[0..981] of cardinal;
//    reserved2: array[0..275] of cardinal;
  end;
//  {$A-} // Reset alignment to default
  PRARHeaderDataEx = ^TRARHeaderDataEx;

  TRAROpenArchiveData = packed record
    arcName: PAnsiChar;
    openMode: cardinal;
    openResult: cardinal;
    cmtBuf: PAnsiChar;
    cmtBufSize: cardinal;
    cmtSize: cardinal;
    cmtState: cardinal;
  end;
  PRAROpenArchiveData = ^TRAROpenArchiveData;

  TRAROpenArchiveDataEx = packed record
    arcName: PAnsiChar;
    arcNameW: PWideChar;
    openMode: cardinal;
    openResult: cardinal;
    cmtBuf: PAnsiChar;
    cmtBufSize: cardinal;
    cmtSize: cardinal;
    cmtState: cardinal;
//
    flags: cardinal;
    callback: {$IFDEF Win32} cardinal {$ELSE} NativeUInt {$ENDIF};
    userData: LPARAM;
    opFlags: cardinal;
    cmtBufW: PWideChar;
    markOfTheWeb: PWideChar;
    reserved: array[1..23] of cardinal;
  end;
  PRAROpenArchiveDataEx = ^TRAROpenArchiveDataEx;

var
  RAROpenArchive: function (ArchiveData: PRAROpenArchiveData): THandle;{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RAROpenArchiveEx: function (ArchiveData: PRAROpenArchiveDataEx): THandle;{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  RARCloseArchive: function (hArcData: THandle): integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARReadHeader: function (hArcData: THandle; HeaderData: PRARHeaderData): integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARReadHeaderEx: function (hArcData: THandle; HeaderData: PRARHeaderDataEx): integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  RARProcessFile: function (hArcData: THandle; Operation: integer; DestPath: PAnsiChar; DestName: PAnsiChar): integer;{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARProcessFileW: function (hArcData: THandle; Operation: integer; DestPath: PWideChar; DestName: PWideChar): integer;{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  RARSetCallback: procedure (hArcData: THandle; Callback: TUnRarCallback; UserData: LPARAM);{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARSetChangeVolProc: procedure (hArcData: THandle; ChangeVolProc: TChangeVolProc);{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARSetProcessDataProc: procedure (hArcData: THandle; ProcessDataProc: TProcessDataProc);{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARSetPassword: procedure (hArcData: THandle; Password: PAnsiChar);{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARGetDllVersion: function: integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

{$IFDEF customDLL}
  RARSetPasswordW: procedure (hArcData: THandle; Password: PWideChar);{$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
{$ENDIF}


function getFileModifiedDate(const aFilePath: string): TDateTime;
function getFileSize(const s: string): int64;
function isSFX(const fileName:String): boolean;

procedure InitRAR;
procedure ShutDownRAR;

type
  TRAROperation = (roOpenArchive, roCloseArchive, roListFiles, roExtract, roTest);
  TRARHeaderType = (htFile, htDirectory, htSplitFile);
  TRAROpenMode = (omRAR_OM_LIST, omRAR_OM_EXTRACT, omRAR_OM_LIST_INCSPLIT);
  TRARReplace = (rrCancel, rrOverwrite, rrSkip);

  TRARProgressInfo = record
    fileName: WideString; // the full path to the file within the RAR archive
    archiveBytesTotal: LongInt;
    archiveBytesDone: LongInt;
    fileBytesTotal: LongInt;
    fileBytesDone: LongInt;
  end;

  TRARFileItem = record
    // derived from processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    fileName: AnsiString;
    fileNameW: WideString;
    splitFile: boolean;
    compressedSize: cardinal;
    unCompressedSize: cardinal;
    hostOS: string;
    CRC32: string;
    attributes: cardinal;
    comment: AnsiString;
    time: TDateTime;
    compressionStrength: cardinal;
    archiverVersion: cardinal;
    encrypted: boolean;
    hash: string;
  end;

  TRARReplaceData = record
    fileName: Ansistring;
    size: int64;
    time: TDateTime;
  end;

  TRAROnErrorNotifyEvent = procedure(Sender: TObject; const aErrorCode: integer; const aOperation: TRAROperation) of object;
  TRAROnListFile = procedure(Sender: TObject; const aFileItem: TRARFileItem) of object;
  TRAROnPasswordRequired = procedure(Sender: TObject; const aFileName: Ansistring; out oNewPassword: Ansistring; out oCancel: boolean) of object;
  TRAROnNextVolumeRequired = procedure(Sender: TObject; const aRequiredFileName: Ansistring; out oNewFileName: Ansistring; out oCancel: boolean) of object;
  TRAROnProgress = procedure(Sender: TObject; const aProgressInfo: TRARProgressInfo) of object;
  TRAROnReplace = procedure(Sender: TObject; const aExistingData: TRARReplaceData; aNewData: TRARReplaceData; out oAction: TRARReplace) of object;

  TRARArchiveInfo = record
    fileName: Ansistring;
    fileNameW: WideString;

    // derived from RAROpenArchiveEx: TRAROpenArchiveDataEx in openArchive()
    volume: boolean;
    archiveComment: boolean;
    locked: boolean;
    solid: boolean; // also set in processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    newNumbering: boolean;
    signed: boolean;
    recovery: boolean;
    headerEncrypted: boolean;
    firstVolume: boolean;
    SFX: boolean;

    // derived from processFileHeader(aHeaderDataEx: TRARHeaderDataEx);
    packedSizeMVVolume: cardinal; // from processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    archiverMajorVersion: cardinal;
    archiverMinorVersion: cardinal;
    hostOS: string;
    totalFiles: integer; // incremented when processing each file header
    dictionarySize: int64;
    compressedSize: int64;   // totalled from aHeaderDataEx.PackSize when processing each file header
    unCompressedSize: int64;   // totalled from aHeaderDataEx.UnpSize  when processing each file header
    multiVolume: boolean;
    fileComment: boolean;
  end;

  TRARArchive = class(TObject)
    handle: THandle;
    opened: boolean;
    password: AnsiString;
    info: TRARArchiveInfo;
    fileItem: TRARFileItem;
    progressInfo: TRARProgressInfo;
    hasComment: boolean;
    comment: AnsiString;
    commentBuf: PAnsiChar;
    commentState: cardinal;
 readMVToEnd: boolean;
    abort: boolean;
  public
    constructor create;
    destructor  destroy; override;
  end;

  ICallBackInfo = interface
  ['{9D062B91-12D3-47E2-821A-215938DAD3CE}']
    function getOnNextVolRequired: TRAROnNextVolumeRequired;
    function getOnPasswordRequired: TRAROnPasswordRequired;
    function getOnProgress: TRAROnProgress;
    function getRAR: TRARArchive;
    procedure setOnNextVolRequired(const Value: TRAROnNextVolumeRequired);
    procedure setOnPasswordRequired(const Value: TRAROnPasswordRequired);
    procedure setOnProgress(const Value: TRAROnProgress);
    procedure setRAR(const Value: TRARArchive);
    property RAR: TRARArchive read getRAR write setRAR;
    property onNextVolRequired: TRAROnNextVolumeRequired read getOnNextVolRequired write setOnNextVolRequired;
    property onPasswordRequired: TRAROnPasswordRequired read getOnPasswordRequired write setOnPasswordRequired;
    property onProgress: TRAROnProgress read getOnProgress write setOnProgress;
  end;

  TCallBackInfo = class(TInterfacedObject, ICallBackInfo)
  private
    FRAR: TRARArchive;
    FOnNextVolRequired: TRAROnNextVolumeRequired;
    FOnPasswordRequired: TRAROnPasswordRequired;
    FOnProgress: TRAROnProgress;
    function getOnPasswordRequired: TRAROnPasswordRequired;
    function getOnProgress: TRAROnProgress;
    function getRAR: TRARArchive;
    procedure setOnPasswordRequired(const Value: TRAROnPasswordRequired);
    procedure setOnProgress(const Value: TRAROnProgress);
    procedure setRAR(const Value: TRARArchive);
    function getOnNextVolRequired: TRAROnNextVolumeRequired;
    procedure setOnNextVolRequired(const Value: TRAROnNextVolumeRequired);
  public
    property RAR: TRARArchive read getRAR write setRAR;
    property onNextVolRequired: TRAROnNextVolumeRequired read getOnNextVolRequired write setOnNextVolRequired;
    property onPasswordRequired: TRAROnPasswordRequired read getOnPasswordRequired write setOnPasswordRequired;
    property onProgress: TRAROnProgress read getOnProgress write setOnProgress;
  end;

  IRARResult = interface
  ['{8CBBE61B-C5F8-4FFD-96D0-32C29CC05AAB}']
    function CheckRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
    function GetLastResult: integer;
    function GetOnError: TRAROnErrorNotifyEvent;
    procedure SetOnError(const Value: TRAROnErrorNotifyEvent);
    property lastResult: integer read GetLastResult;
    property onError: TRAROnErrorNotifyEvent read GetOnError write SetOnError;
  end;

  TRARResult = class(TInterfacedObject, IRARResult)
  private
    FOnError: TRAROnErrorNotifyEvent;
    FLastResult: integer;
  public
    function CheckRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
    function GetLastResult: integer;
    function GetOnError: TRAROnErrorNotifyEvent;
    procedure SetOnError(const Value: TRAROnErrorNotifyEvent);

    property lastResult: integer read GetLastResult;
    property onError: TRAROnErrorNotifyEvent read GetOnError write SetOnError;
  end;

type
  TRAR = class(TComponent)
  private
    FRAR: TRARArchive;

    FPassword: AnsiString;
    FReadMVToEnd: boolean;

    FOnListFile: TRAROnListFile;
    FOnPasswordRequired: TRAROnPasswordRequired;
    FOnNextVolumeRequired: TRAROnNextVolumeRequired;
    FOnProgress: TRAROnProgress;
    FOnReplace: TRAROnReplace;

    FFiles: TStringList; // list of filenames (and their full paths) within an archive, to be extracted
    FFoundFiles: TStringList; // files found using findFiles()

    FlastResult: integer;

    function GetOnError: TRAROnErrorNotifyEvent;
    procedure SetOnError(const Value: TRAROnErrorNotifyEvent);

    function getVersion:string;

    procedure OnRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);

    function getArchiveInfo: TRARArchiveInfo;
    function getDLLVersion: integer;
    function getReadMVToEnd: boolean;
    procedure setReadMVToEnd(const Value: boolean);
    function getPassword: AnsiString;
    procedure setPassword(const Value: AnsiString);
    procedure SetLastResult(const Value: integer);
    function GetLastResult: integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure abort;

    procedure AddFile(const aFile: string);
    procedure ClearFiles;
    function FileCount: integer;

    function ExtractArchive(const aArchivePath: string; const aExtractPath: string;
      const aFileName: string = ''): boolean;
    function ExtractArchiveEx(const aArchivePath: string; const aExtractPath: string;
      const aFiles, aFiles2: TStringList; const aFileName: string = ''): boolean;
    function ExtractPreparedArchive(const aArchivePath: string; const aExtractPath: string;
      const aFileName: string = ''): boolean;
    function ListArchive(const aArchivePath: string): boolean;
    function PrepareArchive(const aArchivePath: string): boolean;
    function testArchive(const aArchivePath: string): boolean;

    function findFiles(const aFolderPath: string; bSubFolders: boolean = True;
      const aFileExts: string = '.rar'): integer;
    function isMultiVol(const aArchivePath: string): boolean;
    function isMultiVolPart(const aArchivePath: string): boolean;

    property archiveInfo: TRARArchiveInfo read getArchiveInfo;
    property DLLVersion: integer read getDLLVersion;
    property foundFiles: TStringList read FFoundFiles;
    property lastResult: integer read GetLastResult;
    //pro: report correct archive compressed size and list all files in all parts
    //con: "all volumes required" means that to open, you have to insert all disks if not all volumes are in the same folder
    property readMultiVolumeToEnd: boolean read getReadMVToEnd write setReadMVToEnd; //if True, mv's will be read until last part of the file
  published
    property version: string read getVersion nodefault;

    property onError: TRAROnErrorNotifyEvent read GetOnError write SetOnError;
    property onListFile: TRAROnListFile read FOnListFile write FOnListFile;
    property onPasswordRequired: TRAROnPasswordRequired read FOnPasswordRequired write FOnPasswordRequired;
    property onNextVolumeRequired: TRAROnNextVolumeRequired read FOnNextVolumeRequired write FOnNextVolumeRequired;
    property onProgress: TRAROnProgress read FOnProgress write FOnProgress;
//    property onReplace: TRAROnReplace read FOnReplace write FOnReplace;

    property password: AnsiString read getPassword write setPassword; // can be supplied by the user before calling an operation
  end;

implementation

uses
  BTMemoryModule, unrardll;

type
  TRARDLL = class(TObject)
  private
    FRARDLLInstance: PBTMemoryModule;
    function loadDLL: PBTMemoryModule;
    procedure unloadDLL;
    function isDLLLoaded: boolean;
   public
    constructor Create; virtual;
    destructor  Destroy; override;
  end;

var
  gDLL: TRARDLL;

procedure InitRAR;
begin
  gDLL := TRARDLL.create;
end;

procedure ShutDownRAR;
begin
  gDLL.Free;
end;

function getFileModifiedDate(const aFilePath: string): TDateTime;
var
  vHandle: THandle;
  vStruct: TOFStruct;
  vLastWrite: integer;
begin
  Result  := 0;
  vHandle := openFile(PAnsiChar(aFilePath), vStruct, OF_SHARE_DENY_NONE);
  try
    if vHandle <> HFILE_ERROR then
    begin
      vLastwrite := fileGetDate(vHandle);
      Result := fileDateToDateTime(vLastwrite);
    end;
  finally
    closeHandle(vHandle);
  end;
end;

function getFileSize(const s:string): int64;
var
  vFindData: TWin32FindDataA;
  vHandle: cardinal;
begin
  vHandle := findFirstFileA(PAnsiChar(s), vFindData);
  if (vHandle <> INVALID_HANDLE_VALUE) then
  begin
    Result := vFindData.nFileSizeLow;
    PCardinal(cardinal(@Result) + sizeOf(cardinal))^ := vFindData.nFileSizeHigh;
    windows.findClose(vHandle);
  end
  else
    Result := 0;
end;

function isSFX(const fileName:String):boolean;
var
  vBinaryType: DWORD;
begin
  if getBinaryTypeA(PAnsiChar(fileName), vBinaryType) then
    Result := (vBinaryType = SCS_32BIT_BINARY)
           or (vBinaryType = SCS_DOS_BINARY)
           or (vBinaryType = SCS_WOW_BINARY)
           or (vBinaryType = SCS_PIF_BINARY)
           or (vBinaryType = SCS_POSIX_BINARY)
           or (vBinaryType = SCS_OS216_BINARY)
  else
    Result := False;
end;

{ TDLL }
constructor TRARDLL.create;
begin
  inherited create;
  FRARDLLInstance := loadDLL;
end;

function TRARDLL.loadDLL: PBTMemoryModule;
begin
  Result := BTMemoryLoadLibary(@UnRAR_dll, UnRAR_dll_SIZE);

  @RAROpenArchive   := BTMemoryGetProcAddress(Result, 'RAROpenArchive');
  @RAROpenArchiveEx := BTMemoryGetProcAddress(Result, 'RAROpenArchiveEx');
  @RARCloseArchive  := BTMemoryGetProcAddress(Result, 'RARCloseArchive');
  @RARReadHeader := BTMemoryGetProcAddress(Result, 'RARReadHeader');
  @RARReadHeaderEx  := BTMemoryGetProcAddress(Result, 'RARReadHeaderEx');
  @RARProcessFile   := BTMemoryGetProcAddress(Result, 'RARProcessFile');
  @RARProcessFileW  := BTMemoryGetProcAddress(Result, 'RARProcessFileW');
  @RARSetCallback   := BTMemoryGetProcAddress(Result, 'RARSetCallback');
  @RARSetChangeVolProc := BTMemoryGetProcAddress(Result, 'RARSetChangeVolProc');
  @RARSetProcessDataProc  := BTMemoryGetProcAddress(Result, 'RARSetProcessDataProc');
  @RARSetPassword   := BTMemoryGetProcAddress(Result, 'RARSetPassword');
  @RARGetDllVersion := BTMemoryGetProcAddress(Result, 'RARGetDllVersion');

  {$IFDEF customDLL}
  @RARSetPasswordW  := BTMemoryGetProcAddress(Result, 'RARSetPasswordW');
  {$ENDIF}


  if (@RAROpenArchive = nil) or (@RAROpenArchiveEx    = nil) or (@RARCloseArchive = nil)
  or (@RARReadHeader  = nil) or (@RARReadHeaderEx     = nil) or (@RARProcessFile = nil) or (@RARProcessFileW = nil)
  or (@RARSetCallback = nil) or (@RARSetChangeVolProc = nil) or (@RARSetProcessDataProc = nil)
  or (@RARSetPassword = nil) or (@RARGetDllVersion    = nil) {$IFDEF customDLL} or (@RARSetPasswordW = nil) {$ENDIF}
  then unloadDLL
  else if RARGetDllVersion < RAR_MIN_VERSION then messageBox(0, 'please download the latest "unrar.dll" file. See www.rarlab.com', 'Error', MB_ICONSTOP or MB_OK);
end;

destructor TRARDLL.destroy;
begin
  unloadDLL;
  inherited destroy;
end;

function TRARDLL.isDLLLoaded: boolean;
begin
  Result := FRARDLLInstance <> nil;
end;

procedure TRARDLL.unloadDLL;
begin
  if isDLLLoaded then
  begin
    BTMemoryFreeLibrary(FRARDLLInstance);
    FRARDLLInstance := nil;
  end;
end;

const
  gVersion = '2.3';

var
  RR: IRARResult;

function CheckRARResult(const aResultCode:integer; const aOperation: TRAROperation): integer;
begin
  Result := RR.CheckRARResult(aResultCode, aOperation);
end;

function unRarCallBack(msg: cardinal; userData: LPARAM; P1: LPARAM; P2: LPARAM): integer; {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
var
  vCancel: boolean;
  vFileName: AnsiString;
  vPassword: AnsiString;
  vCBI: ICallBackInfo;
begin
  vCancel := False;
  Result := RAR_CONTINUE;
  vCBI := ICallbackInfo(userData);

  try
    case msg of
      UCM_CHANGEVOLUME:
        begin
          case vCBI.RAR.ReadMVToEnd of False:
            begin
              Result := RAR_CANCEL;
              Exit;
            end;
          end;

          case P2 of
            RAR_VOL_ASK:
              begin
                vFileName := PAnsiChar(P1);
                case assigned(vCBI.onNextVolRequired) of True:
                  vCBI.onNextVolRequired(vCBI.RAR, PAnsiChar(P1), vFileName, vCancel);
                end;
                case vCancel of True:
                  Result := RAR_CANCEL;
                end;
                strPCopy(PAnsiChar(P1), vFileName);
              end;
            RAR_VOL_NOTIFY: {$IFDEF BazDebugWindow} {debug('found next vol')} {$ENDIF}; // occurs when next volume required and next part was found
          end;
        end;

      UCM_NEEDPASSWORD:
        begin
          vFileName := vCBI.RAR.info.fileName;
          case assigned(vCBI.onPasswordRequired) of True:
            vCBI.onPasswordRequired(vCBI.RAR, vFileName, vPassword, vCancel);
          end;
          case vCancel of True:
            Result := RAR_CANCEL;
          end;
          strPCopy(Pointer(P1), copy(vPassword, 1, P2)); // P1 = pointer to the password buffer in unrar; P2 = maximum size of the buffer
        end;

      UCM_PROCESSDATA:
        begin
        //      setLength(vData, P2);
        //      move(Pointer(P1)^, vData[0], P2);
        //      setString(vDataString, PAnsiChar(@vData[0]), P2);

          vCBI.RAR.progressInfo.ArchiveBytesDone := vCBI.RAR.progressInfo.ArchiveBytesDone + P2;
          vCBI.RAR.progressInfo.FileBytesDone := vCBI.RAR.progressInfo.FileBytesDone + P2;

          case assigned(vCBI.onProgress) of True:
            vCBI.onProgress(vCBI.RAR, vCBI.RAR.progressInfo);
          end;
          case vCBI.RAR.abort of True:
            Result := RAR_CANCEL;
          end;
        end;
    end;

  except
    MessageBox(0, 'TRAR exception in unRarCallBack', 'TRAR', MB_ICONEXCLAMATION or MB_OK);
  end;
end;

function getOpenMode(bReadMVToEnd: boolean): TRAROpenMode;
begin
  if bReadMVToEnd then
    Result := omRAR_OM_LIST_INCSPLIT
  else
    Result := omRAR_OM_LIST;
end;

procedure initArchive(const aRAR: TRARArchive);
begin
  with aRAR do
  begin
    handle := 0;
    opened := False;
    ZeroMemory(@info, SizeOf(info));
    ZeroMemory(@fileItem, SizeOf(fileItem));
    ZeroMemory(@progressInfo, SizeOf(progressInfo));
    hasComment := False;
    comment := '';
    commentState := 0;
    abort := False;
  end;
end;

function ITBS(aFolderPath: string): string;
const
  BACKSLASH = #92;
begin
  Result := aFolderPath;
  if Length(Result) = 0 then
    Exit;
  if Result[Length(Result)] <> BACKSLASH then
    Result := Result + BACKSLASH;
end;

function ProcessDataCallBack(addr: PByte; size: integer): integer;
//var
//  vData: TBytes;
//  vDataString: string;
begin
//  debugInteger('data size', size);
//  setLength(vData, size);
//  move(pointer(addr)^, vData[0], size);
//  setString(vDataString, PAnsiChar(@vData[0]), size);
//  debugString('vDataString', vDataString);
//  memoryStream.write(addr^, size);
//  Result := 0; // 0 = cancel. Anything else continues (rdwrfn.cpp)
 Result := 1;
end;

function initCallBack(const aRAR: TRARArchive; aOnProgress: TRAROnProgress = nil;
  aOnPasswordRequired: TRAROnPasswordRequired = nil; aOnNextVolRequired: TRAROnNextVolumeRequired = nil): ICallBackInfo;
begin
  Result := TCallBackInfo.create;
  Result.RAR := aRAR;
  Result.onNextVolRequired := aOnNextVolRequired;
  Result.onProgress := aOnProgress;
  Result.onPasswordRequired := aOnPasswordRequired;
  RARSetCallback(aRAR.handle, unRarCallBack, LPARAM(Result));
//  RARSetProcessDataProc(aRAR.handle, ProcessDataCallBack);
end;

function processFileHeader(const aFileHeaderDataEx: TRARHeaderDataEx; const aRAR: TRARArchive): TRARHeaderType; // populate FArchiveInfo: TRARArchiveInfo and vFileItem: TRARFileItem from aHeaderDataEx
var
  ft: _FILETIME;
  st: TSystemTime;
  OS: string;
  gSplitFile: boolean;

  function binToStr(const bin: array of byte): string;
  var
    i: integer;
  begin
    setLength(Result, 2 * Length(bin));
    for i := low(bin) to high(bin) do
    begin
      Result[i * 2 + 1] := lowerCase(intToHex(bin[i], 2))[1];
      Result[i * 2 + 2] := lowerCase(intToHex(bin[i], 2))[2];
    end;
  end;

begin
  Result := htDirectory;
  if aFileHeaderDataEx.flags and RHDF_DIRECTORY = RHDF_DIRECTORY then
    Exit;

  gSplitFile := False;

  Result := htSplitFile;
  if aRAR.ReadMVToEnd then
  begin
    // a split file continued from the previous part of this archive or continued in the next part of this archive
    // will have multiple file headers, each with a portion of the compressed size, so we have to total them all to get the correct size for the file
    gSplitFile := ((aFileHeaderDataEx.Flags and RHDF_SPLITBEFORE) = RHDF_SPLITBEFORE) or
                  ((aFileHeaderDataEx.Flags and RHDF_SPLITAFTER) = RHDF_SPLITAFTER);

    if gSplitFile then
    begin
      // the first file header of a split file
      case (aRAR.readMVToEnd) and (not ((aFileHeaderDataEx.Flags  and RHDF_SPLITBEFORE) = RHDF_SPLITBEFORE))
                             and (((aFileHeaderDataEx.Flags      and RHDF_SPLITAFTER ) = RHDF_SPLITAFTER )) of
        True:
          begin
            aRAR.info.packedSizeMVVolume := aFileHeaderDataEx.PackSize; // start accumulating
            Exit;
          end;
      end; // skip to the next header for this split file

      // not the last header of a split file, not the first header of split file, so a middle header then
      case (aRAR.readMVToEnd) and (((aFileHeaderDataEx.Flags and RHDF_SPLITBEFORE) = RHDF_SPLITBEFORE))
                              and (((aFileHeaderDataEx.Flags and RHDF_SPLITAFTER) = RHDF_SPLITAFTER)) of
        True:
          begin
            inc(aRAR.info.packedSizeMVVolume, aFileHeaderDataEx.PackSize);
            Exit;
          end;
      end; // skip to the next header for this split file

      // last file header of a split file - we can now populate the TFileItem below
      case (aRAR.readMVToEnd) and (((aFileHeaderDataEx.Flags and RHDF_SPLITBEFORE) = RHDF_SPLITBEFORE))
                              and (not ((aFileHeaderDataEx.Flags and RHDF_SPLITAFTER) = RHDF_SPLITAFTER)) of
        True:
          inc(aRAR.info.packedSizeMVVolume, aFileHeaderDataEx.PackSize);
      end;

    end;
  end;

  Result := htFile; // now we can notify via FOnListFile

  aRAR.info.multiVolume := gSplitFile;

  if aRAR.info.archiverMajorVersion * 10 + aRAR.info.archiverMinorVersion < aFileHeaderDataEx.UnpVer then
  begin
    aRAR.info.archiverMinorVersion := aFileHeaderDataEx.UnpVer mod 10;
    aRAR.info.archiverMajorVersion :=(aFileHeaderDataEx.UnpVer - aRAR.info.archiverMinorVersion) div 10;
  end;

  aRAR.info.solid := ((aFileHeaderDataEx.Flags and ROADF_SOLID ) = ROADF_SOLID );  // ???

  OS:='unknown';
  case aFileHeaderDataEx.HostOS of
    0: OS:='DOS';
    1: OS:='IBM OS/2';
    2: OS:='Windows';
    3: OS:='Unix';
  end;
  aRAR.info.HostOS := OS;

  inc(aRAR.info.totalFiles);
  aRAR.info.dictionarySize := aFileHeaderDataEx.dictSize;

  case aRAR.ReadMVToEnd and gSplitFile of
     True: inc(aRAR.info.compressedSize, aRAR.info.packedSizeMVVolume);
    False: inc(aRAR.info.compressedSize, aFileHeaderDataEx.packSize);
  end;
  inc(aRAR.info.unCompressedSize, aFileHeaderDataEx.unpSize);

  ZeroMemory(@aRAR.fileItem, SizeOf(aRAR.fileItem));
  with aRAR.fileItem do
  begin
    fileName := strPas(aFileHeaderDataEx.fileName);
    fileNameW := aFileHeaderDataEx.fileNameW;

    splitFile := gSplitFile;
    if aRAR.ReadMVToEnd and gSplitFile then
       compressedSize := aRAR.info.packedSizeMVVolume
    else
       compressedSize := aFileHeaderDataEx.packSize;

    unCompressedSize := aFileHeaderDataEx.unpSize;
    hostOS := OS;
    CRC32 := format('%x',[aFileHeaderDataEx.fileCRC]);
    attributes := aFileHeaderDataEx.fileAttr;
    comment := aFileHeaderDataEx.cmtBuf;

    dosDateTimeToFileTime(hiWord(aFileHeaderDataEx.fileTime), loWord(aFileHeaderDataEx.fileTime), ft);
    fileTimeToSystemTime(ft, st);
    time := systemTimeToDateTime(st);

    compressionStrength := aFileHeaderDataEx.method;
    archiverVersion := aFileHeaderDataEx.unpVer;
    encrypted := (aFileHeaderDataEx.flags and RHDF_ENCRYPTED) = RHDF_ENCRYPTED;

    case aFileHeaderDataEx.hashType of
      RAR_HASH_BLAKE2: hash := binToStr(aFileHeaderDataEx.hash);
    end;
  end;
  gSplitFile := False;
end;

function processOpenArchive(const aOpenArchiveDataEx: TRAROpenArchiveDataEx; const aRAR: TRARArchive): boolean;
begin
  aRAR.info.volume := ((aOpenArchiveDataEx.Flags and ROADF_VOLUME) = ROADF_VOLUME);                    // Volume attribute (archive volume) ???
  aRAR.info.archiveComment := ((aOpenArchiveDataEx.Flags and ROADF_COMMENT) = ROADF_COMMENT);          // not available if header is encrypted
  aRAR.info.locked := ((aOpenArchiveDataEx.Flags and ROADF_LOCK) = ROADF_LOCK);                        // not available if header is encrypted
  aRAR.info.solid := ((aOpenArchiveDataEx.Flags and ROADF_SOLID) = ROADF_SOLID);                       // not available if header is encrypted
  aRAR.info.newNumbering := ((aOpenArchiveDataEx.Flags and ROADF_NEWNUMBERING) = ROADF_NEWNUMBERING);  // new volume naming scheme ('volname.partN.rar')
  aRAR.info.signed := ((aOpenArchiveDataEx.Flags and ROADF_SIGNED) = ROADF_SIGNED);                    // not available if header is encrypted
  aRAR.info.recovery := ((aOpenArchiveDataEx.Flags and ROADF_RECOVERY) = ROADF_RECOVERY);              // not available if header is encrypted
  aRAR.info.headerEncrypted := ((aOpenArchiveDataEx.Flags and ROADF_ENCHEADERS) = ROADF_ENCHEADERS);   // always available
  aRAR.info.firstVolume := ((aOpenArchiveDataEx.Flags and ROADF_FIRSTVOLUME) = ROADF_FIRSTVOLUME);     // ???

  aRAR.info.SFX := isSFX(aRAR.info.fileName);

  case aOpenArchiveDataEx.cmtState of // read archive comment - cmtState is actually aRAR.commentState
    RAR_COMMENT_EXISTS:
      begin // not available if header is encrypted
        aRAR.comment := strPas(aRAR.commentBuf);
        aRAR.hasComment := True;
      end;
    ERAR_NO_MEMORY: CheckRARResult(ERAR_NO_MEMORY, roOpenArchive);
    ERAR_BAD_DATA: CheckRARResult(ERAR_BAD_DATA, roOpenArchive);
    ERAR_UNKNOWN_FORMAT: CheckRARResult(ERAR_UNKNOWN_FORMAT, roOpenArchive);
    ERAR_SMALL_BUF: CheckRARResult(ERAR_SMALL_BUF, roOpenArchive);
  end;

  case aOpenArchiveDataEx.cmtState in [RAR_NO_COMMENT, RAR_COMMENT_EXISTS] of False: CheckRARResult(RAR_COMMENT_UNKNOWN, roOpenArchive); end; // unknown comment condition

  Result := True;
end;

function openArchive(const aArchivePath: string; const aOpenMode: TRAROpenMode; const aRAR: TRARArchive; bInitArchive: boolean): boolean;
var
  vOpenArchiveDataEx: TRAROpenArchiveDataEx;
begin
  case bInitArchive of True: initArchive(aRAR); end;

  aRAR.info.fileName := aArchivePath;
  aRAR.info.fileNameW := aArchivePath;

  Result := False;

  ZeroMemory(@vOpenArchiveDataEx, SizeOf(vOpenArchiveDataEx));
  with vOpenArchiveDataEx do
  begin
    arcName := PAnsiChar(aRAR.info.fileName);
    arcNameW := PWideChar(aRAR.info.fileNameW);
    openMode := ord(aOpenMode);

    cmtBuf := aRAR.commentBuf;
    cmtBufSize := RAR_MAX_COMMENT_SIZE;
    cmtSize := Length(aRAR.commentBuf);
    cmtState := aRAR.commentState; // feedback variable
  end;

  aRAR.handle := RAROpenArchiveEx(@vOpenArchiveDataEx);
  aRAR.opened := aRAR.handle <> RAR_INVALID_HANDLE;
  if aRAR.handle = RAR_INVALID_HANDLE then
  begin
    CheckRARResult(ERAR_EOPEN, roOpenArchive);
    Exit;
  end;

  Result := processOpenArchive(vOpenArchiveDataEx, aRAR);
end;

function closeArchive(const aArchiveHandle: THANDLE): boolean;
begin
  Result := not (aArchiveHandle = RAR_INVALID_HANDLE);
  case Result of True: Result := RARCloseArchive(aArchiveHandle) = RAR_SUCCESS; end;
end;

function extractArchiveFiles(const aExtractPath: string; const aFileName: string;
  const aFiles: TStringList; const aRAR: TRARArchive): boolean;
// perform the extract operation
var
  vHeaderDataEx: TRARHeaderDataEx;
  vUnrarOp: Integer;
  idx: integer;
begin
  Result := False;

  ZeroMemory(@vHeaderDataEx, SizeOf(vHeaderDataEx));
  ZeroMemory(@aRAR.progressInfo, SizeOf(aRAR.progressInfo));

  {$IFDEF BazDebugWindow}
  // debug('extractArchiveFiles: aExtractPath = ' + aExtractPath);
  // debug('extractArchiveFiles: aFileName = ' + aFileName);
  {$ENDIF}

  aRAR.progressInfo.ArchiveBytesTotal := aRAR.info.unCompressedSize; // the list operation in testRARArchive obtained aRAR.info.unCompressedSize

  try
    repeat
      // get the next file header in the archive
      if CheckRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roExtract) <> RAR_SUCCESS then
        Exit;
      processFileHeader(vHeaderDataEx, aRAR);

      aRAR.progressInfo.FileBytesDone := 0;
      aRAR.progressInfo.FileBytesTotal := vHeaderDataEx.UnpSize;   // aRAR.fileItem.unCompressedSize
      aRAR.progressInfo.FileName := vHeaderDataEx.FileNameW; // aRAR.info.fileNameW

      vUnrarOp := RAR_SKIP;
      case aFiles.count = 0 of
        True:
          if (aFileName = '') or sameText(aFileName, vHeaderDataEx.fileNameW) or sameText(aFileName, vHeaderDataEx.fileName) then
            vUnrarOp := RAR_EXTRACT;
        False:
          begin
            idx := aFiles.indexOf(vHeaderDataEx.fileNameW);
            if idx < 0 then
              idx := aFiles.indexOf(vHeaderDataEx.fileName);
            if idx <> -1 then
              vUnrarOp := RAR_EXTRACT;
          end;
      end;

      {$IFDEF WIN32}
      if CheckRARResult(RARProcessFile(aRAR.handle, vUnrarOp, PAnsiChar(aExtractPath), nil), roTest) <> RAR_SUCCESS then
        Exit;
      {$ELSE}
      if CheckRARResult(RARProcessFileW(aRAR.handle, vUnrarOp,  PWideChar(aExtractPath), nil), roTest) <> RAR_SUCCESS then
        Exit;
      {$ENDIF}

      // Application.proccessmessages
    until aRAR.abort; // RARReadHeaderEx = ERAR_END_ARCHIVE will usually Exit the loop

  finally
    Result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function extractArchiveFilesEx(const aExtractPath: string; const aFileName: string;
  const aFiles, aFiles2: TStringList; const aRAR: TRARArchive): boolean;
// perform the extract operation
var
  vHeaderDataEx: TRARHeaderDataEx;
  vUnrarOp: Integer;
  idx: integer;
  newfname: string;
begin
  Result := False;

  if aFiles.Count <> aFiles2.Count then
    Exit;

  ZeroMemory(@vHeaderDataEx, SizeOf(vHeaderDataEx));
  ZeroMemory(@aRAR.progressInfo, SizeOf(aRAR.progressInfo));

  {$IFDEF BazDebugWindow}
  // debug('extractArchiveFiles: aExtractPath = ' + aExtractPath);
  // debug('extractArchiveFiles: aFileName = ' + aFileName);
  {$ENDIF}

  aRAR.progressInfo.ArchiveBytesTotal := aRAR.info.unCompressedSize; // the list operation in testRARArchive obtained aRAR.info.unCompressedSize

  try
    repeat
      // get the next file header in the archive
      if CheckRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roExtract) <> RAR_SUCCESS then
        Exit;
      processFileHeader(vHeaderDataEx, aRAR);

      aRAR.progressInfo.FileBytesDone := 0;
      aRAR.progressInfo.FileBytesTotal := vHeaderDataEx.UnpSize;   // aRAR.fileItem.unCompressedSize
      aRAR.progressInfo.FileName := vHeaderDataEx.FileNameW; // aRAR.info.fileNameW

      vUnrarOp := RAR_SKIP;
      idx := -1;
      case aFiles.count = 0 of
        True:
          if (aFileName = '') or sameText(aFileName, vHeaderDataEx.fileNameW) or sameText(aFileName, vHeaderDataEx.fileName) then
            vUnrarOp := RAR_EXTRACT;
        False:
          begin
            idx := aFiles.indexOf(vHeaderDataEx.fileNameW);
            if idx < 0 then
              idx := aFiles.indexOf(vHeaderDataEx.fileName);
            if idx <> -1 then
              vUnrarOp := RAR_EXTRACT;
          end;
      end;

      {$IFDEF WIN32}
      if idx < 0 then
      begin
        if CheckRARResult(RARProcessFile(aRAR.handle, vUnrarOp, PAnsiChar(aExtractPath), nil), roTest) <> RAR_SUCCESS then
          Exit;
      end
      else
      begin
        newfname := aFiles2.Strings[idx];
        if CheckRARResult(RARProcessFile(aRAR.handle, vUnrarOp, PAnsiChar(aExtractPath), PAnsiChar(newfname)), roTest) <> RAR_SUCCESS then
          Exit;
      end;
      {$ELSE}
      if idx < 0 then
      begin
        if CheckRARResult(RARProcessFileW(aRAR.handle, vUnrarOp, PWideChar(aExtractPath), nil), roTest) <> RAR_SUCCESS then
          Exit;
      end
      else
      begin
        newfname := aFiles2.Strings[idx];
        if CheckRARResult(RARProcessFileW(aRAR.handle, vUnrarOp, PWideChar(aExtractPath), PWideChar(newfname)), roTest) <> RAR_SUCCESS then
          Exit;
      end;
      {$ENDIF}

      // Application.proccessmessages
    until aRAR.abort; // RARReadHeaderEx = ERAR_END_ARCHIVE will usually Exit the loop

  finally
    Result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function listArchiveFiles(const aRAR: TRARArchive; bNotify: boolean = True; const aOnListFile: TRAROnListFile = nil): boolean; forward;

function ExtractRARArchive(const aArchivePath: string; const aExtractPath: string;
  const aFileName: string; const aFiles: TStringList; aRAR: TRARArchive;
  aOnRARProgress: TRAROnProgress = nil; aOnPasswordRequired: TRAROnPasswordRequired = nil;
  aOnNextVolRequired: TRAROnNextVolumeRequired = nil): boolean;
// setup the extract operation
begin
  Result := openArchive(aArchivePath, getOpenMode(aRAR.readMVToEnd), aRAR, True);
  if not Result then
    Exit;

  initCallBack(aRAR, nil, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := listArchiveFiles(aRAR, False); // get archive total unCompressedSize for the progress callback below (reqd for multi-volume archives)
  finally
    closeArchive(aRAR.handle);
  end;

  if not Result then
    Exit;

  Result := openArchive(aArchivePath, omRAR_OM_EXTRACT, aRAR, False);
  if not Result then
    Exit;

  initCallBack(aRAR, aOnRARProgress, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := extractArchiveFiles(aExtractPath, aFileName, aFiles, aRAR); // perform the extract operation
  finally
    closeArchive(aRAR.handle);
  end;
end;

function ExtractRARArchiveEx(const aArchivePath: string; const aExtractPath: string;
  const aFileName: string; const aFiles, aFiles2: TStringList; aRAR: TRARArchive;
  aOnRARProgress: TRAROnProgress = nil; aOnPasswordRequired: TRAROnPasswordRequired = nil;
  aOnNextVolRequired: TRAROnNextVolumeRequired = nil): boolean;
// setup the extract operation
begin
  Result := openArchive(aArchivePath, getOpenMode(aRAR.readMVToEnd), aRAR, True);
  if not Result then
    Exit;

  initCallBack(aRAR, nil, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := listArchiveFiles(aRAR, False); // get archive total unCompressedSize for the progress callback below (reqd for multi-volume archives)
  finally
    closeArchive(aRAR.handle);
  end;

  if not Result then
    Exit;

  Result := openArchive(aArchivePath, omRAR_OM_EXTRACT, aRAR, False);
  if not Result then
    Exit;

  initCallBack(aRAR, aOnRARProgress, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := extractArchiveFilesEx(aExtractPath, aFileName, aFiles, aFiles2, aRAR); // perform the extract operation
  finally
    closeArchive(aRAR.handle);
  end;
end;

function listArchiveFiles(const aRAR: TRARArchive; bNotify: boolean = True; const aOnListFile: TRAROnListFile = nil): boolean;
// perform the list operation
var
  vHeaderDataEx: TRARHeaderDataEx;
begin
  Result := False;

  ZeroMemory(@vHeaderDataEx, SizeOf(vHeaderDataEx)); // is really TRARFileHeaderDataEx

  vHeaderDataEx.cmtBuf := aRAR.commentBuf;
  vHeaderDataEx.cmtBufSize := Length(aRAR.commentBuf);
  vHeaderDataEx.cmtSize := RAR_MAX_COMMENT_SIZE;
  vHeaderDataEx.cmtState := aRAR.commentState;

  try
    repeat // really = RARReadFileHeaderEx
      case CheckRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roListFiles) = RAR_SUCCESS of False: Exit; end;  // get the next file header in the archive
      case (processFileHeader(vHeaderDataEx, aRAR) = htFile) and bNotify and assigned(aOnListFile) of  True: aOnListFile(aRAR, aRAR.fileItem); end;
      case CheckRARResult(RARProcessFile(aRAR.handle, RAR_SKIP, nil, nil), roListFiles) = RAR_SUCCESS  of False: Exit; end;  // do nothing - skip to next file header

      //application.processMessages;  // allow the user to actually press a cancel button
    until aRAR.abort;               // RARReadHeaderEx = ERAR_END_ARCHIVE will usually Exit the loop

  finally
    Result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function listRARFiles(const aArchivePath: string; aRAR: TRARArchive; aOnListFile: TRAROnListFile = nil;
  aOnPasswordRequired: TRAROnPasswordRequired = nil; aOnNextVolRequired: TRAROnNextVolumeRequired = nil): boolean;
// setup the list operation
begin
  Result := openArchive(aArchivePath, getOpenMode(aRAR.readMVToEnd), aRAR, True);
  if not Result then
    Exit;

  initCallBack(aRAR, nil, aOnPasswordRequired, aOnNextVolRequired);
  try
    Result := listArchiveFiles(aRAR, True, aOnListFile); // call the list operation
  finally
    closeArchive(aRAR.handle);
  end;
end;

function extractPreparedRARArchive(const aArchivePath: string; const aExtractPath: string;
  const aFileName: string; const aFiles: TStringList; aRAR: TRARArchive;
  aOnRARProgress: TRAROnProgress = nil; aOnPasswordRequired: TRAROnPasswordRequired = nil;
  aOnNextVolRequired: TRAROnNextVolumeRequired = nil): boolean;
begin
  Result := openArchive(aArchivePath, omRAR_OM_EXTRACT, aRAR, False);
  if not Result then
    Exit;

  initCallBack(aRAR, aOnRARProgress, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := extractArchiveFiles(aExtractPath, aFileName, aFiles, aRAR); // perform the extract operation
  finally
    closeArchive(aRAR.handle);
  end;
end;

function prepareRARArchive(const aArchivePath: string; aRAR: TRARArchive;
  aOnRARProgress: TRAROnProgress = nil; aOnPasswordRequired: TRAROnPasswordRequired = nil;
  aOnNextVolRequired: TRAROnNextVolumeRequired = nil): boolean;
// setup the extract operation
begin
  Result := openArchive(aArchivePath, getOpenMode(aRAR.readMVToEnd), aRAR, True);
  if not Result then
    Exit;

  initCallBack(aRAR, nil, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := listArchiveFiles(aRAR, False); // get archive total unCompressedSize for the progress callback below
  finally
    closeArchive(aRAR.handle);
  end;
end;

function testArchiveFiles(const aRAR: TRARArchive): boolean;
// perform the test operation
var
  vHeaderDataEx: TRARHeaderDataEx;
begin
  Result := False;

  ZeroMemory(@vHeaderDataEx, SizeOf(vHeaderDataEx));
  ZeroMemory(@aRAR.progressInfo, SizeOf(aRAR.progressInfo));

  aRAR.progressInfo.ArchiveBytesTotal := aRAR.info.unCompressedSize; // the list operation in testRARArchive obtained aRAR.info.unCompressedSize

  try
    repeat
      case CheckRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roTest) = RAR_SUCCESS of False: Exit; end;  // get the next file header in the archive
      processFileHeader(vHeaderDataEx, aRAR);

      aRAR.progressInfo.FileBytesDone := 0;
      aRAR.progressInfo.FileBytesTotal := vHeaderDataEx.UnpSize; // aRAR.fileItem.unCompressedSize
      aRAR.progressInfo.FileName := vHeaderDataEx.FileNameW; // aRAR.info.fileNameW

      case CheckRARResult(RARProcessFile(aRAR.handle, RAR_TEST, nil, nil), roTest) = RAR_SUCCESS of False: Exit; end;

      // application.processMessages; // allow the user to actually press a cancel button
    until aRAR.abort;              // RARReadHeaderEx = ERAR_END_ARCHIVE will usually Exit the loop

  finally
    Result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function testRARArchive(const aArchivePath: string; aRAR: TRARArchive;
  aOnRARProgress: TRAROnProgress = nil; aOnPasswordRequired: TRAROnPasswordRequired = nil;
  aOnNextVolRequired: TRAROnNextVolumeRequired = nil): boolean;
// setup the test operation
begin
  Result := openArchive(aArchivePath, getOpenMode(aRAR.readMVToEnd), aRAR, True);
  if not Result then
    Exit;

  initCallBack(aRAR, nil, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := listArchiveFiles(aRAR, False); // get archive total unCompressedSize for the progress callback below
  finally
    closeArchive(aRAR.handle);
  end;

  if not Result then
    Exit;

  Result := openArchive(aArchivePath, omRAR_OM_EXTRACT, aRAR, False);
  if not Result then
    Exit;

  initCallBack(aRAR, aOnRARProgress, aOnPasswordRequired, aOnNextVolRequired);
  try
    case aRAR.password = '' of False: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    Result := testArchiveFiles(aRAR); // perform the test operation
  finally
    closeArchive(aRAR.handle);
  end;
end;

{ TRAR }

procedure TRAR.onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
  {$IFDEF BazDebugWindow}
//  debugString('FileName', aProgressInfo.FileName);
//  debugFormat('Archive Bytes: %d, Archive Bytes Done: %d', [aProgressInfo.ArchiveBytesTotal, aProgressInfo.ArchiveBytesDone]);
//  debugFormat('FileBytesTotal: %d, FileBytesDone: %d', [aProgressInfo.FileBytesTotal, aProgressInfo.FileBytesDone]);
  {$ENDIF}
end;

procedure TRAR.AddFile(const aFile: string);
begin
  FFiles.add(aFile);
end;

procedure TRAR.ClearFiles;
begin
  FFiles.clear;
end;

constructor TRAR.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  case csDesigning in componentState of False: initRAR; end;

  FRAR := TRARArchive.Create;
  readMultiVolumeToEnd := True;

  FFiles := TStringList.create;
  FFiles.sorted := False;
  FFiles.duplicates := dupIgnore;
  FFiles.caseSensitive := False;

  FFoundFiles := TStringList.create;

  FOnProgress := onRARProgressTest;
end;

destructor TRAR.Destroy;
begin
  if assigned(FRAR) then
    FRAR.free;
  if assigned(FFiles) then
    FFiles.free;
  if assigned(FFoundFiles) then
    FFoundFiles.free;
  ShutDownRAR;
  inherited destroy;
end;

function TRAR.ExtractArchive(const aArchivePath: string; const aExtractPath: string; const aFileName: string = ''): boolean;
var
  vExtractPath: string;
begin
  vExtractPath := aExtractPath;
  case (Length(vExtractPath) > 0) and (vExtractPath[Length(vExtractPath)] <> '\') of True: vExtractPath := vExtractPath + '\'; end;

  {$IFDEF BazDebugWindow}
//  debugString('aArchivePath', aArchivePath);
//  debugString('vExtractPath', vExtractPath);
//  debugString('aFileName', aFileName);
  {$ENDIF}

  if vExtractPath  <> '' then
    ForceDirectories(vExtractPath);

  Result := ExtractRARArchive(aArchivePath, vExtractPath, aFileName, FFiles, FRAR, FOnProgress, FOnPasswordRequired, FOnNextVolumeRequired);
end;

function TRAR.ExtractArchiveEx(const aArchivePath: string; const aExtractPath: string;
  const aFiles, aFiles2: TStringList; const aFileName: string = ''): boolean;
var
  vExtractPath: string;
begin
  vExtractPath := aExtractPath;
  if (Length(vExtractPath) > 0) and (vExtractPath[Length(vExtractPath)] <> '\') then
    vExtractPath := vExtractPath + '\';

  {$IFDEF BazDebugWindow}
//  debugString('aArchivePath', aArchivePath);
//  debugString('vExtractPath', vExtractPath);
//  debugString('aFileName', aFileName);
  {$ENDIF}

  if vExtractPath <> '' then
    ForceDirectories(vExtractPath);

  Result := ExtractRARArchiveEx(aArchivePath, vExtractPath, aFileName, aFiles, aFiles2, FRAR, FOnProgress, FOnPasswordRequired, FOnNextVolumeRequired);
end;

function TRAR.ExtractPreparedArchive(const aArchivePath: string; const aExtractPath: string; const aFileName: string = ''): boolean;
var
  vExtractPath: string;
begin
  vExtractPath := aExtractPath;
  case (Length(vExtractPath) > 0) and (vExtractPath[Length(vExtractPath)] <> '\') of True: vExtractPath := vExtractPath + '\'; end;

  {$IFDEF BazDebugWindow}
//  debugString('aArchivePath', aArchivePath);
//  debugString('vExtractPath', vExtractPath);
//  debugString('aFileName', aFileName);
  {$ENDIF}

  if vExtractPath <> '' then
    ForceDirectories(vExtractPath);

  Result := extractPreparedRARArchive(aArchivePath, vExtractPath, aFileName, FFiles, FRAR, FOnProgress, FOnPasswordRequired, FOnNextVolumeRequired);
end;

function TRAR.FileCount: integer;
begin
  Result := FFiles.count;
end;

function TRAR.findFiles(const aFolderPath: string; bSubFolders: boolean = True; const aFileExts: string = '.rar'): integer;
var
  SR: TSearchRec;
  RC: integer;
  vFolderPath: string;
  vAttr: integer;

  function extOK: boolean;
  var
    vExt: string;
  begin
    vExt := lowerCase(extractFileExt(SR.name));
    Result := (aFileExts = '') or (Pos(vExt, aFileExts) > 0);
  end;

begin
  Result := 0;
  vFolderPath := ITBS(aFolderPath);
  vAttr := faAnyFile and not faHidden and not faSysFile;
  case bSubFolders of False: vAttr := vAttr and not faDirectory; end;

  case findFirst(vFolderPath + '*.*', vAttr, SR) = 0 of True:
    repeat
      if (SR.attr and faDirectory) = faDirectory then
      begin
         case (SR.name <> '.') and (SR.name <> '..') of True:
          findFiles(vFolderPath + SR.name, bSubFolders, aFileExts);
         end;
      end
      else
      begin
        case extOK and not isMultiVolPart(SR.name) of True:
          FFoundFiles.add(vFolderPath + SR.name);
        end;
      end;
    until findNext(SR) <> 0;
  end;

  sysUtils.findClose(SR);
  Result := FFoundFiles.count;
end;

function TRAR.ListArchive(const aArchivePath: string): boolean;
begin
  Result := listRARFiles(aArchivePath, FRAR, FOnListFile, FOnPasswordRequired, FOnNextVolumeRequired);
end;

function TRAR.PrepareArchive(const aArchivePath: string): boolean;
begin
  Result := prepareRARArchive(aArchivePath, FRAR, FOnProgress, FOnPasswordRequired, FOnNextVolumeRequired);
end;

procedure TRAR.SetLastResult(const Value: integer);
begin
  FlastResult := Value;
end;

function TRAR.testArchive(const aArchivePath: string): boolean;
begin
  Result := testRARArchive(aArchivePath, FRAR, FOnProgress, FOnPasswordRequired, FOnNextVolumeRequired);
end;

procedure TRAR.SetOnError(const Value: TRAROnErrorNotifyEvent);
begin
  RR.onError := value;
end;

procedure TRAR.setPassword(const Value: AnsiString);
begin
  FPassword := value;
  FRAR.password := value;
end;

procedure TRAR.setReadMVToEnd(const Value: boolean);
begin
  FReadMVToEnd := value;
  FRAR.readMVToEnd := value;
end;

function TRAR.getArchiveInfo: TRARArchiveInfo;
begin
  Result := FRAR.info;
end;

function TRAR.getDLLVersion: integer;
begin
  case csDesigning in componentState of False: Result := RARGetDLLVersion; end;
end;

function TRAR.GetLastResult: integer;
begin
  Result := RR.lastResult;
end;

function TRAR.GetOnError: TRAROnErrorNotifyEvent;
begin
  Result := RR.onError;
end;

function TRAR.getPassword: AnsiString;
begin
  Result := FPassword;
end;

function TRAR.getReadMVToEnd: boolean;
begin
  Result := FRAR.readMVToEnd;
end;

procedure TRAR.abort;
begin
  FRAR.abort := True;
end;

function TRAR.getVersion: string;
begin
  Result := gVersion;
end;

function ExtractFileNameWithoutExtension(const apath: string): string;
var
  i: integer;
begin
  Result := ExtractFileName(apath);
  if (Pos('.', Result) > 0) then
  begin
    for i := Length(Result) downto 1 do
      if Result[i] = '.' then
      begin
        SetLength(Result, i - 1);
        Exit;
      end;
  end;
end;

function TRAR.isMultiVol(const aArchivePath: string): boolean;
// It's the first part of a multi-volume set
var
  vFile, vExt: string;
begin
  vFile := ExtractFileNameWithoutExtension(aArchivePath); // strip off the .rar extension
  vExt := LowerCase(ExtractFileExt(vFile));                // isolate the [potential] .partn, .partnn or .partnnn part
  Result := (vExt = '.part1') or (vExt = '.part01') or (vExt = '.part001');
end;

function TRAR.isMultiVolPart(const aArchivePath: string): boolean;
// It's a continuation of a multi-volume set but not the first part
var
  vFile, vExt: string;
begin
  vFile := ExtractFileNameWithoutExtension(aArchivePath); // strip off the .rar extension
  vExt := LowerCase(ExtractFileExt(vFile));                // isolate the [potential] .partn, .partnn or .partnnn part
  Result := (Pos('.part', vExt) = 1) and (vExt <> '.part1') and (vExt <> '.part01') and (vExt <> '.part001'); // it's a subordinate part
end;

{ TRARArchive }
constructor TRARArchive.create;
begin
  inherited create;
  getMem(commentBuf, RAR_MAX_COMMENT_SIZE);
end;

destructor TRARArchive.destroy;
begin
  if assigned(commentBuf) then
    freeMem(commentBuf);
  inherited destroy;
end;

{ TRARResult }

function TRARResult.CheckRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
//  (ResultCode=ERAR_END_ARCHIVE) = 10 is not usually an error
var
  vReport: boolean;
begin
  Result := aResultCode;
  FLastResult := aResultCode;

  {$IFDEF BazDebugWindow}
//    debugFormat('Error: aResultCode: %d, aOperation: %d', [aResultCode, ord(aOperation)]);
  {$ENDIF}

  vReport := aResultCode in [
                             RAR_DLL_LOAD_ERROR,
                             ERAR_NO_MEMORY,
                             ERAR_BAD_DATA,
                             ERAR_BAD_ARCHIVE,
                             ERAR_UNKNOWN_FORMAT,
                             ERAR_EOPEN,
                             ERAR_ECREATE,
                             ERAR_ECLOSE,
                             ERAR_EREAD,
                             ERAR_EWRITE,
                             ERAR_SMALL_BUF,
                             ERAR_UNKNOWN,
                             ERAR_MISSING_PASSWORD,
                             ERAR_EREFERENCE,
                             ERAR_BAD_PASSWORD,
                             ERAR_LARGE_DICT,
                             RAR_COMMENT_UNKNOWN
                            ];

  if vReport and assigned(FOnError) then
    FOnError(self, aResultCode, aOperation);
end;

function TRARResult.GetLastResult: integer;
begin
  Result := FLastResult;
end;

function TRARResult.GetOnError: TRAROnErrorNotifyEvent;
begin
  Result := FOnError;
end;

procedure TRARResult.SetOnError(const Value: TRAROnErrorNotifyEvent);
begin
  FOnError := value;
end;

{ TCallBackInfo }

function TCallBackInfo.getOnNextVolRequired: TRAROnNextVolumeRequired;
begin
  Result := FOnNextVolRequired;
end;

function TCallBackInfo.getOnPasswordRequired: TRAROnPasswordRequired;
begin
  Result := FOnPasswordRequired;
end;

function TCallBackInfo.getOnProgress: TRAROnProgress;
begin
  Result := FOnProgress;
end;

function TCallBackInfo.getRAR: TRARArchive;
begin
  Result := FRAR;
end;

procedure TCallBackInfo.setOnNextVolRequired(const Value: TRAROnNextVolumeRequired);
begin
  FOnNextVolRequired := value;
end;

procedure TCallBackInfo.setOnPasswordRequired(const Value: TRAROnPasswordRequired);
begin
  FOnPasswordRequired := value;
end;

procedure TCallBackInfo.setOnProgress(const Value: TRAROnProgress);
begin
  FOnProgress := value;
end;

procedure TCallBackInfo.setRAR(const Value: TRARArchive);
begin
  FRAR := value;
end;

initialization
  RR := TRARResult.Create;

end.
