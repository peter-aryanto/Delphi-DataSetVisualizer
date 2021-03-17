unit DataSetVisualizerFrame;


interface

uses
  Vcl.Forms
  , ToolsAPI
  , Vcl.ComCtrls
  , Vcl.Grids
  , Vcl.DBGrids
  , Data.DB
  , Datasnap.Provider
  , Datasnap.DBClient
  , Vcl.StdCtrls
  , System.Classes
  , Vcl.Controls
  ;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope, asNotAvailable);

  TFrameDataSetVisualizer = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater,
    IOTAThreadNotifier, IOTAThreadNotifier160)
    MemoExtraInfo: TMemo;
    DBGridOutput: TDBGrid;
    DataSourceOutput: TDataSource;
    ClientDataSetOutput: TClientDataSet;
    DataSetProviderInput: TDataSetProvider;
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FItems: TStrings;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
    function SetUpTempFile(out ATempFilePathAndNameToUse: string): Boolean;
    function CanDeleteFileIfExists(const AFilePathAndName: string): Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure DisplayDataSet(const Expression, TypeName, EvalResult: string);

    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);

    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer);
  end;

procedure Register;

implementation

uses
  System.SysUtils, Actnlist, ImgList, Menus, IniFiles, DesignIntf
  , System.StrUtils
  , System.TypInfo
  , System.IOUtils
  ;

{$R *.dfm}

resourcestring
  sDataSetVisualizerName = 'DataSet Visualizer for Delphi';
  sDataSetVisualizerDescription = 'Displays the value of each field and record in a DataSet';
  sMenuText = 'Show DataSet content';
  sFormCaption = 'DataSet Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';

type

  IDataSetVisualizerFrameFormHelper = interface
    ['{E7EF12F0-7529-409C-80F9-C4A2531960CE}']
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
  end;

  TFormDataSetVisualizer = class(TInterfacedObject, INTACustomDockableForm
    , IDataSetVisualizerFrameFormHelper
  )
  private
    FMyFrame: TFrameDataSetVisualizer;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IDataSetVisualizerFrameFormHelper }
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
  end;

  TDataSetVisualizer = class(TInterfacedObject,
    IOTADebuggerVisualizer, IOTADebuggerVisualizerExternalViewer)
  public
    { IOTADebuggerVisualizer }
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    { IOTADebuggerVisualizerExternalViewer }
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string;
      Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

{ TDebuggerDateTimeVisualizer }

function TDataSetVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDataSetVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TDataSet';
  AllDescendants := True;
end;

function TDataSetVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDataSetVisualizer.GetVisualizerDescription: string;
begin
  Result := sDataSetVisualizerDescription;
end;

function TDataSetVisualizer.GetVisualizerIdentifier: string;
begin
  Result := 'DataSet Visualizer by Peter Aryanto';
end;

function TDataSetVisualizer.GetVisualizerName: string;
begin
  Result := sDataSetVisualizerName;
end;

function TDataSetVisualizer.Show(const Expression, TypeName, EvalResult: string;
  SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TFrameDataSetVisualizer;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TFormDataSetVisualizer.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IDataSetVisualizerFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IDataSetVisualizerFrameFormHelper).GetFrame as TFrameDataSetVisualizer;
  AFrame.DisplayDataSet(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TFrameDataSetVisualizer }

procedure TFrameDataSetVisualizer.DisplayDataSet(
  const Expression, TypeName, EvalResult: string
);
var
  LTempFilePathAndName: string;
begin
  FAvailableState := asAvailable;
  FExpression := 'TClientDataSet(' + Expression + ')';

  MemoExtraInfo.Text := 'Cannot visualize Data Set.';

  try
    if not SetUpTempFile(LTempFilePathAndName) then
    begin
      Exit;
    end;

    Evaluate(FExpression + '.SaveToFile(''' + LTempFilePathAndName + ''')');
    MemoExtraInfo.Text := 'Temp file saved: ' + LTempFilePathAndName;

    if FileExists(LTempFilePathAndName) then
    begin
      ClientDataSetOutput.LoadFromFile(LTempFilePathAndName);
      MemoExtraInfo.Text :=
        'Record Count: ' + IntToStr(ClientDataSetOutput.RecordCount);
    end;
  except
    // Suppress the Exception!
  end;
end;

function TFrameDataSetVisualizer.SetUpTempFile(
  out ATempFilePathAndNameToUse: string): Boolean;
const
  CBasicTempFileName = 'TestClientDataSet';
  CTempFileExtension = '.xml';
var
  LBasicTempFilePathAndName: string;
begin
  ATempFilePathAndNameToUse := '';

  try
    LBasicTempFilePathAndName := IncludeTrailingPathDelimiter(TPath.GetTempPath)
      + CBasicTempFileName + CTempFileExtension;

    if CanDeleteFileIfExists(LBasicTempFilePathAndName) then
    begin
      ATempFilePathAndNameToUse := LBasicTempFilePathAndName;
    end
    else
    begin
      ATempFilePathAndNameToUse := TPath.GetGUIDFileName + CTempFileExtension;
    end;

    Result := ATempFilePathAndNameToUse <> '';
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

function TFrameDataSetVisualizer.CanDeleteFileIfExists(
  const AFilePathAndName: string): Boolean;
var
  LCanDelete: Boolean;
  LNotExist: Boolean;
begin
  try
    if not FileExists(AFilePathAndName) then
    begin
      Result := True;
      Exit;
    end;

    LCanDelete := DeleteFile(AFilePathAndName);
    LNotExist := not FileExists (AFilePathAndName);
    Result := LCanDelete and LNotExist;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

procedure TFrameDataSetVisualizer.AfterSave;
begin
end;

procedure TFrameDataSetVisualizer.BeforeSave;
begin
end;

procedure TFrameDataSetVisualizer.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

procedure TFrameDataSetVisualizer.Destroyed;
begin
end;

function TFrameDataSetVisualizer.Evaluate(Expression: string): string;

  function StripSingleQuotePrefixAndSuffix(const SourceStr: string): string;
  const
    SINGLE_QUOTE = '''';
  var
    HasSingleQuotePrefixAndSuffix: Boolean;
  begin
    if SourceStr = '' then
    begin
      Result := SourceStr;
      Exit;
    end;

    try
      HasSingleQuotePrefixAndSuffix := (SourceStr[1] = SINGLE_QUOTE)
        and (SourceStr[Length(SourceStr)] = SINGLE_QUOTE);

      if HasSingleQuotePrefixAndSuffix then
        Result := Copy(SourceStr, 2, Length(SourceStr) - 2)
      else
        Result := SourceStr;
    except
      Result := '<<DS..Vis..Error>>';
    end;
  end;

var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        repeat
        begin
          Done := True;
          EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
          case EvalRes of
            erOK: Result := ResultStr;
            erDeferred:
              begin
                FCompleted := False;
                FDeferredResult := '';
                FDeferredError := False;
                FNotifierIndex := CurThread.AddNotifier(Self);
                while not FCompleted do
                  DebugSvcs.ProcessDebugEvents;
                CurThread.RemoveNotifier(FNotifierIndex);
                FNotifierIndex := -1;
                if not FDeferredError then
                begin
                  if FDeferredResult <> '' then
                    Result := FDeferredResult
                  else
                    Result := ResultStr;
                end;
              end;
            erBusy:
              begin
                DebugSvcs.ProcessDebugEvents;
                Done := False;
              end;
          end;
        end
        until Done = True;
      end;
    end;
  end;

  Result := StripSingleQuotePrefixAndSuffix(Result);
end;

procedure TFrameDataSetVisualizer.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress), ResultSize, ReturnCode);
end;

procedure TFrameDataSetVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

procedure TFrameDataSetVisualizer.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;
end;

procedure TFrameDataSetVisualizer.Modified;
begin
end;

procedure TFrameDataSetVisualizer.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin
end;

procedure TFrameDataSetVisualizer.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  DisplayDataSet(Expression, TypeName, EvalResult);
end;

procedure TFrameDataSetVisualizer.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TFrameDataSetVisualizer.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TFrameDataSetVisualizer.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    FreeAndNil(FItems);
    if Assigned(FClosedProc) then
      try
        FClosedProc;
      except
        //
      end;
  end;
  inherited;
end;

procedure TFrameDataSetVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin
end;

{ TFormDataSetVisualizer }

constructor TFormDataSetVisualizer.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TFormDataSetVisualizer.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
end;

procedure TFormDataSetVisualizer.CustomizeToolBar(ToolBar: TToolBar);
begin
end;

function TFormDataSetVisualizer.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TFormDataSetVisualizer.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame :=  TFrameDataSetVisualizer(AFrame);
end;

function TFormDataSetVisualizer.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TFormDataSetVisualizer.GetEditState: TEditState;
begin
  Result := [];
end;

function TFormDataSetVisualizer.GetFrameClass: TCustomFrameClass;
begin
  Result := TFrameDataSetVisualizer;
end;

function TFormDataSetVisualizer.GetIdentifier: string;
begin
  Result := 'DataSet Visualizer by Peter Aryanto';
end;

function TFormDataSetVisualizer.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TFormDataSetVisualizer.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TFormDataSetVisualizer.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TFormDataSetVisualizer.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TFormDataSetVisualizer.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
end;

procedure TFormDataSetVisualizer.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
end;

function TFormDataSetVisualizer.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

procedure TFormDataSetVisualizer.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

var
  DataSetVisualizer: IOTADebuggerVisualizer;

procedure Register;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DataSetVisualizer := TDataSetVisualizer.Create;
    DebuggerServices.RegisterDebugVisualizer(DataSetVisualizer);
  end;
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(DataSetVisualizer);
    DataSetVisualizer := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.

