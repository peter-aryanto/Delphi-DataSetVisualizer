unit DataSetVisualizerFrame;


interface
(*
uses
//  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
//  Vcl.Graphics, Vcl.Controls,
  DesignIntf,
  Vcl.ComCtrls,
  Vcl.Menus,
  System.IniFiles,
  Vcl.ImgList,
  Vcl.ActnList,
  ToolsAPI,
  Vcl.Forms, Data.DB, System.Classes, Vcl.Controls, Vcl.Grids, Vcl.DBGrids,
  Datasnap.Provider, Datasnap.DBClient, Vcl.StdCtrls
  ;//, Vcl.Dialogs;

type
  TFrameDataSetVisualizer = class(TFrame,
    IOTADebuggerVisualizerExternalViewerUpdater)
    StringListView: TListView;
    DBGridOutput: TDBGrid;
    DataSourceOutput: TDataSource;
    DataSetProviderInput: TDataSetProvider;
    ClientDataSetOutput: TClientDataSet;
    Memo1: TMemo;
    procedure StringListViewData(Sender: TObject; Item: TListItem);
  private
    FIsAvailable: Boolean;
//    function Evaluate(const Expression: string): TDataSet;
    function Evaluate(const Expression: string): string;
  public
    procedure ShowDataSetOnGrid(const Expression: string);

    // Start of IOTADebuggerVisualizerExternalViewerUpdater Implementation
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    // End of IOTADebuggerVisualizerExternalViewerUpdater Implementation
{
    class procedure CreateAndShow (
      const Expression, TypeName, EvalResult: string;
      SuggestedLeft, SuggestedTop: Integer);
}
  end;

  TFormDataSetVisualizer = class(TInterfacedObject,
    INTACustomDockableForm{, IFrameFormHelper})
  private
//    FForm: TCustomForm;
    FFrame: TFrameDataSetVisualizer;
    FExpression: string;
  public const
    IDENTIFIER = 'DataSet Visualizer by Peter Aryanto';
  public
    constructor Create(const Expression: string);
    // Start of INTACustomDockableForm Implementation
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
    // End of INTACustomDockableForm Implementation

    { IFrameFormHelper }
//    function GetForm: TCustomForm;
//    function GetFrame: TCustomFrame;
//    procedure SetForm(Form: TCustomForm);
//    procedure SetFrame(Frame: TCustomFrame);

    property Frame: TFrameDataSetVisualizer read FFrame;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
    System.SysUtils
  ;










type
  TDataSetVisualizer = class(TInterfacedObject,
    IOTADebuggerVisualizer, IOTADebuggerVisualizerExternalViewer)
  public
    // Start of IOTADebuggerVisualizer Implementation
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(
      Index: Integer;
      var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    // End of IOTADebuggerVisualizer Implementation

    // Start of IOTADebuggerVisualizerExternalViewer
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string;
      SuggestedLeft, SuggestedTop: Integer
    ): IOTADebuggerVisualizerExternalViewerUpdater;
    // End of IOTADebuggerVisualizerExternalViewer
  end;

{ TDataSetVisualizer }

function TDataSetVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

procedure TDataSetVisualizer.GetSupportedType(
  Index: Integer;
  var TypeName: string;
  var AllDescendants: Boolean);
begin
  TypeName := 'TDataSet';
  AllDescendants := True;
end;

function TDataSetVisualizer.GetVisualizerIdentifier: string;
begin
  Result := TFormDataSetVisualizer.IDENTIFIER;
end;

function TDataSetVisualizer.GetVisualizerName: string;
begin
  Result := 'DataSet Visualizer for Delphi';
end;

function TDataSetVisualizer.GetVisualizerDescription: string;
begin
  Result := 'Display fields and records, as well as cell values in DataSet';
end;

function TDataSetVisualizer.GetMenuText: string;
begin
  Result := 'Show DataSet contents';
end;

function TDataSetVisualizer.Show(const Expression, TypeName, EvalResult: string;
  SuggestedLeft, SuggestedTop: Integer
): IOTADebuggerVisualizerExternalViewerUpdater;
var
  VisDockForm: TFormDataSetVisualizer;
  Form: TCustomForm;
//  Frame: TFrameDataSetVisualizer;
begin
{
  TFrameDataSetVisualizer.CreateAndShow(Expression, TypeName, EvalResult,
    SuggestedLeft, SuggestedTop);
}
  VisDockForm := TFormDataSetVisualizer.Create(Expression);

  Form := (BorlandIDEServices as INTAServices).CreateDockableForm(
    VisDockForm as INTACustomDockableForm);
  Form.Left := SuggestedLeft;
  Form.Top := SuggestedTop;

//  (VisDockForm as IFrameFormHelper).SetForm(AForm);
//  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TStringListViewerFrame;
//  AFrame.AddStringListItems(Expression, TypeName, EvalResult);
//  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;

  VisDockForm.Frame.ShowDataSetOnGrid(Expression);

  Result := VisDockForm.Frame as IOTADebuggerVisualizerExternalViewerUpdater;
end;

































{ TFrameDataSetVisualizer }

procedure TFrameDataSetVisualizer.ShowDataSetOnGrid(const Expression: string);
var
  OutputStr: string;
begin
  FIsAvailable := True;
//  Result := Evaluate(Format('%s.DelimitedText', [FExpression]));
//  Result := Copy(Result, 2, Length(Result) -2);

//  DBGridOutput.DataSource.DataSet := Evaluate(Expression);
//  DataSourceDataSetVisualizer.DataSet := Evaluate(Expression);
//  DataSetProviderInput.DataSet := Evaluate(Expression);
//  ClientDataSetOutput.Data := DataSetProviderInput.Data;
//  Label1.Caption := IntToStr(Evaluate(Expression).RecordCount);
//  Memo1.Text:= Evaluate(Expression);
  OutputStr := Evaluate(Expression);
//  Memo1.Text:= Format('%s-%s-%p', [OutputStr, IntToHex(StrToInt(OutputStr)), Pointer(StrToUInt64('$' + IntToHex(StrToInt(OutputStr)))) {TDataSet(    Pointer(StrToUInt64('$' + IntToHex(StrToInt(OutputStr))))^           ).Fields[0].AsInteger}]);
  DBGridOutput.DataSource.DataSet := TDataSet(PInteger(StrToInt(OutputStr))^);
end;

//function TFrameDataSetVisualizer.Evaluate(const Expression: string): TDataSet;
function TFrameDataSetVisualizer.Evaluate(const Expression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  IsDone: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
//  Result := nil;
  Result := '';

  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;

  if CurProcess <> nil then
  begin
    CurThread := CurProcess.CurrentThread;
    if CurThread <> nil then
    begin
      repeat
        IsDone := True;

        EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
          CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);

        case EvalRes of
//          erOK: Result := TDataSet(PInteger(ResultAddr)^);//ResultStr;
//          erOK: Result := TDataSet(Pointer(StrToUInt64('$' + IntToHex(ResultAddr)))^);//ResultStr;
//          erOK: Result := Format('%s-erOK-%s-%d-%d-%d',[Expression, 'ResultStr', Length(ResultStr), ResultAddr, ResultSize]);//ResultStr;
//          erOK: Result := Format('%s-erOK-%d-%p-%p-%p-%p-%p',[Expression, ResultAddr, Pointer(ResultAddr), PInteger(ResultAddr), Pointer(@ResultAddr), PInteger(@ResultAddr), Addr(ResultAddr)]);//ResultStr;
            erOK: Result := IntToStr(ResultAddr);
          erDeferred:
            begin
//              FCompleted := False;
//              FDeferredResult := '';
//              FDeferredError := False;
//              FNotifierIndex := CurThread.AddNotifier(Self);
//              while not FCompleted do
//                DebugSvcs.ProcessDebugEvents;
//              CurThread.RemoveNotifier(FNotifierIndex);
//              FNotifierIndex := -1;
//              if not FDeferredError then
//              begin
//                if FDeferredResult <> '' then
//                  Result := FDeferredResult
//                else
//                  Result := ResultStr;
//              end;
              DebugSvcs.ProcessDebugEvents;
              IsDone := False;
            end;
          erBusy:
            begin
              IsDone := False;
            end;
        end;
      until IsDone = True;;
    end;
  end;
{
begin
  Result := '';

      repeat
      begin

      end
      until IsDone = True;
end;
}
end;

procedure TFrameDataSetVisualizer.CloseVisualizer;
begin
  (Parent as TCustomForm).Close;
end;

procedure TFrameDataSetVisualizer.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  FIsAvailable := False;
end;

procedure TFrameDataSetVisualizer.RefreshVisualizer(
  const Expression, TypeName, EvalResult: string);
begin
//  FIsAvailable := True;
  ShowDataSetOnGrid(Expression);
end;

procedure TFrameDataSetVisualizer.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
end;

procedure TFrameDataSetVisualizer.StringListViewData(Sender: TObject; Item: TListItem);
begin
end;

{
class procedure TFrameDataSetVisualizer.CreateAndShow(
  const Expression, TypeName, EvalResult: string;
  SuggestedLeft, SuggestedTop: Integer);
begin

end;
}

{ TFormDataSetVisualizer }

constructor TFormDataSetVisualizer.Create(const Expression: string);
begin
  FExpression := Expression;
end;

function TFormDataSetVisualizer.GetCaption: string;
begin
  Result := Format('Visualizer: %s', [FExpression]);
end;

function TFormDataSetVisualizer.GetFrameClass: TCustomFrameClass;
begin
  Result := TFrameDataSetVisualizer;
end;

procedure TFormDataSetVisualizer.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := AFrame as TFrameDataSetVisualizer;
  FFrame.DataSetProviderInput := TDataSetProvider.Create(FFrame);
end;

function TFormDataSetVisualizer.GetIdentifier: string;
begin
  Result := IDENTIFIER;
end;

function TFormDataSetVisualizer.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TFormDataSetVisualizer.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TFormDataSetVisualizer.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
end;

function TFormDataSetVisualizer.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TFormDataSetVisualizer.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TFormDataSetVisualizer.CustomizeToolBar(ToolBar: TToolBar);
begin
end;

procedure TFormDataSetVisualizer.LoadWindowState(Desktop: TCustomIniFile; const Section: string);
begin
end;

procedure TFormDataSetVisualizer.SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
begin
end;

function TFormDataSetVisualizer.GetEditState: TEditState;
begin
  Result := [];
end;

function TFormDataSetVisualizer.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

var
  _DataSetVisualizer: TDataSetVisualizer;

procedure Register;
var
  Services: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, Services) then
  begin
    _DataSetVisualizer := TDataSetVisualizer.Create;
    Services.RegisterDebugVisualizer(_DataSetVisualizer);
  end;
//  _DataSetVisualizer := TDataSetVisualizer.Create;
//  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(_DataSetVisualizer);
end;

procedure RemoveVisualizer;
var
  Services: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, Services) then
  begin
    Services.UnregisterDebugVisualizer(_DataSetVisualizer);
    _DataSetVisualizer := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
*)
uses
//  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
    Vcl.Forms
  , ToolsAPI
//  ,  Dialogs
  , Vcl.ComCtrls
//  , Vcl.Grids
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
    StringListView: TListView;
    DBGridOutput: TDBGrid;
    DataSourceOutput: TDataSource;
    DataSetProviderInput: TDataSetProvider;
    ClientDataSetOutput: TClientDataSet;
    Memo1: TMemo;
    procedure StringListViewData(Sender: TObject; Item: TListItem);
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
    function GetDelimiter: string;
    function GetStrictDelimiter: Boolean;
    function GetDelimitedText: string;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure AddStringListItems(const Expression, TypeName, EvalResult: string);

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
  System.SysUtils, Actnlist, ImgList, Menus, IniFiles, DesignIntf;

{$R *.dfm}

resourcestring
  sStringListVisualizerName = 'DataSet Visualizer for Delphi';
  sStringListVisualizerDescription = 'Displays the value of each field and record in a DataSet';
  sMenuText = 'Show DataSet content';
  sFormCaption = 'DataSet Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';

type

  // TODO: Try not to use this interface at all.
  IFrameFormHelper = interface
    ['{786451C2-C5EA-4F7A-8AC5-5D070B6E57C8}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TStringListVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
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
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerStringListVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

{ TDebuggerDateTimeVisualizer }

function TDebuggerStringListVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDebuggerStringListVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TStrings';
  AllDescendants := True;
end;

function TDebuggerStringListVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerStringListVisualizer.GetVisualizerDescription: string;
begin
  Result := sStringListVisualizerDescription;
end;

// TODO: Make this return the same value as GetIdentifier and contain "Peter Aryanto".
function TDebuggerStringListVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerStringListVisualizer.GetVisualizerName: string;
begin
  Result := sStringListVisualizerName;
end;

function TDebuggerStringListVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TFrameDataSetVisualizer;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TStringListVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TFrameDataSetVisualizer;
  AFrame.AddStringListItems(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TFrameDataSetVisualizer }

procedure TFrameDataSetVisualizer.AddStringListItems(const Expression, TypeName,
  EvalResult: string);
var
  Delim, DelimText: string;
begin
  FAvailableState := asAvailable;
  FExpression := Expression;
  if FItems = nil then
    FItems := TStringList.Create
  else
    FItems.Clear;

  Delim := GetDelimiter;
  if Length(Delim) > 1 then
  begin
    FItems.Delimiter := Delim[2];
    FItems.StrictDelimiter := GetStrictDelimiter;
    DelimText := GetDelimitedText;
    if DelimText <> '' then
    begin
      FItems.DelimitedText := DelimText;
      StringListView.Items.Count := FItems.Count;
    end else
      StringListView.Items.Count := 0;
    StringListView.Invalidate;
  end else
  begin
    FAvailableState := asNotAvailable;
    StringListView.Invalidate;
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

function TFrameDataSetVisualizer.GetDelimiter: string;
begin
  Result := Evaluate(Format('%s.Delimiter', [FExpression]));
end;

function TFrameDataSetVisualizer.GetStrictDelimiter: Boolean;
begin
  Result := StrToBool(Evaluate(Format('%s.StrictDelimiter', [FExpression])));
end;

function TFrameDataSetVisualizer.GetDelimitedText: string;
begin
  Result := Evaluate(Format('%s.DelimitedText', [FExpression]));
  Result := Copy(Result, 2, Length(Result) -2);
end;

procedure TFrameDataSetVisualizer.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;
  StringListView.Items.Count := 1;
  StringListView.Invalidate;
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
  AddStringListItems(Expression, TypeName, EvalResult);
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
      FClosedProc;
  end;
  inherited;
end;

procedure TFrameDataSetVisualizer.StringListViewData(Sender: TObject;
  Item: TListItem);
var
  ItemCaption: string;
  ItemText: string;
begin
  case FAvailableState of
    asAvailable:
      begin
        ItemCaption := Format('[%d]', [Item.Index]);
        ItemText := FItems[Item.Index];
      end;
    asProcRunning:
      begin
        ItemCaption := sProcessNotAccessible;
        ItemText := sProcessNotAccessible;
      end;
    asOutOfScope:
      begin
        ItemCaption := sOutOfScope;
        ItemText := sOutOfScope;
      end;
    asNotAvailable:
      begin
        ItemCaption := sValueNotAccessible;
        ItemText := sValueNotAccessible;
      end;
  end;
  Item.Caption := ItemCaption;
  if Item.SubItems.Count = 0 then
    Item.SubItems.Add(ItemText)
  else
    Item.SubItems[0] := ItemText;
end;

procedure TFrameDataSetVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{ TStringListVisualizerForm }

constructor TStringListVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TStringListVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TStringListVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
// no toolbar
end;

function TStringListVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TStringListVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame :=  TFrameDataSetVisualizer(AFrame);
end;

function TStringListVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TStringListVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TStringListVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TStringListVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TStringListVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TFrameDataSetVisualizer;
end;

function TStringListVisualizerForm.GetIdentifier: string;
begin
  Result := 'StringListDebugVisualizer';
end;

function TStringListVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TStringListVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TStringListVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TStringListVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TStringListVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TFrameDataSetVisualizer(Frame);
end;

var
  StringListVis: IOTADebuggerVisualizer;

procedure Register;
begin
  StringListVis := TDebuggerStringListVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(StringListVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(StringListVis);
    StringListVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;

end.
