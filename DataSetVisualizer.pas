unit DataSetVisualizer;

interface
(*
uses
  ToolsAPI;

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

procedure Register;
*)
implementation
(*
uses
  Vcl.Forms,
  DataSetVisualizerFrame,
  System.SysUtils;

var
  _DataSetVisualizer: TDataSetVisualizer;

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

procedure Register;
//var
//  Services: IOTADebuggerServices;
begin
//  if Supports(BorlandIDEServices, IOTADebuggerServices, Services) then
//  begin
//    _DataSetVisualizer := TDataSetVisualizer.Create;
//    Services.RegisterDebugVisualizer(_DataSetVisualizer);
//  end;
  _DataSetVisualizer := TDataSetVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(_DataSetVisualizer);
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
end.
