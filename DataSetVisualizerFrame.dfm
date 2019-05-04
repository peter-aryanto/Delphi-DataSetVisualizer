object FrameDataSetVisualizer: TFrameDataSetVisualizer
  Left = 0
  Top = 0
  Width = 547
  Height = 275
  TabOrder = 0
  object StringListView: TListView
    Left = 0
    Top = 0
    Width = 547
    Height = 100
    Align = alTop
    Columns = <
      item
        Caption = 'Index'
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    OwnerData = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = StringListViewData
  end
  object DBGridOutput: TDBGrid
    Left = 0
    Top = 100
    Width = 547
    Height = 100
    Align = alTop
    DataSource = DataSourceOutput
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    Left = 16
    Top = 206
    Width = 513
    Height = 59
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    TabOrder = 2
  end
  object DataSourceOutput: TDataSource
    DataSet = ClientDataSetOutput
    Left = 136
    Top = 136
  end
  object DataSetProviderInput: TDataSetProvider
    Left = 240
    Top = 216
  end
  object ClientDataSetOutput: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 352
    Top = 136
  end
end
