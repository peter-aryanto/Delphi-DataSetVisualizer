object FrameDataSetVisualizer: TFrameDataSetVisualizer
  Left = 0
  Top = 0
  Width = 547
  Height = 275
  TabOrder = 0
  object DBGridOutput: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 541
    Height = 204
    Align = alClient
    DataSource = DataSourceOutput
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object MemoExtraInfo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 213
    Width = 541
    Height = 59
    Align = alBottom
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 1
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
