unit UtilCommon;

interface

uses classes,ComCtrls;

procedure ListViewToCSV(var pListView : TListView;var pStringList : TStringList; pFilename : String);

implementation

procedure ListViewToCSV(var pListView : TListView;var pStringList : TStringList; pFilename : String);
var
  i,x : Integer;
  stroutput : String;
begin
  // First row of the CSV file, should match the column headings of the listview.
  if pListView.ViewStyle = vsReport then
  begin
    for i:= 0 to pListView.Columns.Count-1 do
    begin
      if i > 0 then stroutput := stroutput + ',';
      stroutput := stroutput + pListView.Columns[i].Caption;
    end;

    pStringList.Add(strOutput);

    for i := 0 to pListView.Items.Count-1 do
    begin
      stroutput := pListView.Items[i].Caption;

      if pListView.Items[i].SubItems.Count > 0 then
      begin
        for x := 0 to pListView.Items[i].SubItems.Count-1 do
        begin
          strOutput := stroutput + ',' + pListView.Items[i].SubItems[x];
        end;
      end;

      pStringList.Add(strOutput);    
    end;

    pStringList.SaveToFile(pFilename);

  end;

end;

end.
