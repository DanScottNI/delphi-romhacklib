unit DanHexEdit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls;

type
  TDanHexEdit = class(TEdit)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure KeyPress(var Key: Char);override;
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDanHexEdit]);
end;

procedure TDanHexEdit.KeyPress(var Key: Char);
begin
  if Key = Chr(8) then exit;

  if (Key >= '0') and (Key <= '9') then
    exit;

  if (Key >= 'A') and (Key <= 'F') then
    exit;

  if (Key >= 'a') and (Key <= 'f') then
    exit;

  Key := chr(0);
end;

end.
