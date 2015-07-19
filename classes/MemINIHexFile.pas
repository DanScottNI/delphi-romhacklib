unit MemINIHexFile;

interface

uses Classes, INIFiles, SysUtils;
  type
  { This class is a descendant of the existing TMemINIFile
    class. My modifications add a hex value loading routine,
    where the the function will load in the hexadecimal values
    as a string, put a $ sign at the front, and return it as
    an integer. I also removed the default parameter from
    the most commonly used functions, as I find
    it totally unnecessary. I did not change any of the
    saving functions, as I do not use them. }
  TMemINIHexFile = class (TMemINIFile)
  public
    function ReadHexValue(const Section, Name: string ) : Integer;
    function ReadInteger(const Section, Name : String) : Integer;overload;
    function ReadString(const Section,Name : String) : String;overload;
  end;

implementation

{ TMemINIHexFile }

{ TMemINIHexFile Public functions}

function TMemINIHexFile.ReadHexValue(const Section, Name: string)
  : Integer;
begin
  result := StrToInt('$' + self.ReadString(Section,Name));
end;

function TMemINIHexFile.ReadInteger(const Section, Name: String): Integer;
begin
  result := inherited ReadInteger(Section,Name,0);
end;

function TMemINIHexFile.ReadString(const Section, Name: String): String;
begin
  result := inherited ReadString(Section,Name,'0');
end;

end.
