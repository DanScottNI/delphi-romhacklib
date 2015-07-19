unit cExceptions;

interface

uses SysUtils;

type
  // Raised if the NES ROM already has a trainer.
  ENESTRAINEREXIST = class(Exception);
  // Raised if the NES ROM doesn't have a trainer.
  ENESTRAINERNOTEXIST = class(Exception);

  // Raised if the OffsetList object doesn't have
  // the requested property
  EPROPERTYNOTEXIST = class(Exception);

implementation

end.
