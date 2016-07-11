unit AM.Freedom.DBPersistent.FireDac.PropertyNames;

interface

type
  TFDPropertyNames = class
  const
    Pooled = 'Pooled';
    DriverID = 'DriverID';
    Database = 'Database';
    Server = 'Server';
    OSAuthent = 'OSAuthent';
    User_Name = 'User_Name';
    Password = 'Password';
    MonitorBy = 'MonitorBy';
    Port = 'Port';
    //exclusive mssql
    ODBCAdvanced = 'ODBCAdvanced';
    LoginTimeout = 'LoginTimeout';
    Network = 'Network';
    Address = 'Address';
    Workstation = 'Workstation';
    Language = 'Language';
    Encrypt = 'Encrypt';
    //exclusive mssql and postgree
    ApplicationName = 'ApplicationName';
    MetaDefCatalog = 'MetaDefCatalog';
    MetaDefSchema = 'MetaDefSchema';
    MetaCurSchema = 'MetaCurSchema';
    MetaCurCatalog = 'MetaCurCatalog';
    //exclusive postgree
    PGAdvanced = 'PGAdvanced';
    OidAsBlob = 'OidAsBlob';
    UnknownFormat = 'UnknownFormat';
    //exclusive postgree and firebird
    CharacterSet = 'CharacterSet';
    ExtendedMetadata = 'ExtendedMetadata';
    //exclusive firebird
    RoleName = 'RoleName';
    IBAdvanced = 'IBAdvanced';
    PageSize = 'PageSize';
    Protocol = 'Protocol';
    CreateDatabase = 'CreateDatabase';
  end;

implementation

end.
