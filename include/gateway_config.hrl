-define(CONFIG_APPLICATION_NAME, "com.helium.Config").

-define(CONFIG_OBJECT_PATH, "/com/helium/Config").
-define(CONFIG_OBJECT_INTERFACE, "com.helium.Config").
-define(CONFIG_OBJECT(M), ?CONFIG_OBJECT_INTERFACE ++ "." ++ M).

-define(CONFIG_MEMBER_POSITION, "Position").
-define(CONFIG_MEMBER_POSITION_LOCK, "PositionLock").
-define(CONFIG_MEMBER_ADD_GW, "AddGateway").
-define(CONFIG_MEMBER_DOWNLOADING, "Downloading").
