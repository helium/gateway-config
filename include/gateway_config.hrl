-define(CONFIG_APPLICATION_NAME, "com.helium.Config").

-define(CONFIG_BLE_ADAPTER_PATH, "/org/bluez/hci0").

-define(CONFIG_OBJECT_PATH, "/").
-define(CONFIG_OBJECT_INTERFACE, "com.helium.Config").
-define(CONFIG_OBJECT(M), ?CONFIG_OBJECT_INTERFACE ++ "." ++ M).

-define(CONFIG_MEMBER_POSITION, "Position").
-define(CONFIG_MEMBER_POSITION_LOCK, "PositionLock").
-define(CONFIG_MEMBER_DOWNLOADING, "Downloading").

-define(MINER_OBJECT_PATH, "/").
-define(MINER_INTERFACE, "com.helium.Miner").
-define(MINER_OBJECT(M), ?MINER_INTERFACE ++ "." ++ M).
