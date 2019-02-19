-define(CONFIG_APPLICATION_NAME, "com.helium.Config").

-define(CONFIG_BLE_ADAPTER_PATH, "/org/bluez/hci0").

-define(CONFIG_OBJECT_PATH, "/").
-define(CONFIG_OBJECT_INTERFACE, "com.helium.Config").
-define(CONFIG_OBJECT(M), ?CONFIG_OBJECT_INTERFACE ++ "." ++ M).

-define(CONFIG_MEMBER_LIGHTS, "Lights").
-define(CONFIG_MEMBER_POSITION, "Position").
-define(CONFIG_MEMBER_POSITION_LOCK, "PositionLock").
-define(CONFIG_MEMBER_DOWNLOADING, "Downloading").

-define(MINER_APPLICATION_NAME, "com.helium.Miner").
-define(MINER_INTERFACE, "com.helium.Miner").
-define(MINER_OBJECT(M), ?MINER_INTERFACE ++ "." ++ M).
-define(MINER_MEMBER_PUBKEY, "PubKey").
-define(MINER_MEMBER_ADD_GW, "AddGateway").
-define(MINER_MEMBER_ASSERT_LOC, "AssertLocation").

-define(MINER_ERROR_BADARGS, "com.helium.Miner.Error.BadArgs").
-define(MINER_ERROR_GW_EXISTS, "com.helium.Miner.Error.GatewayExists").
-define(MINER_ERROR_INTERNAL, "com.helium.Miner.Error.Internal").
