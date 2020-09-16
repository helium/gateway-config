-define(CONFIG_APPLICATION_NAME, "com.helium.Config").

-define(CONFIG_BLE_ADAPTER_PATH, "/org/bluez/hci0").

-define(CONFIG_OBJECT_PATH, "/").
-define(CONFIG_OBJECT_INTERFACE, "com.helium.Config").
-define(CONFIG_OBJECT(M), ?CONFIG_OBJECT_INTERFACE ++ "." ++ M).

-define(MINER_APPLICATION_NAME, "com.helium.Miner").
-define(MINER_INTERFACE, "com.helium.Miner").
-define(MINER_OBJECT(M), ?MINER_INTERFACE ++ "." ++ M).
-define(MINER_MEMBER_ADD_GW, "AddGateway").
-define(MINER_MEMBER_ASSERT_LOC, "AssertLocation").
-define(MINER_MEMBER_P2P_STATUS, "P2PStatus").

-define(MINER_ERROR_BADARGS, "com.helium.Miner.Error.BadArgs").
-define(MINER_ERROR_INTERNAL, "com.helium.Miner.Error.Internal").

-define(CONNMAN_PROFILES_PATH, "/var/lib/connman/").
