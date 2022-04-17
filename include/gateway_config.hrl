-define(CONFIG_APPLICATION_NAME, "com.helium.Config").

-define(CONFIG_BLE_ADAPTER_PATH, "/org/bluez/hci0").

-define(CONFIG_OBJECT_PATH, "/").
-define(CONFIG_OBJECT_INTERFACE, "com.helium.Config").
-define(CONFIG_OBJECT(M), ?CONFIG_OBJECT_INTERFACE ++ "." ++ M).

-define(MINER_ERROR_BADARGS, "com.helium.Miner.Error.BadArgs").
-define(MINER_ERROR_INTERNAL, "com.helium.Miner.Error.Internal").

-define(CONNMAN_PROFILES_PATH, "/var/lib/connman/").
