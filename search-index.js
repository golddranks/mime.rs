var searchIndex = {};
searchIndex["mime"] = {"doc":"# Mime","items":[[3,"Mime","mime","Mime, or Media Type. Encapsulates common registers types.",null,null],[12,"0","","",0,null],[12,"1","","",0,null],[12,"2","","",0,null],[4,"TopLevel","","",null,null],[13,"Star","","",1,null],[13,"Text","","",1,null],[13,"Image","","",1,null],[13,"Audio","","",1,null],[13,"Video","","",1,null],[13,"Application","","",1,null],[13,"Multipart","","",1,null],[13,"Message","","",1,null],[13,"Model","","",1,null],[13,"Ext","","",1,null],[4,"SubLevel","","",null,null],[13,"Star","","",2,null],[13,"Plain","","",2,null],[13,"Html","","",2,null],[13,"Xml","","",2,null],[13,"Javascript","","",2,null],[13,"Css","","",2,null],[13,"EventStream","","",2,null],[13,"Json","","",2,null],[13,"WwwFormUrlEncoded","","",2,null],[13,"Msgpack","","",2,null],[13,"OctetStream","","",2,null],[13,"FormData","","",2,null],[13,"Png","","",2,null],[13,"Gif","","",2,null],[13,"Bmp","","",2,null],[13,"Jpeg","","",2,null],[13,"Ext","","",2,null],[4,"Attr","","",null,null],[13,"Charset","","",3,null],[13,"Boundary","","",3,null],[13,"Q","","",3,null],[13,"Ext","","",3,null],[4,"Value","","",null,null],[13,"Utf8","","",4,null],[13,"Ext","","",4,null],[6,"Param","","",null,null],[11,"hash","","",0,null],[11,"fmt","","",0,null],[11,"clone","","",0,null],[11,"eq","","",0,null],[11,"hash","","",1,null],[11,"fmt","","",1,null],[11,"clone","","",1,null],[11,"as_str","","",1,null],[11,"deref","","",1,null],[11,"eq","","",1,null],[11,"eq","","",1,null],[11,"eq","","",1,null],[11,"eq","","",1,null],[11,"eq","collections::string","",5,null],[11,"fmt","mime","",1,null],[11,"from_str","","",1,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[11,"hash","","",2,null],[11,"fmt","","",2,null],[11,"clone","","",2,null],[11,"as_str","","",2,null],[11,"deref","","",2,null],[11,"eq","","",2,null],[11,"eq","","",2,null],[11,"eq","","",2,null],[11,"eq","","",2,null],[11,"eq","collections::string","",5,null],[11,"fmt","mime","",2,null],[11,"from_str","","",2,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[11,"hash","","",3,null],[11,"fmt","","",3,null],[11,"clone","","",3,null],[11,"as_str","","",3,null],[11,"deref","","",3,null],[11,"eq","","",3,null],[11,"eq","","",3,null],[11,"eq","","",3,null],[11,"eq","","",3,null],[11,"eq","collections::string","",5,null],[11,"fmt","mime","",3,null],[11,"from_str","","",3,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[11,"hash","","",4,null],[11,"fmt","","",4,null],[11,"clone","","",4,null],[11,"as_str","","",4,null],[11,"deref","","",4,null],[11,"eq","","",4,null],[11,"eq","","",4,null],[11,"eq","","",4,null],[11,"eq","","",4,null],[11,"eq","collections::string","",5,null],[11,"fmt","mime","",4,null],[11,"from_str","","",4,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[11,"fmt","","",0,null],[11,"get_param","","",0,null],[11,"from_str","","",0,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[14,"mime!","","Easily create a Mime without having to import so many enums.",null,null]],"paths":[[3,"Mime"],[4,"TopLevel"],[4,"SubLevel"],[4,"Attr"],[4,"Value"],[3,"String"]]};
searchIndex["log"] = {"doc":"A lightweight logging facade.","items":[[3,"LogRecord","log","The &quot;payload&quot; of a log message.",null,null],[3,"LogMetadata","","Metadata about a log message.",null,null],[3,"LogLocation","","The location of a log message.",null,null],[3,"MaxLogLevelFilter","","A token providing read and write access to the global maximum log level\nfilter.",null,null],[3,"SetLoggerError","","The type returned by `set_logger` if `set_logger` has already been called.",null,null],[3,"ShutdownLoggerError","","The type returned by `shutdown_logger_raw` if `shutdown_logger_raw` has\nalready been called or if `set_logger_raw` has not been called yet.",null,null],[4,"LogLevel","","An enum representing the available verbosity levels of the logging framework",null,null],[13,"Error","","The &quot;error&quot; level.",0,null],[13,"Warn","","The &quot;warn&quot; level.",0,null],[13,"Info","","The &quot;info&quot; level.",0,null],[13,"Debug","","The &quot;debug&quot; level.",0,null],[13,"Trace","","The &quot;trace&quot; level.",0,null],[4,"LogLevelFilter","","An enum representing the available verbosity level filters of the logging\nframework.",null,null],[13,"Off","","A level lower than all log levels.",1,null],[13,"Error","","Corresponds to the `Error` log level.",1,null],[13,"Warn","","Corresponds to the `Warn` log level.",1,null],[13,"Info","","Corresponds to the `Info` log level.",1,null],[13,"Debug","","Corresponds to the `Debug` log level.",1,null],[13,"Trace","","Corresponds to the `Trace` log level.",1,null],[5,"max_log_level","","Returns the current maximum log level.",null,{"inputs":[],"output":{"name":"loglevelfilter"}}],[5,"set_logger","","Sets the global logger.",null,{"inputs":[{"name":"m"}],"output":{"name":"result"}}],[5,"set_logger_raw","","Sets the global logger from a raw pointer.",null,{"inputs":[{"name":"m"}],"output":{"name":"result"}}],[5,"shutdown_logger","","Shuts down the global logger.",null,{"inputs":[],"output":{"name":"result"}}],[5,"shutdown_logger_raw","","Shuts down the global logger.",null,{"inputs":[],"output":{"name":"result"}}],[8,"Log","","A trait encapsulating the operations required of a logger",null,null],[10,"enabled","","Determines if a log message with the specified metadata would be\nlogged.",2,null],[10,"log","","Logs the `LogRecord`.",2,null],[11,"fmt","","",0,null],[11,"clone","","",0,null],[11,"eq","","",0,null],[11,"eq","","",0,null],[11,"partial_cmp","","",0,null],[11,"partial_cmp","","",0,null],[11,"cmp","","",0,null],[11,"from_str","","",0,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[11,"fmt","","",0,null],[11,"max","","Returns the most verbose logging level.",0,{"inputs":[],"output":{"name":"loglevel"}}],[11,"to_log_level_filter","","Converts the `LogLevel` to the equivalent `LogLevelFilter`.",0,null],[11,"fmt","","",1,null],[11,"clone","","",1,null],[11,"eq","","",1,null],[11,"eq","","",1,null],[11,"partial_cmp","","",1,null],[11,"partial_cmp","","",1,null],[11,"cmp","","",1,null],[11,"from_str","","",1,{"inputs":[{"name":"str"}],"output":{"name":"result"}}],[11,"fmt","","",1,null],[11,"max","","Returns the most verbose logging level filter.",1,{"inputs":[],"output":{"name":"loglevelfilter"}}],[11,"to_log_level","","Converts `self` to the equivalent `LogLevel`.",1,null],[11,"args","","The message body.",3,null],[11,"metadata","","Metadata about the log directive.",3,null],[11,"location","","The location of the log directive.",3,null],[11,"level","","The verbosity level of the message.",3,null],[11,"target","","The name of the target of the directive.",3,null],[11,"level","","The verbosity level of the message.",4,null],[11,"target","","The name of the target of the directive.",4,null],[11,"fmt","","",5,null],[11,"clone","","",5,null],[11,"module_path","","The module path of the message.",5,null],[11,"file","","The source file containing the message.",5,null],[11,"line","","The line containing the message.",5,null],[11,"fmt","","",6,null],[11,"get","","Gets the current maximum log level filter.",6,null],[11,"set","","Sets the maximum log level.",6,null],[11,"fmt","","",7,null],[11,"fmt","","",7,null],[11,"description","","",7,null],[11,"fmt","","",8,null],[11,"fmt","","",8,null],[11,"description","","",8,null],[14,"log!","","The standard logging macro.",null,null],[14,"error!","","Logs a message at the error level.",null,null],[14,"warn!","","Logs a message at the warn level.",null,null],[14,"info!","","Logs a message at the info level.",null,null],[14,"debug!","","Logs a message at the debug level.",null,null],[14,"trace!","","Logs a message at the trace level.",null,null],[14,"log_enabled!","","Determines if a message logged at the specified level in that module will\nbe logged.",null,null]],"paths":[[4,"LogLevel"],[4,"LogLevelFilter"],[8,"Log"],[3,"LogRecord"],[3,"LogMetadata"],[3,"LogLocation"],[3,"MaxLogLevelFilter"],[3,"SetLoggerError"],[3,"ShutdownLoggerError"]]};
initSearch(searchIndex);
