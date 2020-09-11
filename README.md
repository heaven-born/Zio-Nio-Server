# Zio-Nio-Server

Purely functional lightweight general purpose TCP server based on ZIO and NIO2 + MtProto-specific server handling 'req_pq' and 'req_DH_params' requests.


### Structure

**ZioNioTcpServer** - general purpose TCP server. I created it in order to abstract from network-specific logic.

**MtProtoTcpServer** - server for handling 'req_pq' and 'req_DH_params' requests. Based on ZioNioTcpServer.

**DemoClient** - just a sandbox for deomonstrating the flow.
