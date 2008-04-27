using System;
using System.Collections.Generic;
using System.Text;
using System.Net;
using System.Net.Sockets;

namespace Electronics
{
    public abstract class NetComponentClient<PinsInEnumT, PinsOutEnumT> : Component<PinsInEnumT, PinsOutEnumT>
    {
        private string addr;
        private int port;
        private Socket sock;

        public NetComponentClient(string name, Signal[] signalsIn, Signal[] signalsOut, string addr, int port)
            : base(name, signalsIn, signalsOut)
        {
            this.addr = addr;
            this.port = port;
            Open();
        }

        private void Open()
        {
            IPEndPoint endpoint = new IPEndPoint(Dns.GetHostAddresses(addr)[0], port);

            sock = new Socket(endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

            sock.Connect(endpoint);
        }

        public override void Clk()
        {
            if (sock.Connected)
            {
                sock.Send(AllPinsIn.valueBytesN);

                byte[] bufferOut = new byte[PinsOut.Length + 1];

                sock.Receive(bufferOut);

                AllPinsOut.latch(bufferOut);
            }
        }

        protected void Close()
        {
            if (sock.Connected)
                sock.Disconnect(false);
        }
    }
}
