using System;
using System.Collections.Generic;
using System.Text;
using System.Net;
using System.Net.Sockets;

namespace Electronics
{
    public abstract class ComponentServer
    {
        private SyncComponent comp;

        public ComponentServer(SyncComponent comp)
        {
            this.comp = comp;
        }

        public abstract void Run();

        public abstract void Close();

        protected SyncComponent Comp
        {
            get { return comp; }
        }
    }

    public class ConsoleComponentServer : ComponentServer
    {
        public ConsoleComponentServer(SyncComponent comp)
            : base(comp)
        {
        }

        public override void Run()
        {
            while (true)
            {
                string s = Console.ReadLine();
                if (s == null)
                    break;
                Comp.AllPinsIn.latch(s);
                Comp.Clk();
                Console.WriteLine(Comp.AllPinsOut.valueString);
            }
        }

        public override void Close()
        {
            Console.In.Close();
        }
    }

    public class NetComponentServerClient : ComponentServer
    {
        private bool verbose;
        private string addr;
        private int port;
        private Socket sock;

        public NetComponentServerClient(SyncComponent comp, string addr, int port, bool verbose)
            : base(comp)
        {
            this.verbose = verbose;
            this.addr = addr;
            this.port = port;
            Open();
        }
        public NetComponentServerClient(SyncComponent comp, string addr, int port)
            : this(comp, addr, port, false)
        {
        }

        private void Open()
        {
            IPEndPoint endpoint = new IPEndPoint(Dns.GetHostAddresses(addr)[0], port);

            sock = new Socket(endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

            sock.Connect(endpoint);

            if (sock.Connected && verbose)
                Console.WriteLine("Connecté à " + endpoint.ToString());
        }

        private string StringOfBytes(byte[] bytes)
        {
            char[] s = new char[bytes.Length];
            for (int i = 0; i < bytes.Length; i++)
                s[i] = (char)bytes[i];
            return new string(s);
        }

        public override void Run()
        {
            byte[] bufferIn = new byte[Comp.PinsIn.Length + 1];

            while (sock.Connected)
            {
                sock.Receive(bufferIn);
                Comp.AllPinsIn.latch(bufferIn);
                Comp.Clk();
                if (verbose)
                    Console.WriteLine("Received {0}, responding {1}", StringOfBytes(bufferIn), Comp.AllPinsOut.valueString);
                sock.Send(Comp.AllPinsOut.valueBytesN);
            }

            if (verbose)
                Console.WriteLine("Déconnecté");
        }

        public override void Close()
        {
            if (sock.Connected)
                sock.Disconnect(false);
        }
    }
}
