using System;
using System.Collections.Generic;
using System.Text;
using Electronics;

namespace Test
{
    class Program
    {
        static void Main(string[] args)
        {
            Signal sigin0 = new Signal("Entr�e 1 du And", PullType.NoPull);
            Signal sigin1 = new Signal("Entr�e 2 du And", PullType.NoPull);
            Signal sigout = new Signal("Sortie du And", PullType.NoPull);

            And and = new And("And", new Signal[] { sigin0, sigin1 }, new Signal[] { sigout });

            NetComponentServerClient netComp = new NetComponentServerClient(and, "felouque", 1400, true);
            netComp.Run();
        }
    }
}
