using System;

namespace Intel400x
{
    public enum Pins4001
    {
        D0, D1, D2, D3,
        VSS, Clk1, Clk2, Sync,
        Reset, CL, CM, VDD,
        IO3, IO2, IO1, IO0
    }

    public class Intel4001 : Component<Pins4001>
    {
        int index;
        PinsGroup D, IO;

        Intel4001 (int index, string name, Signal[] signals) : base(name, signals)
        {
            this.index = index;
            pins[(int)Pins4001.Clk1].ondown += onclk1down;
            pins[(int)Pins4001.Clk2].ondown += onclk2down;
            pins[(int)Pins4001.Clk2].onup += onclk2up;
            pins[(int)Pins4001.Reset].ondown += onresetdown;
            pins[(int)Pins4001.Sync].ondown += onsyncdown;
            D = makePinsGroup("D", (int)Pins4001.D0, (int)Pins4001.D3);
            IO = makePinsGroup("IO", (int)Pins4001.IO0, (int)Pins4001.IO3);
        }
        public override string familyname
        {
            get { return "4001"; }
        }

        Cycle4004 cycle;

        void onsyncdown() { cycle = Cycle4004.A1; }

        byte[] rom = new byte[256];
        int addr = 0;
        IOOperations op = 0;
        bool CSE = false, IOE = false;

        void onresetdown()
        {
            addr = 0;
            op = 0;
            CSE = false;
            IOE = false;
            foreach (Pin pin in pins)
                pin.unlatch();
        }
        void onclk1down()
        {
            switch (cycle)
            {
                case Cycle4004.M1:
                    if (CSE)
                        D.latch(rom[addr] & 0xF);
                    break;
                case Cycle4004.M2:
                    if (CSE)
                        D.latch(rom[addr] >> 4);
                    break;
                case Cycle4004.X2:
                    if (op == IOOperations.RDR)
                        D.latch(IO.value);
                    break;
            }
        }           
        void onclk2down()
        {
            
            switch (cycle)
            {
                case Cycle4004.A1:
                    addr = D.value;
                    break;
                case Cycle4004.A2:
                    addr |= D.value << 4;
                    break;
                case Cycle4004.A3:
                    CSE = pins[(int)Pins4001.CM].value && D.value == index;
                    break;
                case Cycle4004.M2:
                    op = (pins[(int)Pins4001.CM].value && IOE) ? (IOOperations)D.value : IOOperations.NOP;
                    break;
                case Cycle4004.X2:
                    if (pins[(int)Pins4001.CM].value)
                        IOE = (D.value == index);
                    if (op == IOOperations.WRR)
                        IO.latch(D.value);  // why here ? when unlatched ?
                    break;
            }
        }
        void onclk2up()
        {
            switch (cycle)
            {
                case Cycle4004.M1:
                    D.unlatch();
                    break;
                case Cycle4004.M2:
                    D.unlatch();
                    break;
                case Cycle4004.X2:
                    D.unlatch();
                    break;
            }
            cycle++;
        }
    }
}
