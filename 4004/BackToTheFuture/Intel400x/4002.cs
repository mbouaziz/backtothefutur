using System;
using Electronics;

namespace Intel400x
{
    public enum Pins4002
    {
        D0, D1, D2, D3,
        VSS, Clk1, Clk2, Sync,
        Reset, CS, CM, VDD,
        IO3, IO2, IO1, IO0
    }
    public class Intel4002 : AsyncComponent<Pins4002>
    {
        int index;
        PinsGroup D, D3D2, D1D0, IO;

        Intel4002 (int index, string name, Signal[] signals) : base(name, signals)
        {
            this.index = index;
            Pins[(int)Pins4002.Clk1].ondown += onclk1down;
            Pins[(int)Pins4002.Clk2].ondown += onclk2down;
            Pins[(int)Pins4002.Clk2].onup += onclk2up;
            Pins[(int)Pins4002.Reset].ondown += onresetdown;
            Pins[(int)Pins4002.Sync].ondown += onsyncdown;
            D = makePinsGroup("D", (int)Pins4002.D0, (int)Pins4002.D3);
            D3D2 = makePinsGroup("D3D2", (int)Pins4002.D2, (int)Pins4002.D3);
            D1D0 = makePinsGroup("D1D0", (int)Pins4002.D0, (int)Pins4002.D1);
            IO = makePinsGroup("IO", (int)Pins4002.IO0, (int)Pins4002.IO3);
        }
        public override string FamilyName
        {
            get { return "4002"; }
        }

        Cycle4004 cycle;

        void onsyncdown() { cycle = Cycle4004.A1; }

        byte[] ram = new byte[4*20];
        IOOperations op = 0;
        byte reg = 0, charno = 0;
        bool IOE = false;

        void onresetdown()
        {
            foreach (Pin pin in Pins)
                pin.unlatch();
            op = 0;
            IOE = false;
            reg = 0;
            charno = 0;
            ram.Initialize();
        }
        void onclk1down()
        {
            /*switch (cycle)
            {
            }*/
        }           
        void onclk2down()
        {
            switch (cycle)
            {
                case Cycle4004.M2:
                    op = (Pins[(int)Pins4001.CM].value && IOE) ? (IOOperations)D.value : IOOperations.NOP;
                    break;
                case Cycle4004.X2:
                    IOE = Pins[(int)Pins4001.CM].value && (D3D2.value == index);
                    if (Pins[(int)Pins4001.CM].value)
                        reg = (byte)D1D0.value;
                    if (op == IOOperations.WMP)
                        IO.latch(D.value);
                    break;
                case Cycle4004.X3:
                    if (IOE)
                        charno = (byte)D.value;
                    break;
            }
        }
        void onclk2up()
        {
            /*switch (cycle)
            {
            }*/
            cycle++;
        }
    }
}
