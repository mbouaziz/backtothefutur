using System;

namespace Electronics
{
    public abstract class AnyComponent
    {
        private string name;

        protected AnyComponent(string name)
        {
            this.name = name;
        }
        
        public abstract string FamilyName
        {
            get;
        }
        
        public string Name
        {
            get { return name; }
        }
    }

    public abstract class SyncComponent : AnyComponent
    {
        private Pin[] pinsIn, pinsOut;
        private PinsGroup allPinsIn, allPinsOut;

        public SyncComponent(string name)
            : base(name)
        {
        }

        public SyncComponent(string name, Signal[] signalsIn, Signal[] signalsOut, Pin[] pinsIn, Pin[] pinsOut)
            : base(name)
        {
            buildMe(signalsIn, signalsOut, pinsIn, pinsOut);
        }

        protected void buildMe(Signal[] signalsIn, Signal[] signalsOut, Pin[] pinsIn, Pin[] pinsOut)
        {
            int nbPinsIn = pinsIn.Length;
            int nbPinsOut = pinsOut.Length;

            if (signalsIn.Length != nbPinsIn)
                throw new Exception("Invalid input signals' number");
            if (signalsOut.Length != nbPinsOut)
                throw new Exception("Invalid output signals' number");

            this.pinsIn = pinsIn;
            this.pinsOut = pinsOut;

            allPinsIn = makePinsInGroup(Name + " all input pins", 0, nbPinsIn - 1);
            allPinsOut = makePinsOutGroup(Name + " all output pins", 0, nbPinsOut - 1);
        }
            
        protected PinsGroup makePinsInGroup(string name, int i1, int i2)
        {
            return new PinsGroup(name, pinsIn, i1, i2);
        }

        protected PinsGroup makePinsOutGroup(string name, int i1, int i2)
        {
            return new PinsGroup(name, pinsOut, i1, i2);
        }

        public Pin[] PinsIn
        {
            get { return pinsIn; }
        }
        public Pin[] PinsOut
        {
            get { return pinsOut; }
        }
        public PinsGroup AllPinsIn
        {
            get { return allPinsIn; }
        }
        public PinsGroup AllPinsOut
        {
            get { return allPinsOut; }
        }

        public abstract void Clk();
    }

    public abstract class Component<PinsInEnumT, PinsOutEnumT> : SyncComponent 
    {
        public Component(string name, Signal[] signalsIn, Signal[] signalsOut)
            : base(name)
        {
            int nbPinsIn = Enum.GetValues(typeof(PinsInEnumT)).Length;
            int nbPinsOut = Enum.GetValues(typeof(PinsOutEnumT)).Length;

            Pin[] pinsIn = new Pin[nbPinsIn];
            Pin[] pinsOut = new Pin[nbPinsOut];

            for (int i = 0; i < nbPinsIn; i++)
                pinsIn[i] = new Pin(Enum.GetName(typeof(PinsInEnumT), i), signalsIn[i], PullType.PullUp);
            for (int i = 0; i < nbPinsOut; i++)
                pinsOut[i] = new Pin(Enum.GetName(typeof(PinsOutEnumT), i), signalsOut[i], PullType.PullUp);

            buildMe(signalsIn, signalsOut, pinsIn, pinsOut);
        }
    }

    public abstract class AsyncComponent<PinsEnumT> : AnyComponent 
    {
        private Pin[] pins;
        private PinsGroup allPins;

        public AsyncComponent(string name, Signal[] signals) : base(name)
        {
            int nbPins = Enum.GetValues(typeof(PinsEnumT)).Length;

            if (signals.Length != nbPins)
                throw new Exception("Invalid signals' number");

            pins = new Pin[nbPins];

            for (int i = 0 ; i < nbPins ; i++)
                pins[i] = new Pin(Enum.GetName(typeof(PinsEnumT), i), signals[i], PullType.PullUp);

            allPins = makePinsGroup(name + " all pins", 0, nbPins);
        }

        protected PinsGroup makePinsGroup(string name, int i1, int i2)
        {
            return new PinsGroup(name, pins, i1, i2);
        }

        public Pin[] Pins
        {
            get { return pins; }
        }
        public PinsGroup AllPins
        {
            get { return allPins; }
        }
    }
}
