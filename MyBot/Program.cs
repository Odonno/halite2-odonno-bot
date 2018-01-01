namespace MyBot
{
    class Program
    {
        static void Main(string[] args)
        {
            string botName = (args.Length > 0) ? args[0] : "Odonno";
            Halite2.Main.execute(botName);
        }
    }
}
