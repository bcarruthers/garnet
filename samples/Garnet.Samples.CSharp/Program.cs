using Garnet.Composition;

var c = new Container();

using (c.AddDefaultSystems())
{
    c.RunLoop();
}
