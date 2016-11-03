module DB

where

type WordList = [String]
type DB       = [WordList]

db :: DB
db = [
 ["character", "Romeo"],
 ["character", "Jack Dawson"],
 ["character", "Sean McGuire"],
 ["character", "Vincent Vega"],
 ["character", "Mr White"],
 ["character", "Winston Wolf"],
 ["character", "Mia"],
 ["character", "Juliet"],
 ["character", "Jimmie"],
 ["character", "Mr Brown"],
 ["character", "Dallas"],
 ["character", "Ellen Ripley"],
 ["character", "Bishop"],
 ["character", "Deckard"],
 ["character", "Rachael"],
 ["character", "Roy Batty"],
 ["character", "Vito Corleone"],
 ["character", "Michael Corleone"],
 ["character", "Tom Hagen"],
 ["character", "William Somerset"],
 ["character", "David Mills"],
 ["character", "Louise"],
 ["character", "Thelma"],
 ["character", "Max Cady"],
 ["character", "Will Hunting"],
 ["character", "Sam Bowden"],
 ["character", "Leigh Bowden"],
 ["character", "Danielle Bowden"],
 ["character", "John Doe"],
 ["character", "Verbal Kint"],
 ["character", "Dean Keaton"],
 ["character", "John Murdoch"],
 ["character", "Frank Bumstead"],
 ["character", "Doctor Schreber"],
 ["character", "James Cole"],
 ["character", "Kathryn Railly"],
 ["character", "Jeffrey Goines"],
 ["character", "John McClane"],
 ["character", "Hans Gruber"],
 ["character", "Hannibal Lecter"],
 ["character", "Clarice Starling"],
 ["character", "Castor Troy"],
 ["character", "Sean Archer"],
 ["character", "Mark Renton"],
 ["character", "Francis Begbie"],
 ["character", "Sidney Prescott"],
 ["character", "Gale Weathers"],
 ["character", "Billy Loomis"],
 ["character", "Ben Sanderson"],
 ["character", "Sera"],
 ["character", "Marge Gunderson"],
 ["character", "Carl Showalter"],
 ["character", "Ethan Hunt"],
 ["character", "Jim Phelps"],
 ["character", "Claire Phelps"],
 ["character", "Vincent Freeman"],
 ["character", "Irene Cassini"],
 ["character", "Indiana Jones"],
 ["character", "Sallah"],
 ["character", "Marion Ravenwood"],
 ["character", "Richard Kimble"],
 ["character", "Samuel Gerard"],
 ["character", "Charles Nichols"],
 ["character", "Kay"],
 ["character", "Jay"],
 ["character", "Laurel"],
 ["character", "Mr Pink"],
 ["character", "Eliot Ness"],
 ["character", "Jim Malone"],
 ["character", "George Stone"],
 ["character", "Al Capone"],
 ["character", "Rose DeWit Bukater"],
 ["character", "Connor MacLeod"],
 ["character", "Ramirez"],
 ["character", "Colonel Kurtz"],
 ["character", "Captain Willard"],
 ["character", "Lieutenant Kilgore"],
 ["character", "Gordon Gekko"],
 ["character", "Bud Fox"],
 ["character", "Carl Fox"],
 ["character", "Chris"],
 ["character", "Big Harold"],
 ["character", "Sgt. Barnes"],
 ["movie", "Apocalypse Now"],
 ["movie", "The Untouchables"],
 ["movie", "Men in Black"],
 ["movie", "Raiders of the Lost Ark"],
 ["movie", "Gattaca"],
 ["movie", "The Fugitive"],
 ["movie", "Blade Runner"],
 ["movie", "Alien"],
 ["movie", "Aliens"],
 ["movie", "Titanic"],
 ["movie", "Good Will Hunting"],
 ["movie", "Pulp Fiction"],
 ["movie", "Reservoir Dogs"],
 ["movie", "Romeo and Juliet"],
 ["movie", "The Godfather"],
 ["movie", "The Godfather II"],
 ["movie", "Seven"],
 ["movie", "Thelma and Louise"],
 ["movie", "Cape Fear"],
 ["movie", "The Usual Suspects"],
 ["movie", "Dark City"],
 ["movie", "Die Hard"],
 ["movie", "Silence of the Lambs"],
 ["movie", "Face/Off"],
 ["movie", "Trainspotting"],
 ["movie", "Scream"],
 ["movie", "Leaving Las Vegas"],
 ["movie", "Fargo"],
 ["movie", "Mission Impossible"],
 ["movie", "Twelve Monkeys"],
 ["movie", "Highlander"],
 ["movie", "Wall Street"],
 ["movie", "Platoon"],
 ["actor", "Andy Garcia"],
 ["actor", "Sean Connery"],
 ["actor", "Kevin Costner"],
 ["actor", "John Rhys-Davies"],
 ["actor", "Karen Allen"],
 ["actor", "Tommy Lee Jones"],
 ["actor", "Jeroen Krabbe"],
 ["actor", "Will Smith"],
 ["actor", "Linda Fiorentino"],
 ["actor", "Ethan Hawke"],
 ["actor", "Leonardo DiCaprio"],
 ["actor", "John Travolta"],
 ["actor", "Robin Williams"],
 ["actor", "Harvey Keitel"],
 ["actor", "Quentin Tarantino"],
 ["actor", "Lance Henriksen"],
 ["actor", "Tom Skerritt"],
 ["actor", "Rutger Hauer"],
 ["actor", "Harrison Ford"],
 ["actor", "Marlon Brando"],
 ["actor", "Al Pacino"],
 ["actor", "Robert Duvall"],
 ["actor", "Morgan Freeman"],
 ["actor", "Brad Pitt"],
 ["actor", "Robert De Niro"],
 ["actor", "Claire Danes"],
 ["actor", "Uma Thurman"],
 ["actor", "Sigourney Weaver"],
 ["actor", "Sean Young"],
 ["actor", "Susan Sarandon"],
 ["actor", "Geena Davis"],
 ["actor", "Matt Damon"],
 ["actor", "Nick Nolte"],
 ["actor", "Jessica Lange"],
 ["actor", "Juliette Lewis"],
 ["actor", "Kevin Spacey"],
 ["actor", "Gabriel Byrne"],
 ["actor", "Rufus Sewell"],
 ["actor", "William Hurt"],
 ["actor", "Kiefer Sutherland"],
 ["actor", "Bruce Willis"],
 ["actor", "Alan Rickman"],
 ["actor", "Anthony Hopkins"],
 ["actor", "Jodie Foster"],
 ["actor", "Nicolas Cage"],
 ["actor", "Ewan McGregor"],
 ["actor", "Robert Carlyle"],
 ["actor", "Neve Campbell"],
 ["actor", "Courteney Cox"],
 ["actor", "Skeet Ulrich"],
 ["actor", "Elisabeth Shue"],
 ["actor", "Frances McDormand"],
 ["actor", "Steve Buscemi"],
 ["actor", "Tom Cruise"],
 ["actor", "Jon Voight"],
 ["actor", "Emmanuelle Beart"],
 ["actor", "Madeleine Stowe"],
 ["actor", "Christopher Lambert"],
 ["actor", "Martin Sheen"],
 ["actor", "Charlie Sheen"],
 ["actor", "Michael Douglas"],
 ["actor", "Forest Whitaker"],
 ["actor", "Tom Berenger"],
 ["director", "Andrew Niccol"],
 ["director", "Barry Sonnenfeld"],
 ["director", "Andrew Davis"],
 ["director", "Steven Spielberg"],
 ["director", "James Cameron"],
 ["director", "Ridley Scott"],
 ["director", "Gus Van Sant"],
 ["director", "Baz Luhrmann"],
 ["director", "Francis Ford Coppola"],
 ["director", "David Fincher"],
 ["director", "Martin Scorsese"],
 ["director", "Bryan Singer"],
 ["director", "Alex Proyas"],
 ["director", "John McTiernan"],
 ["director", "Jonathan Demme"],
 ["director", "John Woo"],
 ["director", "Danny Boyle"],
 ["director", "Wes Craven"],
 ["director", "Mike Figgis"],
 ["director", "Brian De Palma"],
 ["director", "Joel Coen"],
 ["director", "Terry Gilliam"],
 ["director", "Russell Mulcahy"],
 ["director", "Oliver Stone"],
 ["release", "Blade Runner", "1982"],
 ["release", "Alien", "1979"],
 ["release", "Aliens", "1986"],
 ["release", "Titanic", "1997"],
 ["release", "Good Will Hunting", "1997"],
 ["release", "Pulp Fiction", "1994"],
 ["release", "Reservoir Dogs", "1992"],
 ["release", "Romeo and Juliet", "1996"],
 ["release", "The Godfather", "1972"],
 ["release", "The Godfather II", "1974"],
 ["release", "Seven", "1995"],
 ["release", "Thelma and Louise", "1991"],
 ["release", "Cape Fear", "1991"],
 ["release", "The Usual Suspects", "1995"],
 ["release", "Dark City", "1998"],
 ["release", "Die Hard", "1988"],
 ["release", "Face/Off", "1997"],
 ["release", "Silence of the Lambs", "1991"],
 ["release", "Trainspotting", "1996"],
 ["release", "Scream", "1996"],
 ["release", "Leaving Las Vegas", "1995"],
 ["release", "Fargo", "1996"],
 ["release", "Mission Impossible", "1996"],
 ["release", "Twelve Monkeys", "1995"],
 ["release", "Gattaca", "1997"],
 ["release", "Raiders of the Lost Ark", "1981"],
 ["release", "The Fugitive", "1993"],
 ["release", "Men in Black", "1997"],
 ["release", "The Untouchables", "1987"],
 ["release", "Highlander", "1986"],
 ["release", "Apocalypse Now", "1979"],
 ["release", "Wall Street", "1987"],
 ["release", "Platoon", "1986"],
 ["release", "The Truman Show", "1998"],
 ["direct", "Brian De Palma", "The Untouchables"],
 ["direct", "James Cameron", "Titanic"],
 ["direct", "James Cameron", "Aliens"],
 ["direct", "Ridley Scott", "Alien"],
 ["direct", "Ridley Scott", "Blade Runner"],
 ["direct", "Ridley Scott", "Thelma and Louise"],
 ["direct", "Gus Van Sant", "Good Will Hunting"],
 ["direct", "Quentin Tarantino", "Pulp Fiction"],
 ["direct", "Quentin Tarantino", "Reservoir Dogs"],
 ["direct", "Baz Luhrmann", "Romeo and Juliet"],
 ["direct", "Francis Ford Coppola", "The Godfather"],
 ["direct", "Francis Ford Coppola", "The Godfather II"],
 ["direct", "David Fincher", "Seven"],
 ["direct", "Martin Scorsese", "Cape Fear"],
 ["direct", "Bryan Singer", "The Usual Suspects"],
 ["direct", "Alex Proyas", "Dark City"],
 ["direct", "John McTiernan", "Die Hard"],
 ["direct", "Jonathan Demme", "Silence of the Lambs"],
 ["direct", "John Woo", "Face/Off"],
 ["direct", "Danny Boyle", "Trainspotting"],
 ["direct", "Wes Craven", "Scream"],
 ["direct", "Mike Figgis", "Leaving Las Vegas"],
 ["direct", "Joel Coen", "Fargo"],
 ["direct", "Brian De Palma", "Mission Impossible"],
 ["direct", "Terry Gilliam", "Twelve Monkeys"],
 ["direct", "Andrew Niccol", "Gattaca"],
 ["direct", "Steven Spielberg", "Raiders of the Lost Ark"],
 ["direct", "Andrew Davis", "The Fugitive"],
 ["direct", "Barry Sonnenfeld", "Men in Black"],
 ["direct", "Russell Mulcahy", "Highlander"],
 ["direct", "Francis Ford Coppola", "Apocalypse Now"],
 ["direct", "Oliver Stone", "Wall Street"],
 ["direct", "Oliver Stone", "Platoon"],
 ["play", "Leonardo DiCaprio", "Romeo and Juliet", "Romeo"],
 ["play", "Leonardo DiCaprio", "Titanic", "Jack Dawson"],
 ["play", "Robin Williams", "Good Will Hunting", "Sean McGuire"],
 ["play", "John Travolta", "Pulp Fiction", "Vincent Vega"],
 ["play", "Harvey Keitel", "Reservoir Dogs", "Mr White"],
 ["play", "Harvey Keitel", "Pulp Fiction", "Winston Wolf"],
 ["play", "Uma Thurman", "Pulp Fiction", "Mia"],
 ["play", "Quentin Tarantino", "Pulp Fiction", "Jimmie"],
 ["play", "Quentin Tarantino", "Reservoir Dogs", "Mr Brown"],
 ["play", "Sigourney Weaver", "Alien", "Ellen Ripley"],
 ["play", "Sigourney Weaver", "Aliens", "Ellen Ripley"],
 ["play", "Lance Henriksen", "Aliens", "Bishop"],
 ["play", "Tom Skerritt", "Alien", "Dallas"],
 ["play", "Harrison Ford", "Blade Runner", "Deckard"],
 ["play", "Rutger Hauer", "Blade Runner", "Roy Batty"],
 ["play", "Sean Young", "Blade Runner", "Rachael"],
 ["play", "Marlon Brando", "The Godfather", "Vito Corleone"],
 ["play", "Al Pacino", "The Godfather", "Michael Corleone"],
 ["play", "Robert Duvall", "The Godfather", "Tom Hagen"],
 ["play", "Morgan Freeman", "Seven", "William Somerset"],
 ["play", "Brad Pitt", "Seven", "David Mills"],
 ["play", "Robert De Niro", "The Godfather II", "Vito Corleone"],
 ["play", "Al Pacino", "The Godfather II", "Michael Corleone"],
 ["play", "Robert Duvall", "The Godfather II", "Tom Hagen"],
 ["play", "Susan Sarandon", "Thelma and Louise", "Louise"],
 ["play", "Geena Davis", "Thelma and Louise", "Thelma"],
 ["play", "Robert De Niro", "Cape Fear", "Max Cady"],
 ["play", "Matt Damon", "Good Will Hunting", "Will Hunting"],
 ["play", "Nick Nolte", "Cape Fear", "Sam Bowden"],
 ["play", "Jessica Lange", "Cape Fear", "Leigh Bowden"],
 ["play", "Juliette Lewis", "Cape Fear", "Danielle Bowden"],
 ["play", "Kevin Spacey", "Seven", "John Doe"],
 ["play", "Kevin Spacey", "The Usual Suspects", "Verbal Kint"],
 ["play", "Gabriel Byrne", "The Usual Suspects", "Dean Keaton"],
 ["play", "Rufus Sewell", "Dark City", "John Murdoch"],
 ["play", "William Hurt", "Dark City", "Frank Bumstead"],
 ["play", "Kiefer Sutherland", "Dark City", "Doctor Schreber"],
 ["play", "Bruce Willis", "Die Hard", "John McClane"],
 ["play", "Alan Rickman", "Die Hard", "Hans Gruber"],
 ["play", "Anthony Hopkins", "Silence of the Lambs", "Hannibal Lecter"],
 ["play", "Jodie Foster", "Silence of the Lambs", "Clarice Starling"],
 ["play", "Nicolas Cage", "Face/Off", "Castor Troy"],
 ["play", "John Travolta", "Face/Off", "Sean Archer"],
 ["play", "Ewan McGregor", "Trainspotting", "Mark Renton"],
 ["play", "Robert Carlyle", "Trainspotting", "Francis Begbie"],
 ["play", "Neve Campbell", "Scream", "Sidney Prescott"],
 ["play", "Courteney Cox", "Scream", "Gale Weathers"],
 ["play", "Skeet Ulrich", "Scream", "Billy Loomis"],
 ["play", "Nicolas Cage", "Leaving Las Vegas", "Ben Sanderson"],
 ["play", "Elisabeth Shue", "Leaving Las Vegas", "Sera"],
 ["play", "Frances McDormand", "Fargo", "Marge Gunderson"],
 ["play", "Steve Buscemi", "Fargo", "Carl Showalter"],
 ["play", "Tom Cruise", "Mission Impossible", "Ethan Hunt"],
 ["play", "Jon Voight", "Mission Impossible", "Jim Phelps"],
 ["play", "Emmanuelle Beart", "Mission Impossible", "Claire Phelps"],
 ["play", "Bruce Willis", "Twelve Monkeys", "James Cole"],
 ["play", "Madeleine Stowe", "Twelve Monkeys", "Kathryn Railly"],
 ["play", "Brad Pitt", "Twelve Monkeys", "Jeffrey Goines"],
 ["play", "Ethan Hawke", "Gattaca", "Vincent Freeman"],
 ["play", "Uma Thurman", "Gattaca", "Irene Cassini"],
 ["play", "Steve Buscemi", "Reservoir Dogs", "Mr Pink"],
 ["play", "Harrison Ford", "Raiders of the Lost Ark", "Indiana Jones"],
 ["play", "John Rhys-Davies", "Raiders of the Lost Ark", "Sallah"],
 ["play", "Karen Allen", "Raiders of the Lost Ark", "Marion Ravenwood"],
 ["play", "Harrison Ford", "The Fugitive", "Richard Kimble"],
 ["play", "Tommy Lee Jones", "The Fugitive", "Samuel Gerard"],
 ["play", "Jeroen Krabbe", "The Fugitive", "Charles Nichols"],
 ["play", "Tommy Lee Jones", "Men in Black", "Kay"],
 ["play", "Will Smith", "Men in Black", "Jay"],
 ["play", "Linda Fiorentino", "Men in Black", "Laurel"],
 ["play", "Kevin Costner", "The Untouchables", "Eliot Ness"],
 ["play", "Sean Connery", "The Untouchables", "Jim Malone"],
 ["play", "Andy Garcia", "The Untouchables", "George Stone"],
 ["play", "Robert De Niro", "The Untouchables", "Al Capone"],
 ["play", "Kate Winslet", "Titanic", "Rose DeWit Bukater"],
 ["play", "Christopher Lambert","Highlander", "Connor MacLeod"],
 ["play", "Sean Connery", "Highlander", "Ramirez"],
 ["play", "Marlon Brando", "Apocalypse Now", "Colonel Kurtz"],
 ["play", "Robert Duvall", "Apocalypse Now", "Lieutenant Kilgore"],
 ["play", "Martin Sheen", "Apocalypse Now", "Captain Willard"],
 ["play", "Michael Douglas", "Wall Street", "Gordon Gekko"],
 ["play", "Charlie Sheen", "Wall Street", "Bud Fox"],
 ["play", "Martin Sheen", "Wall Street", "Carl Fox"],
 ["play", "Charlie Sheen", "Platoon", "Chris"],
 ["play", "Forest Whitaker", "Platoon", "Big Haplayd"],
 ["play", "Tom Berenger", "Platoon", "Sgt. Barnes"]]
