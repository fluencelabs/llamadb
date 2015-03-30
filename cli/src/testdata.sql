-- Modified data from the Chinook Database
-- Original author: Luis Rocha
-- License: http://www.codeplex.com/ChinookDatabase/license

CREATE TABLE Album
(
    AlbumId U32,
    Title STRING,
    ArtistId U32
);

CREATE TABLE Artist
(
    ArtistId U32,
    Name STRING
);


CREATE TABLE Genre
(
    GenreId U32,
    Name STRING
);

CREATE TABLE MediaType
(
    MediaTypeId U32,
    Name STRING
);

CREATE TABLE Track
(
    TrackId U32,
    Name STRING,
    AlbumId U32,
    MediaTypeId U32,
    GenreId U32,
    Composer STRING,    -- todo: make nullable
    Milliseconds U32,
    Bytes U32,
    UnitPrice F64
);

INSERT INTO Genre (GenreId, Name) VALUES (1, 'Rock'),
(2, 'Jazz'),
(3, 'Metal'),
(4, 'Alternative & Punk'),
(5, 'Rock And Roll'),
(6, 'Blues'),
(7, 'Latin'),
(8, 'Reggae'),
(9, 'Pop'),
(10, 'Soundtrack'),
(11, 'Bossa Nova'),
(12, 'Easy Listening'),
(13, 'Heavy Metal'),
(14, 'R&B/Soul'),
(15, 'Electronica/Dance'),
(16, 'World'),
(17, 'Hip Hop/Rap'),
(18, 'Science Fiction'),
(19, 'TV Shows'),
(20, 'Sci Fi & Fantasy'),
(21, 'Drama'),
(22, 'Comedy'),
(23, 'Alternative'),
(24, 'Classical'),
(25, 'Opera');

INSERT INTO Artist (ArtistId, Name) VALUES (1, 'AC/DC'),
(2, 'Accept'),
(3, 'Aerosmith'),
(4, 'Alanis Morissette'),
(5, 'Alice In Chains'),
(6, 'Antônio Carlos Jobim'),
(7, 'Apocalyptica'),
(8, 'Audioslave'),
(9, 'BackBeat'),
(10, 'Billy Cobham'),
(11, 'Black Label Society'),
(12, 'Black Sabbath'),
(13, 'Body Count'),
(14, 'Bruce Dickinson'),
(15, 'Buddy Guy'),
(16, 'Caetano Veloso'),
(17, 'Chico Buarque'),
(18, 'Chico Science & Nação Zumbi'),
(19, 'Cidade Negra'),
(20, 'Cláudio Zoli'),
(21, 'Various Artists'),
(22, 'Led Zeppelin'),
(23, 'Frank Zappa & Captain Beefheart'),
(24, 'Marcos Valle'),
(25, 'Milton Nascimento & Bebeto'),
(26, 'Azymuth'),
(27, 'Gilberto Gil'),
(28, 'João Gilberto'),
(29, 'Bebel Gilberto'),
(30, 'Jorge Vercilo'),
(31, 'Baby Consuelo'),
(32, 'Ney Matogrosso'),
(33, 'Luiz Melodia'),
(34, 'Nando Reis'),
(35, 'Pedro Luís & A Parede'),
(36, 'O Rappa'),
(37, 'Ed Motta'),
(38, 'Banda Black Rio'),
(39, 'Fernanda Porto'),
(40, 'Os Cariocas'),
(41, 'Elis Regina'),
(42, 'Milton Nascimento'),
(43, 'A Cor Do Som'),
(44, 'Kid Abelha'),
(45, 'Sandra De Sá'),
(46, 'Jorge Ben'),
(47, 'Hermeto Pascoal'),
(48, 'Barão Vermelho'),
(49, 'Edson, DJ Marky & DJ Patife Featuring Fernanda Porto'),
(50, 'Metallica'),
(51, 'Queen'),
(52, 'Kiss'),
(53, 'Spyro Gyra'),
(54, 'Green Day'),
(55, 'David Coverdale'),
(56, 'Gonzaguinha'),
(57, 'Os Mutantes'),
(58, 'Deep Purple'),
(59, 'Santana'),
(60, 'Santana Feat. Dave Matthews'),
(61, 'Santana Feat. Everlast'),
(62, 'Santana Feat. Rob Thomas'),
(63, 'Santana Feat. Lauryn Hill & Cee-Lo'),
(64, 'Santana Feat. The Project G&B'),
(65, 'Santana Feat. Maná'),
(66, 'Santana Feat. Eagle-Eye Cherry'),
(67, 'Santana Feat. Eric Clapton'),
(68, 'Miles Davis'),
(69, 'Gene Krupa'),
(70, 'Toquinho & Vinícius'),
(71, 'Vinícius De Moraes & Baden Powell'),
(72, 'Vinícius De Moraes'),
(73, 'Vinícius E Qurteto Em Cy'),
(74, 'Vinícius E Odette Lara'),
(75, 'Vinicius, Toquinho & Quarteto Em Cy'),
(76, 'Creedence Clearwater Revival'),
(77, 'Cássia Eller'),
(78, 'Def Leppard'),
(79, 'Dennis Chambers'),
(80, 'Djavan'),
(81, 'Eric Clapton'),
(82, 'Faith No More'),
(83, 'Falamansa'),
(84, 'Foo Fighters'),
(85, 'Frank Sinatra'),
(86, 'Funk Como Le Gusta'),
(87, 'Godsmack'),
(88, 'Guns N\' Roses'),
(89, 'Incognito'),
(90, 'Iron Maiden'),
(91, 'James Brown'),
(92, 'Jamiroquai'),
(93, 'JET'),
(94, 'Jimi Hendrix'),
(95, 'Joe Satriani'),
(96, 'Jota Quest'),
(97, 'João Suplicy'),
(98, 'Judas Priest'),
(99, 'Legião Urbana'),
(100, 'Lenny Kravitz'),
(101, 'Lulu Santos'),
(102, 'Marillion'),
(103, 'Marisa Monte'),
(104, 'Marvin Gaye'),
(105, 'Men At Work'),
(106, 'Motörhead'),
(107, 'Motörhead & Girlschool'),
(108, 'Mônica Marianno'),
(109, 'Mötley Crüe'),
(110, 'Nirvana'),
(111, 'O Terço'),
(112, 'Olodum'),
(113, 'Os Paralamas Do Sucesso'),
(114, 'Ozzy Osbourne'),
(115, 'Page & Plant'),
(116, 'Passengers'),
(117, 'Paul D\'Ianno'),
(118, 'Pearl Jam'),
(119, 'Peter Tosh'),
(120, 'Pink Floyd'),
(121, 'Planet Hemp'),
(122, 'R.E.M. Feat. Kate Pearson'),
(123, 'R.E.M. Feat. KRS-One'),
(124, 'R.E.M.'),
(125, 'Raimundos'),
(126, 'Raul Seixas'),
(127, 'Red Hot Chili Peppers'),
(128, 'Rush'),
(129, 'Simply Red'),
(130, 'Skank'),
(131, 'Smashing Pumpkins'),
(132, 'Soundgarden'),
(133, 'Stevie Ray Vaughan & Double Trouble'),
(134, 'Stone Temple Pilots'),
(135, 'System Of A Down'),
(136, 'Terry Bozzio, Tony Levin & Steve Stevens'),
(137, 'The Black Crowes'),
(138, 'The Clash'),
(139, 'The Cult'),
(140, 'The Doors'),
(141, 'The Police'),
(142, 'The Rolling Stones'),
(143, 'The Tea Party'),
(144, 'The Who'),
(145, 'Tim Maia'),
(146, 'Titãs'),
(147, 'Battlestar Galactica'),
(148, 'Heroes'),
(149, 'Lost'),
(150, 'U2'),
(151, 'UB40'),
(152, 'Van Halen'),
(153, 'Velvet Revolver'),
(154, 'Whitesnake'),
(155, 'Zeca Pagodinho'),
(156, 'The Office'),
(157, 'Dread Zeppelin'),
(158, 'Battlestar Galactica (Classic)'),
(159, 'Aquaman'),
(160, 'Christina Aguilera featuring BigElf'),
(161, 'Aerosmith & Sierra Leone\'s Refugee Allstars'),
(162, 'Los Lonely Boys'),
(163, 'Corinne Bailey Rae'),
(164, 'Dhani Harrison & Jakob Dylan'),
(165, 'Jackson Browne'),
(166, 'Avril Lavigne'),
(167, 'Big & Rich'),
(168, 'Youssou N\'Dour'),
(169, 'Black Eyed Peas'),
(170, 'Jack Johnson'),
(171, 'Ben Harper'),
(172, 'Snow Patrol'),
(173, 'Matisyahu'),
(174, 'The Postal Service'),
(175, 'Jaguares'),
(176, 'The Flaming Lips'),
(177, 'Jack\'s Mannequin & Mick Fleetwood'),
(178, 'Regina Spektor'),
(179, 'Scorpions'),
(180, 'House Of Pain'),
(181, 'Xis'),
(182, 'Nega Gizza'),
(183, 'Gustavo & Andres Veiga & Salazar'),
(184, 'Rodox'),
(185, 'Charlie Brown Jr.'),
(186, 'Pedro Luís E A Parede'),
(187, 'Los Hermanos'),
(188, 'Mundo Livre S/A'),
(189, 'Otto'),
(190, 'Instituto'),
(191, 'Nação Zumbi'),
(192, 'DJ Dolores & Orchestra Santa Massa'),
(193, 'Seu Jorge'),
(194, 'Sabotage E Instituto'),
(195, 'Stereo Maracana'),
(196, 'Cake'),
(197, 'Aisha Duo'),
(198, 'Habib Koité and Bamada'),
(199, 'Karsh Kale'),
(200, 'The Posies'),
(201, 'Luciana Souza/Romero Lubambo'),
(202, 'Aaron Goldberg'),
(203, 'Nicolaus Esterhazy Sinfonia'),
(204, 'Temple of the Dog'),
(205, 'Chris Cornell'),
(206, 'Alberto Turco & Nova Schola Gregoriana'),
(207, 'Richard Marlow & The Choir of Trinity College, Cambridge'),
(208, 'English Concert & Trevor Pinnock'),
(209, 'Anne-Sophie Mutter, Herbert Von Karajan & Wiener Philharmoniker'),
(210, 'Hilary Hahn, Jeffrey Kahane, Los Angeles Chamber Orchestra & Margaret Batjer'),
(211, 'Wilhelm Kempff'),
(212, 'Yo-Yo Ma'),
(213, 'Scholars Baroque Ensemble'),
(214, 'Academy of St. Martin in the Fields & Sir Neville Marriner'),
(215, 'Academy of St. Martin in the Fields Chamber Ensemble & Sir Neville Marriner'),
(216, 'Berliner Philharmoniker, Claudio Abbado & Sabine Meyer'),
(217, 'Royal Philharmonic Orchestra & Sir Thomas Beecham'),
(218, 'Orchestre Révolutionnaire et Romantique & John Eliot Gardiner'),
(219, 'Britten Sinfonia, Ivor Bolton & Lesley Garrett'),
(220, 'Chicago Symphony Chorus, Chicago Symphony Orchestra & Sir Georg Solti'),
(221, 'Sir Georg Solti & Wiener Philharmoniker'),
(222, 'Academy of St. Martin in the Fields, John Birch, Sir Neville Marriner & Sylvia McNair'),
(223, 'London Symphony Orchestra & Sir Charles Mackerras'),
(224, 'Barry Wordsworth & BBC Concert Orchestra'),
(225, 'Herbert Von Karajan, Mirella Freni & Wiener Philharmoniker'),
(226, 'Eugene Ormandy'),
(227, 'Luciano Pavarotti'),
(228, 'Leonard Bernstein & New York Philharmonic'),
(229, 'Boston Symphony Orchestra & Seiji Ozawa'),
(230, 'Aaron Copland & London Symphony Orchestra'),
(231, 'Ton Koopman'),
(232, 'Sergei Prokofiev & Yuri Temirkanov'),
(233, 'Chicago Symphony Orchestra & Fritz Reiner'),
(234, 'Orchestra of The Age of Enlightenment'),
(235, 'Emanuel Ax, Eugene Ormandy & Philadelphia Orchestra'),
(236, 'James Levine'),
(237, 'Berliner Philharmoniker & Hans Rosbaud'),
(238, 'Maurizio Pollini'),
(239, 'Academy of St. Martin in the Fields, Sir Neville Marriner & William Bennett'),
(240, 'Gustav Mahler'),
(241, 'Felix Schmidt, London Symphony Orchestra & Rafael Frühbeck de Burgos'),
(242, 'Edo de Waart & San Francisco Symphony'),
(243, 'Antal Doráti & London Symphony Orchestra'),
(244, 'Choir Of Westminster Abbey & Simon Preston'),
(245, 'Michael Tilson Thomas & San Francisco Symphony'),
(246, 'Chor der Wiener Staatsoper, Herbert Von Karajan & Wiener Philharmoniker'),
(247, 'The King\'s Singers'),
(248, 'Berliner Philharmoniker & Herbert Von Karajan'),
(249, 'Sir Georg Solti, Sumi Jo & Wiener Philharmoniker'),
(250, 'Christopher O\'Riley'),
(251, 'Fretwork'),
(252, 'Amy Winehouse'),
(253, 'Calexico'),
(254, 'Otto Klemperer & Philharmonia Orchestra'),
(255, 'Yehudi Menuhin'),
(256, 'Philharmonia Orchestra & Sir Neville Marriner'),
(257, 'Academy of St. Martin in the Fields, Sir Neville Marriner & Thurston Dart'),
(258, 'Les Arts Florissants & William Christie'),
(259, 'The 12 Cellists of The Berlin Philharmonic'),
(260, 'Adrian Leaper & Doreen de Feis'),
(261, 'Roger Norrington, London Classical Players'),
(262, 'Charles Dutoit & L\'Orchestre Symphonique de Montréal'),
(263, 'Equale Brass Ensemble, John Eliot Gardiner & Munich Monteverdi Orchestra and Choir'),
(264, 'Kent Nagano and Orchestre de l\'Opéra de Lyon'),
(265, 'Julian Bream'),
(266, 'Martin Roscoe'),
(267, 'Göteborgs Symfoniker & Neeme Järvi'),
(268, 'Itzhak Perlman'),
(269, 'Michele Campanella'),
(270, 'Gerald Moore'),
(271, 'Mela Tenenbaum, Pro Musica Prague & Richard Kapp'),
(272, 'Emerson String Quartet'),
(273, 'C. Monteverdi, Nigel Rogers - Chiaroscuro; London Baroque; London Cornett & Sackbu'),
(274, 'Nash Ensemble'),
(275, 'Philip Glass Ensemble');

INSERT INTO Album (AlbumId, Title, ArtistId) VALUES (1, 'For Those About To Rock We Salute You', 1),
(2, 'Balls to the Wall', 2),
(3, 'Restless and Wild', 2),
(4, 'Let There Be Rock', 1),
(5, 'Big Ones', 3),
(6, 'Jagged Little Pill', 4),
(7, 'Facelift', 5),
(8, 'Warner 25 Anos', 6),
(9, 'Plays Metallica By Four Cellos', 7),
(10, 'Audioslave', 8),
(11, 'Out Of Exile', 8),
(12, 'BackBeat Soundtrack', 9),
(13, 'The Best Of Billy Cobham', 10),
(14, 'Alcohol Fueled Brewtality Live! [Disc 1]', 11),
(15, 'Alcohol Fueled Brewtality Live! [Disc 2]', 11),
(16, 'Black Sabbath', 12),
(17, 'Black Sabbath Vol. 4 (Remaster)', 12),
(18, 'Body Count', 13),
(19, 'Chemical Wedding', 14),
(20, 'The Best Of Buddy Guy - The Millenium Collection', 15),
(21, 'Prenda Minha', 16),
(22, 'Sozinho Remix Ao Vivo', 16),
(23, 'Minha Historia', 17),
(24, 'Afrociberdelia', 18),
(25, 'Da Lama Ao Caos', 18),
(26, 'Acústico MTV [Live]', 19),
(27, 'Cidade Negra - Hits', 19),
(28, 'Na Pista', 20),
(29, 'Axé Bahia 2001', 21),
(30, 'BBC Sessions [Disc 1] [Live]', 22),
(31, 'Bongo Fury', 23),
(32, 'Carnaval 2001', 21),
(33, 'Chill: Brazil (Disc 1)', 24),
(34, 'Chill: Brazil (Disc 2)', 6),
(35, 'Garage Inc. (Disc 1)', 50),
(36, 'Greatest Hits II', 51),
(37, 'Greatest Kiss', 52),
(38, 'Heart of the Night', 53),
(39, 'International Superhits', 54),
(40, 'Into The Light', 55),
(41, 'Meus Momentos', 56),
(42, 'Minha História', 57),
(43, 'MK III The Final Concerts [Disc 1]', 58),
(44, 'Physical Graffiti [Disc 1]', 22),
(45, 'Sambas De Enredo 2001', 21),
(46, 'Supernatural', 59),
(47, 'The Best of Ed Motta', 37),
(48, 'The Essential Miles Davis [Disc 1]', 68),
(49, 'The Essential Miles Davis [Disc 2]', 68),
(50, 'The Final Concerts (Disc 2)', 58),
(51, 'Up An\' Atom', 69),
(52, 'Vinícius De Moraes - Sem Limite', 70),
(53, 'Vozes do MPB', 21),
(54, 'Chronicle, Vol. 1', 76),
(55, 'Chronicle, Vol. 2', 76),
(56, 'Cássia Eller - Coleção Sem Limite [Disc 2]', 77),
(57, 'Cássia Eller - Sem Limite [Disc 1]', 77),
(58, 'Come Taste The Band', 58),
(59, 'Deep Purple In Rock', 58),
(60, 'Fireball', 58),
(61, 'Knocking at Your Back Door: The Best Of Deep Purple in the 80\'s', 58),
(62, 'Machine Head', 58),
(63, 'Purpendicular', 58),
(64, 'Slaves And Masters', 58),
(65, 'Stormbringer', 58),
(66, 'The Battle Rages On', 58),
(67, 'Vault: Def Leppard\'s Greatest Hits', 78),
(68, 'Outbreak', 79),
(69, 'Djavan Ao Vivo - Vol. 02', 80),
(70, 'Djavan Ao Vivo - Vol. 1', 80),
(71, 'Elis Regina-Minha História', 41),
(72, 'The Cream Of Clapton', 81),
(73, 'Unplugged', 81),
(74, 'Album Of The Year', 82),
(75, 'Angel Dust', 82),
(76, 'King For A Day Fool For A Lifetime', 82),
(77, 'The Real Thing', 82),
(78, 'Deixa Entrar', 83),
(79, 'In Your Honor [Disc 1]', 84),
(80, 'In Your Honor [Disc 2]', 84),
(81, 'One By One', 84),
(82, 'The Colour And The Shape', 84),
(83, 'My Way: The Best Of Frank Sinatra [Disc 1]', 85),
(84, 'Roda De Funk', 86),
(85, 'As Canções de Eu Tu Eles', 27),
(86, 'Quanta Gente Veio Ver (Live)', 27),
(87, 'Quanta Gente Veio ver--Bônus De Carnaval', 27),
(88, 'Faceless', 87),
(89, 'American Idiot', 54),
(90, 'Appetite for Destruction', 88),
(91, 'Use Your Illusion I', 88),
(92, 'Use Your Illusion II', 88),
(93, 'Blue Moods', 89),
(94, 'A Matter of Life and Death', 90),
(95, 'A Real Dead One', 90),
(96, 'A Real Live One', 90),
(97, 'Brave New World', 90),
(98, 'Dance Of Death', 90),
(99, 'Fear Of The Dark', 90),
(100, 'Iron Maiden', 90),
(101, 'Killers', 90),
(102, 'Live After Death', 90),
(103, 'Live At Donington 1992 (Disc 1)', 90),
(104, 'Live At Donington 1992 (Disc 2)', 90),
(105, 'No Prayer For The Dying', 90),
(106, 'Piece Of Mind', 90),
(107, 'Powerslave', 90),
(108, 'Rock In Rio [CD1]', 90),
(109, 'Rock In Rio [CD2]', 90),
(110, 'Seventh Son of a Seventh Son', 90),
(111, 'Somewhere in Time', 90),
(112, 'The Number of The Beast', 90),
(113, 'The X Factor', 90),
(114, 'Virtual XI', 90),
(115, 'Sex Machine', 91),
(116, 'Emergency On Planet Earth', 92),
(117, 'Synkronized', 92),
(118, 'The Return Of The Space Cowboy', 92),
(119, 'Get Born', 93),
(120, 'Are You Experienced?', 94),
(121, 'Surfing with the Alien (Remastered)', 95),
(122, 'Jorge Ben Jor 25 Anos', 46),
(123, 'Jota Quest-1995', 96),
(124, 'Cafezinho', 97),
(125, 'Living After Midnight', 98),
(126, 'Unplugged [Live]', 52),
(127, 'BBC Sessions [Disc 2] [Live]', 22),
(128, 'Coda', 22),
(129, 'Houses Of The Holy', 22),
(130, 'In Through The Out Door', 22),
(131, 'IV', 22),
(132, 'Led Zeppelin I', 22),
(133, 'Led Zeppelin II', 22),
(134, 'Led Zeppelin III', 22),
(135, 'Physical Graffiti [Disc 2]', 22),
(136, 'Presence', 22),
(137, 'The Song Remains The Same (Disc 1)', 22),
(138, 'The Song Remains The Same (Disc 2)', 22),
(139, 'A TempestadeTempestade Ou O Livro Dos Dias', 99),
(140, 'Mais Do Mesmo', 99),
(141, 'Greatest Hits', 100),
(142, 'Lulu Santos - RCA 100 Anos De Música - Álbum 01', 101),
(143, 'Lulu Santos - RCA 100 Anos De Música - Álbum 02', 101),
(144, 'Misplaced Childhood', 102),
(145, 'Barulhinho Bom', 103),
(146, 'Seek And Shall Find: More Of The Best (1963-1981)', 104),
(147, 'The Best Of Men At Work', 105),
(148, 'Black Album', 50),
(149, 'Garage Inc. (Disc 2)', 50),
(150, 'Kill \'Em All', 50),
(151, 'Load', 50),
(152, 'Master Of Puppets', 50),
(153, 'ReLoad', 50),
(154, 'Ride The Lightning', 50),
(155, 'St. Anger', 50),
(156, '...And Justice For All', 50),
(157, 'Miles Ahead', 68),
(158, 'Milton Nascimento Ao Vivo', 42),
(159, 'Minas', 42),
(160, 'Ace Of Spades', 106),
(161, 'Demorou...', 108),
(162, 'Motley Crue Greatest Hits', 109),
(163, 'From The Muddy Banks Of The Wishkah [Live]', 110),
(164, 'Nevermind', 110),
(165, 'Compositores', 111),
(166, 'Olodum', 112),
(167, 'Acústico MTV', 113),
(168, 'Arquivo II', 113),
(169, 'Arquivo Os Paralamas Do Sucesso', 113),
(170, 'Bark at the Moon (Remastered)', 114),
(171, 'Blizzard of Ozz', 114),
(172, 'Diary of a Madman (Remastered)', 114),
(173, 'No More Tears (Remastered)', 114),
(174, 'Tribute', 114),
(175, 'Walking Into Clarksdale', 115),
(176, 'Original Soundtracks 1', 116),
(177, 'The Beast Live', 117),
(178, 'Live On Two Legs [Live]', 118),
(179, 'Pearl Jam', 118),
(180, 'Riot Act', 118),
(181, 'Ten', 118),
(182, 'Vs.', 118),
(183, 'Dark Side Of The Moon', 120),
(184, 'Os Cães Ladram Mas A Caravana Não Pára', 121),
(185, 'Greatest Hits I', 51),
(186, 'News Of The World', 51),
(187, 'Out Of Time', 122),
(188, 'Green', 124),
(189, 'New Adventures In Hi-Fi', 124),
(190, 'The Best Of R.E.M.: The IRS Years', 124),
(191, 'Cesta Básica', 125),
(192, 'Raul Seixas', 126),
(193, 'Blood Sugar Sex Magik', 127),
(194, 'By The Way', 127),
(195, 'Californication', 127),
(196, 'Retrospective I (1974-1980)', 128),
(197, 'Santana - As Years Go By', 59),
(198, 'Santana Live', 59),
(199, 'Maquinarama', 130),
(200, 'O Samba Poconé', 130),
(201, 'Judas 0: B-Sides and Rarities', 131),
(202, 'Rotten Apples: Greatest Hits', 131),
(203, 'A-Sides', 132),
(204, 'Morning Dance', 53),
(205, 'In Step', 133),
(206, 'Core', 134),
(207, 'Mezmerize', 135),
(208, '[1997] Black Light Syndrome', 136),
(209, 'Live [Disc 1]', 137),
(210, 'Live [Disc 2]', 137),
(211, 'The Singles', 138),
(212, 'Beyond Good And Evil', 139),
(213, 'Pure Cult: The Best Of The Cult (For Rockers, Ravers, Lovers & Sinners) [UK]', 139),
(214, 'The Doors', 140),
(215, 'The Police Greatest Hits', 141),
(216, 'Hot Rocks, 1964-1971 (Disc 1)', 142),
(217, 'No Security', 142),
(218, 'Voodoo Lounge', 142),
(219, 'Tangents', 143),
(220, 'Transmission', 143),
(221, 'My Generation - The Very Best Of The Who', 144),
(222, 'Serie Sem Limite (Disc 1)', 145),
(223, 'Serie Sem Limite (Disc 2)', 145),
(224, 'Acústico', 146),
(225, 'Volume Dois', 146),
(226, 'Battlestar Galactica: The Story So Far', 147),
(227, 'Battlestar Galactica, Season 3', 147),
(228, 'Heroes, Season 1', 148),
(229, 'Lost, Season 3', 149),
(230, 'Lost, Season 1', 149),
(231, 'Lost, Season 2', 149),
(232, 'Achtung Baby', 150),
(233, 'All That You Can\'t Leave Behind', 150),
(234, 'B-Sides 1980-1990', 150),
(235, 'How To Dismantle An Atomic Bomb', 150),
(236, 'Pop', 150),
(237, 'Rattle And Hum', 150),
(238, 'The Best Of 1980-1990', 150),
(239, 'War', 150),
(240, 'Zooropa', 150),
(241, 'UB40 The Best Of - Volume Two [UK]', 151),
(242, 'Diver Down', 152),
(243, 'The Best Of Van Halen, Vol. I', 152),
(244, 'Van Halen', 152),
(245, 'Van Halen III', 152),
(246, 'Contraband', 153),
(247, 'Vinicius De Moraes', 72),
(248, 'Ao Vivo [IMPORT]', 155),
(249, 'The Office, Season 1', 156),
(250, 'The Office, Season 2', 156),
(251, 'The Office, Season 3', 156),
(252, 'Un-Led-Ed', 157),
(253, 'Battlestar Galactica (Classic), Season 1', 158),
(254, 'Aquaman', 159),
(255, 'Instant Karma: The Amnesty International Campaign to Save Darfur', 150),
(256, 'Speak of the Devil', 114),
(257, '20th Century Masters - The Millennium Collection: The Best of Scorpions', 179),
(258, 'House of Pain', 180),
(259, 'Radio Brasil (O Som da Jovem Vanguarda) - Seleccao de Henrique Amaro', 36),
(260, 'Cake: B-Sides and Rarities', 196),
(261, 'LOST, Season 4', 149),
(262, 'Quiet Songs', 197),
(263, 'Muso Ko', 198),
(264, 'Realize', 199),
(265, 'Every Kind of Light', 200),
(266, 'Duos II', 201),
(267, 'Worlds', 202),
(268, 'The Best of Beethoven', 203),
(269, 'Temple of the Dog', 204),
(270, 'Carry On', 205),
(271, 'Revelations', 8),
(272, 'Adorate Deum: Gregorian Chant from the Proper of the Mass', 206),
(273, 'Allegri: Miserere', 207),
(274, 'Pachelbel: Canon & Gigue', 208),
(275, 'Vivaldi: The Four Seasons', 209),
(276, 'Bach: Violin Concertos', 210),
(277, 'Bach: Goldberg Variations', 211),
(278, 'Bach: The Cello Suites', 212),
(279, 'Handel: The Messiah (Highlights)', 213),
(280, 'The World of Classical Favourites', 214),
(281, 'Sir Neville Marriner: A Celebration', 215),
(282, 'Mozart: Wind Concertos', 216),
(283, 'Haydn: Symphonies 99 - 104', 217),
(284, 'Beethoven: Symhonies Nos. 5 & 6', 218),
(285, 'A Soprano Inspired', 219),
(286, 'Great Opera Choruses', 220),
(287, 'Wagner: Favourite Overtures', 221),
(288, 'Fauré: Requiem, Ravel: Pavane & Others', 222),
(289, 'Tchaikovsky: The Nutcracker', 223),
(290, 'The Last Night of the Proms', 224),
(291, 'Puccini: Madama Butterfly - Highlights', 225),
(292, 'Holst: The Planets, Op. 32 & Vaughan Williams: Fantasies', 226),
(293, 'Pavarotti\'s Opera Made Easy', 227),
(294, 'Great Performances - Barber\'s Adagio and Other Romantic Favorites for Strings', 228),
(295, 'Carmina Burana', 229),
(296, 'A Copland Celebration, Vol. I', 230),
(297, 'Bach: Toccata & Fugue in D Minor', 231),
(298, 'Prokofiev: Symphony No.1', 232),
(299, 'Scheherazade', 233),
(300, 'Bach: The Brandenburg Concertos', 234),
(301, 'Chopin: Piano Concertos Nos. 1 & 2', 235),
(302, 'Mascagni: Cavalleria Rusticana', 236),
(303, 'Sibelius: Finlandia', 237),
(304, 'Beethoven Piano Sonatas: Moonlight & Pastorale', 238),
(305, 'Great Recordings of the Century - Mahler: Das Lied von der Erde', 240),
(306, 'Elgar: Cello Concerto & Vaughan Williams: Fantasias', 241),
(307, 'Adams, John: The Chairman Dances', 242),
(308, 'Tchaikovsky: 1812 Festival Overture, Op.49, Capriccio Italien & Beethoven: Wellington\'s Victory', 243),
(309, 'Palestrina: Missa Papae Marcelli & Allegri: Miserere', 244),
(310, 'Prokofiev: Romeo & Juliet', 245),
(311, 'Strauss: Waltzes', 226),
(312, 'Berlioz: Symphonie Fantastique', 245),
(313, 'Bizet: Carmen Highlights', 246),
(314, 'English Renaissance', 247),
(315, 'Handel: Music for the Royal Fireworks (Original Version 1749)', 208),
(316, 'Grieg: Peer Gynt Suites & Sibelius: Pelléas et Mélisande', 248),
(317, 'Mozart Gala: Famous Arias', 249),
(318, 'SCRIABIN: Vers la flamme', 250),
(319, 'Armada: Music from the Courts of England and Spain', 251),
(320, 'Mozart: Symphonies Nos. 40 & 41', 248),
(321, 'Back to Black', 252),
(322, 'Frank', 252),
(323, 'Carried to Dust (Bonus Track Version)', 253),
(324, 'Beethoven: Symphony No. 6 \'Pastoral\' Etc.', 254),
(325, 'Bartok: Violin & Viola Concertos', 255),
(326, 'Mendelssohn: A Midsummer Night\'s Dream', 256),
(327, 'Bach: Orchestral Suites Nos. 1 - 4', 257),
(328, 'Charpentier: Divertissements, Airs & Concerts', 258),
(329, 'South American Getaway', 259),
(330, 'Górecki: Symphony No. 3', 260),
(331, 'Purcell: The Fairy Queen', 261),
(332, 'The Ultimate Relexation Album', 262),
(333, 'Purcell: Music for the Queen Mary', 263),
(334, 'Weill: The Seven Deadly Sins', 264),
(335, 'J.S. Bach: Chaconne, Suite in E Minor, Partita in E Major & Prelude, Fugue and Allegro', 265),
(336, 'Prokofiev: Symphony No.5 & Stravinksy: Le Sacre Du Printemps', 248),
(337, 'Szymanowski: Piano Works, Vol. 1', 266),
(338, 'Nielsen: The Six Symphonies', 267),
(339, 'Great Recordings of the Century: Paganini\'s 24 Caprices', 268),
(340, 'Liszt - 12 Études D\'Execution Transcendante', 269),
(341, 'Great Recordings of the Century - Shubert: Schwanengesang, 4 Lieder', 270),
(342, 'Locatelli: Concertos for Violin, Strings and Continuo, Vol. 3', 271),
(343, 'Respighi:Pines of Rome', 226),
(344, 'Schubert: The Late String Quartets & String Quintet (3 CD\'s)', 272),
(345, 'Monteverdi: L\'Orfeo', 273),
(346, 'Mozart: Chamber Music', 274),
(347, 'Koyaanisqatsi (Soundtrack from the Motion Picture)', 275);

INSERT INTO MediaType (MediaTypeId, Name) VALUES (1, 'MPEG audio file');
INSERT INTO MediaType (MediaTypeId, Name) VALUES (2, 'Protected AAC audio file');
INSERT INTO MediaType (MediaTypeId, Name) VALUES (3, 'Protected MPEG-4 video file');
INSERT INTO MediaType (MediaTypeId, Name) VALUES (4, 'Purchased AAC audio file');
INSERT INTO MediaType (MediaTypeId, Name) VALUES (5, 'AAC audio file');

INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1, 'For Those About To Rock (We Salute You)', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 343719, 11170334, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2, 'Balls to the Wall', 2, 2, 1, 342562, 5510424, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3, 'Fast As a Shark', 3, 2, 1, 'F. Baltes, S. Kaufman, U. Dirkscneider & W. Hoffman', 230619, 3990994, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (4, 'Restless and Wild', 3, 2, 1, 'F. Baltes, R.A. Smith-Diesel, S. Kaufman, U. Dirkscneider & W. Hoffman', 252051, 4331779, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (5, 'Princess of the Dawn', 3, 2, 1, 'Deaffy & R.A. Smith-Diesel', 375418, 6290521, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (6, 'Put The Finger On You', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 205662, 6713451, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (7, 'Let\'s Get It Up', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 233926, 7636561, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (8, 'Inject The Venom', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 210834, 6852860, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (9, 'Snowballed', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 203102, 6599424, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (10, 'Evil Walks', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 263497, 8611245, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (11, 'C.O.D.', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 199836, 6566314, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (12, 'Breaking The Rules', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 263288, 8596840, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (13, 'Night Of The Long Knives', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 205688, 6706347, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (14, 'Spellbound', 1, 1, 1, 'Angus Young, Malcolm Young, Brian Johnson', 270863, 8817038, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (15, 'Go Down', 4, 1, 1, 'AC/DC', 331180, 10847611, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (16, 'Dog Eat Dog', 4, 1, 1, 'AC/DC', 215196, 7032162, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (17, 'Let There Be Rock', 4, 1, 1, 'AC/DC', 366654, 12021261, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (18, 'Bad Boy Boogie', 4, 1, 1, 'AC/DC', 267728, 8776140, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (19, 'Problem Child', 4, 1, 1, 'AC/DC', 325041, 10617116, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (20, 'Overdose', 4, 1, 1, 'AC/DC', 369319, 12066294, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (21, 'Hell Ain\'t A Bad Place To Be', 4, 1, 1, 'AC/DC', 254380, 8331286, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (22, 'Whole Lotta Rosie', 4, 1, 1, 'AC/DC', 323761, 10547154, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (23, 'Walk On Water', 5, 1, 1, 'Steven Tyler, Joe Perry, Jack Blades, Tommy Shaw', 295680, 9719579, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (24, 'Love In An Elevator', 5, 1, 1, 'Steven Tyler, Joe Perry', 321828, 10552051, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (25, 'Rag Doll', 5, 1, 1, 'Steven Tyler, Joe Perry, Jim Vallance, Holly Knight', 264698, 8675345, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (26, 'What It Takes', 5, 1, 1, 'Steven Tyler, Joe Perry, Desmond Child', 310622, 10144730, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (27, 'Dude (Looks Like A Lady)', 5, 1, 1, 'Steven Tyler, Joe Perry, Desmond Child', 264855, 8679940, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (28, 'Janie\'s Got A Gun', 5, 1, 1, 'Steven Tyler, Tom Hamilton', 330736, 10869391, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (29, 'Cryin\'', 5, 1, 1, 'Steven Tyler, Joe Perry, Taylor Rhodes', 309263, 10056995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (30, 'Amazing', 5, 1, 1, 'Steven Tyler, Richie Supa', 356519, 11616195, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (31, 'Blind Man', 5, 1, 1, 'Steven Tyler, Joe Perry, Taylor Rhodes', 240718, 7877453, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (32, 'Deuces Are Wild', 5, 1, 1, 'Steven Tyler, Jim Vallance', 215875, 7074167, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (33, 'The Other Side', 5, 1, 1, 'Steven Tyler, Jim Vallance', 244375, 7983270, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (34, 'Crazy', 5, 1, 1, 'Steven Tyler, Joe Perry, Desmond Child', 316656, 10402398, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (35, 'Eat The Rich', 5, 1, 1, 'Steven Tyler, Joe Perry, Jim Vallance', 251036, 8262039, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (36, 'Angel', 5, 1, 1, 'Steven Tyler, Desmond Child', 307617, 9989331, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (37, 'Livin\' On The Edge', 5, 1, 1, 'Steven Tyler, Joe Perry, Mark Hudson', 381231, 12374569, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (38, 'All I Really Want', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 284891, 9375567, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (39, 'You Oughta Know', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 249234, 8196916, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (40, 'Perfect', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 188133, 6145404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (41, 'Hand In My Pocket', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 221570, 7224246, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (42, 'Right Through You', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 176117, 5793082, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (43, 'Forgiven', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 300355, 9753256, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (44, 'You Learn', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 239699, 7824837, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (45, 'Head Over Feet', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 267493, 8758008, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (46, 'Mary Jane', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 280607, 9163588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (47, 'Ironic', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 229825, 7598866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (48, 'Not The Doctor', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 227631, 7604601, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (49, 'Wake Up', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 293485, 9703359, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (50, 'You Oughta Know (Alternate)', 6, 1, 1, 'Alanis Morissette & Glenn Ballard', 491885, 16008629, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (51, 'We Die Young', 7, 1, 1, 'Jerry Cantrell', 152084, 4925362, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (52, 'Man In The Box', 7, 1, 1, 'Jerry Cantrell, Layne Staley', 286641, 9310272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (53, 'Sea Of Sorrow', 7, 1, 1, 'Jerry Cantrell', 349831, 11316328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (54, 'Bleed The Freak', 7, 1, 1, 'Jerry Cantrell', 241946, 7847716, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (55, 'I Can\'t Remember', 7, 1, 1, 'Jerry Cantrell, Layne Staley', 222955, 7302550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (56, 'Love, Hate, Love', 7, 1, 1, 'Jerry Cantrell, Layne Staley', 387134, 12575396, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (57, 'It Ain\'t Like That', 7, 1, 1, 'Jerry Cantrell, Michael Starr, Sean Kinney', 277577, 8993793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (58, 'Sunshine', 7, 1, 1, 'Jerry Cantrell', 284969, 9216057, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (59, 'Put You Down', 7, 1, 1, 'Jerry Cantrell', 196231, 6420530, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (60, 'Confusion', 7, 1, 1, 'Jerry Cantrell, Michael Starr, Layne Staley', 344163, 11183647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (61, 'I Know Somethin (Bout You)', 7, 1, 1, 'Jerry Cantrell', 261955, 8497788, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (62, 'Real Thing', 7, 1, 1, 'Jerry Cantrell, Layne Staley', 243879, 7937731, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (63, 'Desafinado', 8, 1, 2, 185338, 5990473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (64, 'Garota De Ipanema', 8, 1, 2, 285048, 9348428, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (65, 'Samba De Uma Nota Só (One Note Samba)', 8, 1, 2, 137273, 4535401, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (66, 'Por Causa De Você', 8, 1, 2, 169900, 5536496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (67, 'Ligia', 8, 1, 2, 251977, 8226934, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (68, 'Fotografia', 8, 1, 2, 129227, 4198774, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (69, 'Dindi (Dindi)', 8, 1, 2, 253178, 8149148, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (70, 'Se Todos Fossem Iguais A Você (Instrumental)', 8, 1, 2, 134948, 4393377, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (71, 'Falando De Amor', 8, 1, 2, 219663, 7121735, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (72, 'Angela', 8, 1, 2, 169508, 5574957, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (73, 'Corcovado (Quiet Nights Of Quiet Stars)', 8, 1, 2, 205662, 6687994, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (74, 'Outra Vez', 8, 1, 2, 126511, 4110053, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (75, 'O Boto (Bôto)', 8, 1, 2, 366837, 12089673, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (76, 'Canta, Canta Mais', 8, 1, 2, 271856, 8719426, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (77, 'Enter Sandman', 9, 1, 3, 'Apocalyptica', 221701, 7286305, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (78, 'Master Of Puppets', 9, 1, 3, 'Apocalyptica', 436453, 14375310, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (79, 'Harvester Of Sorrow', 9, 1, 3, 'Apocalyptica', 374543, 12372536, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (80, 'The Unforgiven', 9, 1, 3, 'Apocalyptica', 322925, 10422447, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (81, 'Sad But True', 9, 1, 3, 'Apocalyptica', 288208, 9405526, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (82, 'Creeping Death', 9, 1, 3, 'Apocalyptica', 308035, 10110980, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (83, 'Wherever I May Roam', 9, 1, 3, 'Apocalyptica', 369345, 12033110, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (84, 'Welcome Home (Sanitarium)', 9, 1, 3, 'Apocalyptica', 350197, 11406431, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (85, 'Cochise', 10, 1, 1, 'Audioslave/Chris Cornell', 222380, 5339931, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (86, 'Show Me How to Live', 10, 1, 1, 'Audioslave/Chris Cornell', 277890, 6672176, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (87, 'Gasoline', 10, 1, 1, 'Audioslave/Chris Cornell', 279457, 6709793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (88, 'What You Are', 10, 1, 1, 'Audioslave/Chris Cornell', 249391, 5988186, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (89, 'Like a Stone', 10, 1, 1, 'Audioslave/Chris Cornell', 294034, 7059624, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (90, 'Set It Off', 10, 1, 1, 'Audioslave/Chris Cornell', 263262, 6321091, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (91, 'Shadow on the Sun', 10, 1, 1, 'Audioslave/Chris Cornell', 343457, 8245793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (92, 'I am the Highway', 10, 1, 1, 'Audioslave/Chris Cornell', 334942, 8041411, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (93, 'Exploder', 10, 1, 1, 'Audioslave/Chris Cornell', 206053, 4948095, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (94, 'Hypnotize', 10, 1, 1, 'Audioslave/Chris Cornell', 206628, 4961887, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (95, 'Bring\'em Back Alive', 10, 1, 1, 'Audioslave/Chris Cornell', 329534, 7911634, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (96, 'Light My Way', 10, 1, 1, 'Audioslave/Chris Cornell', 303595, 7289084, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (97, 'Getaway Car', 10, 1, 1, 'Audioslave/Chris Cornell', 299598, 7193162, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (98, 'The Last Remaining Light', 10, 1, 1, 'Audioslave/Chris Cornell', 317492, 7622615, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (99, 'Your Time Has Come', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 255529, 8273592, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (100, 'Out Of Exile', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 291291, 9506571, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (101, 'Be Yourself', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 279484, 9106160, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (102, 'Doesn\'t Remind Me', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 255869, 8357387, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (103, 'Drown Me Slowly', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 233691, 7609178, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (104, 'Heaven\'s Dead', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 276688, 9006158, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (105, 'The Worm', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 237714, 7710800, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (106, 'Man Or Animal', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 233195, 7542942, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (107, 'Yesterday To Tomorrow', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 273763, 8944205, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (108, 'Dandelion', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 278125, 9003592, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (109, '#1 Zero', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 299102, 9731988, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (110, 'The Curse', 11, 1, 4, 'Cornell, Commerford, Morello, Wilk', 309786, 10029406, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (111, 'Money', 12, 1, 5, 'Berry Gordy, Jr./Janie Bradford', 147591, 2365897, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (112, 'Long Tall Sally', 12, 1, 5, 'Enotris Johnson/Little Richard/Robert "Bumps" Blackwell', 106396, 1707084, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (113, 'Bad Boy', 12, 1, 5, 'Larry Williams', 116088, 1862126, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (114, 'Twist And Shout', 12, 1, 5, 'Bert Russell/Phil Medley', 161123, 2582553, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (115, 'Please Mr. Postman', 12, 1, 5, 'Brian Holland/Freddie Gorman/Georgia Dobbins/Robert Bateman/William Garrett', 137639, 2206986, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (116, 'C\'Mon Everybody', 12, 1, 5, 'Eddie Cochran/Jerry Capehart', 140199, 2247846, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (117, 'Rock \'N\' Roll Music', 12, 1, 5, 'Chuck Berry', 141923, 2276788, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (118, 'Slow Down', 12, 1, 5, 'Larry Williams', 163265, 2616981, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (119, 'Roadrunner', 12, 1, 5, 'Bo Diddley', 143595, 2301989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (120, 'Carol', 12, 1, 5, 'Chuck Berry', 143830, 2306019, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (121, 'Good Golly Miss Molly', 12, 1, 5, 'Little Richard', 106266, 1704918, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (122, '20 Flight Rock', 12, 1, 5, 'Ned Fairchild', 107807, 1299960, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (123, 'Quadrant', 13, 1, 2, 'Billy Cobham', 261851, 8538199, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (124, 'Snoopy\'s search-Red baron', 13, 1, 2, 'Billy Cobham', 456071, 15075616, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (125, 'Spanish moss-"A sound portrait"-Spanish moss', 13, 1, 2, 'Billy Cobham', 248084, 8217867, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (126, 'Moon germs', 13, 1, 2, 'Billy Cobham', 294060, 9714812, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (127, 'Stratus', 13, 1, 2, 'Billy Cobham', 582086, 19115680, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (128, 'The pleasant pheasant', 13, 1, 2, 'Billy Cobham', 318066, 10630578, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (129, 'Solo-Panhandler', 13, 1, 2, 'Billy Cobham', 246151, 8230661, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (130, 'Do what cha wanna', 13, 1, 2, 'George Duke', 274155, 9018565, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (131, 'Intro/ Low Down', 14, 1, 3, 323683, 10642901, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (132, '13 Years Of Grief', 14, 1, 3, 246987, 8137421, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (133, 'Stronger Than Death', 14, 1, 3, 300747, 9869647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (134, 'All For You', 14, 1, 3, 235833, 7726948, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (135, 'Super Terrorizer', 14, 1, 3, 319373, 10513905, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (136, 'Phoney Smile Fake Hellos', 14, 1, 3, 273606, 9011701, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (137, 'Lost My Better Half', 14, 1, 3, 284081, 9355309, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (138, 'Bored To Tears', 14, 1, 3, 247327, 8130090, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (139, 'A.N.D.R.O.T.A.Z.', 14, 1, 3, 266266, 8574746, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (140, 'Born To Booze', 14, 1, 3, 282122, 9257358, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (141, 'World Of Trouble', 14, 1, 3, 359157, 11820932, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (142, 'No More Tears', 14, 1, 3, 555075, 18041629, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (143, 'The Begining... At Last', 14, 1, 3, 365662, 11965109, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (144, 'Heart Of Gold', 15, 1, 3, 194873, 6417460, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (145, 'Snowblind', 15, 1, 3, 420022, 13842549, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (146, 'Like A Bird', 15, 1, 3, 276532, 9115657, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (147, 'Blood In The Wall', 15, 1, 3, 284368, 9359475, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (148, 'The Beginning...At Last', 15, 1, 3, 271960, 8975814, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (149, 'Black Sabbath', 16, 1, 3, 382066, 12440200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (150, 'The Wizard', 16, 1, 3, 264829, 8646737, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (151, 'Behind The Wall Of Sleep', 16, 1, 3, 217573, 7169049, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (152, 'N.I.B.', 16, 1, 3, 368770, 12029390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (153, 'Evil Woman', 16, 1, 3, 204930, 6655170, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (154, 'Sleeping Village', 16, 1, 3, 644571, 21128525, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (155, 'Warning', 16, 1, 3, 212062, 6893363, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (156, 'Wheels Of Confusion / The Straightener', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 494524, 16065830, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (157, 'Tomorrow\'s Dream', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 192496, 6252071, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (158, 'Changes', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 286275, 9175517, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (159, 'FX', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 103157, 3331776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (160, 'Supernaut', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 285779, 9245971, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (161, 'Snowblind', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 331676, 10813386, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (162, 'Cornucopia', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 234814, 7653880, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (163, 'Laguna Sunrise', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 173087, 5671374, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (164, 'St. Vitus Dance', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 149655, 4884969, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (165, 'Under The Sun/Every Day Comes and Goes', 17, 1, 3, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 350458, 11360486, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (166, 'Smoked Pork', 18, 1, 4, 47333, 1549074, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (167, 'Body Count\'s In The House', 18, 1, 4, 204251, 6715413, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (168, 'Now Sports', 18, 1, 4, 4884, 161266, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (169, 'Body Count', 18, 1, 4, 317936, 10489139, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (170, 'A Statistic', 18, 1, 4, 6373, 211997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (171, 'Bowels Of The Devil', 18, 1, 4, 223216, 7324125, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (172, 'The Real Problem', 18, 1, 4, 11650, 387360, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (173, 'KKK Bitch', 18, 1, 4, 173008, 5709631, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (174, 'D Note', 18, 1, 4, 95738, 3067064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (175, 'Voodoo', 18, 1, 4, 300721, 9875962, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (176, 'The Winner Loses', 18, 1, 4, 392254, 12843821, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (177, 'There Goes The Neighborhood', 18, 1, 4, 350171, 11443471, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (178, 'Oprah', 18, 1, 4, 6635, 224313, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (179, 'Evil Dick', 18, 1, 4, 239020, 7828873, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (180, 'Body Count Anthem', 18, 1, 4, 166426, 5463690, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (181, 'Momma\'s Gotta Die Tonight', 18, 1, 4, 371539, 12122946, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (182, 'Freedom Of Speech', 18, 1, 4, 281234, 9337917, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (183, 'King In Crimson', 19, 1, 3, 'Roy Z', 283167, 9218499, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (184, 'Chemical Wedding', 19, 1, 3, 'Roy Z', 246177, 8022764, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (185, 'The Tower', 19, 1, 3, 'Roy Z', 285257, 9435693, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (186, 'Killing Floor', 19, 1, 3, 'Adrian Smith', 269557, 8854240, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (187, 'Book Of Thel', 19, 1, 3, 'Eddie Casillas/Roy Z', 494393, 16034404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (188, 'Gates Of Urizen', 19, 1, 3, 'Roy Z', 265351, 8627004, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (189, 'Jerusalem', 19, 1, 3, 'Roy Z', 402390, 13194463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (190, 'Trupets Of Jericho', 19, 1, 3, 'Roy Z', 359131, 11820908, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (191, 'Machine Men', 19, 1, 3, 'Adrian Smith', 341655, 11138147, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (192, 'The Alchemist', 19, 1, 3, 'Roy Z', 509413, 16545657, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (193, 'Realword', 19, 1, 3, 'Roy Z', 237531, 7802095, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (194, 'First Time I Met The Blues', 20, 1, 6, 'Eurreal Montgomery', 140434, 4604995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (195, 'Let Me Love You Baby', 20, 1, 6, 'Willie Dixon', 175386, 5716994, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (196, 'Stone Crazy', 20, 1, 6, 'Buddy Guy', 433397, 14184984, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (197, 'Pretty Baby', 20, 1, 6, 'Willie Dixon', 237662, 7848282, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (198, 'When My Left Eye Jumps', 20, 1, 6, 'Al Perkins/Willie Dixon', 235311, 7685363, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (199, 'Leave My Girl Alone', 20, 1, 6, 'Buddy Guy', 204721, 6859518, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (200, 'She Suits Me To A Tee', 20, 1, 6, 'Buddy Guy', 136803, 4456321, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (201, 'Keep It To Myself (Aka Keep It To Yourself)', 20, 1, 6, 'Sonny Boy Williamson [I]', 166060, 5487056, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (202, 'My Time After Awhile', 20, 1, 6, 'Robert Geddins/Ron Badger/Sheldon Feinberg', 182491, 6022698, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (203, 'Too Many Ways (Alternate)', 20, 1, 6, 'Willie Dixon', 135053, 4459946, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (204, 'Talkin\' \'Bout Women Obviously', 20, 1, 6, 'Amos Blakemore/Buddy Guy', 589531, 19161377, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (205, 'Jorge Da Capadócia', 21, 1, 7, 'Jorge Ben', 177397, 5842196, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (206, 'Prenda Minha', 21, 1, 7, 'Tradicional', 99369, 3225364, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (207, 'Meditação', 21, 1, 7, 'Tom Jobim - Newton Mendoça', 148793, 4865597, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (208, 'Terra', 21, 1, 7, 'Caetano Veloso', 482429, 15889054, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (209, 'Eclipse Oculto', 21, 1, 7, 'Caetano Veloso', 221936, 7382703, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (210, 'Texto "Verdade Tropical"', 21, 1, 7, 'Caetano Veloso', 84088, 2752161, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (211, 'Bem Devagar', 21, 1, 7, 'Gilberto Gil', 133172, 4333651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (212, 'Drão', 21, 1, 7, 'Gilberto Gil', 156264, 5065932, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (213, 'Saudosismo', 21, 1, 7, 'Caetano Veloso', 144326, 4726981, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (214, 'Carolina', 21, 1, 7, 'Chico Buarque', 181812, 5924159, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (215, 'Sozinho', 21, 1, 7, 'Peninha', 190589, 6253200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (216, 'Esse Cara', 21, 1, 7, 'Caetano Veloso', 223111, 7217126, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (217, 'Mel', 21, 1, 7, 'Caetano Veloso - Waly Salomão', 294765, 9854062, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (218, 'Linha Do Equador', 21, 1, 7, 'Caetano Veloso - Djavan', 299337, 10003747, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (219, 'Odara', 21, 1, 7, 'Caetano Veloso', 141270, 4704104, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (220, 'A Luz De Tieta', 21, 1, 7, 'Caetano Veloso', 251742, 8507446, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (221, 'Atrás Da Verd-E-Rosa Só Não Vai Quem Já Morreu', 21, 1, 7, 'David Corrêa - Paulinho Carvalho - Carlos Sena - Bira do Ponto', 307252, 10364247, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (222, 'Vida Boa', 21, 1, 7, 'Fausto Nilo - Armandinho', 281730, 9411272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (223, 'Sozinho (Hitmakers Classic Mix)', 22, 1, 7, 436636, 14462072, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (224, 'Sozinho (Hitmakers Classic Radio Edit)', 22, 1, 7, 195004, 6455134, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (225, 'Sozinho (Caêdrum \'n\' Bass)', 22, 1, 7, 328071, 10975007, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (226, 'Carolina', 23, 1, 7, 163056, 5375395, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (227, 'Essa Moça Ta Diferente', 23, 1, 7, 167235, 5568574, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (228, 'Vai Passar', 23, 1, 7, 369763, 12359161, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (229, 'Samba De Orly', 23, 1, 7, 162429, 5431854, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (230, 'Bye, Bye Brasil', 23, 1, 7, 283402, 9499590, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (231, 'Atras Da Porta', 23, 1, 7, 189675, 6132843, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (232, 'Tatuagem', 23, 1, 7, 172120, 5645703, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (233, 'O Que Será (À Flor Da Terra)', 23, 1, 7, 167288, 5574848, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (234, 'Morena De Angola', 23, 1, 7, 186801, 6373932, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (235, 'Apesar De Você', 23, 1, 7, 234501, 7886937, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (236, 'A Banda', 23, 1, 7, 132493, 4349539, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (237, 'Minha Historia', 23, 1, 7, 182256, 6029673, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (238, 'Com Açúcar E Com Afeto', 23, 1, 7, 175386, 5846442, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (239, 'Brejo Da Cruz', 23, 1, 7, 214099, 7270749, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (240, 'Meu Caro Amigo', 23, 1, 7, 260257, 8778172, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (241, 'Geni E O Zepelim', 23, 1, 7, 317570, 10342226, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (242, 'Trocando Em Miúdos', 23, 1, 7, 169717, 5461468, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (243, 'Vai Trabalhar Vagabundo', 23, 1, 7, 139154, 4693941, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (244, 'Gota D\'água', 23, 1, 7, 153208, 5074189, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (245, 'Construção / Deus Lhe Pague', 23, 1, 7, 383059, 12675305, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (246, 'Mateus Enter', 24, 1, 7, 'Chico Science', 33149, 1103013, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (247, 'O Cidadão Do Mundo', 24, 1, 7, 'Chico Science', 200933, 6724966, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (248, 'Etnia', 24, 1, 7, 'Chico Science', 152555, 5061413, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (249, 'Quilombo Groove [Instrumental]', 24, 1, 7, 'Chico Science', 151823, 5042447, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (250, 'Macô', 24, 1, 7, 'Chico Science', 249600, 8253934, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (251, 'Um Passeio No Mundo Livre', 24, 1, 7, 'Chico Science', 240091, 7984291, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (252, 'Samba Do Lado', 24, 1, 7, 'Chico Science', 227317, 7541688, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (253, 'Maracatu Atômico', 24, 1, 7, 'Chico Science', 284264, 9670057, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (254, 'O Encontro De Isaac Asimov Com Santos Dumont No Céu', 24, 1, 7, 'Chico Science', 99108, 3240816, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (255, 'Corpo De Lama', 24, 1, 7, 'Chico Science', 232672, 7714954, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (256, 'Sobremesa', 24, 1, 7, 'Chico Science', 240091, 7960868, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (257, 'Manguetown', 24, 1, 7, 'Chico Science', 194560, 6475159, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (258, 'Um Satélite Na Cabeça', 24, 1, 7, 'Chico Science', 126615, 4272821, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (259, 'Baião Ambiental [Instrumental]', 24, 1, 7, 'Chico Science', 152659, 5198539, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (260, 'Sangue De Bairro', 24, 1, 7, 'Chico Science', 132231, 4415557, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (261, 'Enquanto O Mundo Explode', 24, 1, 7, 'Chico Science', 88764, 2968650, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (262, 'Interlude Zumbi', 24, 1, 7, 'Chico Science', 71627, 2408550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (263, 'Criança De Domingo', 24, 1, 7, 'Chico Science', 208222, 6984813, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (264, 'Amor De Muito', 24, 1, 7, 'Chico Science', 175333, 5881293, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (265, 'Samidarish [Instrumental]', 24, 1, 7, 'Chico Science', 272431, 8911641, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (266, 'Maracatu Atômico [Atomic Version]', 24, 1, 7, 'Chico Science', 273084, 9019677, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (267, 'Maracatu Atômico [Ragga Mix]', 24, 1, 7, 'Chico Science', 210155, 6986421, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (268, 'Maracatu Atômico [Trip Hop]', 24, 1, 7, 'Chico Science', 221492, 7380787, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (269, 'Banditismo Por Uma Questa', 25, 1, 7, 307095, 10251097, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (270, 'Banditismo Por Uma Questa', 25, 1, 7, 243644, 8147224, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (271, 'Rios Pontes & Overdrives', 25, 1, 7, 286720, 9659152, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (272, 'Cidade', 25, 1, 7, 216346, 7241817, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (273, 'Praiera', 25, 1, 7, 183640, 6172781, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (274, 'Samba Makossa', 25, 1, 7, 271856, 9095410, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (275, 'Da Lama Ao Caos', 25, 1, 7, 251559, 8378065, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (276, 'Maracatu De Tiro Certeiro', 25, 1, 7, 88868, 2901397, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (277, 'Salustiano Song', 25, 1, 7, 215405, 7183969, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (278, 'Antene Se', 25, 1, 7, 248372, 8253618, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (279, 'Risoflora', 25, 1, 7, 105586, 3536938, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (280, 'Lixo Do Mangue', 25, 1, 7, 193253, 6534200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (281, 'Computadores Fazem Arte', 25, 1, 7, 404323, 13702771, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (282, 'Girassol', 26, 1, 8, 'Bino Farias/Da Gama/Lazão/Pedro Luis/Toni Garrido', 249808, 8327676, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (283, 'A Sombra Da Maldade', 26, 1, 8, 'Da Gama/Toni Garrido', 230922, 7697230, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (284, 'Johnny B. Goode', 26, 1, 8, 'Chuck Berry', 254615, 8505985, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (285, 'Soldado Da Paz', 26, 1, 8, 'Herbert Vianna', 194220, 6455080, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (286, 'Firmamento', 26, 1, 8, 'Bino Farias/Da Gama/Henry Lawes/Lazão/Toni Garrido/Winston Foser-Vers', 222145, 7402658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (287, 'Extra', 26, 1, 8, 'Gilberto Gil', 304352, 10078050, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (288, 'O Erê', 26, 1, 8, 'Bernardo Vilhena/Bino Farias/Da Gama/Lazão/Toni Garrido', 236382, 7866924, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (289, 'Podes Crer', 26, 1, 8, 'Bino Farias/Da Gama/Lazão/Toni Garrido', 232280, 7747747, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (290, 'A Estrada', 26, 1, 8, 'Bino Farias/Da Gama/Lazão/Toni Garrido', 248842, 8275673, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (291, 'Berlim', 26, 1, 8, 'Da Gama/Toni Garrido', 207542, 6920424, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (292, 'Já Foi', 26, 1, 8, 'Bino Farias/Da Gama/Lazão/Toni Garrido', 221544, 7388466, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (293, 'Onde Você Mora?', 26, 1, 8, 'Marisa Monte/Nando Reis', 256026, 8502588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (294, 'Pensamento', 26, 1, 8, 'Bino Farias/Da Gamma/Lazão/Rás Bernard', 173008, 5748424, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (295, 'Conciliação', 26, 1, 8, 'Da Gama/Lazão/Rás Bernardo', 257619, 8552474, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (296, 'Realidade Virtual', 26, 1, 8, 'Bino Farias/Da Gama/Lazão/Toni Garrido', 195239, 6503533, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (297, 'Mensagem', 26, 1, 8, 'Bino Farias/Da Gama/Lazão/Rás Bernardo', 225332, 7488852, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (298, 'A Cor Do Sol', 26, 1, 8, 'Bernardo Vilhena/Da Gama/Lazão', 231392, 7663348, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (299, 'Onde Você Mora?', 27, 1, 8, 'Marisa Monte/Nando Reis', 298396, 10056970, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (300, 'O Erê', 27, 1, 8, 'Bernardo Vilhena/Bino/Da Gama/Lazao/Toni Garrido', 206942, 6950332, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (301, 'A Sombra Da Maldade', 27, 1, 8, 'Da Gama/Toni Garrido', 285231, 9544383, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (302, 'A Estrada', 27, 1, 8, 'Da Gama/Lazao/Toni Garrido', 282174, 9344477, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (303, 'Falar A Verdade', 27, 1, 8, 'Bino/Da Gama/Ras Bernardo', 244950, 8189093, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (304, 'Firmamento', 27, 1, 8, 'Harry Lawes/Winston Foster-Vers', 225488, 7507866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (305, 'Pensamento', 27, 1, 8, 'Bino/Da Gama/Ras Bernardo', 192391, 6399761, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (306, 'Realidade Virtual', 27, 1, 8, 'Bino/Da Gamma/Lazao/Toni Garrido', 240300, 8069934, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (307, 'Doutor', 27, 1, 8, 'Bino/Da Gama/Toni Garrido', 178155, 5950952, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (308, 'Na Frente Da TV', 27, 1, 8, 'Bino/Da Gama/Lazao/Ras Bernardo', 289750, 9633659, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (309, 'Downtown', 27, 1, 8, 'Cidade Negra', 239725, 8024386, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (310, 'Sábado A Noite', 27, 1, 8, 'Lulu Santos', 267363, 8895073, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (311, 'A Cor Do Sol', 27, 1, 8, 'Bernardo Vilhena/Da Gama/Lazao', 273031, 9142937, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (312, 'Eu Também Quero Beijar', 27, 1, 8, 'Fausto Nilo/Moraes Moreira/Pepeu Gomes', 211147, 7029400, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (313, 'Noite Do Prazer', 28, 1, 7, 311353, 10309980, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (314, 'À Francesa', 28, 1, 7, 244532, 8150846, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (315, 'Cada Um Cada Um (A Namoradeira)', 28, 1, 7, 253492, 8441034, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (316, 'Linha Do Equador', 28, 1, 7, 244715, 8123466, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (317, 'Amor Demais', 28, 1, 7, 254040, 8420093, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (318, 'Férias', 28, 1, 7, 264202, 8731945, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (319, 'Gostava Tanto De Você', 28, 1, 7, 230452, 7685326, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (320, 'Flor Do Futuro', 28, 1, 7, 275748, 9205941, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (321, 'Felicidade Urgente', 28, 1, 7, 266605, 8873358, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (322, 'Livre Pra Viver', 28, 1, 7, 214595, 7111596, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (323, 'Dig-Dig, Lambe-Lambe (Ao Vivo)', 29, 1, 9, 'Cassiano Costa/Cintia Maviane/J.F./Lucas Costa', 205479, 6892516, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (324, 'Pererê', 29, 1, 9, 'Augusto Conceição/Chiclete Com Banana', 198661, 6643207, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (325, 'TriboTchan', 29, 1, 9, 'Cal Adan/Paulo Levi', 194194, 6507950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (326, 'Tapa Aqui, Descobre Ali', 29, 1, 9, 'Paulo Levi/W. Rangel', 188630, 6327391, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (327, 'Daniela', 29, 1, 9, 'Jorge Cardoso/Pierre Onasis', 230791, 7748006, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (328, 'Bate Lata', 29, 1, 9, 'Fábio Nolasco/Gal Sales/Ivan Brasil', 206733, 7034985, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (329, 'Garotas do Brasil', 29, 1, 9, 'Garay, Ricardo Engels/Luca Predabom/Ludwig, Carlos Henrique/Maurício Vieira', 210155, 6973625, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (330, 'Levada do Amor (Ailoviu)', 29, 1, 9, 'Luiz Wanderley/Paulo Levi', 190093, 6457752, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (331, 'Lavadeira', 29, 1, 9, 'Do Vale, Valverde/Gal Oliveira/Luciano Pinto', 214256, 7254147, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (332, 'Reboladeira', 29, 1, 9, 'Cal Adan/Ferrugem/Julinho Carioca/Tríona Ní Dhomhnaill', 210599, 7027525, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (333, 'É que Nessa Encarnação Eu Nasci Manga', 29, 1, 9, 'Lucina/Luli', 196519, 6568081, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (334, 'Reggae Tchan', 29, 1, 9, 'Cal Adan/Del Rey, Tension/Edu Casanova', 206654, 6931328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (335, 'My Love', 29, 1, 9, 'Jauperi/Zeu Góes', 203493, 6772813, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (336, 'Latinha de Cerveja', 29, 1, 9, 'Adriano Bernandes/Edmar Neves', 166687, 5532564, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (337, 'You Shook Me', 30, 1, 1, 'J B Lenoir/Willie Dixon', 315951, 10249958, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (338, 'I Can\'t Quit You Baby', 30, 1, 1, 'Willie Dixon', 263836, 8581414, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (339, 'Communication Breakdown', 30, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones', 192653, 6287257, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (340, 'Dazed and Confused', 30, 1, 1, 'Jimmy Page', 401920, 13035765, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (341, 'The Girl I Love She Got Long Black Wavy Hair', 30, 1, 1, 'Jimmy Page/John Bonham/John Estes/John Paul Jones/Robert Plant', 183327, 5995686, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (342, 'What is and Should Never Be', 30, 1, 1, 'Jimmy Page/Robert Plant', 260675, 8497116, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (343, 'Communication Breakdown(2)', 30, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones', 161149, 5261022, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (344, 'Travelling Riverside Blues', 30, 1, 1, 'Jimmy Page/Robert Johnson/Robert Plant', 312032, 10232581, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (345, 'Whole Lotta Love', 30, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones/Robert Plant/Willie Dixon', 373394, 12258175, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (346, 'Somethin\' Else', 30, 1, 1, 'Bob Cochran/Sharon Sheeley', 127869, 4165650, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (347, 'Communication Breakdown(3)', 30, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones', 185260, 6041133, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (348, 'I Can\'t Quit You Baby(2)', 30, 1, 1, 'Willie Dixon', 380551, 12377615, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (349, 'You Shook Me(2)', 30, 1, 1, 'J B Lenoir/Willie Dixon', 619467, 20138673, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (350, 'How Many More Times', 30, 1, 1, 'Chester Burnett/Jimmy Page/John Bonham/John Paul Jones/Robert Plant', 711836, 23092953, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (351, 'Debra Kadabra', 31, 1, 1, 'Frank Zappa', 234553, 7649679, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (352, 'Carolina Hard-Core Ecstasy', 31, 1, 1, 'Frank Zappa', 359680, 11731061, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (353, 'Sam With The Showing Scalp Flat Top', 31, 1, 1, 'Don Van Vliet', 171284, 5572993, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (354, 'Poofter\'s Froth Wyoming Plans Ahead', 31, 1, 1, 'Frank Zappa', 183902, 6007019, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (355, '200 Years Old', 31, 1, 1, 'Frank Zappa', 272561, 8912465, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (356, 'Cucamonga', 31, 1, 1, 'Frank Zappa', 144483, 4728586, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (357, 'Advance Romance', 31, 1, 1, 'Frank Zappa', 677694, 22080051, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (358, 'Man With The Woman Head', 31, 1, 1, 'Don Van Vliet', 88894, 2922044, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (359, 'Muffin Man', 31, 1, 1, 'Frank Zappa', 332878, 10891682, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (360, 'Vai-Vai 2001', 32, 1, 10, 276349, 9402241, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (361, 'X-9 2001', 32, 1, 10, 273920, 9310370, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (362, 'Gavioes 2001', 32, 1, 10, 282723, 9616640, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (363, 'Nene 2001', 32, 1, 10, 284969, 9694508, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (364, 'Rosas De Ouro 2001', 32, 1, 10, 284342, 9721084, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (365, 'Mocidade Alegre 2001', 32, 1, 10, 282488, 9599937, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (366, 'Camisa Verde 2001', 32, 1, 10, 283454, 9633755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (367, 'Leandro De Itaquera 2001', 32, 1, 10, 274808, 9451845, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (368, 'Tucuruvi 2001', 32, 1, 10, 287921, 9883335, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (369, 'Aguia De Ouro 2001', 32, 1, 10, 284160, 9698729, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (370, 'Ipiranga 2001', 32, 1, 10, 248293, 8522591, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (371, 'Morro Da Casa Verde 2001', 32, 1, 10, 284708, 9718778, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (372, 'Perola Negra 2001', 32, 1, 10, 281626, 9619196, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (373, 'Sao Lucas 2001', 32, 1, 10, 296254, 10020122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (374, 'Guanabara', 33, 1, 7, 'Marcos Valle', 247614, 8499591, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (375, 'Mas Que Nada', 33, 1, 7, 'Jorge Ben', 248398, 8255254, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (376, 'Vôo Sobre o Horizonte', 33, 1, 7, 'J.r.Bertami/Parana', 225097, 7528825, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (377, 'A Paz', 33, 1, 7, 'Donato/Gilberto Gil', 263183, 8619173, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (378, 'Wave (Vou te Contar)', 33, 1, 7, 'Antonio Carlos Jobim', 271647, 9057557, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (379, 'Água de Beber', 33, 1, 7, 'Antonio Carlos Jobim/Vinicius de Moraes', 146677, 4866476, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (380, 'Samba da Bençaco', 33, 1, 7, 'Baden Powell/Vinicius de Moraes', 282200, 9440676, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (381, 'Pode Parar', 33, 1, 7, 'Jorge Vercilo/Jota Maranhao', 179408, 6046678, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (382, 'Menino do Rio', 33, 1, 7, 'Caetano Veloso', 262713, 8737489, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (383, 'Ando Meio Desligado', 33, 1, 7, 'Caetano Veloso', 195813, 6547648, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (384, 'Mistério da Raça', 33, 1, 7, 'Luiz Melodia/Ricardo Augusto', 184320, 6191752, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (385, 'All Star', 33, 1, 7, 'Nando Reis', 176326, 5891697, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (386, 'Menina Bonita', 33, 1, 7, 'Alexandre Brazil/Pedro Luis/Rodrigo Cabelo', 237087, 7938246, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (387, 'Pescador de Ilusões', 33, 1, 7, 'Macelo Yuka/O Rappa', 245524, 8267067, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (388, 'À Vontade (Live Mix)', 33, 1, 7, 'Bombom/Ed Motta', 180636, 5972430, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (389, 'Maria Fumaça', 33, 1, 7, 'Luiz Carlos/Oberdan', 141008, 4743149, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (390, 'Sambassim (dj patife remix)', 33, 1, 7, 'Alba Carvalho/Fernando Porto', 213655, 7243166, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (391, 'Garota De Ipanema', 34, 1, 7, 'Vários', 279536, 9141343, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (392, 'Tim Tim Por Tim Tim', 34, 1, 7, 'Vários', 213237, 7143328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (393, 'Tarde Em Itapoã', 34, 1, 7, 'Vários', 313704, 10344491, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (394, 'Tanto Tempo', 34, 1, 7, 'Vários', 170292, 5572240, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (395, 'Eu Vim Da Bahia - Live', 34, 1, 7, 'Vários', 157988, 5115428, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (396, 'Alô Alô Marciano', 34, 1, 7, 'Vários', 238106, 8013065, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (397, 'Linha Do Horizonte', 34, 1, 7, 'Vários', 279484, 9275929, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (398, 'Only A Dream In Rio', 34, 1, 7, 'Vários', 371356, 12192989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (399, 'Abrir A Porta', 34, 1, 7, 'Vários', 271960, 8991141, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (400, 'Alice', 34, 1, 7, 'Vários', 165982, 5594341, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (401, 'Momentos Que Marcam', 34, 1, 7, 'Vários', 280137, 9313740, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (402, 'Um Jantar Pra Dois', 34, 1, 7, 'Vários', 237714, 7819755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (403, 'Bumbo Da Mangueira', 34, 1, 7, 'Vários', 270158, 9073350, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (404, 'Mr Funk Samba', 34, 1, 7, 'Vários', 213890, 7102545, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (405, 'Santo Antonio', 34, 1, 7, 'Vários', 162716, 5492069, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (406, 'Por Você', 34, 1, 7, 'Vários', 205557, 6792493, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (407, 'Só Tinha De Ser Com Você', 34, 1, 7, 'Vários', 389642, 13085596, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (408, 'Free Speech For The Dumb', 35, 1, 3, 'Molaney/Morris/Roberts/Wainwright', 155428, 5076048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (409, 'It\'s Electric', 35, 1, 3, 'Harris/Tatler', 213995, 6978601, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (410, 'Sabbra Cadabra', 35, 1, 3, 'Black Sabbath', 380342, 12418147, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (411, 'Turn The Page', 35, 1, 3, 'Seger', 366524, 11946327, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (412, 'Die Die My Darling', 35, 1, 3, 'Danzig', 149315, 4867667, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (413, 'Loverman', 35, 1, 3, 'Cave', 472764, 15446975, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (414, 'Mercyful Fate', 35, 1, 3, 'Diamond/Shermann', 671712, 21942829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (415, 'Astronomy', 35, 1, 3, 'A.Bouchard/J.Bouchard/S.Pearlman', 397531, 13065612, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (416, 'Whiskey In The Jar', 35, 1, 3, 'Traditional', 305005, 9943129, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (417, 'Tuesday\'s Gone', 35, 1, 3, 'Collins/Van Zandt', 545750, 17900787, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (418, 'The More I See', 35, 1, 3, 'Molaney/Morris/Roberts/Wainwright', 287973, 9378873, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (419, 'A Kind Of Magic', 36, 1, 1, 'Roger Taylor', 262608, 8689618, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (420, 'Under Pressure', 36, 1, 1, 'Queen & David Bowie', 236617, 7739042, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (421, 'Radio GA GA', 36, 1, 1, 'Roger Taylor', 343745, 11358573, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (422, 'I Want It All', 36, 1, 1, 'Queen', 241684, 7876564, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (423, 'I Want To Break Free', 36, 1, 1, 'John Deacon', 259108, 8552861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (424, 'Innuendo', 36, 1, 1, 'Queen', 387761, 12664591, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (425, 'It\'s A Hard Life', 36, 1, 1, 'Freddie Mercury', 249417, 8112242, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (426, 'Breakthru', 36, 1, 1, 'Queen', 249234, 8150479, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (427, 'Who Wants To Live Forever', 36, 1, 1, 'Brian May', 297691, 9577577, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (428, 'Headlong', 36, 1, 1, 'Queen', 273057, 8921404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (429, 'The Miracle', 36, 1, 1, 'Queen', 294974, 9671923, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (430, 'I\'m Going Slightly Mad', 36, 1, 1, 'Queen', 248032, 8192339, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (431, 'The Invisible Man', 36, 1, 1, 'Queen', 238994, 7920353, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (432, 'Hammer To Fall', 36, 1, 1, 'Brian May', 220316, 7255404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (433, 'Friends Will Be Friends', 36, 1, 1, 'Freddie Mercury & John Deacon', 248920, 8114582, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (434, 'The Show Must Go On', 36, 1, 1, 'Queen', 263784, 8526760, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (435, 'One Vision', 36, 1, 1, 'Queen', 242599, 7936928, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (436, 'Detroit Rock City', 37, 1, 1, 'Paul Stanley, B. Ezrin', 218880, 7146372, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (437, 'Black Diamond', 37, 1, 1, 'Paul Stanley', 314148, 10266007, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (438, 'Hard Luck Woman', 37, 1, 1, 'Paul Stanley', 216032, 7109267, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (439, 'Sure Know Something', 37, 1, 1, 'Paul Stanley, Vincent Poncia', 242468, 7939886, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (440, 'Love Gun', 37, 1, 1, 'Paul Stanley', 196257, 6424915, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (441, 'Deuce', 37, 1, 1, 'Gene Simmons', 185077, 6097210, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (442, 'Goin\' Blind', 37, 1, 1, 'Gene Simmons, S. Coronel', 216215, 7045314, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (443, 'Shock Me', 37, 1, 1, 'Ace Frehley', 227291, 7529336, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (444, 'Do You Love Me', 37, 1, 1, 'Paul Stanley, B. Ezrin, K. Fowley', 214987, 6976194, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (445, 'She', 37, 1, 1, 'Gene Simmons, S. Coronel', 248346, 8229734, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (446, 'I Was Made For Loving You', 37, 1, 1, 'Paul Stanley, Vincent Poncia, Desmond Child', 271360, 9018078, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (447, 'Shout It Out Loud', 37, 1, 1, 'Paul Stanley, Gene Simmons, B. Ezrin', 219742, 7194424, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (448, 'God Of Thunder', 37, 1, 1, 'Paul Stanley', 255791, 8309077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (449, 'Calling Dr. Love', 37, 1, 1, 'Gene Simmons', 225332, 7395034, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (450, 'Beth', 37, 1, 1, 'S. Penridge, Bob Ezrin, Peter Criss', 166974, 5360574, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (451, 'Strutter', 37, 1, 1, 'Paul Stanley, Gene Simmons', 192496, 6317021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (452, 'Rock And Roll All Nite', 37, 1, 1, 'Paul Stanley, Gene Simmons', 173609, 5735902, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (453, 'Cold Gin', 37, 1, 1, 'Ace Frehley', 262243, 8609783, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (454, 'Plaster Caster', 37, 1, 1, 'Gene Simmons', 207333, 6801116, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (455, 'God Gave Rock \'n\' Roll To You', 37, 1, 1, 'Paul Stanley, Gene Simmons, Rus Ballard, Bob Ezrin', 320444, 10441590, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (456, 'Heart of the Night', 38, 1, 2, 273737, 9098263, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (457, 'De La Luz', 38, 1, 2, 315219, 10518284, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (458, 'Westwood Moon', 38, 1, 2, 295627, 9765802, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (459, 'Midnight', 38, 1, 2, 266866, 8851060, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (460, 'Playtime', 38, 1, 2, 273580, 9070880, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (461, 'Surrender', 38, 1, 2, 287634, 9422926, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (462, 'Valentino\'s', 38, 1, 2, 296124, 9848545, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (463, 'Believe', 38, 1, 2, 310778, 10317185, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (464, 'As We Sleep', 38, 1, 2, 316865, 10429398, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (465, 'When Evening Falls', 38, 1, 2, 298135, 9863942, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (466, 'J Squared', 38, 1, 2, 288757, 9480777, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (467, 'Best Thing', 38, 1, 2, 274259, 9069394, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (468, 'Maria', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 167262, 5484747, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (469, 'Poprocks And Coke', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 158354, 5243078, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (470, 'Longview', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 234083, 7714939, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (471, 'Welcome To Paradise', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 224208, 7406008, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (472, 'Basket Case', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 181629, 5951736, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (473, 'When I Come Around', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 178364, 5839426, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (474, 'She', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 134164, 4425128, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (475, 'J.A.R. (Jason Andrew Relva)', 39, 1, 4, 'Mike Dirnt -Words Green Day -Music', 170997, 5645755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (476, 'Geek Stink Breath', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 135888, 4408983, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (477, 'Brain Stew', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 193149, 6305550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (478, 'Jaded', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 90331, 2950224, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (479, 'Walking Contradiction', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 151170, 4932366, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (480, 'Stuck With Me', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 135523, 4431357, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (481, 'Hitchin\' A Ride', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 171546, 5616891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (482, 'Good Riddance (Time Of Your Life)', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 153600, 5075241, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (483, 'Redundant', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 198164, 6481753, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (484, 'Nice Guys Finish Last', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 170187, 5604618, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (485, 'Minority', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 168803, 5535061, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (486, 'Warning', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 221910, 7343176, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (487, 'Waiting', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 192757, 6316430, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (488, 'Macy\'s Day Parade', 39, 1, 4, 'Billie Joe Armstrong -Words Green Day -Music', 213420, 7075573, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (489, 'Into The Light', 40, 1, 1, 'David Coverdale', 76303, 2452653, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (490, 'River Song', 40, 1, 1, 'David Coverdale', 439510, 14359478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (491, 'She Give Me ...', 40, 1, 1, 'David Coverdale', 252551, 8385478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (492, 'Don\'t You Cry', 40, 1, 1, 'David Coverdale', 347036, 11269612, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (493, 'Love Is Blind', 40, 1, 1, 'David Coverdale/Earl Slick', 344999, 11409720, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (494, 'Slave', 40, 1, 1, 'David Coverdale/Earl Slick', 291892, 9425200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (495, 'Cry For Love', 40, 1, 1, 'Bossi/David Coverdale/Earl Slick', 293015, 9567075, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (496, 'Living On Love', 40, 1, 1, 'Bossi/David Coverdale/Earl Slick', 391549, 12785876, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (497, 'Midnight Blue', 40, 1, 1, 'David Coverdale/Earl Slick', 298631, 9750990, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (498, 'Too Many Tears', 40, 1, 1, 'Adrian Vanderberg/David Coverdale', 359497, 11810238, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (499, 'Don\'t Lie To Me', 40, 1, 1, 'David Coverdale/Earl Slick', 283585, 9288007, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (500, 'Wherever You May Go', 40, 1, 1, 'David Coverdale', 239699, 7803074, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (501, 'Grito De Alerta', 41, 1, 7, 'Gonzaga Jr.', 202213, 6539422, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (502, 'Não Dá Mais Pra Segurar (Explode Coração)', 41, 1, 7, 219768, 7083012, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (503, 'Começaria Tudo Outra Vez', 41, 1, 7, 196545, 6473395, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (504, 'O Que É O Que É ?', 41, 1, 7, 259291, 8650647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (505, 'Sangrando', 41, 1, 7, 'Gonzaga Jr/Gonzaguinha', 169717, 5494406, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (506, 'Diga Lá, Coração', 41, 1, 7, 255921, 8280636, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (507, 'Lindo Lago Do Amor', 41, 1, 7, 'Gonzaga Jr.', 249678, 8353191, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (508, 'Eu Apenas Queria Que Voçê Soubesse', 41, 1, 7, 155637, 5130056, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (509, 'Com A Perna No Mundo', 41, 1, 7, 'Gonzaga Jr.', 227448, 7747108, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (510, 'E Vamos À Luta', 41, 1, 7, 222406, 7585112, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (511, 'Um Homem Também Chora (Guerreiro Menino)', 41, 1, 7, 207229, 6854219, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (512, 'Comportamento Geral', 41, 1, 7, 'Gonzaga Jr', 181577, 5997444, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (513, 'Ponto De Interrogação', 41, 1, 7, 180950, 5946265, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (514, 'Espere Por Mim, Morena', 41, 1, 7, 'Gonzaguinha', 207072, 6796523, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (515, 'Meia-Lua Inteira', 23, 1, 7, 222093, 7466288, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (516, 'Voce e Linda', 23, 1, 7, 242938, 8050268, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (517, 'Um Indio', 23, 1, 7, 195944, 6453213, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (518, 'Podres Poderes', 23, 1, 7, 259761, 8622495, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (519, 'Voce Nao Entende Nada - Cotidiano', 23, 1, 7, 421982, 13885612, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (520, 'O Estrangeiro', 23, 1, 7, 374700, 12472890, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (521, 'Menino Do Rio', 23, 1, 7, 147670, 4862277, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (522, 'Qualquer Coisa', 23, 1, 7, 193410, 6372433, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (523, 'Sampa', 23, 1, 7, 185051, 6151831, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (524, 'Queixa', 23, 1, 7, 299676, 9953962, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (525, 'O Leaozinho', 23, 1, 7, 184398, 6098150, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (526, 'Fora Da Ordem', 23, 1, 7, 354011, 11746781, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (527, 'Terra', 23, 1, 7, 401319, 13224055, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (528, 'Alegria, Alegria', 23, 1, 7, 169221, 5497025, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (529, 'Balada Do Louco', 42, 1, 4, 'Arnaldo Baptista - Rita Lee', 241057, 7852328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (530, 'Ando Meio Desligado', 42, 1, 4, 'Arnaldo Baptista - Rita Lee -  Sérgio Dias', 287817, 9484504, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (531, 'Top Top', 42, 1, 4, 'Os Mutantes - Arnolpho Lima Filho', 146938, 4875374, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (532, 'Baby', 42, 1, 4, 'Caetano Veloso', 177188, 5798202, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (533, 'A E O Z', 42, 1, 4, 'Mutantes', 518556, 16873005, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (534, 'Panis Et Circenses', 42, 1, 4, 'Caetano Veloso - Gilberto Gil', 125152, 4069688, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (535, 'Chão De Estrelas', 42, 1, 4, 'Orestes Barbosa-Sílvio Caldas', 284813, 9433620, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (536, 'Vida De Cachorro', 42, 1, 4, 'Rita Lee - Arnaldo Baptista - Sérgio Baptista', 195186, 6411149, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (537, 'Bat Macumba', 42, 1, 4, 'Gilberto Gil - Caetano Veloso', 187794, 6295223, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (538, 'Desculpe Babe', 42, 1, 4, 'Arnaldo Baptista - Rita Lee', 170422, 5637959, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (539, 'Rita Lee', 42, 1, 4, 'Arnaldo Baptista/Rita Lee/Sérgio Dias', 189257, 6270503, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (540, 'Posso Perder Minha Mulher, Minha Mãe, Desde Que Eu Tenha O Rock And Roll', 42, 1, 4, 'Arnaldo Baptista - Rita Lee - Arnolpho Lima Filho', 222955, 7346254, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (541, 'Banho De Lua', 42, 1, 4, 'B. de Filippi - F. Migliaci - Versão: Fred Jorge', 221831, 7232123, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (542, 'Meu Refrigerador Não Funciona', 42, 1, 4, 'Arnaldo Baptista - Rita Lee - Sérgio Dias', 382981, 12495906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (543, 'Burn', 43, 1, 1, 'Coverdale/Lord/Paice', 453955, 14775708, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (544, 'Stormbringer', 43, 1, 1, 'Coverdale', 277133, 9050022, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (545, 'Gypsy', 43, 1, 1, 'Coverdale/Hughes/Lord/Paice', 339173, 11046952, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (546, 'Lady Double Dealer', 43, 1, 1, 'Coverdale', 233586, 7608759, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (547, 'Mistreated', 43, 1, 1, 'Coverdale', 758648, 24596235, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (548, 'Smoke On The Water', 43, 1, 1, 'Gillan/Glover/Lord/Paice', 618031, 20103125, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (549, 'You Fool No One', 43, 1, 1, 'Coverdale/Lord/Paice', 804101, 26369966, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (550, 'Custard Pie', 44, 1, 1, 'Jimmy Page/Robert Plant', 253962, 8348257, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (551, 'The Rover', 44, 1, 1, 'Jimmy Page/Robert Plant', 337084, 11011286, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (552, 'In My Time Of Dying', 44, 1, 1, 'John Bonham/John Paul Jones', 666017, 21676727, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (553, 'Houses Of The Holy', 44, 1, 1, 'Jimmy Page/Robert Plant', 242494, 7972503, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (554, 'Trampled Under Foot', 44, 1, 1, 'John Paul Jones', 336692, 11154468, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (555, 'Kashmir', 44, 1, 1, 'John Bonham', 508604, 16686580, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (556, 'Imperatriz', 45, 1, 7, 'Guga/Marquinho Lessa/Tuninho Professor', 339173, 11348710, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (557, 'Beija-Flor', 45, 1, 7, 'Caruso/Cleber/Deo/Osmar', 327000, 10991159, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (558, 'Viradouro', 45, 1, 7, 'Dadinho/Gilbreto Gomes/Gustavo/P.C. Portugal/R. Mocoto', 344320, 11484362, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (559, 'Mocidade', 45, 1, 7, 'Domenil/J. Brito/Joaozinho/Rap, Marcelo Do', 261720, 8817757, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (560, 'Unidos Da Tijuca', 45, 1, 7, 'Douglas/Neves, Vicente Das/Silva, Gilmar L./Toninho Gentil/Wantuir', 338834, 11440689, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (561, 'Salgueiro', 45, 1, 7, 'Augusto/Craig Negoescu/Rocco Filho/Saara, Ze Carlos Da', 305920, 10294741, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (562, 'Mangueira', 45, 1, 7, 'Bizuca/Clóvis Pê/Gilson Bernini/Marelo D\'Aguia', 298318, 9999506, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (563, 'União Da Ilha', 45, 1, 7, 'Dito/Djalma Falcao/Ilha, Almir Da/Márcio André', 330945, 11100945, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (564, 'Grande Rio', 45, 1, 7, 'Carlos Santos/Ciro/Claudio Russo/Zé Luiz', 307252, 10251428, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (565, 'Portela', 45, 1, 7, 'Flavio Bororo/Paulo Apparicio/Wagner Alves/Zeca Sereno', 319608, 10712216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (566, 'Caprichosos', 45, 1, 7, 'Gule/Jorge 101/Lequinho/Luiz Piao', 351320, 11870956, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (567, 'Tradição', 45, 1, 7, 'Adalto Magalha/Lourenco', 269165, 9114880, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (568, 'Império Serrano', 45, 1, 7, 'Arlindo Cruz/Carlos Sena/Elmo Caetano/Mauricao', 334942, 11161196, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (569, 'Tuiuti', 45, 1, 7, 'Claudio Martins/David Lima/Kleber Rodrigues/Livre, Cesare Som', 259657, 8749492, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (570, '(Da Le) Yaleo', 46, 1, 1, 'Santana', 353488, 11769507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (571, 'Love Of My Life', 46, 1, 1, 'Carlos Santana & Dave Matthews', 347820, 11634337, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (572, 'Put Your Lights On', 46, 1, 1, 'E. Shrody', 285178, 9394769, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (573, 'Africa Bamba', 46, 1, 1, 'I. Toure, S. Tidiane Toure, Carlos Santana & K. Perazzo', 282827, 9492487, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (574, 'Smooth', 46, 1, 1, 'M. Itaal Shur & Rob Thomas', 298161, 9867455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (575, 'Do You Like The Way', 46, 1, 1, 'L. Hill', 354899, 11741062, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (576, 'Maria Maria', 46, 1, 1, 'W. Jean, J. Duplessis, Carlos Santana, K. Perazzo & R. Rekow', 262635, 8664601, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (577, 'Migra', 46, 1, 1, 'R. Taha, Carlos Santana & T. Lindsay', 329064, 10963305, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (578, 'Corazon Espinado', 46, 1, 1, 'F. Olivera', 276114, 9206802, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (579, 'Wishing It Was', 46, 1, 1, 'Eale-Eye Cherry, M. Simpson, J. King & M. Nishita', 292832, 9771348, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (580, 'El Farol', 46, 1, 1, 'Carlos Santana & KC Porter', 291160, 9599353, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (581, 'Primavera', 46, 1, 1, 'KC Porter & JB Eckl', 378618, 12504234, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (582, 'The Calling', 46, 1, 1, 'Carlos Santana & C. Thompson', 747755, 24703884, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (583, 'Solução', 47, 1, 7, 247431, 8100449, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (584, 'Manuel', 47, 1, 7, 230269, 7677671, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (585, 'Entre E Ouça', 47, 1, 7, 286302, 9391004, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (586, 'Um Contrato Com Deus', 47, 1, 7, 202501, 6636465, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (587, 'Um Jantar Pra Dois', 47, 1, 7, 244009, 8021589, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (588, 'Vamos Dançar', 47, 1, 7, 226194, 7617432, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (589, 'Um Love', 47, 1, 7, 181603, 6095524, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (590, 'Seis Da Tarde', 47, 1, 7, 238445, 7935898, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (591, 'Baixo Rio', 47, 1, 7, 198008, 6521676, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (592, 'Sombras Do Meu Destino', 47, 1, 7, 280685, 9161539, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (593, 'Do You Have Other Loves?', 47, 1, 7, 295235, 9604273, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (594, 'Agora Que O Dia Acordou', 47, 1, 7, 323213, 10572752, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (595, 'Já!!!', 47, 1, 7, 217782, 7103608, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (596, 'A Rua', 47, 1, 7, 238027, 7930264, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (597, 'Now\'s The Time', 48, 1, 2, 'Miles Davis', 197459, 6358868, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (598, 'Jeru', 48, 1, 2, 'Miles Davis', 193410, 6222536, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (599, 'Compulsion', 48, 1, 2, 'Miles Davis', 345025, 11254474, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (600, 'Tempus Fugit', 48, 1, 2, 'Miles Davis', 231784, 7548434, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (601, 'Walkin\'', 48, 1, 2, 'Miles Davis', 807392, 26411634, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (602, '\'Round Midnight', 48, 1, 2, 'Miles Davis', 357459, 11590284, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (603, 'Bye Bye Blackbird', 48, 1, 2, 'Miles Davis', 476003, 15549224, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (604, 'New Rhumba', 48, 1, 2, 'Miles Davis', 277968, 9018024, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (605, 'Generique', 48, 1, 2, 'Miles Davis', 168777, 5437017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (606, 'Summertime', 48, 1, 2, 'Miles Davis', 200437, 6461370, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (607, 'So What', 48, 1, 2, 'Miles Davis', 564009, 18360449, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (608, 'The Pan Piper', 48, 1, 2, 'Miles Davis', 233769, 7593713, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (609, 'Someday My Prince Will Come', 48, 1, 2, 'Miles Davis', 544078, 17890773, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (610, 'My Funny Valentine (Live)', 49, 1, 2, 'Miles Davis', 907520, 29416781, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (611, 'E.S.P.', 49, 1, 2, 'Miles Davis', 330684, 11079866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (612, 'Nefertiti', 49, 1, 2, 'Miles Davis', 473495, 15478450, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (613, 'Petits Machins (Little Stuff)', 49, 1, 2, 'Miles Davis', 487392, 16131272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (614, 'Miles Runs The Voodoo Down', 49, 1, 2, 'Miles Davis', 843964, 27967919, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (615, 'Little Church (Live)', 49, 1, 2, 'Miles Davis', 196101, 6273225, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (616, 'Black Satin', 49, 1, 2, 'Miles Davis', 316682, 10529483, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (617, 'Jean Pierre (Live)', 49, 1, 2, 'Miles Davis', 243461, 7955114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (618, 'Time After Time', 49, 1, 2, 'Miles Davis', 220734, 7292197, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (619, 'Portia', 49, 1, 2, 'Miles Davis', 378775, 12520126, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (620, 'Space Truckin\'', 50, 1, 1, 'Blackmore/Gillan/Glover/Lord/Paice', 1196094, 39267613, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (621, 'Going Down / Highway Star', 50, 1, 1, 'Gillan/Glover/Lord/Nix - Blackmore/Paice', 913658, 29846063, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (622, 'Mistreated (Alternate Version)', 50, 1, 1, 'Blackmore/Coverdale', 854700, 27775442, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (623, 'You Fool No One (Alternate Version)', 50, 1, 1, 'Blackmore/Coverdale/Lord/Paice', 763924, 24887209, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (624, 'Jeepers Creepers', 51, 1, 2, 185965, 5991903, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (625, 'Blue Rythm Fantasy', 51, 1, 2, 348212, 11204006, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (626, 'Drum Boogie', 51, 1, 2, 191555, 6185636, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (627, 'Let Me Off Uptown', 51, 1, 2, 187637, 6034685, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (628, 'Leave Us Leap', 51, 1, 2, 182726, 5898810, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (629, 'Opus No.1', 51, 1, 2, 179800, 5846041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (630, 'Boogie Blues', 51, 1, 2, 204199, 6603153, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (631, 'How High The Moon', 51, 1, 2, 201430, 6529487, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (632, 'Disc Jockey Jump', 51, 1, 2, 193149, 6260820, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (633, 'Up An\' Atom', 51, 1, 2, 179565, 5822645, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (634, 'Bop Boogie', 51, 1, 2, 189596, 6093124, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (635, 'Lemon Drop', 51, 1, 2, 194089, 6287531, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (636, 'Coronation Drop', 51, 1, 2, 176222, 5899898, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (637, 'Overtime', 51, 1, 2, 163030, 5432236, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (638, 'Imagination', 51, 1, 2, 289306, 9444385, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (639, 'Don\'t Take Your Love From Me', 51, 1, 2, 282331, 9244238, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (640, 'Midget', 51, 1, 2, 217025, 7257663, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (641, 'I\'m Coming Virginia', 51, 1, 2, 280163, 9209827, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (642, 'Payin\' Them Dues Blues', 51, 1, 2, 198556, 6536918, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (643, 'Jungle Drums', 51, 1, 2, 199627, 6546063, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (644, 'Showcase', 51, 1, 2, 201560, 6697510, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (645, 'Swedish Schnapps', 51, 1, 2, 191268, 6359750, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (646, 'Samba Da Bênção', 52, 1, 11, 409965, 13490008, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (647, 'Pot-Pourri N.º 4', 52, 1, 11, 392437, 13125975, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (648, 'Onde Anda Você', 52, 1, 11, 168437, 5550356, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (649, 'Samba Da Volta', 52, 1, 11, 170631, 5676090, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (650, 'Canto De Ossanha', 52, 1, 11, 204956, 6771624, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (651, 'Pot-Pourri N.º 5', 52, 1, 11, 219898, 7117769, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (652, 'Formosa', 52, 1, 11, 137482, 4560873, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (653, 'Como É Duro Trabalhar', 52, 1, 11, 226168, 7541177, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (654, 'Minha Namorada', 52, 1, 11, 244297, 7927967, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (655, 'Por Que Será', 52, 1, 11, 162142, 5371483, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (656, 'Berimbau', 52, 1, 11, 190667, 6335548, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (657, 'Deixa', 52, 1, 11, 179826, 5932799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (658, 'Pot-Pourri N.º 2', 52, 1, 11, 211748, 6878359, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (659, 'Samba Em Prelúdio', 52, 1, 11, 212636, 6923473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (660, 'Carta Ao Tom 74', 52, 1, 11, 162560, 5382354, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (661, 'Linha de Passe (João Bosco)', 53, 1, 7, 230948, 7902328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (662, 'Pela Luz dos Olhos Teus (Miúcha e Tom Jobim)', 53, 1, 7, 163970, 5399626, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (663, 'Chão de Giz (Elba Ramalho)', 53, 1, 7, 274834, 9016916, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (664, 'Marina (Dorival Caymmi)', 53, 1, 7, 172643, 5523628, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (665, 'Aquarela (Toquinho)', 53, 1, 7, 259944, 8480140, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (666, 'Coração do Agreste (Fafá de Belém)', 53, 1, 7, 258194, 8380320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (667, 'Dona (Roupa Nova)', 53, 1, 7, 243356, 7991295, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (668, 'Começaria Tudo Outra Vez (Maria Creuza)', 53, 1, 7, 206994, 6851151, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (669, 'Caçador de Mim (Sá & Guarabyra)', 53, 1, 7, 238341, 7751360, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (670, 'Romaria (Renato Teixeira)', 53, 1, 7, 244793, 8033885, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (671, 'As Rosas Não Falam (Beth Carvalho)', 53, 1, 7, 116767, 3836641, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (672, 'Wave (Os Cariocas)', 53, 1, 7, 130063, 4298006, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (673, 'Garota de Ipanema (Dick Farney)', 53, 1, 7, 174367, 5767474, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (674, 'Preciso Apender a Viver Só (Maysa)', 53, 1, 7, 143464, 4642359, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (675, 'Susie Q', 54, 1, 1, 'Hawkins-Lewis-Broadwater', 275565, 9043825, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (676, 'I Put A Spell On You', 54, 1, 1, 'Jay Hawkins', 272091, 8943000, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (677, 'Proud Mary', 54, 1, 1, 'J. C. Fogerty', 189022, 6229590, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (678, 'Bad Moon Rising', 54, 1, 1, 'J. C. Fogerty', 140146, 4609835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (679, 'Lodi', 54, 1, 1, 'J. C. Fogerty', 191451, 6260214, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (680, 'Green River', 54, 1, 1, 'J. C. Fogerty', 154279, 5105874, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (681, 'Commotion', 54, 1, 1, 'J. C. Fogerty', 162899, 5354252, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (682, 'Down On The Corner', 54, 1, 1, 'J. C. Fogerty', 164858, 5521804, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (683, 'Fortunate Son', 54, 1, 1, 'J. C. Fogerty', 140329, 4617559, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (684, 'Travelin\' Band', 54, 1, 1, 'J. C. Fogerty', 129358, 4270414, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (685, 'Who\'ll Stop The Rain', 54, 1, 1, 'J. C. Fogerty', 149394, 4899579, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (686, 'Up Around The Bend', 54, 1, 1, 'J. C. Fogerty', 162429, 5368701, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (687, 'Run Through The Jungle', 54, 1, 1, 'J. C. Fogerty', 186044, 6156567, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (688, 'Lookin\' Out My Back Door', 54, 1, 1, 'J. C. Fogerty', 152946, 5034670, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (689, 'Long As I Can See The Light', 54, 1, 1, 'J. C. Fogerty', 213237, 6924024, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (690, 'I Heard It Through The Grapevine', 54, 1, 1, 'Whitfield-Strong', 664894, 21947845, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (691, 'Have You Ever Seen The Rain?', 54, 1, 1, 'J. C. Fogerty', 160052, 5263675, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (692, 'Hey Tonight', 54, 1, 1, 'J. C. Fogerty', 162847, 5343807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (693, 'Sweet Hitch-Hiker', 54, 1, 1, 'J. C. Fogerty', 175490, 5716603, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (694, 'Someday Never Comes', 54, 1, 1, 'J. C. Fogerty', 239360, 7945235, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (695, 'Walking On The Water', 55, 1, 1, 'J.C. Fogerty', 281286, 9302129, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (696, 'Suzie-Q, Pt. 2', 55, 1, 1, 'J.C. Fogerty', 244114, 7986637, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (697, 'Born On The Bayou', 55, 1, 1, 'J.C. Fogerty', 316630, 10361866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (698, 'Good Golly Miss Molly', 55, 1, 1, 'J.C. Fogerty', 163604, 5348175, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (699, 'Tombstone Shadow', 55, 1, 1, 'J.C. Fogerty', 218880, 7209080, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (700, 'Wrote A Song For Everyone', 55, 1, 1, 'J.C. Fogerty', 296385, 9675875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (701, 'Night Time Is The Right Time', 55, 1, 1, 'J.C. Fogerty', 190119, 6211173, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (702, 'Cotton Fields', 55, 1, 1, 'J.C. Fogerty', 178181, 5919224, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (703, 'It Came Out Of The Sky', 55, 1, 1, 'J.C. Fogerty', 176718, 5807474, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (704, 'Don\'t Look Now', 55, 1, 1, 'J.C. Fogerty', 131918, 4366455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (705, 'The Midnight Special', 55, 1, 1, 'J.C. Fogerty', 253596, 8297482, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (706, 'Before You Accuse Me', 55, 1, 1, 'J.C. Fogerty', 207804, 6815126, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (707, 'My Baby Left Me', 55, 1, 1, 'J.C. Fogerty', 140460, 4633440, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (708, 'Pagan Baby', 55, 1, 1, 'J.C. Fogerty', 385619, 12713813, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (709, '(Wish I Could) Hideaway', 55, 1, 1, 'J.C. Fogerty', 228466, 7432978, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (710, 'It\'s Just A Thought', 55, 1, 1, 'J.C. Fogerty', 237374, 7778319, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (711, 'Molina', 55, 1, 1, 'J.C. Fogerty', 163239, 5390811, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (712, 'Born To Move', 55, 1, 1, 'J.C. Fogerty', 342804, 11260814, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (713, 'Lookin\' For A Reason', 55, 1, 1, 'J.C. Fogerty', 209789, 6933135, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (714, 'Hello Mary Lou', 55, 1, 1, 'J.C. Fogerty', 132832, 4476563, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (715, 'Gatas Extraordinárias', 56, 1, 7, 212506, 7095702, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (716, 'Brasil', 56, 1, 7, 243696, 7911683, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (717, 'Eu Sou Neguinha (Ao Vivo)', 56, 1, 7, 251768, 8376000, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (718, 'Geração Coca-Cola (Ao Vivo)', 56, 1, 7, 228153, 7573301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (719, 'Lanterna Dos Afogados', 56, 1, 7, 204538, 6714582, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (720, 'Coroné Antonio Bento', 56, 1, 7, 200437, 6713066, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (721, 'Você Passa, Eu Acho Graça (Ao Vivo)', 56, 1, 7, 206733, 6943576, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (722, 'Meu Mundo Fica Completo (Com Você)', 56, 1, 7, 247771, 8322240, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (723, '1° De Julho', 56, 1, 7, 270262, 9017535, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (724, 'Música Urbana 2', 56, 1, 7, 194899, 6383472, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (725, 'Vida Bandida (Ao Vivo)', 56, 1, 7, 192626, 6360785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (726, 'Palavras Ao Vento', 56, 1, 7, 212453, 7048676, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (727, 'Não Sei O Que Eu Quero Da Vida', 56, 1, 7, 151849, 5024963, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (728, 'Woman Is The Nigger Of The World (Ao Vivo)', 56, 1, 7, 298919, 9724145, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (729, 'Juventude Transviada (Ao Vivo)', 56, 1, 7, 278622, 9183808, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (730, 'Malandragem', 57, 1, 7, 247588, 8165048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (731, 'O Segundo Sol', 57, 1, 7, 252133, 8335629, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (732, 'Smells Like Teen Spirit (Ao Vivo)', 57, 1, 7, 316865, 10384506, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (733, 'E.C.T.', 57, 1, 7, 227500, 7571834, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (734, 'Todo Amor Que Houver Nesta Vida', 57, 1, 7, 227160, 7420347, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (735, 'Metrô. Linha 743', 57, 1, 7, 174654, 5837495, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (736, 'Nós (Ao Vivo)', 57, 1, 7, 193828, 6498661, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (737, 'Na Cadência Do Samba', 57, 1, 7, 196075, 6483952, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (738, 'Admirável Gado Novo', 57, 1, 7, 274390, 9144031, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (739, 'Eleanor Rigby', 57, 1, 7, 189466, 6303205, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (740, 'Socorro', 57, 1, 7, 258586, 8549393, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (741, 'Blues Da Piedade', 57, 1, 7, 257123, 8472964, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (742, 'Rubens', 57, 1, 7, 211853, 7026317, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (743, 'Não Deixe O Samba Morrer - Cassia Eller e Alcione', 57, 1, 7, 268173, 8936345, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (744, 'Mis Penas Lloraba Yo (Ao Vivo) Soy Gitano (Tangos)', 57, 1, 7, 188473, 6195854, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (745, 'Comin\' Home', 58, 1, 1, 'Bolin/Coverdale/Paice', 235781, 7644604, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (746, 'Lady Luck', 58, 1, 1, 'Cook/Coverdale', 168202, 5501379, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (747, 'Gettin\' Tighter', 58, 1, 1, 'Bolin/Hughes', 218044, 7176909, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (748, 'Dealer', 58, 1, 1, 'Bolin/Coverdale', 230922, 7591066, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (749, 'I Need Love', 58, 1, 1, 'Bolin/Coverdale', 263836, 8701064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (750, 'Drifter', 58, 1, 1, 'Bolin/Coverdale', 242834, 8001505, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (751, 'Love Child', 58, 1, 1, 'Bolin/Coverdale', 188160, 6173806, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (752, 'This Time Around / Owed to \'G\' [Instrumental]', 58, 1, 1, 'Bolin/Hughes/Lord', 370102, 11995679, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (753, 'You Keep On Moving', 58, 1, 1, 'Coverdale/Hughes', 319111, 10447868, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (754, 'Speed King', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 264385, 8587578, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (755, 'Bloodsucker', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 256261, 8344405, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (756, 'Child In Time', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 620460, 20230089, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (757, 'Flight Of The Rat', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 478302, 15563967, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (758, 'Into The Fire', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 210259, 6849310, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (759, 'Living Wreck', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 274886, 8993056, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (760, 'Hard Lovin\' Man', 59, 1, 1, 'Blackmore, Gillan, Glover, Lord, Paice', 431203, 13931179, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (761, 'Fireball', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 204721, 6714807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (762, 'No No No', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 414902, 13646606, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (763, 'Strange Kind Of Woman', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 247092, 8072036, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (764, 'Anyone\'s Daughter', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 284682, 9354480, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (765, 'The Mule', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 322063, 10638390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (766, 'Fools', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 500427, 16279366, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (767, 'No One Came', 60, 1, 1, 'Ritchie Blackmore, Ian Gillan, Roger Glover, Jon Lord, Ian Paice', 385880, 12643813, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (768, 'Knocking At Your Back Door', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover', 424829, 13779332, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (769, 'Bad Attitude', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord', 307905, 10035180, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (770, 'Child In Time (Son Of Aleric - Instrumental)', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord, Ian Paice', 602880, 19712753, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (771, 'Nobody\'s Home', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord, Ian Paice', 243017, 7929493, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (772, 'Black Night', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord, Ian Paice', 368770, 12058906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (773, 'Perfect Strangers', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover', 321149, 10445353, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (774, 'The Unwritten Law', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Ian Paice', 295053, 9740361, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (775, 'Call Of The Wild', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord', 293851, 9575295, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (776, 'Hush', 61, 1, 1, 'South', 213054, 6944928, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (777, 'Smoke On The Water', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord, Ian Paice', 464378, 15180849, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (778, 'Space Trucking', 61, 1, 1, 'Richie Blackmore, Ian Gillian, Roger Glover, Jon Lord, Ian Paice', 341185, 11122183, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (779, 'Highway Star', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 368770, 12012452, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (780, 'Maybe I\'m A Leo', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 290455, 9502646, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (781, 'Pictures Of Home', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 303777, 9903835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (782, 'Never Before', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 239830, 7832790, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (783, 'Smoke On The Water', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 340871, 11246496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (784, 'Lazy', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 442096, 14397671, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (785, 'Space Truckin\'', 62, 1, 1, 'Ian Gillan/Ian Paice/Jon Lord/Ritchie Blckmore/Roger Glover', 272796, 8981030, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (786, 'Vavoom : Ted The Mechanic', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 257384, 8510755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (787, 'Loosen My Strings', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 359680, 11702232, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (788, 'Soon Forgotten', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 287791, 9401383, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (789, 'Sometimes I Feel Like Screaming', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 451840, 14789410, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (790, 'Cascades : I\'m Not Your Lover', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 283689, 9209693, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (791, 'The Aviator', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 320992, 10532053, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (792, 'Rosa\'s Cantina', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 312372, 10323804, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (793, 'A Castle Full Of Rascals', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 311693, 10159566, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (794, 'A Touch Away', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 276323, 9098561, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (795, 'Hey Cisco', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 354089, 11600029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (796, 'Somebody Stole My Guitar', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 249443, 8180421, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (797, 'The Purpendicular Waltz', 63, 1, 1, 'Ian Gillan, Roger Glover, Jon Lord, Steve Morse, Ian Paice', 283924, 9299131, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (798, 'King Of Dreams', 64, 1, 1, 'Blackmore, Glover, Turner', 328385, 10733847, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (799, 'The Cut Runs Deep', 64, 1, 1, 'Blackmore, Glover, Turner, Lord, Paice', 342752, 11191650, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (800, 'Fire In The Basement', 64, 1, 1, 'Blackmore, Glover, Turner, Lord, Paice', 283977, 9267550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (801, 'Truth Hurts', 64, 1, 1, 'Blackmore, Glover, Turner', 314827, 10224612, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (802, 'Breakfast In Bed', 64, 1, 1, 'Blackmore, Glover, Turner', 317126, 10323804, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (803, 'Love Conquers All', 64, 1, 1, 'Blackmore, Glover, Turner', 227186, 7328516, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (804, 'Fortuneteller', 64, 1, 1, 'Blackmore, Glover, Turner, Lord, Paice', 349335, 11369671, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (805, 'Too Much Is Not Enough', 64, 1, 1, 'Turner, Held, Greenwood', 257724, 8382800, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (806, 'Wicked Ways', 64, 1, 1, 'Blackmore, Glover, Turner, Lord, Paice', 393691, 12826582, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (807, 'Stormbringer', 65, 1, 1, 'D.Coverdale/R.Blackmore/Ritchie Blackmore', 246413, 8044864, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (808, 'Love Don\'t Mean a Thing', 65, 1, 1, 'D.Coverdale/G.Hughes/Glenn Hughes/I.Paice/Ian Paice/J.Lord/John Lord/R.Blackmore/Ritchie Blackmore', 263862, 8675026, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (809, 'Holy Man', 65, 1, 1, 'D.Coverdale/G.Hughes/Glenn Hughes/J.Lord/John Lord', 270236, 8818093, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (810, 'Hold On', 65, 1, 1, 'D.Coverdal/G.Hughes/Glenn Hughes/I.Paice/Ian Paice/J.Lord/John Lord', 306860, 10022428, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (811, 'Lady Double Dealer', 65, 1, 1, 'D.Coverdale/R.Blackmore/Ritchie Blackmore', 201482, 6554330, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (812, 'You Can\'t Do it Right (With the One You Love)', 65, 1, 1, 'D.Coverdale/G.Hughes/Glenn Hughes/R.Blackmore/Ritchie Blackmore', 203755, 6709579, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (813, 'High Ball Shooter', 65, 1, 1, 'D.Coverdale/G.Hughes/Glenn Hughes/I.Paice/Ian Paice/J.Lord/John Lord/R.Blackmore/Ritchie Blackmore', 267833, 8772471, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (814, 'The Gypsy', 65, 1, 1, 'D.Coverdale/G.Hughes/Glenn Hughes/I.Paice/Ian Paice/J.Lord/John Lord/R.Blackmore/Ritchie Blackmore', 242886, 7946614, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (815, 'Soldier Of Fortune', 65, 1, 1, 'D.Coverdale/R.Blackmore/Ritchie Blackmore', 193750, 6315321, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (816, 'The Battle Rages On', 66, 1, 1, 'ian paice/jon lord', 356963, 11626228, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (817, 'Lick It Up', 66, 1, 1, 'roger glover', 240274, 7792604, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (818, 'Anya', 66, 1, 1, 'jon lord/roger glover', 392437, 12754921, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (819, 'Talk About Love', 66, 1, 1, 'roger glover', 247823, 8072171, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (820, 'Time To Kill', 66, 1, 1, 'roger glover', 351033, 11354742, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (821, 'Ramshackle Man', 66, 1, 1, 'roger glover', 334445, 10874679, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (822, 'A Twist In The Tail', 66, 1, 1, 'roger glover', 257462, 8413103, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (823, 'Nasty Piece Of Work', 66, 1, 1, 'jon lord/roger glover', 276662, 9076997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (824, 'Solitaire', 66, 1, 1, 'roger glover', 282226, 9157021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (825, 'One Man\'s Meat', 66, 1, 1, 'roger glover', 278804, 9068960, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (826, 'Pour Some Sugar On Me', 67, 1, 1, 292519, 9518842, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (827, 'Photograph', 67, 1, 1, 248633, 8108507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (828, 'Love Bites', 67, 1, 1, 346853, 11305791, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (829, 'Let\'s Get Rocked', 67, 1, 1, 296019, 9724150, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (830, 'Two Steps Behind [Acoustic Version]', 67, 1, 1, 259787, 8523388, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (831, 'Animal', 67, 1, 1, 244741, 7985133, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (832, 'Heaven Is', 67, 1, 1, 214021, 6988128, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (833, 'Rocket', 67, 1, 1, 247248, 8092463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (834, 'When Love & Hate Collide', 67, 1, 1, 257280, 8364633, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (835, 'Action', 67, 1, 1, 220604, 7130830, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (836, 'Make Love Like A Man', 67, 1, 1, 255660, 8309725, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (837, 'Armageddon It', 67, 1, 1, 322455, 10522352, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (838, 'Have You Ever Needed Someone So Bad', 67, 1, 1, 319320, 10400020, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (839, 'Rock Of Ages', 67, 1, 1, 248424, 8150318, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (840, 'Hysteria', 67, 1, 1, 355056, 11622738, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (841, 'Bringin\' On The Heartbreak', 67, 1, 1, 272457, 8853324, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (842, 'Roll Call', 68, 1, 2, 'Jim Beard', 321358, 10653494, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (843, 'Otay', 68, 1, 2, 'John Scofield, Robert Aries, Milton Chambers and Gary Grainger', 423653, 14176083, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (844, 'Groovus Interruptus', 68, 1, 2, 'Jim Beard', 319373, 10602166, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (845, 'Paris On Mine', 68, 1, 2, 'Jon Herington', 368875, 12059507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (846, 'In Time', 68, 1, 2, 'Sylvester Stewart', 368953, 12287103, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (847, 'Plan B', 68, 1, 2, 'Dean Brown, Dennis Chambers & Jim Beard', 272039, 9032315, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (848, 'Outbreak', 68, 1, 2, 'Jim Beard & Jon Herington', 659226, 21685807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (849, 'Baltimore, DC', 68, 1, 2, 'John Scofield', 346932, 11394473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (850, 'Talkin Loud and Saying Nothin', 68, 1, 2, 'James Brown & Bobby Byrd', 360411, 11994859, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (851, 'Pétala', 69, 1, 7, 270080, 8856165, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (852, 'Meu Bem-Querer', 69, 1, 7, 255608, 8330047, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (853, 'Cigano', 69, 1, 7, 304692, 10037362, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (854, 'Boa Noite', 69, 1, 7, 338755, 11283582, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (855, 'Fato Consumado', 69, 1, 7, 211565, 7018586, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (856, 'Faltando Um Pedaço', 69, 1, 7, 267728, 8788760, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (857, 'Álibi', 69, 1, 7, 213237, 6928434, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (858, 'Esquinas', 69, 1, 7, 280999, 9096726, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (859, 'Se...', 69, 1, 7, 286432, 9413777, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (860, 'Eu Te Devoro', 69, 1, 7, 311614, 10312775, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (861, 'Lilás', 69, 1, 7, 274181, 9049542, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (862, 'Acelerou', 69, 1, 7, 284081, 9396942, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (863, 'Um Amor Puro', 69, 1, 7, 327784, 10687311, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (864, 'Samurai', 70, 1, 7, 'Djavan', 330997, 10872787, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (865, 'Nem Um Dia', 70, 1, 7, 'Djavan', 337423, 11181446, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (866, 'Oceano', 70, 1, 7, 'Djavan', 217338, 7026441, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (867, 'Açai', 70, 1, 7, 'Djavan', 270968, 8893682, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (868, 'Serrado', 70, 1, 7, 'Djavan', 295314, 9842240, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (869, 'Flor De Lis', 70, 1, 7, 'Djavan', 236355, 7801108, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (870, 'Amar É Tudo', 70, 1, 7, 'Djavan', 211617, 7073899, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (871, 'Azul', 70, 1, 7, 'Djavan', 253962, 8381029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (872, 'Seduzir', 70, 1, 7, 'Djavan', 277524, 9163253, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (873, 'A Carta', 70, 1, 7, 'Djavan - Gabriel, O Pensador', 347297, 11493463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (874, 'Sina', 70, 1, 7, 'Djavan', 268173, 8906539, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (875, 'Acelerou', 70, 1, 7, 'Djavan', 284133, 9391439, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (876, 'Um Amor Puro', 70, 1, 7, 'Djavan', 327105, 10664698, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (877, 'O Bêbado e a Equilibrista', 71, 1, 7, 223059, 7306143, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (878, 'O Mestre-Sala dos Mares', 71, 1, 7, 186226, 6180414, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (879, 'Atrás da Porta', 71, 1, 7, 166608, 5432518, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (880, 'Dois Pra Lá, Dois Pra Cá', 71, 1, 7, 263026, 8684639, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (881, 'Casa no Campo', 71, 1, 7, 170788, 5531841, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (882, 'Romaria', 71, 1, 7, 242834, 7968525, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (883, 'Alô, Alô, Marciano', 71, 1, 7, 241397, 8137254, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (884, 'Me Deixas Louca', 71, 1, 7, 214831, 6888030, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (885, 'Fascinação', 71, 1, 7, 180793, 5793959, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (886, 'Saudosa Maloca', 71, 1, 7, 278125, 9059416, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (887, 'As Aparências Enganam', 71, 1, 7, 247379, 8014346, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (888, 'Madalena', 71, 1, 7, 157387, 5243721, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (889, 'Maria Rosa', 71, 1, 7, 232803, 7592504, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (890, 'Aprendendo A Jogar', 71, 1, 7, 290664, 9391041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (891, 'Layla', 72, 1, 6, 'Clapton/Gordon', 430733, 14115792, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (892, 'Badge', 72, 1, 6, 'Clapton/Harrison', 163552, 5322942, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (893, 'I Feel Free', 72, 1, 6, 'Bruce/Clapton', 174576, 5725684, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (894, 'Sunshine Of Your Love', 72, 1, 6, 'Bruce/Clapton', 252891, 8225889, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (895, 'Crossroads', 72, 1, 6, 'Clapton/Robert Johnson Arr: Eric Clapton', 253335, 8273540, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (896, 'Strange Brew', 72, 1, 6, 'Clapton/Collins/Pappalardi', 167810, 5489787, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (897, 'White Room', 72, 1, 6, 'Bruce/Clapton', 301583, 9872606, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (898, 'Bell Bottom Blues', 72, 1, 6, 'Clapton', 304744, 9946681, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (899, 'Cocaine', 72, 1, 6, 'Cale/Clapton', 215928, 7138399, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (900, 'I Shot The Sheriff', 72, 1, 6, 'Marley', 263862, 8738973, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (901, 'After Midnight', 72, 1, 6, 'Clapton/J. J. Cale', 191320, 6460941, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (902, 'Swing Low Sweet Chariot', 72, 1, 6, 'Clapton/Trad. Arr. Clapton', 208143, 6896288, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (903, 'Lay Down Sally', 72, 1, 6, 'Clapton/Levy', 231732, 7774207, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (904, 'Knockin On Heavens Door', 72, 1, 6, 'Clapton/Dylan', 264411, 8758819, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (905, 'Wonderful Tonight', 72, 1, 6, 'Clapton', 221387, 7326923, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (906, 'Let It Grow', 72, 1, 6, 'Clapton', 297064, 9742568, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (907, 'Promises', 72, 1, 6, 'Clapton/F.eldman/Linn', 180401, 6006154, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (908, 'I Can\'t Stand It', 72, 1, 6, 'Clapton', 249730, 8271980, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (909, 'Signe', 73, 1, 6, 'Eric Clapton', 193515, 6475042, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (910, 'Before You Accuse Me', 73, 1, 6, 'Eugene McDaniel', 224339, 7456807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (911, 'Hey Hey', 73, 1, 6, 'Big Bill Broonzy', 196466, 6543487, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (912, 'Tears In Heaven', 73, 1, 6, 'Eric Clapton, Will Jennings', 274729, 9032835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (913, 'Lonely Stranger', 73, 1, 6, 'Eric Clapton', 328724, 10894406, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (914, 'Nobody Knows You When You\'re Down & Out', 73, 1, 6, 'Jimmy Cox', 231836, 7669922, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (915, 'Layla', 73, 1, 6, 'Eric Clapton, Jim Gordon', 285387, 9490542, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (916, 'Running On Faith', 73, 1, 6, 'Jerry Lynn Williams', 378984, 12536275, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (917, 'Walkin\' Blues', 73, 1, 6, 'Robert Johnson', 226429, 7435192, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (918, 'Alberta', 73, 1, 6, 'Traditional', 222406, 7412975, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (919, 'San Francisco Bay Blues', 73, 1, 6, 'Jesse Fuller', 203363, 6724021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (920, 'Malted Milk', 73, 1, 6, 'Robert Johnson', 216528, 7096781, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (921, 'Old Love', 73, 1, 6, 'Eric Clapton, Robert Cray', 472920, 15780747, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (922, 'Rollin\' And Tumblin\'', 73, 1, 6, 'McKinley Morgenfield (Muddy Waters)', 251768, 8407355, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (923, 'Collision', 74, 1, 4, 'Jon Hudson/Mike Patton', 204303, 6656596, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (924, 'Stripsearch', 74, 1, 4, 'Jon Hudson/Mike Bordin/Mike Patton', 270106, 8861119, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (925, 'Last Cup Of Sorrow', 74, 1, 4, 'Bill Gould/Mike Patton', 251663, 8221247, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (926, 'Naked In Front Of The Computer', 74, 1, 4, 'Mike Patton', 128757, 4225077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (927, 'Helpless', 74, 1, 4, 'Bill Gould/Mike Bordin/Mike Patton', 326217, 10753135, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (928, 'Mouth To Mouth', 74, 1, 4, 'Bill Gould/Jon Hudson/Mike Bordin/Mike Patton', 228493, 7505887, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (929, 'Ashes To Ashes', 74, 1, 4, 'Bill Gould/Jon Hudson/Mike Bordin/Mike Patton/Roddy Bottum', 217391, 7093746, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (930, 'She Loves Me Not', 74, 1, 4, 'Bill Gould/Mike Bordin/Mike Patton', 209867, 6887544, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (931, 'Got That Feeling', 74, 1, 4, 'Mike Patton', 140852, 4643227, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (932, 'Paths Of Glory', 74, 1, 4, 'Bill Gould/Jon Hudson/Mike Bordin/Mike Patton/Roddy Bottum', 257253, 8436300, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (933, 'Home Sick Home', 74, 1, 4, 'Mike Patton', 119040, 3898976, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (934, 'Pristina', 74, 1, 4, 'Bill Gould/Mike Patton', 232698, 7497361, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (935, 'Land Of Sunshine', 75, 1, 4, 223921, 7353567, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (936, 'Caffeine', 75, 1, 4, 267937, 8747367, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (937, 'Midlife Crisis', 75, 1, 4, 263235, 8628841, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (938, 'RV', 75, 1, 4, 223242, 7288162, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (939, 'Smaller And Smaller', 75, 1, 4, 310831, 10180103, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (940, 'Everything\'s Ruined', 75, 1, 4, 273658, 9010917, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (941, 'Malpractice', 75, 1, 4, 241371, 7900683, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (942, 'Kindergarten', 75, 1, 4, 270680, 8853647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (943, 'Be Aggressive', 75, 1, 4, 222432, 7298027, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (944, 'A Small Victory', 75, 1, 4, 297168, 9733572, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (945, 'Crack Hitler', 75, 1, 4, 279144, 9162435, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (946, 'Jizzlobber', 75, 1, 4, 398341, 12926140, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (947, 'Midnight Cowboy', 75, 1, 4, 251924, 8242626, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (948, 'Easy', 75, 1, 4, 185835, 6073008, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (949, 'Get Out', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 137482, 4524972, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (950, 'Ricochet', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 269400, 8808812, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (951, 'Evidence', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton, Trey Spruance', 293590, 9626136, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (952, 'The Gentle Art Of Making Enemies', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 209319, 6908609, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (953, 'Star A.D.', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 203807, 6747658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (954, 'Cuckoo For Caca', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton, Trey Spruance', 222902, 7388369, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (955, 'Caralho Voador', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton, Trey Spruance', 242102, 8029054, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (956, 'Ugly In The Morning', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 186435, 6224997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (957, 'Digging The Grave', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 185129, 6109259, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (958, 'Take This Bottle', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton, Trey Spruance', 298997, 9779971, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (959, 'King For A Day', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton, Trey Spruance', 395859, 13163733, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (960, 'What A Day', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 158275, 5203430, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (961, 'The Last To Know', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 267833, 8736776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (962, 'Just A Man', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 336666, 11031254, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (963, 'Absolute Zero', 76, 1, 1, 'Mike Bordin, Billy Gould, Mike Patton', 181995, 5929427, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (964, 'From Out Of Nowhere', 77, 1, 4, 'Faith No More', 202527, 6587802, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (965, 'Epic', 77, 1, 4, 'Faith No More', 294008, 9631296, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (966, 'Falling To Pieces', 77, 1, 4, 'Faith No More', 316055, 10333123, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (967, 'Surprise! You\'re Dead!', 77, 1, 4, 'Faith No More', 147226, 4823036, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (968, 'Zombie Eaters', 77, 1, 4, 'Faith No More', 360881, 11835367, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (969, 'The Real Thing', 77, 1, 4, 'Faith No More', 493635, 16233080, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (970, 'Underwater Love', 77, 1, 4, 'Faith No More', 231993, 7634387, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (971, 'The Morning After', 77, 1, 4, 'Faith No More', 223764, 7355898, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (972, 'Woodpecker From Mars', 77, 1, 4, 'Faith No More', 340532, 11174250, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (973, 'War Pigs', 77, 1, 4, 'Tony Iommi, Bill Ward, Geezer Butler, Ozzy Osbourne', 464770, 15267802, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (974, 'Edge Of The World', 77, 1, 4, 'Faith No More', 250357, 8235607, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (975, 'Deixa Entrar', 78, 1, 7, 33619, 1095012, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (976, 'Falamansa Song', 78, 1, 7, 237165, 7921313, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (977, 'Xote Dos Milagres', 78, 1, 7, 269557, 8897778, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (978, 'Rindo À Toa', 78, 1, 7, 222066, 7365321, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (979, 'Confidência', 78, 1, 7, 222197, 7460829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (980, 'Forró De Tóquio', 78, 1, 7, 169273, 5588756, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (981, 'Zeca Violeiro', 78, 1, 7, 143673, 4781949, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (982, 'Avisa', 78, 1, 7, 355030, 11844320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (983, 'Principiando/Decolagem', 78, 1, 7, 116767, 3923789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (984, 'Asas', 78, 1, 7, 231915, 7711669, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (985, 'Medo De Escuro', 78, 1, 7, 213760, 7056323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (986, 'Oração', 78, 1, 7, 271072, 9003882, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (987, 'Minha Gata', 78, 1, 7, 181838, 6039502, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (988, 'Desaforo', 78, 1, 7, 174524, 5853561, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (989, 'In Your Honor', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 230191, 7468463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (990, 'No Way Back', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 196675, 6421400, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (991, 'Best Of You', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 255712, 8363467, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (992, 'DOA', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 252186, 8232342, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (993, 'Hell', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 117080, 3819255, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (994, 'The Last Song', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 199523, 6496742, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (995, 'Free Me', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 278700, 9109340, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (996, 'Resolve', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 288731, 9416186, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (997, 'The Deepest Blues Are Black', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 238419, 7735473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (998, 'End Over End', 79, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett', 352078, 11395296, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (999, 'Still', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 313182, 10323157, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1000, 'What If I Do?', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 302994, 9929799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1001, 'Miracle', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 209684, 6877994, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1002, 'Another Round', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 265848, 8752670, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1003, 'Friend Of A Friend', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 193280, 6355088, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1004, 'Over And Out', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 316264, 10428382, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1005, 'On The Mend', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 271908, 9071997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1006, 'Virginia Moon', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 229198, 7494639, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1007, 'Cold Day In The Sun', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 200724, 6596617, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1008, 'Razor', 80, 1, 1, 'Dave Grohl, Taylor Hawkins, Nate Mendel, Chris Shiflett/FOO FIGHTERS', 293276, 9721373, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1009, 'All My Life', 81, 1, 4, 'Foo Fighters', 263653, 8665545, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1010, 'Low', 81, 1, 4, 'Foo Fighters', 268120, 8847196, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1011, 'Have It All', 81, 1, 4, 'Foo Fighters', 298057, 9729292, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1012, 'Times Like These', 81, 1, 4, 'Foo Fighters', 266370, 8624691, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1013, 'Disenchanted Lullaby', 81, 1, 4, 'Foo Fighters', 273528, 8919111, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1014, 'Tired Of You', 81, 1, 4, 'Foo Fighters', 311353, 10094743, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1015, 'Halo', 81, 1, 4, 'Foo Fighters', 306442, 10026371, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1016, 'Lonely As You', 81, 1, 4, 'Foo Fighters', 277185, 9022628, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1017, 'Overdrive', 81, 1, 4, 'Foo Fighters', 270550, 8793187, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1018, 'Burn Away', 81, 1, 4, 'Foo Fighters', 298396, 9678073, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1019, 'Come Back', 81, 1, 4, 'Foo Fighters', 469968, 15371980, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1020, 'Doll', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 83487, 2702572, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1021, 'Monkey Wrench', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 231523, 7527531, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1022, 'Hey, Johnny Park!', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 248528, 8079480, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1023, 'My Poor Brain', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 213446, 6973746, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1024, 'Wind Up', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 152163, 4950667, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1025, 'Up In Arms', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 135732, 4406227, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1026, 'My Hero', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 260101, 8472365, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1027, 'See You', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 146782, 4888173, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1028, 'Enough Space', 82, 1, 1, 'Dave Grohl', 157387, 5169280, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1029, 'February Stars', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 289306, 9344875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1030, 'Everlong', 82, 1, 1, 'Dave Grohl', 250749, 8270816, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1031, 'Walking After You', 82, 1, 1, 'Dave Grohl', 303856, 9898992, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1032, 'New Way Home', 82, 1, 1, 'Dave, Taylor, Nate, Chris', 342230, 11205664, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1033, 'My Way', 83, 1, 12, 'claude françois/gilles thibault/jacques revaux/paul anka', 275879, 8928684, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1034, 'Strangers In The Night', 83, 1, 12, 'berthold kaempfert/charles singleton/eddie snyder', 155794, 5055295, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1035, 'New York, New York', 83, 1, 12, 'fred ebb/john kander', 206001, 6707993, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1036, 'I Get A Kick Out Of You', 83, 1, 12, 'cole porter', 194429, 6332441, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1037, 'Something Stupid', 83, 1, 12, 'carson c. parks', 158615, 5210643, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1038, 'Moon River', 83, 1, 12, 'henry mancini/johnny mercer', 198922, 6395808, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1039, 'What Now My Love', 83, 1, 12, 'carl sigman/gilbert becaud/pierre leroyer', 149995, 4913383, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1040, 'Summer Love', 83, 1, 12, 'hans bradtke/heinz meier/johnny mercer', 174994, 5693242, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1041, 'For Once In My Life', 83, 1, 12, 'orlando murden/ronald miller', 171154, 5557537, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1042, 'Love And Marriage', 83, 1, 12, 'jimmy van heusen/sammy cahn', 89730, 2930596, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1043, 'They Can\'t Take That Away From Me', 83, 1, 12, 'george gershwin/ira gershwin', 161227, 5240043, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1044, 'My Kind Of Town', 83, 1, 12, 'jimmy van heusen/sammy cahn', 188499, 6119915, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1045, 'Fly Me To The Moon', 83, 1, 12, 'bart howard', 149263, 4856954, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1046, 'I\'ve Got You Under My Skin', 83, 1, 12, 'cole porter', 210808, 6883787, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1047, 'The Best Is Yet To Come', 83, 1, 12, 'carolyn leigh/cy coleman', 173583, 5633730, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1048, 'It Was A Very Good Year', 83, 1, 12, 'ervin drake', 266605, 8554066, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1049, 'Come Fly With Me', 83, 1, 12, 'jimmy van heusen/sammy cahn', 190458, 6231029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1050, 'That\'s Life', 83, 1, 12, 'dean kay thompson/kelly gordon', 187010, 6095727, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1051, 'The Girl From Ipanema', 83, 1, 12, 'antonio carlos jobim/norman gimbel/vinicius de moraes', 193750, 6410674, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1052, 'The Lady Is A Tramp', 83, 1, 12, 'lorenz hart/richard rodgers', 184111, 5987372, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1053, 'Bad, Bad Leroy Brown', 83, 1, 12, 'jim croce', 169900, 5548581, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1054, 'Mack The Knife', 83, 1, 12, 'bert brecht/kurt weill/marc blitzstein', 292075, 9541052, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1055, 'Loves Been Good To Me', 83, 1, 12, 'rod mckuen', 203964, 6645365, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1056, 'L.A. Is My Lady', 83, 1, 12, 'alan bergman/marilyn bergman/peggy lipton jones/quincy jones', 193175, 6378511, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1057, 'Entrando Na Sua (Intro)', 84, 1, 7, 179252, 5840027, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1058, 'Nervosa', 84, 1, 7, 229537, 7680421, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1059, 'Funk De Bamba (Com Fernanda Abreu)', 84, 1, 7, 237191, 7866165, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1060, 'Call Me At Cleo´s', 84, 1, 7, 236617, 7920510, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1061, 'Olhos Coloridos (Com Sandra De Sá)', 84, 1, 7, 321332, 10567404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1062, 'Zambação', 84, 1, 7, 301113, 10030604, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1063, 'Funk Hum', 84, 1, 7, 244453, 8084475, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1064, 'Forty Days (Com DJ Hum)', 84, 1, 7, 221727, 7347172, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1065, 'Balada Da Paula', 84, 1, 7, 'Emerson Villani', 322821, 10603717, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1066, 'Dujji', 84, 1, 7, 324597, 10833935, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1067, 'Meu Guarda-Chuva', 84, 1, 7, 248528, 8216625, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1068, 'Motéis', 84, 1, 7, 213498, 7041077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1069, 'Whistle Stop', 84, 1, 7, 526132, 17533664, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1070, '16 Toneladas', 84, 1, 7, 191634, 6390885, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1071, 'Divirta-Se (Saindo Da Sua)', 84, 1, 7, 74919, 2439206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1072, 'Forty Days Instrumental', 84, 1, 7, 292493, 9584317, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1073, 'Óia Eu Aqui De Novo', 85, 1, 10, 219454, 7469735, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1074, 'Baião Da Penha', 85, 1, 10, 247928, 8393047, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1075, 'Esperando Na Janela', 85, 1, 10, 'Manuca/Raimundinho DoAcordion/Targino Godim', 261041, 8660617, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1076, 'Juazeiro', 85, 1, 10, 'Humberto Teixeira/Luiz Gonzaga', 222275, 7349779, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1077, 'Último Pau-De-Arara', 85, 1, 10, 'Corumbá/José Gumarães/Venancio', 200437, 6638563, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1078, 'Asa Branca', 85, 1, 10, 'Humberto Teixeira/Luiz Gonzaga', 217051, 7387183, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1079, 'Qui Nem Jiló', 85, 1, 10, 'Humberto Teixeira/Luiz Gonzaga', 204695, 6937472, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1080, 'Assum Preto', 85, 1, 10, 'Humberto Teixeira/Luiz Gonzaga', 199653, 6625000, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1081, 'Pau-De-Arara', 85, 1, 10, 'Guio De Morais E Seus "Parentes"/Luiz Gonzaga', 191660, 6340649, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1082, 'A Volta Da Asa Branca', 85, 1, 10, 'Luiz Gonzaga/Zé Dantas', 271020, 9098093, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1083, 'O Amor Daqui De Casa', 85, 1, 10, 'Gilberto Gil', 148636, 4888292, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1084, 'As Pegadas Do Amor', 85, 1, 10, 'Gilberto Gil', 209136, 6899062, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1085, 'Lamento Sertanejo', 85, 1, 10, 'Dominguinhos/Gilberto Gil', 260963, 8518290, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1086, 'Casinha Feliz', 85, 1, 10, 'Gilberto Gil', 32287, 1039615, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1087, 'Introdução (Live)', 86, 1, 7, 154096, 5227579, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1088, 'Palco (Live)', 86, 1, 7, 238315, 8026622, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1089, 'Is This Love (Live)', 86, 1, 7, 295262, 9819759, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1090, 'Stir It Up (Live)', 86, 1, 7, 282409, 9594738, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1091, 'Refavela (Live)', 86, 1, 7, 236695, 7985305, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1092, 'Vendedor De Caranguejo (Live)', 86, 1, 7, 248842, 8358128, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1093, 'Quanta (Live)', 86, 1, 7, 357485, 11774865, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1094, 'Estrela (Live)', 86, 1, 7, 285309, 9436411, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1095, 'Pela Internet (Live)', 86, 1, 7, 263471, 8804401, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1096, 'Cérebro Eletrônico (Live)', 86, 1, 7, 231627, 7805352, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1097, 'Opachorô (Live)', 86, 1, 7, 259526, 8596384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1098, 'Copacabana (Live)', 86, 1, 7, 289671, 9673672, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1099, 'A Novidade (Live)', 86, 1, 7, 316969, 10508000, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1100, 'Ghandi (Live)', 86, 1, 7, 222458, 7481950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1101, 'De Ouro E Marfim (Live)', 86, 1, 7, 234971, 7838453, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1102, 'Doce De Carnaval (Candy All)', 87, 1, 2, 356101, 11998470, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1103, 'Lamento De Carnaval', 87, 1, 2, 294530, 9819276, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1104, 'Pretinha', 87, 1, 2, 265273, 8914579, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1105, 'A Novidade', 73, 1, 7, 'Gilberto Gil', 324780, 10765600, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1106, 'Tenho Sede', 73, 1, 7, 'Gilberto Gil', 261616, 8708114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1107, 'Refazenda', 73, 1, 7, 'Gilberto Gil', 218305, 7237784, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1108, 'Realce', 73, 1, 7, 'Gilberto Gil', 264489, 8847612, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1109, 'Esotérico', 73, 1, 7, 'Gilberto Gil', 317779, 10530533, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1110, 'Drão', 73, 1, 7, 'Gilberto Gil', 301453, 9931950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1111, 'A Paz', 73, 1, 7, 'Gilberto Gil', 293093, 9593064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1112, 'Beira Mar', 73, 1, 7, 'Gilberto Gil', 295444, 9597994, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1113, 'Sampa', 73, 1, 7, 'Gilberto Gil', 225697, 7469905, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1114, 'Parabolicamará', 73, 1, 7, 'Gilberto Gil', 284943, 9543435, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1115, 'Tempo Rei', 73, 1, 7, 'Gilberto Gil', 302733, 10019269, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1116, 'Expresso 2222', 73, 1, 7, 'Gilberto Gil', 284760, 9690577, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1117, 'Aquele Abraço', 73, 1, 7, 'Gilberto Gil', 263993, 8805003, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1118, 'Palco', 73, 1, 7, 'Gilberto Gil', 270550, 9049901, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1119, 'Toda Menina Baiana', 73, 1, 7, 'Gilberto Gil', 278177, 9351000, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1120, 'Sítio Do Pica-Pau Amarelo', 73, 1, 7, 'Gilberto Gil', 218070, 7217955, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1121, 'Straight Out Of Line', 88, 1, 3, 'Sully Erna', 259213, 8511877, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1122, 'Faceless', 88, 1, 3, 'Sully Erna', 216006, 6992417, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1123, 'Changes', 88, 1, 3, 'Sully Erna; Tony Rombola', 260022, 8455835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1124, 'Make Me Believe', 88, 1, 3, 'Sully Erna', 248607, 8075050, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1125, 'I Stand Alone', 88, 1, 3, 'Sully Erna', 246125, 8017041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1126, 'Re-Align', 88, 1, 3, 'Sully Erna', 260884, 8513891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1127, 'I Fucking Hate You', 88, 1, 3, 'Sully Erna', 247170, 8059642, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1128, 'Releasing The Demons', 88, 1, 3, 'Sully Erna', 252760, 8276372, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1129, 'Dead And Broken', 88, 1, 3, 'Sully Erna', 251454, 8206611, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1130, 'I Am', 88, 1, 3, 'Sully Erna', 239516, 7803270, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1131, 'The Awakening', 88, 1, 3, 'Sully Erna', 89547, 3035251, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1132, 'Serenity', 88, 1, 3, 'Sully Erna; Tony Rombola', 274834, 9172976, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1133, 'American Idiot', 89, 1, 4, 'Billie Joe Armstrong, Mike Dirnt, Tré Cool', 174419, 5705793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1134, 'Jesus Of Suburbia / City Of The Damned / I Don\'t Care / Dearly Beloved / Tales Of Another Broken Home', 89, 1, 4, 'Billie Joe Armstrong/Green Day', 548336, 17875209, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1135, 'Holiday', 89, 1, 4, 'Billie Joe Armstrong, Mike Dirnt, Tré Cool', 232724, 7599602, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1136, 'Boulevard Of Broken Dreams', 89, 1, 4, 'Mike Dint, Billie Joe, Tré Cool', 260858, 8485122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1137, 'Are We The Waiting', 89, 1, 4, 'Green Day', 163004, 5328329, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1138, 'St. Jimmy', 89, 1, 4, 'Green Day', 175307, 5716589, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1139, 'Give Me Novacaine', 89, 1, 4, 'Green Day', 205871, 6752485, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1140, 'She\'s A Rebel', 89, 1, 4, 'Green Day', 120528, 3901226, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1141, 'Extraordinary Girl', 89, 1, 4, 'Green Day', 214021, 6975177, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1142, 'Letterbomb', 89, 1, 4, 'Green Day', 246151, 7980902, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1143, 'Wake Me Up When September Ends', 89, 1, 4, 'Mike Dint, Billie Joe, Tré Cool', 285753, 9325597, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1144, 'Homecoming / The Death Of St. Jimmy / East 12th St. / Nobody Likes You / Rock And Roll Girlfriend / We\'re Coming Home Again', 89, 1, 4, 'Mike Dirnt/Tré Cool', 558602, 18139840, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1145, 'Whatsername', 89, 1, 4, 'Green Day', 252316, 8244843, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1146, 'Welcome to the Jungle', 90, 2, 1, 273552, 4538451, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1147, 'It\'s So Easy', 90, 2, 1, 202824, 3394019, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1148, 'Nightrain', 90, 2, 1, 268537, 4457283, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1149, 'Out Ta Get Me', 90, 2, 1, 263893, 4382147, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1150, 'Mr. Brownstone', 90, 2, 1, 228924, 3816323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1151, 'Paradise City', 90, 2, 1, 406347, 6687123, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1152, 'My Michelle', 90, 2, 1, 219961, 3671299, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1153, 'Think About You', 90, 2, 1, 231640, 3860275, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1154, 'Sweet Child O\' Mine', 90, 2, 1, 356424, 5879347, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1155, 'You\'re Crazy', 90, 2, 1, 197135, 3301971, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1156, 'Anything Goes', 90, 2, 1, 206400, 3451891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1157, 'Rocket Queen', 90, 2, 1, 375349, 6185539, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1158, 'Right Next Door to Hell', 91, 2, 1, 182321, 3175950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1159, 'Dust N\' Bones', 91, 2, 1, 298374, 5053742, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1160, 'Live and Let Die', 91, 2, 1, 184016, 3203390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1161, 'Don\'t Cry (Original)', 91, 2, 1, 284744, 4833259, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1162, 'Perfect Crime', 91, 2, 1, 143637, 2550030, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1163, 'You Ain\'t the First', 91, 2, 1, 156268, 2754414, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1164, 'Bad Obsession', 91, 2, 1, 328282, 5537678, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1165, 'Back off Bitch', 91, 2, 1, 303436, 5135662, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1166, 'Double Talkin\' Jive', 91, 2, 1, 203637, 3520862, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1167, 'November Rain', 91, 2, 1, 537540, 8923566, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1168, 'The Garden', 91, 2, 1, 322175, 5438862, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1169, 'Garden of Eden', 91, 2, 1, 161539, 2839694, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1170, 'Don\'t Damn Me', 91, 2, 1, 318901, 5385886, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1171, 'Bad Apples', 91, 2, 1, 268351, 4567966, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1172, 'Dead Horse', 91, 2, 1, 257600, 4394014, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1173, 'Coma', 91, 2, 1, 616511, 10201342, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1174, 'Civil War', 92, 1, 3, 'Duff McKagan/Slash/W. Axl Rose', 461165, 15046579, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1175, '14 Years', 92, 1, 3, 'Izzy Stradlin\'/W. Axl Rose', 261355, 8543664, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1176, 'Yesterdays', 92, 1, 3, 'Billy/Del James/W. Axl Rose/West Arkeen', 196205, 6398489, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1177, 'Knockin\' On Heaven\'s Door', 92, 1, 3, 'Bob Dylan', 336457, 10986716, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1178, 'Get In The Ring', 92, 1, 3, 'Duff McKagan/Slash/W. Axl Rose', 341054, 11134105, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1179, 'Shotgun Blues', 92, 1, 3, 'W. Axl Rose', 203206, 6623916, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1180, 'Breakdown', 92, 1, 3, 'W. Axl Rose', 424960, 13978284, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1181, 'Pretty Tied Up', 92, 1, 3, 'Izzy Stradlin\'', 287477, 9408754, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1182, 'Locomotive', 92, 1, 3, 'Slash/W. Axl Rose', 522396, 17236842, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1183, 'So Fine', 92, 1, 3, 'Duff McKagan', 246491, 8039484, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1184, 'Estranged', 92, 1, 3, 'W. Axl Rose', 563800, 18343787, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1185, 'You Could Be Mine', 92, 1, 3, 'Izzy Stradlin\'/W. Axl Rose', 343875, 11207355, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1186, 'Don\'t Cry', 92, 1, 3, 'Izzy Stradlin\'/W. Axl Rose', 284238, 9222458, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1187, 'My World', 92, 1, 3, 'W. Axl Rose', 84532, 2764045, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1188, 'Colibri', 93, 1, 2, 'Richard Bull', 361012, 12055329, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1189, 'Love Is The Colour', 93, 1, 2, 'R. Carless', 251585, 8419165, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1190, 'Magnetic Ocean', 93, 1, 2, 'Patrick Claher/Richard Bull', 321123, 10720741, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1191, 'Deep Waters', 93, 1, 2, 'Richard Bull', 396460, 13075359, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1192, 'L\'Arc En Ciel De Miles', 93, 1, 2, 'Kevin Robinson/Richard Bull', 242390, 8053997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1193, 'Gypsy', 93, 1, 2, 'Kevin Robinson', 330997, 11083374, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1194, 'Journey Into Sunlight', 93, 1, 2, 'Jean Paul Maunick', 249756, 8241177, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1195, 'Sunchild', 93, 1, 2, 'Graham Harvey', 259970, 8593143, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1196, 'Millenium', 93, 1, 2, 'Maxton Gig Beesley Jnr.', 379167, 12511939, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1197, 'Thinking \'Bout Tomorrow', 93, 1, 2, 'Fayyaz Virgi/Richard Bull', 355395, 11865384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1198, 'Jacob\'s Ladder', 93, 1, 2, 'Julian Crampton', 367647, 12201595, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1199, 'She Wears Black', 93, 1, 2, 'G Harvey/R Hope-Taylor', 528666, 17617944, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1200, 'Dark Side Of The Cog', 93, 1, 2, 'Jean Paul Maunick', 377155, 12491122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1201, 'Different World', 94, 2, 1, 258692, 4383764, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1202, 'These Colours Don\'t Run', 94, 2, 1, 412152, 6883500, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1203, 'Brighter Than a Thousand Suns', 94, 2, 1, 526255, 8721490, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1204, 'The Pilgrim', 94, 2, 1, 307593, 5172144, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1205, 'The Longest Day', 94, 2, 1, 467810, 7785748, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1206, 'Out of the Shadows', 94, 2, 1, 336896, 5647303, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1207, 'The Reincarnation of Benjamin Breeg', 94, 2, 1, 442106, 7367736, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1208, 'For the Greater Good of God', 94, 2, 1, 564893, 9367328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1209, 'Lord of Light', 94, 2, 1, 444614, 7393698, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1210, 'The Legacy', 94, 2, 1, 562966, 9314287, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1211, 'Hallowed Be Thy Name (Live) [Non Album Bonus Track]', 94, 2, 1, 431262, 7205816, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1212, 'The Number Of The Beast', 95, 1, 3, 'Steve Harris', 294635, 4718897, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1213, 'The Trooper', 95, 1, 3, 'Steve Harris', 235311, 3766272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1214, 'Prowler', 95, 1, 3, 'Steve Harris', 255634, 4091904, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1215, 'Transylvania', 95, 1, 3, 'Steve Harris', 265874, 4255744, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1216, 'Remember Tomorrow', 95, 1, 3, 'Paul Di\'Anno/Steve Harris', 352731, 5648438, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1217, 'Where Eagles Dare', 95, 1, 3, 'Steve Harris', 289358, 4630528, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1218, 'Sanctuary', 95, 1, 3, 'David Murray/Paul Di\'Anno/Steve Harris', 293250, 4694016, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1219, 'Running Free', 95, 1, 3, 'Paul Di\'Anno/Steve Harris', 228937, 3663872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1220, 'Run To The Hilss', 95, 1, 3, 'Steve Harris', 237557, 3803136, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1221, '2 Minutes To Midnight', 95, 1, 3, 'Adrian Smith/Bruce Dickinson', 337423, 5400576, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1222, 'Iron Maiden', 95, 1, 3, 'Steve Harris', 324623, 5195776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1223, 'Hallowed Be Thy Name', 95, 1, 3, 'Steve Harris', 471849, 7550976, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1224, 'Be Quick Or Be Dead', 96, 1, 3, 'Bruce Dickinson/Janick Gers', 196911, 3151872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1225, 'From Here To Eternity', 96, 1, 3, 'Steve Harris', 259866, 4159488, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1226, 'Can I Play With Madness', 96, 1, 3, 'Adrian Smith/Bruce Dickinson/Steve Harris', 282488, 4521984, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1227, 'Wasting Love', 96, 1, 3, 'Bruce Dickinson/Janick Gers', 347846, 5566464, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1228, 'Tailgunner', 96, 1, 3, 'Bruce Dickinson/Steve Harris', 249469, 3993600, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1229, 'The Evil That Men Do', 96, 1, 3, 'Adrian Smith/Bruce Dickinson/Steve Harris', 325929, 5216256, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1230, 'Afraid To Shoot Strangers', 96, 1, 3, 'Steve Harris', 407980, 6529024, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1231, 'Bring Your Daughter... To The Slaughter', 96, 1, 3, 'Bruce Dickinson', 317727, 5085184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1232, 'Heaven Can Wait', 96, 1, 3, 'Steve Harris', 448574, 7178240, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1233, 'The Clairvoyant', 96, 1, 3, 'Steve Harris', 269871, 4319232, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1234, 'Fear Of The Dark', 96, 1, 3, 'Steve Harris', 431333, 6906078, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1235, 'The Wicker Man', 97, 1, 1, 'Adrian Smith/Bruce Dickinson/Steve Harris', 275539, 11022464, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1236, 'Ghost Of The Navigator', 97, 1, 1, 'Bruce Dickinson/Janick Gers/Steve Harris', 410070, 16404608, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1237, 'Brave New World', 97, 1, 1, 'Bruce Dickinson/David Murray/Steve Harris', 378984, 15161472, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1238, 'Blood Brothers', 97, 1, 1, 'Steve Harris', 434442, 17379456, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1239, 'The Mercenary', 97, 1, 1, 'Janick Gers/Steve Harris', 282488, 11300992, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1240, 'Dream Of Mirrors', 97, 1, 1, 'Janick Gers/Steve Harris', 561162, 22448256, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1241, 'The Fallen Angel', 97, 1, 1, 'Adrian Smith/Steve Harris', 240718, 9629824, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1242, 'The Nomad', 97, 1, 1, 'David Murray/Steve Harris', 546115, 21846144, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1243, 'Out Of The Silent Planet', 97, 1, 1, 'Bruce Dickinson/Janick Gers/Steve Harris', 385541, 15423616, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1244, 'The Thin Line Between Love & Hate', 97, 1, 1, 'David Murray/Steve Harris', 506801, 20273280, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1245, 'Wildest Dreams', 98, 1, 13, 'Adrian Smith/Steve Harris', 232777, 9312384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1246, 'Rainmaker', 98, 1, 13, 'Bruce Dickinson/David Murray/Steve Harris', 228623, 9146496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1247, 'No More Lies', 98, 1, 13, 'Steve Harris', 441782, 17672320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1248, 'Montsegur', 98, 1, 13, 'Bruce Dickinson/Janick Gers/Steve Harris', 350484, 14020736, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1249, 'Dance Of Death', 98, 1, 13, 'Janick Gers/Steve Harris', 516649, 20670727, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1250, 'Gates Of Tomorrow', 98, 1, 13, 'Bruce Dickinson/Janick Gers/Steve Harris', 312032, 12482688, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1251, 'New Frontier', 98, 1, 13, 'Adrian Smith/Bruce Dickinson/Nicko McBrain', 304509, 12181632, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1252, 'Paschendale', 98, 1, 13, 'Adrian Smith/Steve Harris', 508107, 20326528, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1253, 'Face In The Sand', 98, 1, 13, 'Adrian Smith/Bruce Dickinson/Steve Harris', 391105, 15648948, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1254, 'Age Of Innocence', 98, 1, 13, 'David Murray/Steve Harris', 370468, 14823478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1255, 'Journeyman', 98, 1, 13, 'Bruce Dickinson/David Murray/Steve Harris', 427023, 17082496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1256, 'Be Quick Or Be Dead', 99, 1, 1, 'Bruce Dickinson/Janick Gers', 204512, 8181888, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1257, 'From Here To Eternity', 99, 1, 1, 'Steve Harris', 218357, 8739038, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1258, 'Afraid To Shoot Strangers', 99, 1, 1, 'Steve Harris', 416496, 16664589, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1259, 'Fear Is The Key', 99, 1, 1, 'Bruce Dickinson/Janick Gers', 335307, 13414528, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1260, 'Childhood\'s End', 99, 1, 1, 'Steve Harris', 280607, 11225216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1261, 'Wasting Love', 99, 1, 1, 'Bruce Dickinson/Janick Gers', 350981, 14041216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1262, 'The Fugitive', 99, 1, 1, 'Steve Harris', 294112, 11765888, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1263, 'Chains Of Misery', 99, 1, 1, 'Bruce Dickinson/David Murray', 217443, 8700032, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1264, 'The Apparition', 99, 1, 1, 'Janick Gers/Steve Harris', 234605, 9386112, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1265, 'Judas Be My Guide', 99, 1, 1, 'Bruce Dickinson/David Murray', 188786, 7553152, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1266, 'Weekend Warrior', 99, 1, 1, 'Janick Gers/Steve Harris', 339748, 13594678, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1267, 'Fear Of The Dark', 99, 1, 1, 'Steve Harris', 436976, 17483789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1268, '01 - Prowler', 100, 1, 6, 'Steve Harris', 236173, 5668992, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1269, '02 - Sanctuary', 100, 1, 6, 'David Murray/Paul Di\'Anno/Steve Harris', 196284, 4712576, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1270, '03 - Remember Tomorrow', 100, 1, 6, 'Harris/Paul Di´Anno', 328620, 7889024, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1271, '04 - Running Free', 100, 1, 6, 'Harris/Paul Di´Anno', 197276, 4739122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1272, '05 - Phantom of the Opera', 100, 1, 6, 'Steve Harris', 428016, 10276872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1273, '06 - Transylvania', 100, 1, 6, 'Steve Harris', 259343, 6226048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1274, '07 - Strange World', 100, 1, 6, 'Steve Harris', 332460, 7981184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1275, '08 - Charlotte the Harlot', 100, 1, 6, 'Murray  Dave', 252708, 6066304, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1276, '09 - Iron Maiden', 100, 1, 6, 'Steve Harris', 216058, 5189891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1277, 'The Ides Of March', 101, 1, 13, 'Steve Harris', 105926, 2543744, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1278, 'Wrathchild', 101, 1, 13, 'Steve Harris', 174471, 4188288, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1279, 'Murders In The Rue Morgue', 101, 1, 13, 'Steve Harris', 258377, 6205786, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1280, 'Another Life', 101, 1, 13, 'Steve Harris', 203049, 4874368, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1281, 'Genghis Khan', 101, 1, 13, 'Steve Harris', 187141, 4493440, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1282, 'Innocent Exile', 101, 1, 13, 'Di´Anno/Harris', 232515, 5584861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1283, 'Killers', 101, 1, 13, 'Steve Harris', 300956, 7227440, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1284, 'Prodigal Son', 101, 1, 13, 'Steve Harris', 372349, 8937600, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1285, 'Purgatory', 101, 1, 13, 'Steve Harris', 200150, 4804736, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1286, 'Drifter', 101, 1, 13, 'Steve Harris', 288757, 6934660, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1287, 'Intro- Churchill S Speech', 102, 1, 13, 48013, 1154488, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1288, 'Aces High', 102, 1, 13, 276375, 6635187, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1289, '2 Minutes To Midnight', 102, 1, 3, 'Smith/Dickinson', 366550, 8799380, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1290, 'The Trooper', 102, 1, 3, 'Harris', 268878, 6455255, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1291, 'Revelations', 102, 1, 3, 'Dickinson', 371826, 8926021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1292, 'Flight Of Icarus', 102, 1, 3, 'Smith/Dickinson', 229982, 5521744, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1293, 'Rime Of The Ancient Mariner', 102, 1, 3, 789472, 18949518, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1294, 'Powerslave', 102, 1, 3, 454974, 10921567, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1295, 'The Number Of The Beast', 102, 1, 3, 'Harris', 275121, 6605094, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1296, 'Hallowed Be Thy Name', 102, 1, 3, 'Harris', 451422, 10836304, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1297, 'Iron Maiden', 102, 1, 3, 'Harris', 261955, 6289117, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1298, 'Run To The Hills', 102, 1, 3, 'Harris', 231627, 5561241, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1299, 'Running Free', 102, 1, 3, 'Harris/Di Anno', 204617, 4912986, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1300, 'Wrathchild', 102, 1, 13, 'Steve Harris', 183666, 4410181, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1301, 'Acacia Avenue', 102, 1, 13, 379872, 9119118, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1302, 'Children Of The Damned', 102, 1, 13, 'Steve Harris', 278177, 6678446, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1303, 'Die With Your Boots On', 102, 1, 13, 'Adrian Smith/Bruce Dickinson/Steve Harris', 314174, 7542367, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1304, 'Phantom Of The Opera', 102, 1, 13, 'Steve Harris', 441155, 10589917, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1305, 'Be Quick Or Be Dead', 103, 1, 1, 233142, 5599853, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1306, 'The Number Of The Beast', 103, 1, 1, 294008, 7060625, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1307, 'Wrathchild', 103, 1, 1, 174106, 4182963, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1308, 'From Here To Eternity', 103, 1, 1, 284447, 6831163, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1309, 'Can I Play With Madness', 103, 1, 1, 213106, 5118995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1310, 'Wasting Love', 103, 1, 1, 336953, 8091301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1311, 'Tailgunner', 103, 1, 1, 247640, 5947795, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1312, 'The Evil That Men Do', 103, 1, 1, 478145, 11479913, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1313, 'Afraid To Shoot Strangers', 103, 1, 1, 412525, 9905048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1314, 'Fear Of The Dark', 103, 1, 1, 431542, 10361452, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1315, 'Bring Your Daughter... To The Slaughter...', 104, 1, 1, 376711, 9045532, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1316, 'The Clairvoyant', 104, 1, 1, 262426, 6302648, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1317, 'Heaven Can Wait', 104, 1, 1, 440555, 10577743, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1318, 'Run To The Hills', 104, 1, 1, 235859, 5665052, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1319, '2 Minutes To Midnight', 104, 1, 1, 'Adrian Smith/Bruce Dickinson', 338233, 8122030, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1320, 'Iron Maiden', 104, 1, 1, 494602, 11874875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1321, 'Hallowed Be Thy Name', 104, 1, 1, 447791, 10751410, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1322, 'The Trooper', 104, 1, 1, 232672, 5588560, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1323, 'Sanctuary', 104, 1, 1, 318511, 7648679, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1324, 'Running Free', 104, 1, 1, 474017, 11380851, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1325, 'Tailgunner', 105, 1, 3, 'Bruce Dickinson/Steve Harris', 255582, 4089856, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1326, 'Holy Smoke', 105, 1, 3, 'Bruce Dickinson/Steve Harris', 229459, 3672064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1327, 'No Prayer For The Dying', 105, 1, 3, 'Steve Harris', 263941, 4225024, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1328, 'Public Enema Number One', 105, 1, 3, 'Bruce Dickinson/David Murray', 254197, 4071587, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1329, 'Fates Warning', 105, 1, 3, 'David Murray/Steve Harris', 250853, 4018088, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1330, 'The Assassin', 105, 1, 3, 'Steve Harris', 258768, 4141056, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1331, 'Run Silent Run Deep', 105, 1, 3, 'Bruce Dickinson/Steve Harris', 275408, 4407296, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1332, 'Hooks In You', 105, 1, 3, 'Adrian Smith/Bruce Dickinson', 247510, 3960832, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1333, 'Bring Your Daughter... ...To The Slaughter', 105, 1, 3, 'Bruce Dickinson', 284238, 4548608, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1334, 'Mother Russia', 105, 1, 3, 'Steve Harris', 332617, 5322752, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1335, 'Where Eagles Dare', 106, 1, 3, 'Steve Harris', 369554, 5914624, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1336, 'Revelations', 106, 1, 3, 'Bruce Dickinson', 408607, 6539264, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1337, 'Flight Of The Icarus', 106, 1, 3, 'Adrian Smith/Bruce Dickinson', 230269, 3686400, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1338, 'Die With Your Boots On', 106, 1, 3, 'Adrian Smith/Bruce Dickinson/Steve Harris', 325694, 5212160, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1339, 'The Trooper', 106, 1, 3, 'Steve Harris', 251454, 4024320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1340, 'Still Life', 106, 1, 3, 'David Murray/Steve Harris', 294347, 4710400, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1341, 'Quest For Fire', 106, 1, 3, 'Steve Harris', 221309, 3543040, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1342, 'Sun And Steel', 106, 1, 3, 'Adrian Smith/Bruce Dickinson', 206367, 3306324, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1343, 'To Tame A Land', 106, 1, 3, 'Steve Harris', 445283, 7129264, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1344, 'Aces High', 107, 1, 3, 'Harris', 269531, 6472088, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1345, '2 Minutes To Midnight', 107, 1, 3, 'Smith/Dickinson', 359810, 8638809, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1346, 'Losfer Words', 107, 1, 3, 'Steve Harris', 252891, 6074756, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1347, 'Flash of The Blade', 107, 1, 3, 'Dickinson', 242729, 5828861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1348, 'Duelists', 107, 1, 3, 'Steve Harris', 366471, 8800686, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1349, 'Back in the Village', 107, 1, 3, 'Dickinson/Smith', 320548, 7696518, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1350, 'Powerslave', 107, 1, 3, 'Dickinson', 407823, 9791106, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1351, 'Rime of the Ancient Mariner', 107, 1, 3, 'Harris', 816509, 19599577, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1352, 'Intro', 108, 1, 3, 115931, 4638848, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1353, 'The Wicker Man', 108, 1, 3, 'Adrian Smith/Bruce Dickinson/Steve Harris', 281782, 11272320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1354, 'Ghost Of The Navigator', 108, 1, 3, 'Bruce Dickinson/Janick Gers/Steve Harris', 408607, 16345216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1355, 'Brave New World', 108, 1, 3, 'Bruce Dickinson/David Murray/Steve Harris', 366785, 14676148, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1356, 'Wrathchild', 108, 1, 3, 'Steve Harris', 185808, 7434368, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1357, '2 Minutes To Midnight', 108, 1, 3, 'Adrian Smith/Bruce Dickinson', 386821, 15474816, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1358, 'Blood Brothers', 108, 1, 3, 'Steve Harris', 435513, 17422464, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1359, 'Sign Of The Cross', 108, 1, 3, 'Steve Harris', 649116, 25966720, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1360, 'The Mercenary', 108, 1, 3, 'Janick Gers/Steve Harris', 282697, 11309184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1361, 'The Trooper', 108, 1, 3, 'Steve Harris', 273528, 10942592, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1362, 'Dream Of Mirrors', 109, 1, 1, 'Janick Gers/Steve Harris', 578324, 23134336, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1363, 'The Clansman', 109, 1, 1, 'Steve Harris', 559203, 22370432, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1364, 'The Evil That Men Do', 109, 1, 3, 'Adrian Smith/Bruce Dickinson/Steve Harris', 280737, 11231360, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1365, 'Fear Of The Dark', 109, 1, 1, 'Steve Harris', 460695, 18430080, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1366, 'Iron Maiden', 109, 1, 1, 'Steve Harris', 351869, 14076032, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1367, 'The Number Of The Beast', 109, 1, 1, 'Steve Harris', 300434, 12022107, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1368, 'Hallowed Be Thy Name', 109, 1, 1, 'Steve Harris', 443977, 17760384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1369, 'Sanctuary', 109, 1, 1, 'David Murray/Paul Di\'Anno/Steve Harris', 317335, 12695680, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1370, 'Run To The Hills', 109, 1, 1, 'Steve Harris', 292179, 11688064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1371, 'Moonchild', 110, 1, 3, 'Adrian Smith; Bruce Dickinson', 340767, 8179151, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1372, 'Infinite Dreams', 110, 1, 3, 'Steve Harris', 369005, 8858669, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1373, 'Can I Play With Madness', 110, 1, 3, 'Adrian Smith; Bruce Dickinson; Steve Harris', 211043, 5067867, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1374, 'The Evil That Men Do', 110, 1, 3, 'Adrian Smith; Bruce Dickinson; Steve Harris', 273998, 6578930, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1375, 'Seventh Son of a Seventh Son', 110, 1, 3, 'Steve Harris', 593580, 14249000, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1376, 'The Prophecy', 110, 1, 3, 'Dave Murray; Steve Harris', 305475, 7334450, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1377, 'The Clairvoyant', 110, 1, 3, 'Adrian Smith; Bruce Dickinson; Steve Harris', 267023, 6411510, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1378, 'Only the Good Die Young', 110, 1, 3, 'Bruce Dickinson; Harris', 280894, 6744431, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1379, 'Caught Somewhere in Time', 111, 1, 3, 'Steve Harris', 445779, 10701149, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1380, 'Wasted Years', 111, 1, 3, 'Adrian Smith', 307565, 7384358, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1381, 'Sea of Madness', 111, 1, 3, 'Adrian Smith', 341995, 8210695, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1382, 'Heaven Can Wait', 111, 1, 3, 'Steve Harris', 441417, 10596431, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1383, 'Stranger in a Strange Land', 111, 1, 3, 'Adrian Smith', 344502, 8270899, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1384, 'Alexander the Great', 111, 1, 3, 'Steve Harris', 515631, 12377742, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1385, 'De Ja Vu', 111, 1, 3, 'David Murray/Steve Harris', 296176, 7113035, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1386, 'The Loneliness of the Long Dis', 111, 1, 3, 'Steve Harris', 391314, 9393598, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1387, '22 Acacia Avenue', 112, 1, 3, 'Adrian Smith/Steve Harris', 395572, 5542516, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1388, 'Children of the Damned', 112, 1, 3, 'Steve Harris', 274364, 3845631, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1389, 'Gangland', 112, 1, 3, 'Adrian Smith/Clive Burr/Steve Harris', 228440, 3202866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1390, 'Hallowed Be Thy Name', 112, 1, 3, 'Steve Harris', 428669, 6006107, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1391, 'Invaders', 112, 1, 3, 'Steve Harris', 203180, 2849181, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1392, 'Run to the Hills', 112, 1, 3, 'Steve Harris', 228884, 3209124, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1393, 'The Number Of The Beast', 112, 1, 1, 'Steve Harris', 293407, 11737216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1394, 'The Prisoner', 112, 1, 3, 'Adrian Smith/Steve Harris', 361299, 5062906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1395, 'Sign Of The Cross', 113, 1, 1, 'Steve Harris', 678008, 27121792, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1396, 'Lord Of The Flies', 113, 1, 1, 'Janick Gers/Steve Harris', 303699, 12148864, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1397, 'Man On The Edge', 113, 1, 1, 'Blaze Bayley/Janick Gers', 253413, 10137728, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1398, 'Fortunes Of War', 113, 1, 1, 'Steve Harris', 443977, 17760384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1399, 'Look For The Truth', 113, 1, 1, 'Blaze Bayley/Janick Gers/Steve Harris', 310230, 12411008, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1400, 'The Aftermath', 113, 1, 1, 'Blaze Bayley/Janick Gers/Steve Harris', 380786, 15233152, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1401, 'Judgement Of Heaven', 113, 1, 1, 'Steve Harris', 312476, 12501120, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1402, 'Blood On The World\'s Hands', 113, 1, 1, 'Steve Harris', 357799, 14313600, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1403, 'The Edge Of Darkness', 113, 1, 1, 'Blaze Bayley/Janick Gers/Steve Harris', 399333, 15974528, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1404, '2 A.M.', 113, 1, 1, 'Blaze Bayley/Janick Gers/Steve Harris', 337658, 13511087, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1405, 'The Unbeliever', 113, 1, 1, 'Janick Gers/Steve Harris', 490422, 19617920, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1406, 'Futureal', 114, 1, 1, 'Blaze Bayley/Steve Harris', 175777, 7032960, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1407, 'The Angel And The Gambler', 114, 1, 1, 'Steve Harris', 592744, 23711872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1408, 'Lightning Strikes Twice', 114, 1, 1, 'David Murray/Steve Harris', 290377, 11616384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1409, 'The Clansman', 114, 1, 1, 'Steve Harris', 539689, 21592327, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1410, 'When Two Worlds Collide', 114, 1, 1, 'Blaze Bayley/David Murray/Steve Harris', 377312, 15093888, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1411, 'The Educated Fool', 114, 1, 1, 'Steve Harris', 404767, 16191616, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1412, 'Don\'t Look To The Eyes Of A Stranger', 114, 1, 1, 'Steve Harris', 483657, 19347584, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1413, 'Como Estais Amigos', 114, 1, 1, 'Blaze Bayley/Janick Gers', 330292, 13213824, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1414, 'Please Please Please', 115, 1, 14, 'James Brown/Johnny Terry', 165067, 5394585, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1415, 'Think', 115, 1, 14, 'Lowman Pauling', 166739, 5513208, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1416, 'Night Train', 115, 1, 14, 'Jimmy Forrest/Lewis C. Simpkins/Oscar Washington', 212401, 7027377, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1417, 'Out Of Sight', 115, 1, 14, 'Ted Wright', 143725, 4711323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1418, 'Papa\'s Got A Brand New Bag Pt.1', 115, 1, 14, 'James Brown', 127399, 4174420, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1419, 'I Got You (I Feel Good)', 115, 1, 14, 'James Brown', 167392, 5468472, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1420, 'It\'s A Man\'s Man\'s Man\'s World', 115, 1, 14, 'Betty Newsome/James Brown', 168228, 5541611, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1421, 'Cold Sweat', 115, 1, 14, 'Alfred Ellis/James Brown', 172408, 5643213, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1422, 'Say It Loud, I\'m Black And I\'m Proud Pt.1', 115, 1, 14, 'Alfred Ellis/James Brown', 167392, 5478117, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1423, 'Get Up (I Feel Like Being A) Sex Machine', 115, 1, 14, 'Bobby Byrd/James Brown/Ron Lenhoff', 316551, 10498031, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1424, 'Hey America', 115, 1, 14, 'Addie William Jones/Nat Jones', 218226, 7187857, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1425, 'Make It Funky Pt.1', 115, 1, 14, 'Charles Bobbitt/James Brown', 196231, 6507782, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1426, 'I\'m A Greedy Man Pt.1', 115, 1, 14, 'Charles Bobbitt/James Brown', 217730, 7251211, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1427, 'Get On The Good Foot', 115, 1, 14, 'Fred Wesley/James Brown/Joseph Mims', 215902, 7182736, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1428, 'Get Up Offa That Thing', 115, 1, 14, 'Deanna Brown/Deidra Jenkins/Yamma Brown', 250723, 8355989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1429, 'It\'s Too Funky In Here', 115, 1, 14, 'Brad Shapiro/George Jackson/Robert Miller/Walter Shaw', 239072, 7973979, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1430, 'Living In America', 115, 1, 14, 'Charlie Midnight/Dan Hartman', 282880, 9432346, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1431, 'I\'m Real', 115, 1, 14, 'Full Force/James Brown', 334236, 11183457, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1432, 'Hot Pants Pt.1', 115, 1, 14, 'Fred Wesley/James Brown', 188212, 6295110, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1433, 'Soul Power (Live)', 115, 1, 14, 'James Brown', 260728, 8593206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1434, 'When You Gonna Learn (Digeridoo)', 116, 1, 1, 'Jay Kay/Kay, Jay', 230635, 7655482, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1435, 'Too Young To Die', 116, 1, 1, 'Smith, Toby', 365818, 12391660, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1436, 'Hooked Up', 116, 1, 1, 'Smith, Toby', 275879, 9301687, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1437, 'If I Like It, I Do It', 116, 1, 1, 'Gelder, Nick van', 293093, 9848207, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1438, 'Music Of The Wind', 116, 1, 1, 'Smith, Toby', 383033, 12870239, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1439, 'Emergency On Planet Earth', 116, 1, 1, 'Smith, Toby', 245263, 8117218, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1440, 'Whatever It Is, I Just Can\'t Stop', 116, 1, 1, 'Jay Kay/Kay, Jay', 247222, 8249453, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1441, 'Blow Your Mind', 116, 1, 1, 'Smith, Toby', 512339, 17089176, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1442, 'Revolution 1993', 116, 1, 1, 'Smith, Toby', 616829, 20816872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1443, 'Didgin\' Out', 116, 1, 1, 'Buchanan, Wallis', 157100, 5263555, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1444, 'Canned Heat', 117, 1, 14, 'Jay Kay', 331964, 11042037, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1445, 'Planet Home', 117, 1, 14, 'Jay Kay/Toby Smith', 284447, 9566237, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1446, 'Black Capricorn Day', 117, 1, 14, 'Jay Kay', 341629, 11477231, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1447, 'Soul Education', 117, 1, 14, 'Jay Kay/Toby Smith', 255477, 8575435, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1448, 'Failling', 117, 1, 14, 'Jay Kay/Toby Smith', 225227, 7503999, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1449, 'Destitute Illusions', 117, 1, 14, 'Derrick McKenzie/Jay Kay/Toby Smith', 340218, 11452651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1450, 'Supersonic', 117, 1, 14, 'Jay Kay', 315872, 10699265, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1451, 'Butterfly', 117, 1, 14, 'Jay Kay/Toby Smith', 268852, 8947356, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1452, 'Were Do We Go From Here', 117, 1, 14, 'Jay Kay', 313626, 10504158, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1453, 'King For A Day', 117, 1, 14, 'Jay Kay/Toby Smith', 221544, 7335693, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1454, 'Deeper Underground', 117, 1, 14, 'Toby Smith', 281808, 9351277, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1455, 'Just Another Story', 118, 1, 15, 'Toby Smith', 529684, 17582818, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1456, 'Stillness In Time', 118, 1, 15, 'Toby Smith', 257097, 8644290, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1457, 'Half The Man', 118, 1, 15, 'Toby Smith', 289854, 9577679, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1458, 'Light Years', 118, 1, 15, 'Toby Smith', 354560, 11796244, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1459, 'Manifest Destiny', 118, 1, 15, 'Toby Smith', 382197, 12676962, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1460, 'The Kids', 118, 1, 15, 'Toby Smith', 309995, 10334529, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1461, 'Mr. Moon', 118, 1, 15, 'Stuard Zender/Toby Smith', 329534, 11043559, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1462, 'Scam', 118, 1, 15, 'Stuart Zender', 422321, 14019705, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1463, 'Journey To Arnhemland', 118, 1, 15, 'Toby Smith/Wallis Buchanan', 322455, 10843832, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1464, 'Morning Glory', 118, 1, 15, 'J. Kay/Jay Kay', 384130, 12777210, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1465, 'Space Cowboy', 118, 1, 15, 'J. Kay/Jay Kay', 385697, 12906520, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1466, 'Last Chance', 119, 1, 4, 'C. Cester/C. Muncey', 112352, 3683130, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1467, 'Are You Gonna Be My Girl', 119, 1, 4, 'C. Muncey/N. Cester', 213890, 6992324, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1468, 'Rollover D.J.', 119, 1, 4, 'C. Cester/N. Cester', 196702, 6406517, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1469, 'Look What You\'ve Done', 119, 1, 4, 'N. Cester', 230974, 7517083, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1470, 'Get What You Need', 119, 1, 4, 'C. Cester/C. Muncey/N. Cester', 247719, 8043765, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1471, 'Move On', 119, 1, 4, 'C. Cester/N. Cester', 260623, 8519353, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1472, 'Radio Song', 119, 1, 4, 'C. Cester/C. Muncey/N. Cester', 272117, 8871509, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1473, 'Get Me Outta Here', 119, 1, 4, 'C. Cester/N. Cester', 176274, 5729098, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1474, 'Cold Hard Bitch', 119, 1, 4, 'C. Cester/C. Muncey/N. Cester', 243278, 7929610, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1475, 'Come Around Again', 119, 1, 4, 'C. Muncey/N. Cester', 270497, 8872405, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1476, 'Take It Or Leave It', 119, 1, 4, 'C. Muncey/N. Cester', 142889, 4643370, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1477, 'Lazy Gun', 119, 1, 4, 'C. Cester/N. Cester', 282174, 9186285, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1478, 'Timothy', 119, 1, 4, 'C. Cester', 270341, 8856507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1479, 'Foxy Lady', 120, 1, 1, 'Jimi Hendrix', 199340, 6480896, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1480, 'Manic Depression', 120, 1, 1, 'Jimi Hendrix', 222302, 7289272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1481, 'Red House', 120, 1, 1, 'Jimi Hendrix', 224130, 7285851, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1482, 'Can You See Me', 120, 1, 1, 'Jimi Hendrix', 153077, 4987068, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1483, 'Love Or Confusion', 120, 1, 1, 'Jimi Hendrix', 193123, 6329408, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1484, 'I Don\'t Live Today', 120, 1, 1, 'Jimi Hendrix', 235311, 7661214, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1485, 'May This Be Love', 120, 1, 1, 'Jimi Hendrix', 191216, 6240028, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1486, 'Fire', 120, 1, 1, 'Jimi Hendrix', 164989, 5383075, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1487, 'Third Stone From The Sun', 120, 1, 1, 'Jimi Hendrix', 404453, 13186975, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1488, 'Remember', 120, 1, 1, 'Jimi Hendrix', 168150, 5509613, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1489, 'Are You Experienced?', 120, 1, 1, 'Jimi Hendrix', 254537, 8292497, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1490, 'Hey Joe', 120, 1, 1, 'Billy Roberts', 210259, 6870054, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1491, 'Stone Free', 120, 1, 1, 'Jimi Hendrix', 216293, 7002331, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1492, 'Purple Haze', 120, 1, 1, 'Jimi Hendrix', 171572, 5597056, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1493, '51st Anniversary', 120, 1, 1, 'Jimi Hendrix', 196388, 6398044, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1494, 'The Wind Cries Mary', 120, 1, 1, 'Jimi Hendrix', 200463, 6540638, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1495, 'Highway Chile', 120, 1, 1, 'Jimi Hendrix', 212453, 6887949, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1496, 'Surfing with the Alien', 121, 2, 1, 263707, 4418504, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1497, 'Ice 9', 121, 2, 1, 239721, 4036215, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1498, 'Crushing Day', 121, 2, 1, 314768, 5232158, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1499, 'Always With Me, Always With You', 121, 2, 1, 202035, 3435777, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1500, 'Satch Boogie', 121, 2, 1, 193560, 3300654, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1501, 'Hill of the Skull', 121, 2, 1, 'J. Satriani', 108435, 1944738, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1502, 'Circles', 121, 2, 1, 209071, 3548553, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1503, 'Lords of Karma', 121, 2, 1, 'J. Satriani', 288227, 4809279, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1504, 'Midnight', 121, 2, 1, 'J. Satriani', 102630, 1851753, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1505, 'Echo', 121, 2, 1, 'J. Satriani', 337570, 5595557, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1506, 'Engenho De Dentro', 122, 1, 7, 310073, 10211473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1507, 'Alcohol', 122, 1, 7, 355239, 12010478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1508, 'Mama Africa', 122, 1, 7, 283062, 9488316, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1509, 'Salve Simpatia', 122, 1, 7, 343484, 11314756, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1510, 'W/Brasil (Chama O Síndico)', 122, 1, 7, 317100, 10599953, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1511, 'País Tropical', 122, 1, 7, 452519, 14946972, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1512, 'Os Alquimistas Estão Chegando', 122, 1, 7, 367281, 12304520, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1513, 'Charles Anjo 45', 122, 1, 7, 389276, 13022833, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1514, 'Selassiê', 122, 1, 7, 326321, 10724982, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1515, 'Menina Sarará', 122, 1, 7, 191477, 6393818, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1516, 'Que Maravilha', 122, 1, 7, 338076, 10996656, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1517, 'Santa Clara Clareou', 122, 1, 7, 380081, 12524725, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1518, 'Filho Maravilha', 122, 1, 7, 227526, 7498259, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1519, 'Taj Mahal', 122, 1, 7, 289750, 9502898, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1520, 'Rapidamente', 123, 1, 7, 252238, 8470107, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1521, 'As Dores do Mundo', 123, 1, 7, 'Hyldon', 255477, 8537092, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1522, 'Vou Pra Ai', 123, 1, 7, 300878, 10053718, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1523, 'My Brother', 123, 1, 7, 253231, 8431821, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1524, 'Há Quanto Tempo', 123, 1, 7, 270027, 9004470, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1525, 'Vício', 123, 1, 7, 269897, 8887216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1526, 'Encontrar Alguém', 123, 1, 7, 'Marco Tulio Lara/Rogerio Flausino', 224078, 7437935, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1527, 'Dance Enquanto é Tempo', 123, 1, 7, 229093, 7583799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1528, 'A Tarde', 123, 1, 7, 266919, 8836127, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1529, 'Always Be All Right', 123, 1, 7, 128078, 4299676, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1530, 'Sem Sentido', 123, 1, 7, 250462, 8292108, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1531, 'Onibusfobia', 123, 1, 7, 315977, 10474904, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1532, 'Pura Elegancia', 124, 1, 16, 'João Suplicy', 284107, 9632269, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1533, 'Choramingando', 124, 1, 16, 'João Suplicy', 190484, 6400532, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1534, 'Por Merecer', 124, 1, 16, 'João Suplicy', 230582, 7764601, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1535, 'No Futuro', 124, 1, 16, 'João Suplicy', 182308, 6056200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1536, 'Voce Inteira', 124, 1, 16, 'João Suplicy', 241084, 8077282, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1537, 'Cuando A Noite Vai Chegando', 124, 1, 16, 'João Suplicy', 270628, 9081874, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1538, 'Naquele Dia', 124, 1, 16, 'João Suplicy', 251768, 8452654, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1539, 'Equinocio', 124, 1, 16, 'João Suplicy', 269008, 8871455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1540, 'Papelão', 124, 1, 16, 'João Suplicy', 213263, 7257390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1541, 'Cuando Eu For Pro Ceu', 124, 1, 16, 'João Suplicy', 118804, 3948371, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1542, 'Do Nosso Amor', 124, 1, 16, 'João Suplicy', 203415, 6774566, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1543, 'Borogodo', 124, 1, 16, 'João Suplicy', 208457, 7104588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1544, 'Cafezinho', 124, 1, 16, 'João Suplicy', 180924, 6031174, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1545, 'Enquanto O Dia Não Vem', 124, 1, 16, 'João Suplicy', 220891, 7248336, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1546, 'The Green Manalishi', 125, 1, 3, 205792, 6720789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1547, 'Living After Midnight', 125, 1, 3, 213289, 7056785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1548, 'Breaking The Law (Live)', 125, 1, 3, 144195, 4728246, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1549, 'Hot Rockin\'', 125, 1, 3, 197328, 6509179, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1550, 'Heading Out To The Highway (Live)', 125, 1, 3, 276427, 9006022, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1551, 'The Hellion', 125, 1, 3, 41900, 1351993, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1552, 'Electric Eye', 125, 1, 3, 222197, 7231368, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1553, 'You\'ve Got Another Thing Comin\'', 125, 1, 3, 305162, 9962558, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1554, 'Turbo Lover', 125, 1, 3, 335542, 11068866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1555, 'Freewheel Burning', 125, 1, 3, 265952, 8713599, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1556, 'Some Heads Are Gonna Roll', 125, 1, 3, 249939, 8198617, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1557, 'Metal Meltdown', 125, 1, 3, 290664, 9390646, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1558, 'Ram It Down', 125, 1, 3, 292179, 9554023, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1559, 'Diamonds And Rust (Live)', 125, 1, 3, 219350, 7163147, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1560, 'Victim Of Change (Live)', 125, 1, 3, 430942, 14067512, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1561, 'Tyrant (Live)', 125, 1, 3, 282253, 9190536, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1562, 'Comin\' Home', 126, 1, 1, 'Paul Stanley, Ace Frehley', 172068, 5661120, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1563, 'Plaster Caster', 126, 1, 1, 'Gene Simmons', 198060, 6528719, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1564, 'Goin\' Blind', 126, 1, 1, 'Gene Simmons, Stephen Coronel', 217652, 7167523, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1565, 'Do You Love Me', 126, 1, 1, 'Paul Stanley, Bob Ezrin, Kim Fowley', 193619, 6343111, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1566, 'Domino', 126, 1, 1, 'Gene Simmons', 226377, 7488191, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1567, 'Sure Know Something', 126, 1, 1, 'Paul Stanley, Vincent Poncia', 254354, 8375190, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1568, 'A World Without Heroes', 126, 1, 1, 'Paul Stanley, Gene Simmons, Bob Ezrin, Lewis Reed', 177815, 5832524, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1569, 'Rock Bottom', 126, 1, 1, 'Paul Stanley, Ace Frehley', 200594, 6560818, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1570, 'See You Tonight', 126, 1, 1, 'Gene Simmons', 146494, 4817521, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1571, 'I Still Love You', 126, 1, 1, 'Paul Stanley', 369815, 12086145, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1572, 'Every Time I Look At You', 126, 1, 1, 'Paul Stanley, Vincent Cusano', 283898, 9290948, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1573, '2,000 Man', 126, 1, 1, 'Mick Jagger, Keith Richard', 312450, 10292829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1574, 'Beth', 126, 1, 1, 'Peter Criss, Stan Penridge, Bob Ezrin', 170187, 5577807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1575, 'Nothin\' To Lose', 126, 1, 1, 'Gene Simmons', 222354, 7351460, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1576, 'Rock And Roll All Nite', 126, 1, 1, 'Paul Stanley, Gene Simmons', 259631, 8549296, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1577, 'Immigrant Song', 127, 1, 1, 'Robert Plant', 201247, 6457766, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1578, 'Heartbreaker', 127, 1, 1, 'John Bonham/John Paul Jones/Robert Plant', 316081, 10179657, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1579, 'Since I\'ve Been Loving You', 127, 1, 1, 'John Paul Jones/Robert Plant', 416365, 13471959, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1580, 'Black Dog', 127, 1, 1, 'John Paul Jones/Robert Plant', 317622, 10267572, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1581, 'Dazed And Confused', 127, 1, 1, 'Jimmy Page/Led Zeppelin', 1116734, 36052247, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1582, 'Stairway To Heaven', 127, 1, 1, 'Robert Plant', 529658, 17050485, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1583, 'Going To California', 127, 1, 1, 'Robert Plant', 234605, 7646749, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1584, 'That\'s The Way', 127, 1, 1, 'Robert Plant', 343431, 11248455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1585, 'Whole Lotta Love (Medley)', 127, 1, 1, 'Arthur Crudup/Bernard Besman/Bukka White/Doc Pomus/John Bonham/John Lee Hooker/John Paul Jones/Mort Shuman/Robert Plant/Willie Dixon', 825103, 26742545, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1586, 'Thank You', 127, 1, 1, 'Robert Plant', 398262, 12831826, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1587, 'We\'re Gonna Groove', 128, 1, 1, 'Ben E.King/James Bethea', 157570, 5180975, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1588, 'Poor Tom', 128, 1, 1, 'Jimmy Page/Robert Plant', 182491, 6016220, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1589, 'I Can\'t Quit You Baby', 128, 1, 1, 'Willie Dixon', 258168, 8437098, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1590, 'Walter\'s Walk', 128, 1, 1, 'Jimmy Page, Robert Plant', 270785, 8712499, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1591, 'Ozone Baby', 128, 1, 1, 'Jimmy Page, Robert Plant', 215954, 7079588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1592, 'Darlene', 128, 1, 1, 'Jimmy Page, Robert Plant, John Bonham, John Paul Jones', 307226, 10078197, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1593, 'Bonzo\'s Montreux', 128, 1, 1, 'John Bonham', 258925, 8557447, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1594, 'Wearing And Tearing', 128, 1, 1, 'Jimmy Page, Robert Plant', 330004, 10701590, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1595, 'The Song Remains The Same', 129, 1, 1, 'Jimmy Page/Jimmy Page & Robert Plant/Robert Plant', 330004, 10708950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1596, 'The Rain Song', 129, 1, 1, 'Jimmy Page/Jimmy Page & Robert Plant/Robert Plant', 459180, 15029875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1597, 'Over The Hills And Far Away', 129, 1, 1, 'Jimmy Page/Jimmy Page & Robert Plant/Robert Plant', 290089, 9552829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1598, 'The Crunge', 129, 1, 1, 'John Bonham/John Paul Jones', 197407, 6460212, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1599, 'Dancing Days', 129, 1, 1, 'Jimmy Page/Jimmy Page & Robert Plant/Robert Plant', 223216, 7250104, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1600, 'D\'Yer Mak\'er', 129, 1, 1, 'John Bonham/John Paul Jones', 262948, 8645935, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1601, 'No Quarter', 129, 1, 1, 'John Paul Jones', 420493, 13656517, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1602, 'The Ocean', 129, 1, 1, 'John Bonham/John Paul Jones', 271098, 8846469, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1603, 'In The Evening', 130, 1, 1, 'Jimmy Page, Robert Plant & John Paul Jones', 410566, 13399734, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1604, 'South Bound Saurez', 130, 1, 1, 'John Paul Jones & Robert Plant', 254406, 8420427, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1605, 'Fool In The Rain', 130, 1, 1, 'Jimmy Page, Robert Plant & John Paul Jones', 372950, 12371433, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1606, 'Hot Dog', 130, 1, 1, 'Jimmy Page & Robert Plant', 197198, 6536167, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1607, 'Carouselambra', 130, 1, 1, 'John Paul Jones, Jimmy Page & Robert Plant', 634435, 20858315, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1608, 'All My Love', 130, 1, 1, 'Robert Plant & John Paul Jones', 356284, 11684862, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1609, 'I\'m Gonna Crawl', 130, 1, 1, 'Jimmy Page, Robert Plant & John Paul Jones', 329639, 10737665, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1610, 'Black Dog', 131, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones', 296672, 9660588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1611, 'Rock & Roll', 131, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones, John Bonham', 220917, 7142127, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1612, 'The Battle Of Evermore', 131, 1, 1, 'Jimmy Page, Robert Plant', 351555, 11525689, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1613, 'Stairway To Heaven', 131, 1, 1, 'Jimmy Page, Robert Plant', 481619, 15706767, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1614, 'Misty Mountain Hop', 131, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones', 278857, 9092799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1615, 'Four Sticks', 131, 1, 1, 'Jimmy Page, Robert Plant', 284447, 9481301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1616, 'Going To California', 131, 1, 1, 'Jimmy Page, Robert Plant', 215693, 7068737, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1617, 'When The Levee Breaks', 131, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones, John Bonham, Memphis Minnie', 427702, 13912107, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1618, 'Good Times Bad Times', 132, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones', 166164, 5464077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1619, 'Babe I\'m Gonna Leave You', 132, 1, 1, 'Jimmy Page/Robert Plant', 401475, 13189312, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1620, 'You Shook Me', 132, 1, 1, 'J. B. Lenoir/Willie Dixon', 388179, 12643067, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1621, 'Dazed and Confused', 132, 1, 1, 'Jimmy Page', 386063, 12610326, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1622, 'Your Time Is Gonna Come', 132, 1, 1, 'Jimmy Page/John Paul Jones', 274860, 9011653, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1623, 'Black Mountain Side', 132, 1, 1, 'Jimmy Page', 132702, 4440602, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1624, 'Communication Breakdown', 132, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones', 150230, 4899554, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1625, 'I Can\'t Quit You Baby', 132, 1, 1, 'Willie Dixon', 282671, 9252733, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1626, 'How Many More Times', 132, 1, 1, 'Jimmy Page/John Bonham/John Paul Jones', 508055, 16541364, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1627, 'Whole Lotta Love', 133, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones, John Bonham', 334471, 11026243, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1628, 'What Is And What Should Never Be', 133, 1, 1, 'Jimmy Page, Robert Plant', 287973, 9369385, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1629, 'The Lemon Song', 133, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones, John Bonham', 379141, 12463496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1630, 'Thank You', 133, 1, 1, 'Jimmy Page, Robert Plant', 287791, 9337392, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1631, 'Heartbreaker', 133, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones, John Bonham', 253988, 8387560, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1632, 'Living Loving Maid (She\'s Just A Woman)', 133, 1, 1, 'Jimmy Page, Robert Plant', 159216, 5219819, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1633, 'Ramble On', 133, 1, 1, 'Jimmy Page, Robert Plant', 275591, 9199710, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1634, 'Moby Dick', 133, 1, 1, 'John Bonham, John Paul Jones, Jimmy Page', 260728, 8664210, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1635, 'Bring It On Home', 133, 1, 1, 'Jimmy Page, Robert Plant', 259970, 8494731, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1636, 'Immigrant Song', 134, 1, 1, 'Jimmy Page, Robert Plant', 144875, 4786461, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1637, 'Friends', 134, 1, 1, 'Jimmy Page, Robert Plant', 233560, 7694220, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1638, 'Celebration Day', 134, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones', 209528, 6871078, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1639, 'Since I\'ve Been Loving You', 134, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones', 444055, 14482460, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1640, 'Out On The Tiles', 134, 1, 1, 'Jimmy Page, Robert Plant, John Bonham', 246047, 8060350, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1641, 'Gallows Pole', 134, 1, 1, 'Traditional', 296228, 9757151, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1642, 'Tangerine', 134, 1, 1, 'Jimmy Page', 189675, 6200893, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1643, 'That\'s The Way', 134, 1, 1, 'Jimmy Page, Robert Plant', 337345, 11202499, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1644, 'Bron-Y-Aur Stomp', 134, 1, 1, 'Jimmy Page, Robert Plant, John Paul Jones', 259500, 8674508, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1645, 'Hats Off To (Roy) Harper', 134, 1, 1, 'Traditional', 219376, 7236640, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1646, 'In The Light', 135, 1, 1, 'John Paul Jones/Robert Plant', 526785, 17033046, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1647, 'Bron-Yr-Aur', 135, 1, 1, 'Jimmy Page', 126641, 4150746, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1648, 'Down By The Seaside', 135, 1, 1, 'Robert Plant', 316186, 10371282, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1649, 'Ten Years Gone', 135, 1, 1, 'Robert Plant', 393116, 12756366, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1650, 'Night Flight', 135, 1, 1, 'John Paul Jones/Robert Plant', 217547, 7160647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1651, 'The Wanton Song', 135, 1, 1, 'Robert Plant', 249887, 8180988, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1652, 'Boogie With Stu', 135, 1, 1, 'Ian Stewart/John Bonham/John Paul Jones/Mrs. Valens/Robert Plant', 233273, 7657086, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1653, 'Black Country Woman', 135, 1, 1, 'Robert Plant', 273084, 8951732, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1654, 'Sick Again', 135, 1, 1, 'Robert Plant', 283036, 9279263, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1655, 'Achilles Last Stand', 136, 1, 1, 'Jimmy Page/Robert Plant', 625502, 20593955, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1656, 'For Your Life', 136, 1, 1, 'Jimmy Page/Robert Plant', 384391, 12633382, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1657, 'Royal Orleans', 136, 1, 1, 'John Bonham/John Paul Jones', 179591, 5930027, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1658, 'Nobody\'s Fault But Mine', 136, 1, 1, 'Jimmy Page/Robert Plant', 376215, 12237859, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1659, 'Candy Store Rock', 136, 1, 1, 'Jimmy Page/Robert Plant', 252055, 8397423, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1660, 'Hots On For Nowhere', 136, 1, 1, 'Jimmy Page/Robert Plant', 284107, 9342342, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1661, 'Tea For One', 136, 1, 1, 'Jimmy Page/Robert Plant', 566752, 18475264, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1662, 'Rock & Roll', 137, 1, 1, 'John Bonham/John Paul Jones/Robert Plant', 242442, 7897065, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1663, 'Celebration Day', 137, 1, 1, 'John Paul Jones/Robert Plant', 230034, 7478487, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1664, 'The Song Remains The Same', 137, 1, 1, 'Robert Plant', 353358, 11465033, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1665, 'Rain Song', 137, 1, 1, 'Robert Plant', 505808, 16273705, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1666, 'Dazed And Confused', 137, 1, 1, 'Jimmy Page', 1612329, 52490554, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1667, 'No Quarter', 138, 1, 1, 'John Paul Jones/Robert Plant', 749897, 24399285, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1668, 'Stairway To Heaven', 138, 1, 1, 'Robert Plant', 657293, 21354766, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1669, 'Moby Dick', 138, 1, 1, 'John Bonham/John Paul Jones', 766354, 25345841, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1670, 'Whole Lotta Love', 138, 1, 1, 'John Bonham/John Paul Jones/Robert Plant/Willie Dixon', 863895, 28191437, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1671, 'Natália', 139, 1, 7, 'Renato Russo', 235728, 7640230, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1672, 'L\'Avventura', 139, 1, 7, 'Renato Russo', 278256, 9165769, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1673, 'Música De Trabalho', 139, 1, 7, 'Renato Russo', 260231, 8590671, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1674, 'Longe Do Meu Lado', 139, 1, 7, 'Renato Russo - Marcelo Bonfá', 266161, 8655249, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1675, 'A Via Láctea', 139, 1, 7, 'Renato Russo', 280084, 9234879, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1676, 'Música Ambiente', 139, 1, 7, 'Renato Russo', 247614, 8234388, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1677, 'Aloha', 139, 1, 7, 'Renato Russo', 325955, 10793301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1678, 'Soul Parsifal', 139, 1, 7, 'Renato Russo - Marisa Monte', 295053, 9853589, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1679, 'Dezesseis', 139, 1, 7, 'Renato Russo', 323918, 10573515, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1680, 'Mil Pedaços', 139, 1, 7, 'Renato Russo', 203337, 6643291, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1681, 'Leila', 139, 1, 7, 'Renato Russo', 323056, 10608239, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1682, '1º De Julho', 139, 1, 7, 'Renato Russo', 290298, 9619257, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1683, 'Esperando Por Mim', 139, 1, 7, 'Renato Russo', 261668, 8844133, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1684, 'Quando Você Voltar', 139, 1, 7, 'Renato Russo', 173897, 5781046, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1685, 'O Livro Dos Dias', 139, 1, 7, 'Renato Russo', 257253, 8570929, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1686, 'Será', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 148401, 4826528, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1687, 'Ainda É Cedo', 140, 1, 7, 'Dado Villa-Lobos/Ico Ouro-Preto/Marcelo Bonfá', 236826, 7796400, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1688, 'Geração Coca-Cola', 140, 1, 7, 'Renato Russo', 141453, 4625731, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1689, 'Eduardo E Mônica', 140, 1, 7, 'Renato Russo', 271229, 9026691, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1690, 'Tempo Perdido', 140, 1, 7, 'Renato Russo', 302158, 9963914, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1691, 'Indios', 140, 1, 7, 'Renato Russo', 258168, 8610226, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1692, 'Que País É Este', 140, 1, 7, 'Renato Russo', 177606, 5822124, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1693, 'Faroeste Caboclo', 140, 1, 7, 'Renato Russo', 543007, 18092739, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1694, 'Há Tempos', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 197146, 6432922, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1695, 'Pais E Filhos', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 308401, 10130685, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1696, 'Meninos E Meninas', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 203781, 6667802, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1697, 'Vento No Litoral', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 366445, 12063806, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1698, 'Perfeição', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 276558, 9258489, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1699, 'Giz', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 202213, 6677671, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1700, 'Dezesseis', 140, 1, 7, 'Dado Villa-Lobos/Marcelo Bonfá', 321724, 10501773, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1701, 'Antes Das Seis', 140, 1, 7, 'Dado Villa-Lobos', 189231, 6296531, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1702, 'Are You Gonna Go My Way', 141, 1, 1, 'Craig Ross/Lenny Kravitz', 211591, 6905135, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1703, 'Fly Away', 141, 1, 1, 'Lenny Kravitz', 221962, 7322085, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1704, 'Rock And Roll Is Dead', 141, 1, 1, 'Lenny Kravitz', 204199, 6680312, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1705, 'Again', 141, 1, 1, 'Lenny Kravitz', 228989, 7490476, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1706, 'It Ain\'t Over \'Til It\'s Over', 141, 1, 1, 'Lenny Kravitz', 242703, 8078936, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1707, 'Can\'t Get You Off My Mind', 141, 1, 1, 'Lenny Kravitz', 273815, 8937150, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1708, 'Mr. Cab Driver', 141, 1, 1, 'Lenny Kravitz', 230321, 7668084, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1709, 'American Woman', 141, 1, 1, 'B. Cummings/G. Peterson/M.J. Kale/R. Bachman', 261773, 8538023, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1710, 'Stand By My Woman', 141, 1, 1, 'Henry Kirssch/Lenny Kravitz/S. Pasch A. Krizan', 259683, 8447611, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1711, 'Always On The Run', 141, 1, 1, 'Lenny Kravitz/Slash', 232515, 7593397, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1712, 'Heaven Help', 141, 1, 1, 'Gerry DeVeaux/Terry Britten', 190354, 6222092, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1713, 'I Belong To You', 141, 1, 1, 'Lenny Kravitz', 257123, 8477980, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1714, 'Believe', 141, 1, 1, 'Henry Hirsch/Lenny Kravitz', 295131, 9661978, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1715, 'Let Love Rule', 141, 1, 1, 'Lenny Kravitz', 342648, 11298085, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1716, 'Black Velveteen', 141, 1, 1, 'Lenny Kravitz', 290899, 9531301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1717, 'Assim Caminha A Humanidade', 142, 1, 7, 210755, 6993763, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1718, 'Honolulu', 143, 1, 7, 261433, 8558481, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1719, 'Dancin´Days', 143, 1, 7, 237400, 7875347, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1720, 'Um Pro Outro', 142, 1, 7, 236382, 7825215, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1721, 'Aviso Aos Navegantes', 143, 1, 7, 242808, 8058651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1722, 'Casa', 142, 1, 7, 307591, 10107269, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1723, 'Condição', 142, 1, 7, 263549, 8778465, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1724, 'Hyperconectividade', 143, 1, 7, 180636, 5948039, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1725, 'O Descobridor Dos Sete Mares', 143, 1, 7, 225854, 7475780, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1726, 'Satisfação', 142, 1, 7, 208065, 6901681, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1727, 'Brumário', 142, 1, 7, 216241, 7243499, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1728, 'Um Certo Alguém', 143, 1, 7, 194063, 6430939, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1729, 'Fullgás', 143, 1, 7, 346070, 11505484, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1730, 'Sábado À Noite', 142, 1, 7, 193854, 6435114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1731, 'A Cura', 142, 1, 7, 280920, 9260588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1732, 'Aquilo', 143, 1, 7, 246073, 8167819, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1733, 'Atrás Do Trio Elétrico', 142, 1, 7, 149080, 4917615, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1734, 'Senta A Pua', 143, 1, 7, 217547, 7205844, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1735, 'Ro-Que-Se-Da-Ne', 143, 1, 7, 146703, 4805897, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1736, 'Tudo Bem', 142, 1, 7, 196101, 6419139, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1737, 'Toda Forma De Amor', 142, 1, 7, 227813, 7496584, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1738, 'Tudo Igual', 143, 1, 7, 276035, 9201645, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1739, 'Fogo De Palha', 143, 1, 7, 246804, 8133732, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1740, 'Sereia', 142, 1, 7, 278047, 9121087, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1741, 'Assaltaram A Gramática', 143, 1, 7, 261041, 8698959, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1742, 'Se Você Pensa', 142, 1, 7, 195996, 6552490, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1743, 'Lá Vem O Sol (Here Comes The Sun)', 142, 1, 7, 189492, 6229645, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1744, 'O Último Romântico (Ao Vivo)', 143, 1, 7, 231993, 7692697, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1745, 'Pseudo Silk Kimono', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 134739, 4334038, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1746, 'Kayleigh', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 234605, 7716005, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1747, 'Lavender', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 153417, 4999814, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1748, 'Bitter Suite: Brief Encounter / Lost Weekend / Blue Angel', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 356493, 11791068, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1749, 'Heart Of Lothian: Wide Boy / Curtain Call', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 366053, 11893723, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1750, 'Waterhole (Expresso Bongo)', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 133093, 4378835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1751, 'Lords Of The Backstage', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 112875, 3741319, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1752, 'Blind Curve: Vocal Under A Bloodlight / Passing Strangers / Mylo / Perimeter Walk / Threshold', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 569704, 18578995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1753, 'Childhoods End?', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 272796, 9015366, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1754, 'White Feather', 144, 1, 1, 'Kelly, Mosley, Rothery, Trewaves', 143595, 4711776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1755, 'Arrepio', 145, 1, 7, 'Carlinhos Brown', 136254, 4511390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1756, 'Magamalabares', 145, 1, 7, 'Carlinhos Brown', 215875, 7183757, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1757, 'Chuva No Brejo', 145, 1, 7, 'Morais', 145606, 4857761, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1758, 'Cérebro Eletrônico', 145, 1, 7, 'Gilberto Gil', 172800, 5760864, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1759, 'Tempos Modernos', 145, 1, 7, 'Lulu Santos', 183066, 6066234, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1760, 'Maraçá', 145, 1, 7, 'Carlinhos Brown', 230008, 7621482, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1761, 'Blanco', 145, 1, 7, 'Marisa Monte/poema de Octavio Paz/versão: Haroldo de Campos', 45191, 1454532, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1762, 'Panis Et Circenses', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 192339, 6318373, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1763, 'De Noite Na Cama', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 209005, 7012658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1764, 'Beija Eu', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 197276, 6512544, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1765, 'Give Me Love', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 249808, 8196331, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1766, 'Ainda Lembro', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 218801, 7211247, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1767, 'A Menina Dança', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 129410, 4326918, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1768, 'Dança Da Solidão', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 203520, 6699368, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1769, 'Ao Meu Redor', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 275591, 9158834, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1770, 'Bem Leve', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 159190, 5246835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1771, 'Segue O Seco', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 178207, 5922018, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1772, 'O Xote Das Meninas', 145, 1, 7, 'Caetano Veloso e Gilberto Gil', 291866, 9553228, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1773, 'Wherever I Lay My Hat', 146, 1, 14, 136986, 4477321, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1774, 'Get My Hands On Some Lovin\'', 146, 1, 14, 149054, 4860380, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1775, 'No Good Without You', 146, 1, 14, 'William "Mickey" Stevenson', 161410, 5259218, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1776, 'You\'ve Been A Long Time Coming', 146, 1, 14, 'Brian Holland/Eddie Holland/Lamont Dozier', 137221, 4437949, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1777, 'When I Had Your Love', 146, 1, 14, 'Robert Rogers/Warren "Pete" Moore/William "Mickey" Stevenson', 152424, 4972815, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1778, 'You\'re What\'s Happening (In The World Today)', 146, 1, 14, 'Allen Story/George Gordy/Robert Gordy', 142027, 4631104, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1779, 'Loving You Is Sweeter Than Ever', 146, 1, 14, 'Ivy Hunter/Stevie Wonder', 166295, 5377546, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1780, 'It\'s A Bitter Pill To Swallow', 146, 1, 14, 'Smokey Robinson/Warren "Pete" Moore', 194821, 6477882, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1781, 'Seek And You Shall Find', 146, 1, 14, 'Ivy Hunter/William "Mickey" Stevenson', 223451, 7306719, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1782, 'Gonna Keep On Tryin\' Till I Win Your Love', 146, 1, 14, 'Barrett Strong/Norman Whitfield', 176404, 5789945, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1783, 'Gonna Give Her All The Love I\'ve Got', 146, 1, 14, 'Barrett Strong/Norman Whitfield', 210886, 6893603, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1784, 'I Wish It Would Rain', 146, 1, 14, 'Barrett Strong/Norman Whitfield/Roger Penzabene', 172486, 5647327, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1785, 'Abraham, Martin And John', 146, 1, 14, 'Dick Holler', 273057, 8888206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1786, 'Save The Children', 146, 1, 14, 'Al Cleveland/Marvin Gaye/Renaldo Benson', 194821, 6342021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1787, 'You Sure Love To Ball', 146, 1, 14, 'Marvin Gaye', 218540, 7217872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1788, 'Ego Tripping Out', 146, 1, 14, 'Marvin Gaye', 314514, 10383887, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1789, 'Praise', 146, 1, 14, 'Marvin Gaye', 235833, 7839179, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1790, 'Heavy Love Affair', 146, 1, 14, 'Marvin Gaye', 227892, 7522232, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1791, 'Down Under', 147, 1, 1, 222171, 7366142, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1792, 'Overkill', 147, 1, 1, 225410, 7408652, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1793, 'Be Good Johnny', 147, 1, 1, 216320, 7139814, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1794, 'Everything I Need', 147, 1, 1, 216476, 7107625, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1795, 'Down by the Sea', 147, 1, 1, 408163, 13314900, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1796, 'Who Can It Be Now?', 147, 1, 1, 202396, 6682850, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1797, 'It\'s a Mistake', 147, 1, 1, 273371, 8979965, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1798, 'Dr. Heckyll & Mr. Jive', 147, 1, 1, 278465, 9110403, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1799, 'Shakes and Ladders', 147, 1, 1, 198008, 6560753, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (1800, 'No Sign of Yesterday', 147, 1, 1, 362004, 11829011, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1801, 'Enter Sandman', 148, 1, 3, 'James Hetfield, Lars Ulrich and Kirk Hammett', 332251, 10852002, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1802, 'Sad But True', 148, 1, 3, 'Ulrich', 324754, 10541258, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1803, 'Holier Than Thou', 148, 1, 3, 'Ulrich', 227892, 7462011, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1804, 'The Unforgiven', 148, 1, 3, 'James Hetfield, Lars Ulrich and Kirk Hammett', 387082, 12646886, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1805, 'Wherever I May Roam', 148, 1, 3, 'Ulrich', 404323, 13161169, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1806, 'Don\'t Tread On Me', 148, 1, 3, 'Ulrich', 240483, 7827907, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1807, 'Through The Never', 148, 1, 3, 'James Hetfield, Lars Ulrich and Kirk Hammett', 244375, 8024047, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1808, 'Nothing Else Matters', 148, 1, 3, 'Ulrich', 388832, 12606241, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1809, 'Of Wolf And Man', 148, 1, 3, 'James Hetfield, Lars Ulrich and Kirk Hammett', 256835, 8339785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1810, 'The God That Failed', 148, 1, 3, 'Ulrich', 308610, 10055959, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1811, 'My Friend Of Misery', 148, 1, 3, 'James Hetfield, Lars Ulrich and Jason Newsted', 409547, 13293515, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1812, 'The Struggle Within', 148, 1, 3, 'Ulrich', 234240, 7654052, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1813, 'Helpless', 149, 1, 3, 'Harris/Tatler', 398315, 12977902, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1814, 'The Small Hours', 149, 1, 3, 'Holocaust', 403435, 13215133, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1815, 'The Wait', 149, 1, 3, 'Killing Joke', 295418, 9688418, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1816, 'Crash Course In Brain Surgery', 149, 1, 3, 'Bourge/Phillips/Shelley', 190406, 6233729, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1817, 'Last Caress/Green Hell', 149, 1, 3, 'Danzig', 209972, 6854313, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1818, 'Am I Evil?', 149, 1, 3, 'Harris/Tatler', 470256, 15387219, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1819, 'Blitzkrieg', 149, 1, 3, 'Jones/Sirotto/Smith', 216685, 7090018, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1820, 'Breadfan', 149, 1, 3, 'Bourge/Phillips/Shelley', 341551, 11100130, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1821, 'The Prince', 149, 1, 3, 'Harris/Tatler', 265769, 8624492, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1822, 'Stone Cold Crazy', 149, 1, 3, 'Deacon/May/Mercury/Taylor', 137717, 4514830, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1823, 'So What', 149, 1, 3, 'Culmer/Exalt', 189152, 6162894, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1824, 'Killing Time', 149, 1, 3, 'Sweet Savage', 183693, 6021197, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1825, 'Overkill', 149, 1, 3, 'Clarke/Kilmister/Tayler', 245133, 7971330, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1826, 'Damage Case', 149, 1, 3, 'Clarke/Farren/Kilmister/Tayler', 220212, 7212997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1827, 'Stone Dead Forever', 149, 1, 3, 'Clarke/Kilmister/Tayler', 292127, 9556060, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1828, 'Too Late Too Late', 149, 1, 3, 'Clarke/Kilmister/Tayler', 192052, 6276291, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1829, 'Hit The Lights', 150, 1, 3, 'James Hetfield, Lars Ulrich', 257541, 8357088, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1830, 'The Four Horsemen', 150, 1, 3, 'James Hetfield, Lars Ulrich, Dave Mustaine', 433188, 14178138, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1831, 'Motorbreath', 150, 1, 3, 'James Hetfield', 188395, 6153933, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1832, 'Jump In The Fire', 150, 1, 3, 'James Hetfield, Lars Ulrich, Dave Mustaine', 281573, 9135755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1833, '(Anesthesia) Pulling Teeth', 150, 1, 3, 'Cliff Burton', 254955, 8234710, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1834, 'Whiplash', 150, 1, 3, 'James Hetfield, Lars Ulrich', 249208, 8102839, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1835, 'Phantom Lord', 150, 1, 3, 'James Hetfield, Lars Ulrich, Dave Mustaine', 302053, 9817143, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1836, 'No Remorse', 150, 1, 3, 'James Hetfield, Lars Ulrich', 386795, 12672166, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1837, 'Seek & Destroy', 150, 1, 3, 'James Hetfield, Lars Ulrich', 415817, 13452301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1838, 'Metal Militia', 150, 1, 3, 'James Hetfield, Lars Ulrich, Dave Mustaine', 311327, 10141785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1839, 'Ain\'t My Bitch', 151, 1, 3, 'James Hetfield, Lars Ulrich', 304457, 9931015, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1840, '2 X 4', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 328254, 10732251, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1841, 'The House Jack Built', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 398942, 13005152, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1842, 'Until It Sleeps', 151, 1, 3, 'James Hetfield, Lars Ulrich', 269740, 8837394, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1843, 'King Nothing', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 328097, 10681477, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1844, 'Hero Of The Day', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 261982, 8540298, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1845, 'Bleeding Me', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 497998, 16249420, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1846, 'Cure', 151, 1, 3, 'James Hetfield, Lars Ulrich', 294347, 9648615, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1847, 'Poor Twisted Me', 151, 1, 3, 'James Hetfield, Lars Ulrich', 240065, 7854349, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1848, 'Wasted My Hate', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 237296, 7762300, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1849, 'Mama Said', 151, 1, 3, 'James Hetfield, Lars Ulrich', 319764, 10508310, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1850, 'Thorn Within', 151, 1, 3, 'James Hetfield, Lars Ulrich, Kirk Hammett', 351738, 11486686, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1851, 'Ronnie', 151, 1, 3, 'James Hetfield, Lars Ulrich', 317204, 10390947, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1852, 'The Outlaw Torn', 151, 1, 3, 'James Hetfield, Lars Ulrich', 588721, 19286261, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1853, 'Battery', 152, 1, 3, 'J.Hetfield/L.Ulrich', 312424, 10229577, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1854, 'Master Of Puppets', 152, 1, 3, 'K.Hammett', 515239, 16893720, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1855, 'The Thing That Should Not Be', 152, 1, 3, 'K.Hammett', 396199, 12952368, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1856, 'Welcome Home (Sanitarium)', 152, 1, 3, 'K.Hammett', 387186, 12679965, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1857, 'Disposable Heroes', 152, 1, 3, 'J.Hetfield/L.Ulrich', 496718, 16135560, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1858, 'Leper Messiah', 152, 1, 3, 'C.Burton', 347428, 11310434, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1859, 'Orion', 152, 1, 3, 'K.Hammett', 500062, 16378477, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1860, 'Damage Inc.', 152, 1, 3, 'K.Hammett', 330919, 10725029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1861, 'Fuel', 153, 1, 3, 'Hetfield, Ulrich, Hammett', 269557, 8876811, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1862, 'The Memory Remains', 153, 1, 3, 'Hetfield, Ulrich', 279353, 9110730, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1863, 'Devil\'s Dance', 153, 1, 3, 'Hetfield, Ulrich', 318955, 10414832, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1864, 'The Unforgiven II', 153, 1, 3, 'Hetfield, Ulrich, Hammett', 395520, 12886474, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1865, 'Better Than You', 153, 1, 3, 'Hetfield, Ulrich', 322899, 10549070, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1866, 'Slither', 153, 1, 3, 'Hetfield, Ulrich, Hammett', 313103, 10199789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1867, 'Carpe Diem Baby', 153, 1, 3, 'Hetfield, Ulrich, Hammett', 372480, 12170693, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1868, 'Bad Seed', 153, 1, 3, 'Hetfield, Ulrich, Hammett', 245394, 8019586, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1869, 'Where The Wild Things Are', 153, 1, 3, 'Hetfield, Ulrich, Newsted', 414380, 13571280, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1870, 'Prince Charming', 153, 1, 3, 'Hetfield, Ulrich', 365061, 12009412, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1871, 'Low Man\'s Lyric', 153, 1, 3, 'Hetfield, Ulrich', 457639, 14855583, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1872, 'Attitude', 153, 1, 3, 'Hetfield, Ulrich', 315898, 10335734, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1873, 'Fixxxer', 153, 1, 3, 'Hetfield, Ulrich, Hammett', 496065, 16190041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1874, 'Fight Fire With Fire', 154, 1, 3, 'Metallica', 285753, 9420856, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1875, 'Ride The Lightning', 154, 1, 3, 'Metallica', 397740, 13055884, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1876, 'For Whom The Bell Tolls', 154, 1, 3, 'Metallica', 311719, 10159725, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1877, 'Fade To Black', 154, 1, 3, 'Metallica', 414824, 13531954, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1878, 'Trapped Under Ice', 154, 1, 3, 'Metallica', 244532, 7975942, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1879, 'Escape', 154, 1, 3, 'Metallica', 264359, 8652332, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1880, 'Creeping Death', 154, 1, 3, 'Metallica', 396878, 12955593, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1881, 'The Call Of Ktulu', 154, 1, 3, 'Metallica', 534883, 17486240, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1882, 'Frantic', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 350458, 11510849, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1883, 'St. Anger', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 441234, 14363779, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1884, 'Some Kind Of Monster', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 505626, 16557497, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1885, 'Dirty Window', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 324989, 10670604, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1886, 'Invisible Kid', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 510197, 16591800, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1887, 'My World', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 345626, 11253756, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1888, 'Shoot Me Again', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 430210, 14093551, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1889, 'Sweet Amber', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 327235, 10616595, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1890, 'The Unnamed Feeling', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 429479, 14014582, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1891, 'Purify', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 314017, 10232537, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1892, 'All Within My Hands', 155, 1, 3, 'Bob Rock/James Hetfield/Kirk Hammett/Lars Ulrich', 527986, 17162741, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1893, 'Blackened', 156, 1, 3, 'James Hetfield, Lars Ulrich & Jason Newsted', 403382, 13254874, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1894, '...And Justice For All', 156, 1, 3, 'James Hetfield, Lars Ulrich & Kirk Hammett', 585769, 19262088, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1895, 'Eye Of The Beholder', 156, 1, 3, 'James Hetfield, Lars Ulrich & Kirk Hammett', 385828, 12747894, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1896, 'One', 156, 1, 3, 'James Hetfield & Lars Ulrich', 446484, 14695721, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1897, 'The Shortest Straw', 156, 1, 3, 'James Hetfield and Lars Ulrich', 395389, 13013990, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1898, 'Harvester Of Sorrow', 156, 1, 3, 'James Hetfield and Lars Ulrich', 345547, 11377339, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1899, 'The Frayed Ends Of Sanity', 156, 1, 3, 'James Hetfield, Lars Ulrich and Kirk Hammett', 464039, 15198986, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1900, 'To Live Is To Die', 156, 1, 3, 'James Hetfield, Lars Ulrich and Cliff Burton', 588564, 19243795, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1901, 'Dyers Eve', 156, 1, 3, 'James Hetfield, Lars Ulrich and Kirk Hammett', 313991, 10302828, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1902, 'Springsville', 157, 1, 2, 'J. Carisi', 207725, 6776219, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1903, 'The Maids Of Cadiz', 157, 1, 2, 'L. Delibes', 233534, 7505275, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1904, 'The Duke', 157, 1, 2, 'Dave Brubeck', 214961, 6977626, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1905, 'My Ship', 157, 1, 2, 'Ira Gershwin, Kurt Weill', 268016, 8581144, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1906, 'Miles Ahead', 157, 1, 2, 'Miles Davis, Gil Evans', 209893, 6807707, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1907, 'Blues For Pablo', 157, 1, 2, 'Gil Evans', 318328, 10218398, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1908, 'New Rhumba', 157, 1, 2, 'A. Jamal', 276871, 8980400, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1909, 'The Meaning Of The Blues', 157, 1, 2, 'R. Troup, L. Worth', 168594, 5395412, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1910, 'Lament', 157, 1, 2, 'J.J. Johnson', 134191, 4293394, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1911, 'I Don\'t Wanna Be Kissed (By Anyone But You)', 157, 1, 2, 'H. Spina, J. Elliott', 191320, 6219487, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1912, 'Springsville (Alternate Take)', 157, 1, 2, 'J. Carisi', 196388, 6382079, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1913, 'Blues For Pablo (Alternate Take)', 157, 1, 2, 'Gil Evans', 212558, 6900619, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1914, 'The Meaning Of The Blues/Lament (Alternate Take)', 157, 1, 2, 'J.J. Johnson/R. Troup, L. Worth', 309786, 9912387, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1915, 'I Don\'t Wanna Be Kissed (By Anyone But You) (Alternate Take)', 157, 1, 2, 'H. Spina, J. Elliott', 192078, 6254796, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1916, 'Coração De Estudante', 158, 1, 7, 'Wagner Tiso, Milton Nascimento', 238550, 7797308, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1917, 'A Noite Do Meu Bem', 158, 1, 7, 'Dolores Duran', 220081, 7125225, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1918, 'Paisagem Na Janela', 158, 1, 7, 'Lô Borges, Fernando Brant', 197694, 6523547, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1919, 'Cuitelinho', 158, 1, 7, 'Folclore', 209397, 6803970, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1920, 'Caxangá', 158, 1, 7, 'Milton Nascimento, Fernando Brant', 245551, 8144179, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1921, 'Nos Bailes Da Vida', 158, 1, 7, 'Milton Nascimento, Fernando Brant', 275748, 9126170, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1922, 'Menestrel Das Alagoas', 158, 1, 7, 'Milton Nascimento, Fernando Brant', 199758, 6542289, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1923, 'Brasil', 158, 1, 7, 'Milton Nascimento, Fernando Brant', 155428, 5252560, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1924, 'Canção Do Novo Mundo', 158, 1, 7, 'Beto Guedes, Ronaldo Bastos', 215353, 7032626, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1925, 'Um Gosto De Sol', 158, 1, 7, 'Milton Nascimento, Ronaldo Bastos', 307200, 9893875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1926, 'Solar', 158, 1, 7, 'Milton Nascimento, Fernando Brant', 156212, 5098288, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1927, 'Para Lennon E McCartney', 158, 1, 7, 'Lô Borges, Márcio Borges, Fernando Brant', 321828, 10626920, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1928, 'Maria, Maria', 158, 1, 7, 'Milton Nascimento, Fernando Brant', 72463, 2371543, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1929, 'Minas', 159, 1, 7, 'Milton Nascimento, Caetano Veloso', 152293, 4921056, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1930, 'Fé Cega, Faca Amolada', 159, 1, 7, 'Milton Nascimento, Ronaldo Bastos', 278099, 9258649, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1931, 'Beijo Partido', 159, 1, 7, 'Toninho Horta', 229564, 7506969, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1932, 'Saudade Dos Aviões Da Panair (Conversando No Bar)', 159, 1, 7, 'Milton Nascimento, Fernando Brant', 268721, 8805088, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1933, 'Gran Circo', 159, 1, 7, 'Milton Nascimento, Márcio Borges', 251297, 8237026, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1934, 'Ponta de Areia', 159, 1, 7, 'Milton Nascimento, Fernando Brant', 272796, 8874285, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1935, 'Trastevere', 159, 1, 7, 'Milton Nascimento, Ronaldo Bastos', 265665, 8708399, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1936, 'Idolatrada', 159, 1, 7, 'Milton Nascimento, Fernando Brant', 286249, 9426153, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1937, 'Leila (Venha Ser Feliz)', 159, 1, 7, 'Milton Nascimento', 209737, 6898507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1938, 'Paula E Bebeto', 159, 1, 7, 'Milton Nascimento, Caetano Veloso', 135732, 4583956, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1939, 'Simples', 159, 1, 7, 'Nelson Angelo', 133093, 4326333, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1940, 'Norwegian Wood', 159, 1, 7, 'John Lennon, Paul McCartney', 413910, 13520382, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1941, 'Caso Você Queira Saber', 159, 1, 7, 'Beto Guedes, Márcio Borges', 205688, 6787901, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1942, 'Ace Of Spades', 160, 1, 3, 'Clarke/Kilmister/Taylor', 169926, 5523552, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1943, 'Love Me Like A Reptile', 160, 1, 3, 'Clarke/Kilmister/Taylor', 203546, 6616389, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1944, 'Shoot You In The Back', 160, 1, 3, 'Clarke/Kilmister/Taylor', 160026, 5175327, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1945, 'Live To Win', 160, 1, 3, 'Clarke/Kilmister/Taylor', 217626, 7102182, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1946, 'Fast And Loose', 160, 1, 3, 'Clarke/Kilmister/Taylor', 203337, 6643350, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1947, '(We Are) The Road Crew', 160, 1, 3, 'Clarke/Kilmister/Taylor', 192600, 6283035, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1948, 'Fire Fire', 160, 1, 3, 'Clarke/Kilmister/Taylor', 164675, 5416114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1949, 'Jailbait', 160, 1, 3, 'Clarke/Kilmister/Taylor', 213916, 6983609, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1950, 'Dance', 160, 1, 3, 'Clarke/Kilmister/Taylor', 158432, 5155099, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1951, 'Bite The Bullet', 160, 1, 3, 'Clarke/Kilmister/Taylor', 98115, 3195536, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1952, 'The Chase Is Better Than The Catch', 160, 1, 3, 'Clarke/Kilmister/Taylor', 258403, 8393310, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1953, 'The Hammer', 160, 1, 3, 'Clarke/Kilmister/Taylor', 168071, 5543267, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1954, 'Dirty Love', 160, 1, 3, 'Clarke/Kilmister/Taylor', 176457, 5805241, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1955, 'Please Don\'t Touch', 160, 1, 3, 'Heath/Robinson', 169926, 5557002, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1956, 'Emergency', 160, 1, 3, 'Dufort/Johnson/McAuliffe/Williams', 180427, 5828728, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1957, 'Kir Royal', 161, 1, 16, 'Mônica Marianno', 234788, 7706552, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1958, 'O Que Vai Em Meu Coração', 161, 1, 16, 'Mônica Marianno', 255373, 8366846, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1959, 'Aos Leões', 161, 1, 16, 'Mônica Marianno', 234684, 7790574, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1960, 'Dois Índios', 161, 1, 16, 'Mônica Marianno', 219271, 7213072, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1961, 'Noite Negra', 161, 1, 16, 'Mônica Marianno', 206811, 6819584, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1962, 'Beijo do Olhar', 161, 1, 16, 'Mônica Marianno', 252682, 8369029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1963, 'É Fogo', 161, 1, 16, 'Mônica Marianno', 194873, 6501520, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1964, 'Já Foi', 161, 1, 16, 'Mônica Marianno', 245681, 8094872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1965, 'Só Se For Pelo Cabelo', 161, 1, 16, 'Mônica Marianno', 238288, 8006345, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1966, 'No Clima', 161, 1, 16, 'Mônica Marianno', 249495, 8362040, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1967, 'A Moça e a Chuva', 161, 1, 16, 'Mônica Marianno', 274625, 8929357, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1968, 'Demorou!', 161, 1, 16, 'Mônica Marianno', 39131, 1287083, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1969, 'Bitter Pill', 162, 1, 3, 'Mick Mars/Nikki Sixx/Tommy Lee/Vince Neil', 266814, 8666786, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1970, 'Enslaved', 162, 1, 3, 'Mick Mars/Nikki Sixx/Tommy Lee', 269844, 8789966, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1971, 'Girls, Girls, Girls', 162, 1, 3, 'Mick Mars/Nikki Sixx/Tommy Lee', 270288, 8874814, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1972, 'Kickstart My Heart', 162, 1, 3, 'Nikki Sixx', 283559, 9237736, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1973, 'Wild Side', 162, 1, 3, 'Nikki Sixx/Tommy Lee/Vince Neil', 276767, 9116997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1974, 'Glitter', 162, 1, 3, 'Bryan Adams/Nikki Sixx/Scott Humphrey', 340114, 11184094, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1975, 'Dr. Feelgood', 162, 1, 3, 'Mick Mars/Nikki Sixx', 282618, 9281875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1976, 'Same Ol\' Situation', 162, 1, 3, 'Mick Mars/Nikki Sixx/Tommy Lee/Vince Neil', 254511, 8283958, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1977, 'Home Sweet Home', 162, 1, 3, 'Nikki Sixx/Tommy Lee/Vince Neil', 236904, 7697538, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1978, 'Afraid', 162, 1, 3, 'Nikki Sixx', 248006, 8077464, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1979, 'Don\'t Go Away Mad (Just Go Away)', 162, 1, 3, 'Mick Mars/Nikki Sixx', 279980, 9188156, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1980, 'Without You', 162, 1, 3, 'Mick Mars/Nikki Sixx', 268956, 8738371, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1981, 'Smokin\' in The Boys Room', 162, 1, 3, 'Cub Coda/Michael Lutz', 206837, 6735408, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1982, 'Primal Scream', 162, 1, 3, 'Mick Mars/Nikki Sixx/Tommy Lee/Vince Neil', 286197, 9421164, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1983, 'Too Fast For Love', 162, 1, 3, 'Nikki Sixx', 200829, 6580542, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1984, 'Looks That Kill', 162, 1, 3, 'Nikki Sixx', 240979, 7831122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1985, 'Shout At The Devil', 162, 1, 3, 'Nikki Sixx', 221962, 7281974, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1986, 'Intro', 163, 1, 1, 'Kurt Cobain', 52218, 1688527, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1987, 'School', 163, 1, 1, 'Kurt Cobain', 160235, 5234885, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1988, 'Drain You', 163, 1, 1, 'Kurt Cobain', 215196, 7013175, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1989, 'Aneurysm', 163, 1, 1, 'Nirvana', 271516, 8862545, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1990, 'Smells Like Teen Spirit', 163, 1, 1, 'Nirvana', 287190, 9425215, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1991, 'Been A Son', 163, 1, 1, 'Kurt Cobain', 127555, 4170369, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1992, 'Lithium', 163, 1, 1, 'Kurt Cobain', 250017, 8148800, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1993, 'Sliver', 163, 1, 1, 'Kurt Cobain', 116218, 3784567, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1994, 'Spank Thru', 163, 1, 1, 'Kurt Cobain', 190354, 6186487, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1995, 'Scentless Apprentice', 163, 1, 1, 'Nirvana', 211200, 6898177, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1996, 'Heart-Shaped Box', 163, 1, 1, 'Kurt Cobain', 281887, 9210982, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1997, 'Milk It', 163, 1, 1, 'Kurt Cobain', 225724, 7406945, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1998, 'Negative Creep', 163, 1, 1, 'Kurt Cobain', 163761, 5354854, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (1999, 'Polly', 163, 1, 1, 'Kurt Cobain', 149995, 4885331, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2000, 'Breed', 163, 1, 1, 'Kurt Cobain', 208378, 6759080, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2001, 'Tourette\'s', 163, 1, 1, 'Kurt Cobain', 115591, 3753246, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2002, 'Blew', 163, 1, 1, 'Kurt Cobain', 216346, 7096936, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2003, 'Smells Like Teen Spirit', 164, 1, 1, 'Kurt Cobain', 301296, 9823847, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2004, 'In Bloom', 164, 1, 1, 'Kurt Cobain', 254928, 8327077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2005, 'Come As You Are', 164, 1, 1, 'Kurt Cobain', 219219, 7123357, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2006, 'Breed', 164, 1, 1, 'Kurt Cobain', 183928, 5984812, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2007, 'Lithium', 164, 1, 1, 'Kurt Cobain', 256992, 8404745, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2008, 'Polly', 164, 1, 1, 'Kurt Cobain', 177031, 5788407, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2009, 'Territorial Pissings', 164, 1, 1, 'Kurt Cobain', 143281, 4613880, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2010, 'Drain You', 164, 1, 1, 'Kurt Cobain', 223973, 7273440, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2011, 'Lounge Act', 164, 1, 1, 'Kurt Cobain', 156786, 5093635, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2012, 'Stay Away', 164, 1, 1, 'Kurt Cobain', 212636, 6956404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2013, 'On A Plain', 164, 1, 1, 'Kurt Cobain', 196440, 6390635, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2014, 'Something In The Way', 164, 1, 1, 'Kurt Cobain', 230556, 7472168, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2015, 'Time', 165, 1, 1, 96888, 3124455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2016, 'P.S.Apareça', 165, 1, 1, 209188, 6842244, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2017, 'Sangue Latino', 165, 1, 1, 223033, 7354184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2018, 'Folhas Secas', 165, 1, 1, 161253, 5284522, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2019, 'Poeira', 165, 1, 1, 267075, 8784141, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2020, 'Mágica', 165, 1, 1, 233743, 7627348, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2021, 'Quem Mata A Mulher Mata O Melhor', 165, 1, 1, 262791, 8640121, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2022, 'Mundaréu', 165, 1, 1, 217521, 7158975, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2023, 'O Braço Da Minha Guitarra', 165, 1, 1, 258351, 8469531, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2024, 'Deus', 165, 1, 1, 284160, 9188110, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2025, 'Mãe Terra', 165, 1, 1, 306625, 9949269, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2026, 'Às Vezes', 165, 1, 1, 330292, 10706614, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2027, 'Menino De Rua', 165, 1, 1, 329795, 10784595, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2028, 'Prazer E Fé', 165, 1, 1, 214831, 7031383, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2029, 'Elza', 165, 1, 1, 199105, 6517629, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2030, 'Requebra', 166, 1, 7, 240744, 8010811, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2031, 'Nossa Gente (Avisa Là)', 166, 1, 7, 188212, 6233201, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2032, 'Olodum - Alegria Geral', 166, 1, 7, 233404, 7754245, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2033, 'Madagáscar Olodum', 166, 1, 7, 252264, 8270584, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2034, 'Faraó Divindade Do Egito', 166, 1, 7, 228571, 7523278, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2035, 'Todo Amor (Asas Da Liberdade)', 166, 1, 7, 245133, 8121434, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2036, 'Denúncia', 166, 1, 7, 159555, 5327433, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2037, 'Olodum, A Banda Do Pelô', 166, 1, 7, 146599, 4900121, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2038, 'Cartao Postal', 166, 1, 7, 211565, 7082301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2039, 'Jeito Faceiro', 166, 1, 7, 217286, 7233608, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2040, 'Revolta Olodum', 166, 1, 7, 230191, 7557065, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2041, 'Reggae Odoyá', 166, 1, 7, 224470, 7499807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2042, 'Protesto Do Olodum (Ao Vivo)', 166, 1, 7, 206001, 6766104, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2043, 'Olodum - Smile (Instrumental)', 166, 1, 7, 235833, 7871409, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2044, 'Vulcão Dub - Fui Eu', 167, 1, 7, 'Bi Ribeira/Herbert Vianna/João Barone', 287059, 9495202, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2045, 'O Trem Da Juventude', 167, 1, 7, 'Herbert Vianna', 225880, 7507655, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2046, 'Manguetown', 167, 1, 7, 'Chico Science/Dengue/Lúcio Maia', 162925, 5382018, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2047, 'Um Amor, Um Lugar', 167, 1, 7, 'Herbert Vianna', 184555, 6090334, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2048, 'Bora-Bora', 167, 1, 7, 'Herbert Vianna', 182987, 6036046, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2049, 'Vai Valer', 167, 1, 7, 'Herbert Vianna', 206524, 6899778, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2050, 'I Feel Good (I Got You) - Sossego', 167, 1, 7, 'James Brown/Tim Maia', 244976, 8091302, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2051, 'Uns Dias', 167, 1, 7, 'Herbert Vianna', 240796, 7931552, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2052, 'Sincero Breu', 167, 1, 7, 'C. A./C.A./Celso Alvim/Herbert Vianna/Mário Moura/Pedro Luís/Sidon Silva', 208013, 6921669, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2053, 'Meu Erro', 167, 1, 7, 'Herbert Vianna', 188577, 6192791, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2054, 'Selvagem', 167, 1, 7, 'Bi Ribeiro/Herbert Vianna/João Barone', 148558, 4942831, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2055, 'Brasília 5:31', 167, 1, 7, 'Herbert Vianna', 178337, 5857116, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2056, 'Tendo A Lua', 167, 1, 7, 'Herbert Vianna/Tet Tillett', 198922, 6568180, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2057, 'Que País É Este', 167, 1, 7, 'Renato Russo', 216685, 7137865, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2058, 'Navegar Impreciso', 167, 1, 7, 'Herbert Vianna', 262870, 8761283, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2059, 'Feira Moderna', 167, 1, 7, 'Beto Guedes/Fernando Brant/L Borges', 182517, 6001793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2060, 'Tequila - Lourinha Bombril (Parate Y Mira)', 167, 1, 7, 'Bahiano/Chuck Rio/Diego Blanco/Herbert Vianna', 255738, 8514961, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2061, 'Vamo Batê Lata', 167, 1, 7, 'Herbert Vianna', 228754, 7585707, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2062, 'Life During Wartime', 167, 1, 7, 'Chris Frantz/David Byrne/Jerry Harrison/Tina Weymouth', 259186, 8543439, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2063, 'Nebulosa Do Amor', 167, 1, 7, 'Herbert Vianna', 203415, 6732496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2064, 'Caleidoscópio', 167, 1, 7, 'Herbert Vianna', 256522, 8484597, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2065, 'Trac Trac', 168, 1, 7, 'Fito Paez/Herbert Vianna', 231653, 7638256, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2066, 'Tendo A Lua', 168, 1, 7, 'Herbert Vianna/Tetê Tillet', 219585, 7342776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2067, 'Mensagen De Amor (2000)', 168, 1, 7, 'Herbert Vianna', 183588, 6061324, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2068, 'Lourinha Bombril', 168, 1, 7, 'Bahiano/Diego Blanco/Herbert Vianna', 159895, 5301882, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2069, 'La Bella Luna', 168, 1, 7, 'Herbert Vianna', 192653, 6428598, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2070, 'Busca Vida', 168, 1, 7, 'Herbert Vianna', 176431, 5798663, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2071, 'Uma Brasileira', 168, 1, 7, 'Carlinhos Brown/Herbert Vianna', 217573, 7280574, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2072, 'Luis Inacio (300 Picaretas)', 168, 1, 7, 'Herbert Vianna', 198191, 6576790, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2073, 'Saber Amar', 168, 1, 7, 'Herbert Vianna', 202788, 6723733, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2074, 'Ela Disse Adeus', 168, 1, 7, 'Herbert Vianna', 226298, 7608999, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2075, 'O Amor Nao Sabe Esperar', 168, 1, 7, 'Herbert Vianna', 241084, 8042534, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2076, 'Aonde Quer Que Eu Va', 168, 1, 7, 'Herbert Vianna/Paulo Sérgio Valle', 258089, 8470121, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2077, 'Caleidoscópio', 169, 1, 7, 211330, 7000017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2078, 'Óculos', 169, 1, 7, 219271, 7262419, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2079, 'Cinema Mudo', 169, 1, 7, 227918, 7612168, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2080, 'Alagados', 169, 1, 7, 302393, 10255463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2081, 'Lanterna Dos Afogados', 169, 1, 7, 190197, 6264318, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2082, 'Melô Do Marinheiro', 169, 1, 7, 208352, 6905668, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2083, 'Vital E Sua Moto', 169, 1, 7, 210207, 6902878, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2084, 'O Beco', 169, 1, 7, 189178, 6293184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2085, 'Meu Erro', 169, 1, 7, 208431, 6893533, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2086, 'Perplexo', 169, 1, 7, 161175, 5355013, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2087, 'Me Liga', 169, 1, 7, 229590, 7565912, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2088, 'Quase Um Segundo', 169, 1, 7, 275644, 8971355, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2089, 'Selvagem', 169, 1, 7, 245890, 8141084, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2090, 'Romance Ideal', 169, 1, 7, 250070, 8260477, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2091, 'Será Que Vai Chover?', 169, 1, 7, 337057, 11133830, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2092, 'SKA', 169, 1, 7, 148871, 4943540, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2093, 'Bark at the Moon', 170, 2, 1, 'O. Osbourne', 257252, 4601224, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2094, 'I Don\'t Know', 171, 2, 1, 'B. Daisley, O. Osbourne & R. Rhoads', 312980, 5525339, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2095, 'Crazy Train', 171, 2, 1, 'B. Daisley, O. Osbourne & R. Rhoads', 295960, 5255083, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2096, 'Flying High Again', 172, 2, 1, 'L. Kerslake, O. Osbourne, R. Daisley & R. Rhoads', 290851, 5179599, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2097, 'Mama, I\'m Coming Home', 173, 2, 1, 'L. Kilmister, O. Osbourne & Z. Wylde', 251586, 4302390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2098, 'No More Tears', 173, 2, 1, 'J. Purdell, M. Inez, O. Osbourne, R. Castillo & Z. Wylde', 444358, 7362964, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2099, 'I Don\'t Know', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 283088, 9207869, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2100, 'Crazy Train', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 322716, 10517408, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2101, 'Believer', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 308897, 10003794, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2102, 'Mr. Crowley', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 344241, 11184130, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2103, 'Flying High Again', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads, L. Kerslake', 261224, 8481822, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2104, 'Relvelation (Mother Earth)', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 349440, 11367866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2105, 'Steal Away (The Night)', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 485720, 15945806, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2106, 'Suicide Solution (With Guitar Solo)', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 467069, 15119938, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2107, 'Iron Man', 174, 1, 3, 'A. F. Iommi, W. Ward, T. Butler, J. Osbourne', 172120, 5609799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2108, 'Children Of The Grave', 174, 1, 3, 'A. F. Iommi, W. Ward, T. Butler, J. Osbourne', 357067, 11626740, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2109, 'Paranoid', 174, 1, 3, 'A. F. Iommi, W. Ward, T. Butler, J. Osbourne', 176352, 5729813, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2110, 'Goodbye To Romance', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 334393, 10841337, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2111, 'No Bone Movies', 174, 1, 3, 'O. Osbourne, R. Daisley, R. Rhoads', 249208, 8095199, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2112, 'Dee', 174, 1, 3, 'R. Rhoads', 261302, 8555963, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2113, 'Shining In The Light', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 240796, 7951688, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2114, 'When The World Was Young', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 373394, 12198930, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2115, 'Upon A Golden Horse', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 232359, 7594829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2116, 'Blue Train', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 405028, 13170391, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2117, 'Please Read The Letter', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 262112, 8603372, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2118, 'Most High', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 336535, 10999203, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2119, 'Heart In Your Hand', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 230896, 7598019, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2120, 'Walking Into Clarksdale', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 318511, 10396315, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2121, 'Burning Up', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 321619, 10525136, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2122, 'When I Was A Child', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 345626, 11249456, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2123, 'House Of Love', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 335699, 10990880, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2124, 'Sons Of Freedom', 175, 1, 1, 'Jimmy Page, Robert Plant, Charlie Jones, Michael Lee', 246465, 8087944, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2125, 'United Colours', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 330266, 10939131, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2126, 'Slug', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 281469, 9295950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2127, 'Your Blue Room', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 328228, 10867860, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2128, 'Always Forever Now', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 383764, 12727928, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2129, 'A Different Kind Of Blue', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 120816, 3884133, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2130, 'Beach Sequence', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 212297, 6928259, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2131, 'Miss Sarajevo', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 340767, 11064884, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2132, 'Ito Okashi', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 205087, 6572813, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2133, 'One Minute Warning', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 279693, 9335453, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2134, 'Corpse (These Chains Are Way Too Long)', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 214909, 6920451, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2135, 'Elvis Ate America', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 180166, 5851053, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2136, 'Plot 180', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 221596, 7253729, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2137, 'Theme From The Swan', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 203911, 6638076, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2138, 'Theme From Let\'s Go Native', 176, 1, 10, 'Brian Eno, Bono, Adam Clayton, The Edge & Larry Mullen Jnr.', 186723, 6179777, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2139, 'Wrathchild', 177, 1, 1, 'Steve Harris', 170396, 5499390, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2140, 'Killers', 177, 1, 1, 'Paul Di\'Anno/Steve Harris', 309995, 10009697, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2141, 'Prowler', 177, 1, 1, 'Steve Harris', 240274, 7782963, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2142, 'Murders In The Rue Morgue', 177, 1, 1, 'Steve Harris', 258638, 8360999, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2143, 'Women In Uniform', 177, 1, 1, 'Greg Macainsh', 189936, 6139651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2144, 'Remember Tomorrow', 177, 1, 1, 'Paul Di\'Anno/Steve Harris', 326426, 10577976, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2145, 'Sanctuary', 177, 1, 1, 'David Murray/Paul Di\'Anno/Steve Harris', 198844, 6423543, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2146, 'Running Free', 177, 1, 1, 'Paul Di\'Anno/Steve Harris', 199706, 6483496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2147, 'Phantom Of The Opera', 177, 1, 1, 'Steve Harris', 418168, 13585530, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2148, 'Iron Maiden', 177, 1, 1, 'Steve Harris', 235232, 7600077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2149, 'Corduroy', 178, 1, 1, 'Pearl Jam & Eddie Vedder', 305293, 9991106, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2150, 'Given To Fly', 178, 1, 1, 'Eddie Vedder & Mike McCready', 233613, 7678347, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2151, 'Hail, Hail', 178, 1, 1, 'Stone Gossard & Eddie Vedder & Jeff Ament & Mike McCready', 223764, 7364206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2152, 'Daughter', 178, 1, 1, 'Dave Abbruzzese & Jeff Ament & Stone Gossard & Mike McCready & Eddie Vedder', 407484, 13420697, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2153, 'Elderly Woman Behind The Counter In A Small Town', 178, 1, 1, 'Dave Abbruzzese & Jeff Ament & Stone Gossard & Mike McCready & Eddie Vedder', 229328, 7509304, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2154, 'Untitled', 178, 1, 1, 'Pearl Jam', 122801, 3957141, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2155, 'MFC', 178, 1, 1, 'Eddie Vedder', 148192, 4817665, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2156, 'Go', 178, 1, 1, 'Dave Abbruzzese & Jeff Ament & Stone Gossard & Mike McCready & Eddie Vedder', 161541, 5290810, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2157, 'Red Mosquito', 178, 1, 1, 'Jeff Ament & Stone Gossard & Jack Irons & Mike McCready & Eddie Vedder', 242991, 7944923, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2158, 'Even Flow', 178, 1, 1, 'Stone Gossard & Eddie Vedder', 317100, 10394239, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2159, 'Off He Goes', 178, 1, 1, 'Eddie Vedder', 343222, 11245109, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2160, 'Nothingman', 178, 1, 1, 'Jeff Ament & Eddie Vedder', 278595, 9107017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2161, 'Do The Evolution', 178, 1, 1, 'Eddie Vedder & Stone Gossard', 225462, 7377286, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2162, 'Better Man', 178, 1, 1, 'Eddie Vedder', 246204, 8019563, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2163, 'Black', 178, 1, 1, 'Stone Gossard & Eddie Vedder', 415712, 13580009, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2164, 'F*Ckin\' Up', 178, 1, 1, 'Neil Young', 377652, 12360893, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2165, 'Life Wasted', 179, 1, 4, 'Stone Gossard', 234344, 7610169, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2166, 'World Wide Suicide', 179, 1, 4, 'Eddie Vedder', 209188, 6885908, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2167, 'Comatose', 179, 1, 4, 'Mike McCready & Stone Gossard', 139990, 4574516, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2168, 'Severed Hand', 179, 1, 4, 'Eddie Vedder', 270341, 8817438, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2169, 'Marker In The Sand', 179, 1, 4, 'Mike McCready', 263235, 8656578, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2170, 'Parachutes', 179, 1, 4, 'Stone Gossard', 216555, 7074973, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2171, 'Unemployable', 179, 1, 4, 'Matt Cameron & Mike McCready', 184398, 6066542, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2172, 'Big Wave', 179, 1, 4, 'Jeff Ament', 178573, 5858788, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2173, 'Gone', 179, 1, 4, 'Eddie Vedder', 249547, 8158204, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2174, 'Wasted Reprise', 179, 1, 4, 'Stone Gossard', 53733, 1731020, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2175, 'Army Reserve', 179, 1, 4, 'Jeff Ament', 225567, 7393771, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2176, 'Come Back', 179, 1, 4, 'Eddie Vedder & Mike McCready', 329743, 10768701, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2177, 'Inside Job', 179, 1, 4, 'Eddie Vedder & Mike McCready', 428643, 14006924, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2178, 'Can\'t Keep', 180, 1, 1, 'Eddie Vedder', 219428, 7215713, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2179, 'Save You', 180, 1, 1, 'Eddie Vedder/Jeff Ament/Matt Cameron/Mike McCready/Stone Gossard', 230112, 7609110, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2180, 'Love Boat Captain', 180, 1, 1, 'Eddie Vedder', 276453, 9016789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2181, 'Cropduster', 180, 1, 1, 'Matt Cameron', 231888, 7588928, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2182, 'Ghost', 180, 1, 1, 'Jeff Ament', 195108, 6383772, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2183, 'I Am Mine', 180, 1, 1, 'Eddie Vedder', 215719, 7086901, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2184, 'Thumbing My Way', 180, 1, 1, 'Eddie Vedder', 250226, 8201437, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2185, 'You Are', 180, 1, 1, 'Matt Cameron', 270863, 8938409, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2186, 'Get Right', 180, 1, 1, 'Matt Cameron', 158589, 5223345, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2187, 'Green Disease', 180, 1, 1, 'Eddie Vedder', 161253, 5375818, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2188, 'Help Help', 180, 1, 1, 'Jeff Ament', 215092, 7033002, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2189, 'Bushleager', 180, 1, 1, 'Stone Gossard', 237479, 7849757, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2190, '1/2 Full', 180, 1, 1, 'Jeff Ament', 251010, 8197219, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2191, 'Arc', 180, 1, 1, 'Pearl Jam', 65593, 2099421, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2192, 'All or None', 180, 1, 1, 'Stone Gossard', 277655, 9104728, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2193, 'Once', 181, 1, 1, 'Stone Gossard', 231758, 7561555, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2194, 'Evenflow', 181, 1, 1, 'Stone Gossard', 293720, 9622017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2195, 'Alive', 181, 1, 1, 'Stone Gossard', 341080, 11176623, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2196, 'Why Go', 181, 1, 1, 'Jeff Ament', 200254, 6539287, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2197, 'Black', 181, 1, 1, 'Dave Krusen/Stone Gossard', 343823, 11213314, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2198, 'Jeremy', 181, 1, 1, 'Jeff Ament', 318981, 10447222, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2199, 'Oceans', 181, 1, 1, 'Jeff Ament/Stone Gossard', 162194, 5282368, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2200, 'Porch', 181, 1, 1, 'Eddie Vedder', 210520, 6877475, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2201, 'Garden', 181, 1, 1, 'Jeff Ament/Stone Gossard', 299154, 9740738, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2202, 'Deep', 181, 1, 1, 'Jeff Ament/Stone Gossard', 258324, 8432497, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2203, 'Release', 181, 1, 1, 'Jeff Ament/Mike McCready/Stone Gossard', 546063, 17802673, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2204, 'Go', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 193123, 6351920, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2205, 'Animal', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 169325, 5503459, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2206, 'Daughter', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 235598, 7824586, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2207, 'Glorified G', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 206968, 6772116, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2208, 'Dissident', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 215510, 7034500, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2209, 'W.M.A.', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 359262, 12037261, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2210, 'Blood', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 170631, 5551478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2211, 'Rearviewmirror', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 284186, 9321053, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2212, 'Rats', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 255425, 8341934, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2213, 'Elderly Woman Behind The Counter In A Small Town', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 196336, 6499398, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2214, 'Leash', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 189257, 6191560, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2215, 'Indifference', 182, 1, 1, 'Dave Abbruzzese/Eddie Vedder/Jeff Ament/Mike McCready/Stone Gossard', 302053, 9756133, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2216, 'Johnny B. Goode', 141, 1, 8, 243200, 8092024, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2217, 'Don\'t Look Back', 141, 1, 8, 221100, 7344023, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2218, 'Jah Seh No', 141, 1, 8, 276871, 9134476, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2219, 'I\'m The Toughest', 141, 1, 8, 230191, 7657594, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2220, 'Nothing But Love', 141, 1, 8, 221570, 7335228, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2221, 'Buk-In-Hamm Palace', 141, 1, 8, 265665, 8964369, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2222, 'Bush Doctor', 141, 1, 8, 239751, 7942299, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2223, 'Wanted Dread And Alive', 141, 1, 8, 260310, 8670933, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2224, 'Mystic Man', 141, 1, 8, 353671, 11812170, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2225, 'Coming In Hot', 141, 1, 8, 213054, 7109414, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2226, 'Pick Myself Up', 141, 1, 8, 234684, 7788255, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2227, 'Crystal Ball', 141, 1, 8, 309733, 10319296, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2228, 'Equal Rights Downpresser Man', 141, 1, 8, 366733, 12086524, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2229, 'Speak To Me/Breathe', 183, 1, 1, 'Mason/Waters, Gilmour, Wright', 234213, 7631305, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2230, 'On The Run', 183, 1, 1, 'Gilmour, Waters', 214595, 7206300, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2231, 'Time', 183, 1, 1, 'Mason, Waters, Wright, Gilmour', 425195, 13955426, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2232, 'The Great Gig In The Sky', 183, 1, 1, 'Wright, Waters', 284055, 9147563, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2233, 'Money', 183, 1, 1, 'Waters', 391888, 12930070, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2234, 'Us And Them', 183, 1, 1, 'Waters, Wright', 461035, 15000299, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2235, 'Any Colour You Like', 183, 1, 1, 'Gilmour, Mason, Wright, Waters', 205740, 6707989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2236, 'Brain Damage', 183, 1, 1, 'Waters', 230556, 7497655, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2237, 'Eclipse', 183, 1, 1, 'Waters', 125361, 4065299, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2238, 'ZeroVinteUm', 184, 1, 17, 315637, 10426550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2239, 'Queimando Tudo', 184, 1, 17, 172591, 5723677, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2240, 'Hip Hop Rio', 184, 1, 17, 151536, 4991935, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2241, 'Bossa', 184, 1, 17, 29048, 967098, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2242, '100% HardCore', 184, 1, 17, 165146, 5407744, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2243, 'Biruta', 184, 1, 17, 213263, 7108200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2244, 'Mão Na Cabeça', 184, 1, 17, 202631, 6642753, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2245, 'O Bicho Tá Pregando', 184, 1, 17, 171964, 5683369, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2246, 'Adoled (Ocean)', 184, 1, 17, 185103, 6009946, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2247, 'Seus Amigos', 184, 1, 17, 100858, 3304738, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2248, 'Paga Pau', 184, 1, 17, 197485, 6529041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2249, 'Rappers Reais', 184, 1, 17, 202004, 6684160, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2250, 'Nega Do Cabelo Duro', 184, 1, 17, 121808, 4116536, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2251, 'Hemp Family', 184, 1, 17, 205923, 6806900, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2252, 'Quem Me Cobrou?', 184, 1, 17, 121704, 3947664, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2253, 'Se Liga', 184, 1, 17, 410409, 13559173, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2254, 'Bohemian Rhapsody', 185, 1, 1, 'Mercury, Freddie', 358948, 11619868, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2255, 'Another One Bites The Dust', 185, 1, 1, 'Deacon, John', 216946, 7172355, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2256, 'Killer Queen', 185, 1, 1, 'Mercury, Freddie', 182099, 5967749, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2257, 'Fat Bottomed Girls', 185, 1, 1, 'May, Brian', 204695, 6630041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2258, 'Bicycle Race', 185, 1, 1, 'Mercury, Freddie', 183823, 6012409, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2259, 'You\'re My Best Friend', 185, 1, 1, 'Deacon, John', 172225, 5602173, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2260, 'Don\'t Stop Me Now', 185, 1, 1, 'Mercury, Freddie', 211826, 6896666, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2261, 'Save Me', 185, 1, 1, 'May, Brian', 228832, 7444624, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2262, 'Crazy Little Thing Called Love', 185, 1, 1, 'Mercury, Freddie', 164231, 5435501, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2263, 'Somebody To Love', 185, 1, 1, 'Mercury, Freddie', 297351, 9650520, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2264, 'Now I\'m Here', 185, 1, 1, 'May, Brian', 255346, 8328312, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2265, 'Good Old-Fashioned Lover Boy', 185, 1, 1, 'Mercury, Freddie', 175960, 5747506, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2266, 'Play The Game', 185, 1, 1, 'Mercury, Freddie', 213368, 6915832, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2267, 'Flash', 185, 1, 1, 'May, Brian', 168489, 5464986, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2268, 'Seven Seas Of Rhye', 185, 1, 1, 'Mercury, Freddie', 170553, 5539957, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2269, 'We Will Rock You', 185, 1, 1, 'Deacon, John/May, Brian', 122880, 4026955, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2270, 'We Are The Champions', 185, 1, 1, 'Mercury, Freddie', 180950, 5880231, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2271, 'We Will Rock You', 186, 1, 1, 'May', 122671, 4026815, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2272, 'We Are The Champions', 186, 1, 1, 'Mercury', 182883, 5939794, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2273, 'Sheer Heart Attack', 186, 1, 1, 'Taylor', 207386, 6642685, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2274, 'All Dead, All Dead', 186, 1, 1, 'May', 190119, 6144878, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2275, 'Spread Your Wings', 186, 1, 1, 'Deacon', 275356, 8936992, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2276, 'Fight From The Inside', 186, 1, 1, 'Taylor', 184737, 6078001, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2277, 'Get Down, Make Love', 186, 1, 1, 'Mercury', 231235, 7509333, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2278, 'Sleep On The Sidewalk', 186, 1, 1, 'May', 187428, 6099840, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2279, 'Who Needs You', 186, 1, 1, 'Deacon', 186958, 6292969, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2280, 'It\'s Late', 186, 1, 1, 'May', 386194, 12519388, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2281, 'My Melancholy Blues', 186, 1, 1, 'Mercury', 206471, 6691838, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2282, 'Shiny Happy People', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 226298, 7475323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2283, 'Me In Honey', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 246674, 8194751, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2284, 'Radio Song', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 255477, 8421172, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2285, 'Pop Song 89', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 185730, 6132218, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2286, 'Get Up', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 160235, 5264376, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2287, 'You Are The Everything', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 226298, 7373181, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2288, 'Stand', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 192862, 6349090, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2289, 'World Leader Pretend', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 259761, 8537282, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2290, 'The Wrong Child', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 216633, 7065060, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2291, 'Orange Crush', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 231706, 7742894, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2292, 'Turn You Inside-Out', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 257358, 8395671, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2293, 'Hairshirt', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 235911, 7753807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2294, 'I Remember California', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 304013, 9950311, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2295, 'Untitled', 188, 1, 4, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 191503, 6332426, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2296, 'How The West Was Won And Where It Got Us', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 271151, 8994291, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2297, 'The Wake-Up Bomb', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 308532, 10077337, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2298, 'New Test Leper', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 326791, 10866447, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2299, 'Undertow', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 309498, 10131005, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2300, 'E-Bow The Letter', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 324963, 10714576, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2301, 'Leave', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 437968, 14433365, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2302, 'Departure', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 209423, 6818425, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2303, 'Bittersweet Me', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 245812, 8114718, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2304, 'Be Mine', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 333087, 10790541, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2305, 'Binky The Doormat', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 301688, 9950320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2306, 'Zither', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 154148, 5032962, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2307, 'So Fast, So Numb', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 252682, 8341223, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2308, 'Low Desert', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 212062, 6989288, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2309, 'Electrolite', 189, 1, 1, 'Bill Berry-Peter Buck-Mike Mills-Michael Stipe', 245315, 8051199, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2310, 'Losing My Religion', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 269035, 8885672, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2311, 'Low', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 296777, 9633860, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2312, 'Near Wild Heaven', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 199862, 6610009, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2313, 'Endgame', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 230687, 7664479, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2314, 'Belong', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 247013, 8219375, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2315, 'Half A World Away', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 208431, 6837283, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2316, 'Texarkana', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 220081, 7260681, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2317, 'Country Feedback', 187, 1, 4, 'Bill Berry/Michael Stipe/Mike Mills/Peter Buck', 249782, 8178943, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2318, 'Carnival Of Sorts', 190, 1, 4, 'R.E.M.', 233482, 7669658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2319, 'Radio Free Aurope', 190, 1, 4, 'R.E.M.', 245315, 8163490, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2320, 'Perfect Circle', 190, 1, 4, 'R.E.M.', 208509, 6898067, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2321, 'Talk About The Passion', 190, 1, 4, 'R.E.M.', 203206, 6725435, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2322, 'So Central Rain', 190, 1, 4, 'R.E.M.', 194768, 6414550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2323, 'Don\'t Go Back To Rockville', 190, 1, 4, 'R.E.M.', 272352, 9010715, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2324, 'Pretty Persuasion', 190, 1, 4, 'R.E.M.', 229929, 7577754, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2325, 'Green Grow The Rushes', 190, 1, 4, 'R.E.M.', 225671, 7422425, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2326, 'Can\'t Get There From Here', 190, 1, 4, 'R.E.M.', 220630, 7285936, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2327, 'Driver 8', 190, 1, 4, 'R.E.M.', 204747, 6779076, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2328, 'Fall On Me', 190, 1, 4, 'R.E.M.', 172016, 5676811, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2329, 'I Believe', 190, 1, 4, 'R.E.M.', 227709, 7542929, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2330, 'Cuyahoga', 190, 1, 4, 'R.E.M.', 260623, 8591057, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2331, 'The One I Love', 190, 1, 4, 'R.E.M.', 197355, 6495125, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2332, 'The Finest Worksong', 190, 1, 4, 'R.E.M.', 229276, 7574856, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2333, 'It\'s The End Of The World As We Know It (And I Feel Fine)', 190, 1, 4, 'R.E.M.', 244819, 7998987, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2334, 'Infeliz Natal', 191, 1, 4, 'Rodolfo', 138266, 4503299, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2335, 'A Sua', 191, 1, 4, 'Rodolfo', 142132, 4622064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2336, 'Papeau Nuky Doe', 191, 1, 4, 'Rodolfo', 121652, 3995022, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2337, 'Merry Christmas', 191, 1, 4, 'Rodolfo', 126040, 4166652, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2338, 'Bodies', 191, 1, 4, 'Rodolfo', 180035, 5873778, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2339, 'Puteiro Em João Pessoa', 191, 1, 4, 'Rodolfo', 195578, 6395490, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2340, 'Esporrei Na Manivela', 191, 1, 4, 'Rodolfo', 293276, 9618499, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2341, 'Bê-a-Bá', 191, 1, 4, 'Rodolfo', 249051, 8130636, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2342, 'Cajueiro', 191, 1, 4, 'Rodolfo', 158589, 5164837, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2343, 'Palhas Do Coqueiro', 191, 1, 4, 'Rodolfo', 133851, 4396466, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2344, 'Maluco Beleza', 192, 1, 1, 203206, 6628067, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2345, 'O Dia Em Que A Terra Parou', 192, 1, 1, 261720, 8586678, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2346, 'No Fundo Do Quintal Da Escola', 192, 1, 1, 177606, 5836953, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2347, 'O Segredo Do Universo', 192, 1, 1, 192679, 6315187, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2348, 'As Profecias', 192, 1, 1, 232515, 7657732, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2349, 'Mata Virgem', 192, 1, 1, 142602, 4690029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2350, 'Sapato 36', 192, 1, 1, 196702, 6507301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2351, 'Todo Mundo Explica', 192, 1, 1, 134896, 4449772, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2352, 'Que Luz É Essa', 192, 1, 1, 165067, 5620058, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2353, 'Diamante De Mendigo', 192, 1, 1, 206053, 6775101, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2354, 'Negócio É', 192, 1, 1, 175464, 5826775, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2355, 'Muita Estrela, Pouca Constelação', 192, 1, 1, 268068, 8781021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2356, 'Século XXI', 192, 1, 1, 244897, 8040563, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2357, 'Rock Das Aranhas (Ao Vivo) (Live)', 192, 1, 1, 231836, 7591945, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2358, 'The Power Of Equality', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 243591, 8148266, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2359, 'If You Have To Ask', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 216790, 7199175, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2360, 'Breaking The Girl', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 295497, 9805526, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2361, 'Funky Monks', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 323395, 10708168, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2362, 'Suck My Kiss', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 217234, 7129137, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2363, 'I Could Have Lied', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 244506, 8088244, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2364, 'Mellowship Slinky In B Major', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 240091, 7971384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2365, 'The Righteous & The Wicked', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 248084, 8134096, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2366, 'Give It Away', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 283010, 9308997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2367, 'Blood Sugar Sex Magik', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 271229, 8940573, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2368, 'Under The Bridge', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 264359, 8682716, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2369, 'Naked In The Rain', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 265717, 8724674, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2370, 'Apache Rose Peacock', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 282226, 9312588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2371, 'The Greeting Song', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 193593, 6346507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2372, 'My Lovely Man', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 279118, 9220114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2373, 'Sir Psycho Sexy', 193, 1, 4, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 496692, 16354362, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2374, 'They\'re Red Hot', 193, 1, 4, 'Robert Johnson', 71941, 2382220, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2375, 'By The Way', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 218017, 7197430, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2376, 'Universally Speaking', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 259213, 8501904, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2377, 'This Is The Place', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 257906, 8469765, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2378, 'Dosed', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 312058, 10235611, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2379, 'Don\'t Forget Me', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 277995, 9107071, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2380, 'The Zephyr Song', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 232960, 7690312, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2381, 'Can\'t Stop', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 269400, 8872479, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2382, 'I Could Die For You', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 193906, 6333311, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2383, 'Midnight', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 295810, 9702450, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2384, 'Throw Away Your Television', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 224574, 7483526, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2385, 'Cabron', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 218592, 7458864, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2386, 'Tear', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 317413, 10395500, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2387, 'On Mercury', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 208509, 6834762, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2388, 'Minor Thing', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 217835, 7148115, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2389, 'Warm Tape', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 256653, 8358200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2390, 'Venice Queen', 194, 1, 1, 'Anthony Kiedis, Flea, John Frusciante, and Chad Smith', 369110, 12280381, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2391, 'Around The World', 195, 1, 1, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 238837, 7859167, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2392, 'Parallel Universe', 195, 1, 1, 'Red Hot Chili Peppers', 270654, 8958519, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2393, 'Scar Tissue', 195, 1, 1, 'Red Hot Chili Peppers', 217469, 7153744, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2394, 'Otherside', 195, 1, 1, 'Red Hot Chili Peppers', 255973, 8357989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2395, 'Get On Top', 195, 1, 1, 'Red Hot Chili Peppers', 198164, 6587883, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2396, 'Californication', 195, 1, 1, 'Red Hot Chili Peppers', 321671, 10568999, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2397, 'Easily', 195, 1, 1, 'Red Hot Chili Peppers', 231418, 7504534, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2398, 'Porcelain', 195, 1, 1, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 163787, 5278793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2399, 'Emit Remmus', 195, 1, 1, 'Red Hot Chili Peppers', 240300, 7901717, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2400, 'I Like Dirt', 195, 1, 1, 'Red Hot Chili Peppers', 157727, 5225917, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2401, 'This Velvet Glove', 195, 1, 1, 'Red Hot Chili Peppers', 225280, 7480537, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2402, 'Savior', 195, 1, 1, 'Anthony Kiedis/Chad Smith/Flea/John Frusciante', 292493, 9551885, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2403, 'Purple Stain', 195, 1, 1, 'Red Hot Chili Peppers', 253440, 8359971, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2404, 'Right On Time', 195, 1, 1, 'Red Hot Chili Peppers', 112613, 3722219, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2405, 'Road Trippin\'', 195, 1, 1, 'Red Hot Chili Peppers', 205635, 6685831, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2406, 'The Spirit Of Radio', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 299154, 9862012, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2407, 'The Trees', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 285126, 9345473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2408, 'Something For Nothing', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 240770, 7898395, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2409, 'Freewill', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 324362, 10694110, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2410, 'Xanadu', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 667428, 21753168, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2411, 'Bastille Day', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 280528, 9264769, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2412, 'By-Tor And The Snow Dog', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 519888, 17076397, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2413, 'Anthem', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 264515, 8693343, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2414, 'Closer To The Heart', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 175412, 5767005, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2415, '2112 Overture', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 272718, 8898066, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2416, 'The Temples Of Syrinx', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 133459, 4360163, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2417, 'La Villa Strangiato', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 577488, 19137855, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2418, 'Fly By Night', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 202318, 6683061, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2419, 'Finding My Way', 196, 1, 1, 'Geddy Lee And Alex Lifeson/Geddy Lee And Neil Peart/Rush', 305528, 9985701, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2420, 'Jingo', 197, 1, 1, 'M.Babatunde Olantunji', 592953, 19736495, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2421, 'El Corazon Manda', 197, 1, 1, 'E.Weiss', 713534, 23519583, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2422, 'La Puesta Del Sol', 197, 1, 1, 'E.Weiss', 628062, 20614621, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2423, 'Persuasion', 197, 1, 1, 'Carlos Santana', 318432, 10354751, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2424, 'As The Years Go by', 197, 1, 1, 'Albert King', 233064, 7566829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2425, 'Soul Sacrifice', 197, 1, 1, 'Carlos Santana', 296437, 9801120, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2426, 'Fried Neckbones And Home Fries', 197, 1, 1, 'W.Correa', 638563, 20939646, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2427, 'Santana Jam', 197, 1, 1, 'Carlos Santana', 882834, 29207100, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2428, 'Evil Ways', 198, 1, 1, 475402, 15289235, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2429, 'We\'ve Got To Get Together/Jingo', 198, 1, 1, 1070027, 34618222, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2430, 'Rock Me', 198, 1, 1, 94720, 3037596, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2431, 'Just Ain\'t Good Enough', 198, 1, 1, 850259, 27489067, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2432, 'Funky Piano', 198, 1, 1, 934791, 30200730, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2433, 'The Way You Do To Mer', 198, 1, 1, 618344, 20028702, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2434, 'Holding Back The Years', 141, 1, 1, 'Mick Hucknall and Neil Moss', 270053, 8833220, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2435, 'Money\'s Too Tight To Mention', 141, 1, 1, 'John and William Valentine', 268408, 8861921, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2436, 'The Right Thing', 141, 1, 1, 'Mick Hucknall', 262687, 8624063, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2437, 'It\'s Only Love', 141, 1, 1, 'Jimmy and Vella Cameron', 232594, 7659017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2438, 'A New Flame', 141, 1, 1, 'Mick Hucknall', 237662, 7822875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2439, 'You\'ve Got It', 141, 1, 1, 'Mick Hucknall and Lamont Dozier', 235232, 7712845, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2440, 'If You Don\'t Know Me By Now', 141, 1, 1, 'Kenny Gamble and Leon Huff', 206524, 6712634, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2441, 'Stars', 141, 1, 1, 'Mick Hucknall', 248137, 8194906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2442, 'Something Got Me Started', 141, 1, 1, 'Mick Hucknall and Fritz McIntyre', 239595, 7997139, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2443, 'Thrill Me', 141, 1, 1, 'Mick Hucknall and Fritz McIntyre', 303934, 10034711, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2444, 'Your Mirror', 141, 1, 1, 'Mick Hucknall', 240666, 7893821, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2445, 'For Your Babies', 141, 1, 1, 'Mick Hucknall', 256992, 8408803, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2446, 'So Beautiful', 141, 1, 1, 'Mick Hucknall', 298083, 9837832, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2447, 'Angel', 141, 1, 1, 'Carolyn Franklin and Sonny Saunders', 240561, 7880256, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2448, 'Fairground', 141, 1, 1, 'Mick Hucknall', 263888, 8793094, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2449, 'Água E Fogo', 199, 1, 1, 'Chico Amaral/Edgard Scandurra/Samuel Rosa', 278987, 9272272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2450, 'Três Lados', 199, 1, 1, 'Chico Amaral/Samuel Rosa', 233665, 7699609, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2451, 'Ela Desapareceu', 199, 1, 1, 'Chico Amaral/Samuel Rosa', 250122, 8289200, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2452, 'Balada Do Amor Inabalável', 199, 1, 1, 'Fausto Fawcett/Samuel Rosa', 240613, 8025816, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2453, 'Canção Noturna', 199, 1, 1, 'Chico Amaral/Lelo Zanettik', 238628, 7874774, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2454, 'Muçulmano', 199, 1, 1, 'Leão, Rodrigo F./Samuel Rosa', 249600, 8270613, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2455, 'Maquinarama', 199, 1, 1, 'Chico Amaral/Samuel Rosa', 245629, 8213710, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2456, 'Rebelião', 199, 1, 1, 'Chico Amaral/Samuel Rosa', 298527, 9817847, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2457, 'A Última Guerra', 199, 1, 1, 'Leão, Rodrigo F./Lô Borges/Samuel Rosa', 314723, 10480391, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2458, 'Fica', 199, 1, 1, 'Chico Amaral/Samuel Rosa', 272169, 8980972, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2459, 'Ali', 199, 1, 1, 'Nando Reis/Samuel Rosa', 306390, 10110351, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2460, 'Preto Damião', 199, 1, 1, 'Chico Amaral/Samuel Rosa', 264568, 8697658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2461, 'É Uma Partida De Futebol', 200, 1, 1, 'Samuel Rosa', 1071, 38747, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2462, 'Eu Disse A Ela', 200, 1, 1, 'Samuel Rosa', 254223, 8479463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2463, 'Zé Trindade', 200, 1, 1, 'Samuel Rosa', 247954, 8331310, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2464, 'Garota Nacional', 200, 1, 1, 'Samuel Rosa', 317492, 10511239, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2465, 'Tão Seu', 200, 1, 1, 'Samuel Rosa', 243748, 8133126, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2466, 'Sem Terra', 200, 1, 1, 'Samuel Rosa', 279353, 9196411, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2467, 'Os Exilados', 200, 1, 1, 'Samuel Rosa', 245551, 8222095, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2468, 'Um Dia Qualquer', 200, 1, 1, 'Samuel Rosa', 292414, 9805570, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2469, 'Los Pretos', 200, 1, 1, 'Samuel Rosa', 239229, 8025667, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2470, 'Sul Da América', 200, 1, 1, 'Samuel Rosa', 254928, 8484871, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2471, 'Poconé', 200, 1, 1, 'Samuel Rosa', 318406, 10771610, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2472, 'Lucky 13', 201, 1, 4, 'Billy Corgan', 189387, 6200617, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2473, 'Aeroplane Flies High', 201, 1, 4, 'Billy Corgan', 473391, 15408329, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2474, 'Because You Are', 201, 1, 4, 'Billy Corgan', 226403, 7405137, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2475, 'Slow Dawn', 201, 1, 4, 'Billy Corgan', 192339, 6269057, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2476, 'Believe', 201, 1, 4, 'James Iha', 192940, 6320652, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2477, 'My Mistake', 201, 1, 4, 'Billy Corgan', 240901, 7843477, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2478, 'Marquis In Spades', 201, 1, 4, 'Billy Corgan', 192731, 6304789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2479, 'Here\'s To The Atom Bomb', 201, 1, 4, 'Billy Corgan', 266893, 8763140, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2480, 'Sparrow', 201, 1, 4, 'Billy Corgan', 176822, 5696989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2481, 'Waiting', 201, 1, 4, 'Billy Corgan', 228336, 7627641, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2482, 'Saturnine', 201, 1, 4, 'Billy Corgan', 229877, 7523502, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2483, 'Rock On', 201, 1, 4, 'David Cook', 366471, 12133825, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2484, 'Set The Ray To Jerry', 201, 1, 4, 'Billy Corgan', 249364, 8215184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2485, 'Winterlong', 201, 1, 4, 'Billy Corgan', 299389, 9670616, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2486, 'Soot & Stars', 201, 1, 4, 'Billy Corgan', 399986, 12866557, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2487, 'Blissed & Gone', 201, 1, 4, 'Billy Corgan', 286302, 9305998, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2488, 'Siva', 202, 1, 4, 'Billy Corgan', 261172, 8576622, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2489, 'Rhinocerous', 202, 1, 4, 'Billy Corgan', 353462, 11526684, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2490, 'Drown', 202, 1, 4, 'Billy Corgan', 270497, 8883496, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2491, 'Cherub Rock', 202, 1, 4, 'Billy Corgan', 299389, 9786739, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2492, 'Today', 202, 1, 4, 'Billy Corgan', 202213, 6596933, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2493, 'Disarm', 202, 1, 4, 'Billy Corgan', 198556, 6508249, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2494, 'Landslide', 202, 1, 4, 'Stevie Nicks', 190275, 6187754, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2495, 'Bullet With Butterfly Wings', 202, 1, 4, 'Billy Corgan', 257306, 8431747, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2496, '1979', 202, 1, 4, 'Billy Corgan', 263653, 8728470, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2497, 'Zero', 202, 1, 4, 'Billy Corgan', 161123, 5267176, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2498, 'Tonight, Tonight', 202, 1, 4, 'Billy Corgan', 255686, 8351543, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2499, 'Eye', 202, 1, 4, 'Billy Corgan', 294530, 9784201, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2500, 'Ava Adore', 202, 1, 4, 'Billy Corgan', 261433, 8590412, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2501, 'Perfect', 202, 1, 4, 'Billy Corgan', 203023, 6734636, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2502, 'The Everlasting Gaze', 202, 1, 4, 'Billy Corgan', 242155, 7844404, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2503, 'Stand Inside Your Love', 202, 1, 4, 'Billy Corgan', 253753, 8270113, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2504, 'Real Love', 202, 1, 4, 'Billy Corgan', 250697, 8025896, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2505, '[Untitled]', 202, 1, 4, 'Billy Corgan', 231784, 7689713, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2506, 'Nothing To Say', 203, 1, 1, 'Chris Cornell/Kim Thayil', 238027, 7744833, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2507, 'Flower', 203, 1, 1, 'Chris Cornell/Kim Thayil', 208822, 6830732, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2508, 'Loud Love', 203, 1, 1, 'Chris Cornell', 297456, 9660953, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2509, 'Hands All Over', 203, 1, 1, 'Chris Cornell/Kim Thayil', 362475, 11893108, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2510, 'Get On The Snake', 203, 1, 1, 'Chris Cornell/Kim Thayil', 225123, 7313744, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2511, 'Jesus Christ Pose', 203, 1, 1, 'Ben Shepherd/Chris Cornell/Kim Thayil/Matt Cameron', 352966, 11739886, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2512, 'Outshined', 203, 1, 1, 'Chris Cornell', 312476, 10274629, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2513, 'Rusty Cage', 203, 1, 1, 'Chris Cornell', 267728, 8779485, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2514, 'Spoonman', 203, 1, 1, 'Chris Cornell', 248476, 8289906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2515, 'The Day I Tried To Live', 203, 1, 1, 'Chris Cornell', 321175, 10507137, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2516, 'Black Hole Sun', 203, 1, 1, 'Soundgarden', 320365, 10425229, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2517, 'Fell On Black Days', 203, 1, 1, 'Chris Cornell', 282331, 9256082, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2518, 'Pretty Noose', 203, 1, 1, 'Chris Cornell', 253570, 8317931, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2519, 'Burden In My Hand', 203, 1, 1, 'Chris Cornell', 292153, 9659911, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2520, 'Blow Up The Outside World', 203, 1, 1, 'Chris Cornell', 347898, 11379527, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2521, 'Ty Cobb', 203, 1, 1, 'Ben Shepherd/Chris Cornell', 188786, 6233136, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2522, 'Bleed Together', 203, 1, 1, 'Chris Cornell', 232202, 7597074, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2523, 'Morning Dance', 204, 1, 2, 'Jay Beckenstein', 238759, 8101979, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2524, 'Jubilee', 204, 1, 2, 'Jeremy Wall', 275147, 9151846, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2525, 'Rasul', 204, 1, 2, 'Jeremy Wall', 238315, 7854737, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2526, 'Song For Lorraine', 204, 1, 2, 'Jay Beckenstein', 240091, 8101723, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2527, 'Starburst', 204, 1, 2, 'Jeremy Wall', 291500, 9768399, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2528, 'Heliopolis', 204, 1, 2, 'Jay Beckenstein', 338729, 11365655, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2529, 'It Doesn\'t Matter', 204, 1, 2, 'Chet Catallo', 270027, 9034177, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2530, 'Little Linda', 204, 1, 2, 'Jeremy Wall', 264019, 8958743, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2531, 'End Of Romanticism', 204, 1, 2, 'Rick Strauss', 320078, 10553155, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2532, 'The House Is Rockin\'', 205, 1, 6, 'Doyle Bramhall/Stevie Ray Vaughan', 144352, 4706253, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2533, 'Crossfire', 205, 1, 6, 'B. Carter/C. Layton/R. Ellsworth/R. Wynans/T. Shannon', 251219, 8238033, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2534, 'Tightrope', 205, 1, 6, 'Doyle Bramhall/Stevie Ray Vaughan', 281155, 9254906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2535, 'Let Me Love You Baby', 205, 1, 6, 'Willie Dixon', 164127, 5378455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2536, 'Leave My Girl Alone', 205, 1, 6, 'B. Guy', 256365, 8438021, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2537, 'Travis Walk', 205, 1, 6, 'Stevie Ray Vaughan', 140826, 4650979, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2538, 'Wall Of Denial', 205, 1, 6, 'Doyle Bramhall/Stevie Ray Vaughan', 336927, 11085915, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2539, 'Scratch-N-Sniff', 205, 1, 6, 'Doyle Bramhall/Stevie Ray Vaughan', 163422, 5353627, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2540, 'Love Me Darlin\'', 205, 1, 6, 'C. Burnett', 201586, 6650869, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2541, 'Riviera Paradise', 205, 1, 6, 'Stevie Ray Vaughan', 528692, 17232776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2542, 'Dead And Bloated', 206, 1, 1, 'R. DeLeo/Weiland', 310386, 10170433, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2543, 'Sex Type Thing', 206, 1, 1, 'D. DeLeo/Kretz/Weiland', 218723, 7102064, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2544, 'Wicked Garden', 206, 1, 1, 'D. DeLeo/R. DeLeo/Weiland', 245368, 7989505, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2545, 'No Memory', 206, 1, 1, 'Dean Deleo', 80613, 2660859, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2546, 'Sin', 206, 1, 1, 'R. DeLeo/Weiland', 364800, 12018823, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2547, 'Naked Sunday', 206, 1, 1, 'D. DeLeo/Kretz/R. DeLeo/Weiland', 229720, 7444201, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2548, 'Creep', 206, 1, 1, 'R. DeLeo/Weiland', 333191, 10894988, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2549, 'Piece Of Pie', 206, 1, 1, 'R. DeLeo/Weiland', 324623, 10605231, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2550, 'Plush', 206, 1, 1, 'R. DeLeo/Weiland', 314017, 10229848, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2551, 'Wet My Bed', 206, 1, 1, 'R. DeLeo/Weiland', 96914, 3198627, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2552, 'Crackerman', 206, 1, 1, 'Kretz/R. DeLeo/Weiland', 194403, 6317361, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2553, 'Where The River Goes', 206, 1, 1, 'D. DeLeo/Kretz/Weiland', 505991, 16468904, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2554, 'Soldier Side - Intro', 207, 1, 3, 'Dolmayan, John/Malakian, Daron/Odadjian, Shavo', 63764, 2056079, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2555, 'B.Y.O.B.', 207, 1, 3, 'Tankian, Serj', 255555, 8407935, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2556, 'Revenga', 207, 1, 3, 'Tankian, Serj', 228127, 7503805, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2557, 'Cigaro', 207, 1, 3, 'Tankian, Serj', 131787, 4321705, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2558, 'Radio/Video', 207, 1, 3, 'Dolmayan, John/Malakian, Daron/Odadjian, Shavo', 249312, 8224917, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2559, 'This Cocaine Makes Me Feel Like I\'m On This Song', 207, 1, 3, 'Tankian, Serj', 128339, 4185193, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2560, 'Violent Pornography', 207, 1, 3, 'Dolmayan, John/Malakian, Daron/Odadjian, Shavo', 211435, 6985960, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2561, 'Question!', 207, 1, 3, 'Tankian, Serj', 200698, 6616398, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2562, 'Sad Statue', 207, 1, 3, 'Tankian, Serj', 205897, 6733449, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2563, 'Old School Hollywood', 207, 1, 3, 'Dolmayan, John/Malakian, Daron/Odadjian, Shavo', 176953, 5830258, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2564, 'Lost in Hollywood', 207, 1, 3, 'Tankian, Serj', 320783, 10535158, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2565, 'The Sun Road', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 880640, 29008407, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2566, 'Dark Corners', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 513541, 16839223, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2567, 'Duende', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 447582, 14956771, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2568, 'Black Light Syndrome', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 526471, 17300835, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2569, 'Falling in Circles', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 549093, 18263248, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2570, 'Book of Hours', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 583366, 19464726, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2571, 'Chaos-Control', 208, 1, 1, 'Terry Bozzio, Steve Stevens, Tony Levin', 529841, 17455568, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2572, 'Midnight From The Inside Out', 209, 1, 6, 'Chris Robinson/Rich Robinson', 286981, 9442157, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2573, 'Sting Me', 209, 1, 6, 'Chris Robinson/Rich Robinson', 268094, 8813561, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2574, 'Thick & Thin', 209, 1, 6, 'Chris Robinson/Rich Robinson', 222720, 7284377, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2575, 'Greasy Grass River', 209, 1, 6, 'Chris Robinson/Rich Robinson', 218749, 7157045, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2576, 'Sometimes Salvation', 209, 1, 6, 'Chris Robinson/Rich Robinson', 389146, 12749424, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2577, 'Cursed Diamonds', 209, 1, 6, 'Chris Robinson/Rich Robinson', 368300, 12047978, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2578, 'Miracle To Me', 209, 1, 6, 'Chris Robinson/Rich Robinson', 372636, 12222116, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2579, 'Wiser Time', 209, 1, 6, 'Chris Robinson/Rich Robinson', 459990, 15161907, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2580, 'Girl From A Pawnshop', 209, 1, 6, 'Chris Robinson/Rich Robinson', 404688, 13250848, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2581, 'Cosmic Fiend', 209, 1, 6, 'Chris Robinson/Rich Robinson', 308401, 10115556, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2582, 'Black Moon Creeping', 210, 1, 6, 'Chris Robinson/Rich Robinson', 359314, 11740886, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2583, 'High Head Blues', 210, 1, 6, 'Chris Robinson/Rich Robinson', 371879, 12227998, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2584, 'Title Song', 210, 1, 6, 'Chris Robinson/Rich Robinson', 505521, 16501316, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2585, 'She Talks To Angels', 210, 1, 6, 'Chris Robinson/Rich Robinson', 361978, 11837342, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2586, 'Twice As Hard', 210, 1, 6, 'Chris Robinson/Rich Robinson', 275565, 9008067, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2587, 'Lickin\'', 210, 1, 6, 'Chris Robinson/Rich Robinson', 314409, 10331216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2588, 'Soul Singing', 210, 1, 6, 'Chris Robinson/Rich Robinson', 233639, 7672489, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2589, 'Hard To Handle', 210, 1, 6, 'A.Isbell/A.Jones/O.Redding', 206994, 6786304, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2590, 'Remedy', 210, 1, 6, 'Chris Robinson/Rich Robinson', 337084, 11049098, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2591, 'White Riot', 211, 1, 4, 'Joe Strummer/Mick Jones', 118726, 3922819, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2592, 'Remote Control', 211, 1, 4, 'Joe Strummer/Mick Jones', 180297, 5949647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2593, 'Complete Control', 211, 1, 4, 'Joe Strummer/Mick Jones', 192653, 6272081, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2594, 'Clash City Rockers', 211, 1, 4, 'Joe Strummer/Mick Jones', 227500, 7555054, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2595, '(White Man) In Hammersmith Palais', 211, 1, 4, 'Joe Strummer/Mick Jones', 240640, 7883532, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2596, 'Tommy Gun', 211, 1, 4, 'Joe Strummer/Mick Jones', 195526, 6399872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2597, 'English Civil War', 211, 1, 4, 'Mick Jones/Traditional arr. Joe Strummer', 156708, 5111226, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2598, 'I Fought The Law', 211, 1, 4, 'Sonny Curtis', 159764, 5245258, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2599, 'London Calling', 211, 1, 4, 'Joe Strummer/Mick Jones', 199706, 6569007, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2600, 'Train In Vain', 211, 1, 4, 'Joe Strummer/Mick Jones', 189675, 6329877, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2601, 'Bankrobber', 211, 1, 4, 'Joe Strummer/Mick Jones', 272431, 9067323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2602, 'The Call Up', 211, 1, 4, 'The Clash', 324336, 10746937, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2603, 'Hitsville UK', 211, 1, 4, 'The Clash', 261433, 8606887, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2604, 'The Magnificent Seven', 211, 1, 4, 'The Clash', 268486, 8889821, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2605, 'This Is Radio Clash', 211, 1, 4, 'The Clash', 249756, 8366573, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2606, 'Know Your Rights', 211, 1, 4, 'The Clash', 217678, 7195726, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2607, 'Rock The Casbah', 211, 1, 4, 'The Clash', 222145, 7361500, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2608, 'Should I Stay Or Should I Go', 211, 1, 4, 'The Clash', 187219, 6188688, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2609, 'War (The Process)', 212, 1, 1, 'Billy Duffy/Ian Astbury', 252630, 8254842, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2610, 'The Saint', 212, 1, 1, 'Billy Duffy/Ian Astbury', 216215, 7061584, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2611, 'Rise', 212, 1, 1, 'Billy Duffy/Ian Astbury', 219088, 7106195, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2612, 'Take The Power', 212, 1, 1, 'Billy Duffy/Ian Astbury', 235755, 7650012, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2613, 'Breathe', 212, 1, 1, 'Billy Duffy/Ian Astbury/Marti Frederiksen/Mick Jones', 299781, 9742361, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2614, 'Nico', 212, 1, 1, 'Billy Duffy/Ian Astbury', 289488, 9412323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2615, 'American Gothic', 212, 1, 1, 'Billy Duffy/Ian Astbury', 236878, 7739840, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2616, 'Ashes And Ghosts', 212, 1, 1, 'Billy Duffy/Bob Rock/Ian Astbury', 300591, 9787692, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2617, 'Shape The Sky', 212, 1, 1, 'Billy Duffy/Ian Astbury', 209789, 6885647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2618, 'Speed Of Light', 212, 1, 1, 'Billy Duffy/Bob Rock/Ian Astbury', 262817, 8563352, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2619, 'True Believers', 212, 1, 1, 'Billy Duffy/Ian Astbury', 308009, 9981359, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2620, 'My Bridges Burn', 212, 1, 1, 'Billy Duffy/Ian Astbury', 231862, 7571370, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2621, 'She Sells Sanctuary', 213, 1, 1, 253727, 8368634, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2622, 'Fire Woman', 213, 1, 1, 312790, 10196995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2623, 'Lil\' Evil', 213, 1, 1, 165825, 5419655, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2624, 'Spirit Walker', 213, 1, 1, 230060, 7555897, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2625, 'The Witch', 213, 1, 1, 258768, 8725403, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2626, 'Revolution', 213, 1, 1, 256026, 8371254, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2627, 'Wild Hearted Son', 213, 1, 1, 266893, 8670550, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2628, 'Love Removal Machine', 213, 1, 1, 257619, 8412167, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2629, 'Rain', 213, 1, 1, 236669, 7788461, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2630, 'Edie (Ciao Baby)', 213, 1, 1, 241632, 7846177, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2631, 'Heart Of Soul', 213, 1, 1, 274207, 8967257, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2632, 'Love', 213, 1, 1, 326739, 10729824, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2633, 'Wild Flower', 213, 1, 1, 215536, 7084321, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2634, 'Go West', 213, 1, 1, 238158, 7777749, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2635, 'Resurrection Joe', 213, 1, 1, 255451, 8532840, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2636, 'Sun King', 213, 1, 1, 368431, 12010865, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2637, 'Sweet Soul Sister', 213, 1, 1, 212009, 6889883, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2638, 'Earth Mofo', 213, 1, 1, 282200, 9204581, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2639, 'Break on Through', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 149342, 4943144, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2640, 'Soul Kitchen', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 215066, 7040865, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2641, 'The Crystal Ship', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 154853, 5052658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2642, 'Twentienth Century Fox', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 153913, 5069211, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2643, 'Alabama Song', 214, 1, 1, 'Weill-Brecht', 200097, 6563411, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2644, 'Light My Fire', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 428329, 13963351, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2645, 'Back Door Man', 214, 1, 1, 'Willie Dixon, C. Burnett', 214360, 7035636, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2646, 'I Looked At You', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 142080, 4663988, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2647, 'End Of The Night', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 172695, 5589732, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2648, 'Take It As It Comes', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 137168, 4512656, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2649, 'The End', 214, 1, 1, 'Robby Krieger, Ray Manzarek, John Densmore, Jim Morrison', 701831, 22927336, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2650, 'Roxanne', 215, 1, 1, 'G M Sumner', 192992, 6330159, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2651, 'Can\'t Stand Losing You', 215, 1, 1, 'G M Sumner', 181159, 5971983, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2652, 'Message in a Bottle', 215, 1, 1, 'G M Sumner', 291474, 9647829, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2653, 'Walking on the Moon', 215, 1, 1, 'G M Sumner', 302080, 10019861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2654, 'Don\'t Stand so Close to Me', 215, 1, 1, 'G M Sumner', 241031, 7956658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2655, 'De Do Do Do, De Da Da Da', 215, 1, 1, 'G M Sumner', 247196, 8227075, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2656, 'Every Little Thing She Does is Magic', 215, 1, 1, 'G M Sumner', 261120, 8646853, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2657, 'Invisible Sun', 215, 1, 1, 'G M Sumner', 225593, 7304320, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2658, 'Spirit\'s in the Material World', 215, 1, 1, 'G M Sumner', 181133, 5986622, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2659, 'Every Breath You Take', 215, 1, 1, 'G M Sumner', 254615, 8364520, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2660, 'King Of Pain', 215, 1, 1, 'G M Sumner', 300512, 9880303, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2661, 'Wrapped Around Your Finger', 215, 1, 1, 'G M Sumner', 315454, 10361490, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2662, 'Don\'t Stand So Close to Me \'86', 215, 1, 1, 'G M Sumner', 293590, 9636683, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2663, 'Message in a Bottle (new classic rock mix)', 215, 1, 1, 'G M Sumner', 290951, 9640349, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2664, 'Time Is On My Side', 216, 1, 1, 'Jerry Ragavoy', 179983, 5855836, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2665, 'Heart Of Stone', 216, 1, 1, 'Jagger/Richards', 164493, 5329538, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2666, 'Play With Fire', 216, 1, 1, 'Nanker Phelge', 132022, 4265297, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2667, 'Satisfaction', 216, 1, 1, 'Jagger/Richards', 226612, 7398766, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2668, 'As Tears Go By', 216, 1, 1, 'Jagger/Richards/Oldham', 164284, 5357350, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2669, 'Get Off Of My Cloud', 216, 1, 1, 'Jagger/Richards', 176013, 5719514, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2670, 'Mother\'s Little Helper', 216, 1, 1, 'Jagger/Richards', 167549, 5422434, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2671, '19th Nervous Breakdown', 216, 1, 1, 'Jagger/Richards', 237923, 7742984, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2672, 'Paint It Black', 216, 1, 1, 'Jagger/Richards', 226063, 7442888, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2673, 'Under My Thumb', 216, 1, 1, 'Jagger/Richards', 221387, 7371799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2674, 'Ruby Tuesday', 216, 1, 1, 'Jagger/Richards', 197459, 6433467, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2675, 'Let\'s Spend The Night Together', 216, 1, 1, 'Jagger/Richards', 217495, 7137048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2676, 'Intro', 217, 1, 1, 'Jagger/Richards', 49737, 1618591, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2677, 'You Got Me Rocking', 217, 1, 1, 'Jagger/Richards', 205766, 6734385, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2678, 'Gimmie Shelters', 217, 1, 1, 'Jagger/Richards', 382119, 12528764, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2679, 'Flip The Switch', 217, 1, 1, 'Jagger/Richards', 252421, 8336591, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2680, 'Memory Motel', 217, 1, 1, 'Jagger/Richards', 365844, 11982431, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2681, 'Corinna', 217, 1, 1, 'Jesse Ed Davis III/Taj Mahal', 257488, 8449471, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2682, 'Saint Of Me', 217, 1, 1, 'Jagger/Richards', 325694, 10725160, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2683, 'Wainting On A Friend', 217, 1, 1, 'Jagger/Richards', 302497, 9978046, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2684, 'Sister Morphine', 217, 1, 1, 'Faithfull/Jagger/Richards', 376215, 12345289, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2685, 'Live With Me', 217, 1, 1, 'Jagger/Richards', 234893, 7709006, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2686, 'Respectable', 217, 1, 1, 'Jagger/Richards', 215693, 7099669, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2687, 'Thief In The Night', 217, 1, 1, 'De Beauport/Jagger/Richards', 337266, 10952756, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2688, 'The Last Time', 217, 1, 1, 'Jagger/Richards', 287294, 9498758, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2689, 'Out Of Control', 217, 1, 1, 'Jagger/Richards', 479242, 15749289, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2690, 'Love Is Strong', 218, 1, 1, 'Jagger/Richards', 230896, 7639774, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2691, 'You Got Me Rocking', 218, 1, 1, 'Jagger/Richards', 215928, 7162159, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2692, 'Sparks Will Fly', 218, 1, 1, 'Jagger/Richards', 196466, 6492847, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2693, 'The Worst', 218, 1, 1, 'Jagger/Richards', 144613, 4750094, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2694, 'New Faces', 218, 1, 1, 'Jagger/Richards', 172146, 5689122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2695, 'Moon Is Up', 218, 1, 1, 'Jagger/Richards', 222119, 7366316, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2696, 'Out Of Tears', 218, 1, 1, 'Jagger/Richards', 327418, 10677236, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2697, 'I Go Wild', 218, 1, 1, 'Jagger/Richards', 264019, 8630833, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2698, 'Brand New Car', 218, 1, 1, 'Jagger/Richards', 256052, 8459344, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2699, 'Sweethearts Together', 218, 1, 1, 'Jagger/Richards', 285492, 9550459, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2700, 'Suck On The Jugular', 218, 1, 1, 'Jagger/Richards', 268225, 8920566, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2701, 'Blinded By Rainbows', 218, 1, 1, 'Jagger/Richards', 273946, 8971343, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2702, 'Baby Break It Down', 218, 1, 1, 'Jagger/Richards', 249417, 8197309, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2703, 'Thru And Thru', 218, 1, 1, 'Jagger/Richards', 375092, 12175406, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2704, 'Mean Disposition', 218, 1, 1, 'Jagger/Richards', 249155, 8273602, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2705, 'Walking Wounded', 219, 1, 4, 'The Tea Party', 277968, 9184345, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2706, 'Temptation', 219, 1, 4, 'The Tea Party', 205087, 6711943, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2707, 'The Messenger', 219, 1, 4, 'Daniel Lanois', 212062, 6975437, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2708, 'Psychopomp', 219, 1, 4, 'The Tea Party', 315559, 10295199, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2709, 'Sister Awake', 219, 1, 4, 'The Tea Party', 343875, 11299407, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2710, 'The Bazaar', 219, 1, 4, 'The Tea Party', 222458, 7245691, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2711, 'Save Me (Remix)', 219, 1, 4, 'The Tea Party', 396303, 13053839, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2712, 'Fire In The Head', 219, 1, 4, 'The Tea Party', 306337, 10005675, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2713, 'Release', 219, 1, 4, 'The Tea Party', 244114, 8014606, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2714, 'Heaven Coming Down', 219, 1, 4, 'The Tea Party', 241867, 7846459, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2715, 'The River (Remix)', 219, 1, 4, 'The Tea Party', 343170, 11193268, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2716, 'Babylon', 219, 1, 4, 'The Tea Party', 169795, 5568808, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2717, 'Waiting On A Sign', 219, 1, 4, 'The Tea Party', 261903, 8558590, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2718, 'Life Line', 219, 1, 4, 'The Tea Party', 277786, 9082773, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2719, 'Paint It Black', 219, 1, 4, 'Keith Richards/Mick Jagger', 214752, 7101572, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2720, 'Temptation', 220, 1, 4, 'The Tea Party', 205244, 6719465, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2721, 'Army Ants', 220, 1, 4, 'The Tea Party', 215405, 7075838, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2722, 'Psychopomp', 220, 1, 4, 'The Tea Party', 317231, 10351778, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2723, 'Gyroscope', 220, 1, 4, 'The Tea Party', 177711, 5810323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2724, 'Alarum', 220, 1, 4, 'The Tea Party', 298187, 9712545, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2725, 'Release', 220, 1, 4, 'The Tea Party', 266292, 8725824, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2726, 'Transmission', 220, 1, 4, 'The Tea Party', 317257, 10351152, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2727, 'Babylon', 220, 1, 4, 'The Tea Party', 292466, 9601786, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2728, 'Pulse', 220, 1, 4, 'The Tea Party', 250253, 8183872, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2729, 'Emerald', 220, 1, 4, 'The Tea Party', 289750, 9543789, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2730, 'Aftermath', 220, 1, 4, 'The Tea Party', 343745, 11085607, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2731, 'I Can\'t Explain', 221, 1, 1, 'Pete Townshend', 125152, 4082896, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2732, 'Anyway, Anyhow, Anywhere', 221, 1, 1, 'Pete Townshend, Roger Daltrey', 161253, 5234173, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2733, 'My Generation', 221, 1, 1, 'John Entwistle/Pete Townshend', 197825, 6446634, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2734, 'Substitute', 221, 1, 1, 'Pete Townshend', 228022, 7409995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2735, 'I\'m A Boy', 221, 1, 1, 'Pete Townshend', 157126, 5120605, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2736, 'Boris The Spider', 221, 1, 1, 'John Entwistle', 149472, 4835202, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2737, 'Happy Jack', 221, 1, 1, 'Pete Townshend', 132310, 4353063, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2738, 'Pictures Of Lily', 221, 1, 1, 'Pete Townshend', 164414, 5329751, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2739, 'I Can See For Miles', 221, 1, 1, 'Pete Townshend', 262791, 8604989, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2740, 'Magic Bus', 221, 1, 1, 'Pete Townshend', 197224, 6452700, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2741, 'Pinball Wizard', 221, 1, 1, 'John Entwistle/Pete Townshend', 181890, 6055580, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2742, 'The Seeker', 221, 1, 1, 'Pete Townshend', 204643, 6736866, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2743, 'Baba O\'Riley', 221, 1, 1, 'John Entwistle/Pete Townshend', 309472, 10141660, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2744, 'Won\'t Get Fooled Again (Full Length Version)', 221, 1, 1, 'John Entwistle/Pete Townshend', 513750, 16855521, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2745, 'Let\'s See Action', 221, 1, 1, 'Pete Townshend', 243513, 8078418, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2746, '5.15', 221, 1, 1, 'Pete Townshend', 289619, 9458549, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2747, 'Join Together', 221, 1, 1, 'Pete Townshend', 262556, 8602485, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2748, 'Squeeze Box', 221, 1, 1, 'Pete Townshend', 161280, 5256508, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2749, 'Who Are You (Single Edit Version)', 221, 1, 1, 'John Entwistle/Pete Townshend', 299232, 9900469, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2750, 'You Better You Bet', 221, 1, 1, 'Pete Townshend', 338520, 11160877, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2751, 'Primavera', 222, 1, 7, 'Genival Cassiano/Silvio Rochael', 126615, 4152604, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2752, 'Chocolate', 222, 1, 7, 'Tim Maia', 194690, 6411587, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2753, 'Azul Da Cor Do Mar', 222, 1, 7, 'Tim Maia', 197955, 6475007, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2754, 'O Descobridor Dos Sete Mares', 222, 1, 7, 'Gilson Mendonça/Michel', 262974, 8749583, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2755, 'Até Que Enfim Encontrei Você', 222, 1, 7, 'Tim Maia', 105064, 3477751, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2756, 'Coroné Antonio Bento', 222, 1, 7, 'Do Vale, João/Luiz Wanderley', 131317, 4340326, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2757, 'New Love', 222, 1, 7, 'Tim Maia', 237897, 7786824, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2758, 'Não Vou Ficar', 222, 1, 7, 'Tim Maia', 172068, 5642919, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2759, 'Música No Ar', 222, 1, 7, 'Tim Maia', 158511, 5184891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2760, 'Salve Nossa Senhora', 222, 1, 7, 'Carlos Imperial/Edardo Araújo', 115461, 3827629, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2761, 'Você Fugiu', 222, 1, 7, 'Genival Cassiano', 238367, 7971147, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2762, 'Cristina Nº 2', 222, 1, 7, 'Carlos Imperial/Tim Maia', 90148, 2978589, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2763, 'Compadre', 222, 1, 7, 'Tim Maia', 171389, 5631446, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2764, 'Over Again', 222, 1, 7, 'Tim Maia', 200489, 6612634, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2765, 'Réu Confesso', 222, 1, 7, 'Tim Maia', 217391, 7189874, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2766, 'O Que Me Importa', 223, 1, 7, 153155, 4977852, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2767, 'Gostava Tanto De Você', 223, 1, 7, 253805, 8380077, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2768, 'Você', 223, 1, 7, 242599, 7911702, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2769, 'Não Quero Dinheiro', 223, 1, 7, 152607, 5031797, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2770, 'Eu Amo Você', 223, 1, 7, 242782, 7914628, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2771, 'A Festa Do Santo Reis', 223, 1, 7, 159791, 5204995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2772, 'I Don\'t Know What To Do With Myself', 223, 1, 7, 221387, 7251478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2773, 'Padre Cícero', 223, 1, 7, 139598, 4581685, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2774, 'Nosso Adeus', 223, 1, 7, 206471, 6793270, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2775, 'Canário Do Reino', 223, 1, 7, 139337, 4552858, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2776, 'Preciso Ser Amado', 223, 1, 7, 174001, 5618895, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2777, 'Balanço', 223, 1, 7, 209737, 6890327, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2778, 'Preciso Aprender A Ser Só', 223, 1, 7, 162220, 5213894, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2779, 'Esta É A Canção', 223, 1, 7, 184450, 6069933, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2780, 'Formigueiro', 223, 1, 7, 252943, 8455132, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2781, 'Comida', 224, 1, 4, 'Titãs', 322612, 10786578, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2782, 'Go Back', 224, 1, 4, 'Titãs', 230504, 7668899, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2783, 'Prá Dizer Adeus', 224, 1, 4, 'Titãs', 222484, 7382048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2784, 'Família', 224, 1, 4, 'Titãs', 218331, 7267458, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2785, 'Os Cegos Do Castelo', 224, 1, 4, 'Titãs', 296829, 9868187, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2786, 'O Pulso', 224, 1, 4, 'Titãs', 199131, 6566998, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2787, 'Marvin', 224, 1, 4, 'Titãs', 264359, 8741444, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2788, 'Nem 5 Minutos Guardados', 224, 1, 4, 'Titãs', 245995, 8143797, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2789, 'Flores', 224, 1, 4, 'Titãs', 215510, 7148017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2790, 'Palavras', 224, 1, 4, 'Titãs', 158458, 5285715, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2791, 'Hereditário', 224, 1, 4, 'Titãs', 151693, 5020547, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2792, 'A Melhor Forma', 224, 1, 4, 'Titãs', 191503, 6349938, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2793, 'Cabeça Dinossauro', 224, 1, 4, 'Titãs', 37120, 1220930, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2794, '32 Dentes', 224, 1, 4, 'Titãs', 184946, 6157904, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2795, 'Bichos Escrotos (Vinheta)', 224, 1, 4, 'Titãs', 104986, 3503755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2796, 'Não Vou Lutar', 224, 1, 4, 'Titãs', 189988, 6308613, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2797, 'Homem Primata (Vinheta)', 224, 1, 4, 'Titãs', 34168, 1124909, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2798, 'Homem Primata', 224, 1, 4, 'Titãs', 195500, 6486470, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2799, 'Polícia (Vinheta)', 224, 1, 4, 'Titãs', 56111, 1824213, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2800, 'Querem Meu Sangue', 224, 1, 4, 'Titãs', 212401, 7069773, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2801, 'Diversão', 224, 1, 4, 'Titãs', 285936, 9531268, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2802, 'Televisão', 224, 1, 4, 'Titãs', 293668, 9776548, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2803, 'Sonifera Ilha', 225, 1, 4, 'Branco Mello/Carlos Barmack/Ciro Pessoa/Marcelo Fromer/Toni Belloto', 170684, 5678290, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2804, 'Lugar Nenhum', 225, 1, 4, 'Arnaldo Antunes/Charles Gavin/Marcelo Fromer/Sérgio Britto/Toni Bellotto', 195840, 6472780, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2805, 'Sua Impossivel Chance', 225, 1, 4, 'Nando Reis', 246622, 8073248, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2806, 'Desordem', 225, 1, 4, 'Charles Gavin/Marcelo Fromer/Sérgio Britto', 213289, 7067340, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2807, 'Não Vou Me Adaptar', 225, 1, 4, 'Arnaldo Antunes', 221831, 7304656, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2808, 'Domingo', 225, 1, 4, 'Sérgio Britto/Toni Bellotto', 208613, 6883180, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2809, 'Amanhã Não Se Sabe', 225, 1, 4, 'Sérgio Britto', 189440, 6243967, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2810, 'Caras Como Eu', 225, 1, 4, 'Toni Bellotto', 183092, 5999048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2811, 'Senhora E Senhor', 225, 1, 4, 'Arnaldo Anutnes/Marcelo Fromer/Paulo Miklos', 203702, 6733733, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2812, 'Era Uma Vez', 225, 1, 4, 'Arnaldo Anutnes/Branco Mello/Marcelo Fromer/Sergio Brotto/Toni Bellotto', 224261, 7453156, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2813, 'Miséria', 225, 1, 4, 'Arnaldo Antunes/Britto, SergioMiklos, Paulo', 262191, 8727645, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2814, 'Insensível', 225, 1, 4, 'Sérgio Britto', 207830, 6893664, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2815, 'Eu E Ela', 225, 1, 4, 'Nando Reis', 276035, 9138846, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2816, 'Toda Cor', 225, 1, 4, 'Ciro Pressoa/Marcelo Fromer', 209084, 6939176, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2817, 'É Preciso Saber Viver', 225, 1, 4, 'Erasmo Carlos/Roberto Carlos', 251115, 8271418, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2818, 'Senhor Delegado/Eu Não Aguento', 225, 1, 4, 'Antonio Lopes', 156656, 5277983, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2819, 'Battlestar Galactica: The Story So Far', 226, 3, 18, 2622250, 490750393, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2820, 'Occupation / Precipice', 227, 3, 19, 5286953, 1054423946, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2821, 'Exodus, Pt. 1', 227, 3, 19, 2621708, 475079441, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2822, 'Exodus, Pt. 2', 227, 3, 19, 2618000, 466820021, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2823, 'Collaborators', 227, 3, 19, 2626626, 483484911, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2824, 'Torn', 227, 3, 19, 2631291, 495262585, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2825, 'A Measure of Salvation', 227, 3, 18, 2563938, 489715554, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2826, 'Hero', 227, 3, 18, 2713755, 506896959, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2827, 'Unfinished Business', 227, 3, 18, 2622038, 528499160, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2828, 'The Passage', 227, 3, 18, 2623875, 490375760, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2829, 'The Eye of Jupiter', 227, 3, 18, 2618750, 517909587, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2830, 'Rapture', 227, 3, 18, 2624541, 508406153, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2831, 'Taking a Break from All Your Worries', 227, 3, 18, 2624207, 492700163, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2832, 'The Woman King', 227, 3, 18, 2626376, 552893447, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2833, 'A Day In the Life', 227, 3, 18, 2620245, 462818231, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2834, 'Dirty Hands', 227, 3, 18, 2627961, 537648614, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2835, 'Maelstrom', 227, 3, 18, 2622372, 514154275, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2836, 'The Son Also Rises', 227, 3, 18, 2621830, 499258498, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2837, 'Crossroads, Pt. 1', 227, 3, 20, 2622622, 486233524, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2838, 'Crossroads, Pt. 2', 227, 3, 20, 2869953, 497335706, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2839, 'Genesis', 228, 3, 19, 2611986, 515671080, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2840, 'Don\'t Look Back', 228, 3, 21, 2571154, 493628775, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2841, 'One Giant Leap', 228, 3, 21, 2607649, 521616246, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2842, 'Collision', 228, 3, 21, 2605480, 526182322, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2843, 'Hiros', 228, 3, 21, 2533575, 488835454, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2844, 'Better Halves', 228, 3, 21, 2573031, 549353481, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2845, 'Nothing to Hide', 228, 3, 19, 2605647, 510058181, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2846, 'Seven Minutes to Midnight', 228, 3, 21, 2613988, 515590682, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2847, 'Homecoming', 228, 3, 21, 2601351, 516015339, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2848, 'Six Months Ago', 228, 3, 19, 2602852, 505133869, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2849, 'Fallout', 228, 3, 21, 2594761, 501145440, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2850, 'The Fix', 228, 3, 21, 2600266, 507026323, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2851, 'Distractions', 228, 3, 21, 2590382, 537111289, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2852, 'Run!', 228, 3, 21, 2602602, 542936677, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2853, 'Unexpected', 228, 3, 21, 2598139, 511777758, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2854, 'Company Man', 228, 3, 21, 2601226, 493168135, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2855, 'Company Man', 228, 3, 21, 2601101, 503786316, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2856, 'Parasite', 228, 3, 21, 2602727, 487461520, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2857, 'A Tale of Two Cities', 229, 3, 19, 2636970, 513691652, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2858, 'Lost (Pilot, Part 1) [Premiere]', 230, 3, 19, 2548875, 217124866, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2859, 'Man of Science, Man of Faith (Premiere)', 231, 3, 19, 2612250, 543342028, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2860, 'Adrift', 231, 3, 19, 2564958, 502663995, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2861, 'Lost (Pilot, Part 2)', 230, 3, 19, 2436583, 204995876, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2862, 'The Glass Ballerina', 229, 3, 21, 2637458, 535729216, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2863, 'Further Instructions', 229, 3, 19, 2563980, 502041019, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2864, 'Orientation', 231, 3, 19, 2609083, 500600434, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2865, 'Tabula Rasa', 230, 3, 19, 2627105, 210526410, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2866, 'Every Man for Himself', 229, 3, 21, 2637387, 513803546, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2867, 'Everybody Hates Hugo', 231, 3, 19, 2609192, 498163145, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2868, 'Walkabout', 230, 3, 19, 2587370, 207748198, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2869, '...And Found', 231, 3, 19, 2563833, 500330548, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2870, 'The Cost of Living', 229, 3, 19, 2637500, 505647192, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2871, 'White Rabbit', 230, 3, 19, 2571965, 201654606, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2872, 'Abandoned', 231, 3, 19, 2587041, 537348711, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2873, 'House of the Rising Sun', 230, 3, 19, 2590032, 210379525, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2874, 'I Do', 229, 3, 19, 2627791, 504676825, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2875, 'Not In Portland', 229, 3, 21, 2637303, 499061234, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2876, 'Not In Portland', 229, 3, 21, 2637345, 510546847, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2877, 'The Moth', 230, 3, 19, 2631327, 228896396, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2878, 'The Other 48 Days', 231, 3, 19, 2610625, 535256753, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2879, 'Collision', 231, 3, 19, 2564916, 475656544, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2880, 'Confidence Man', 230, 3, 19, 2615244, 223756475, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2881, 'Flashes Before Your Eyes', 229, 3, 21, 2636636, 537760755, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2882, 'Lost Survival Guide', 229, 3, 21, 2632590, 486675063, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2883, 'Solitary', 230, 3, 19, 2612894, 207045178, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2884, 'What Kate Did', 231, 3, 19, 2610250, 484583988, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2885, 'Raised By Another', 230, 3, 19, 2590459, 223623810, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2886, 'Stranger In a Strange Land', 229, 3, 21, 2636428, 505056021, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2887, 'The 23rd Psalm', 231, 3, 19, 2610416, 487401604, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2888, 'All the Best Cowboys Have Daddy Issues', 230, 3, 19, 2555492, 211743651, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2889, 'The Hunting Party', 231, 3, 21, 2611333, 520350364, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2890, 'Tricia Tanaka Is Dead', 229, 3, 21, 2635010, 548197162, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2891, 'Enter 77', 229, 3, 21, 2629796, 517521422, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2892, 'Fire + Water', 231, 3, 21, 2600333, 488458695, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2893, 'Whatever the Case May Be', 230, 3, 19, 2616410, 183867185, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2894, 'Hearts and Minds', 230, 3, 19, 2619462, 207607466, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2895, 'Par Avion', 229, 3, 21, 2629879, 517079642, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2896, 'The Long Con', 231, 3, 19, 2679583, 518376636, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2897, 'One of Them', 231, 3, 21, 2698791, 542332389, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2898, 'Special', 230, 3, 19, 2618530, 219961967, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2899, 'The Man from Tallahassee', 229, 3, 21, 2637637, 550893556, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2900, 'Exposé', 229, 3, 21, 2593760, 511338017, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2901, 'Homecoming', 230, 3, 19, 2515882, 210675221, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2902, 'Maternity Leave', 231, 3, 21, 2780416, 555244214, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2903, 'Left Behind', 229, 3, 21, 2635343, 538491964, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2904, 'Outlaws', 230, 3, 19, 2619887, 206500939, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2905, 'The Whole Truth', 231, 3, 21, 2610125, 495487014, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2906, '...In Translation', 230, 3, 19, 2604575, 215441983, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2907, 'Lockdown', 231, 3, 21, 2610250, 543886056, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2908, 'One of Us', 229, 3, 21, 2638096, 502387276, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2909, 'Catch-22', 229, 3, 21, 2561394, 489773399, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2910, 'Dave', 231, 3, 19, 2825166, 574325829, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2911, 'Numbers', 230, 3, 19, 2609772, 214709143, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2912, 'D.O.C.', 229, 3, 21, 2616032, 518556641, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2913, 'Deus Ex Machina', 230, 3, 19, 2582009, 214996732, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2914, 'S.O.S.', 231, 3, 19, 2639541, 517979269, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2915, 'Do No Harm', 230, 3, 19, 2618487, 212039309, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2916, 'Two for the Road', 231, 3, 21, 2610958, 502404558, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2917, 'The Greater Good', 230, 3, 19, 2617784, 214130273, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2918, '"?"', 231, 3, 19, 2782333, 528227089, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2919, 'Born to Run', 230, 3, 19, 2618619, 213772057, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2920, 'Three Minutes', 231, 3, 19, 2763666, 531556853, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2921, 'Exodus (Part 1)', 230, 3, 19, 2620747, 213107744, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2922, 'Live Together, Die Alone, Pt. 1', 231, 3, 21, 2478041, 457364940, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2923, 'Exodus (Part 2) [Season Finale]', 230, 3, 19, 2605557, 208667059, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2924, 'Live Together, Die Alone, Pt. 2', 231, 3, 19, 2656531, 503619265, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (2925, 'Exodus (Part 3) [Season Finale]', 230, 3, 19, 2619869, 197937785, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2926, 'Zoo Station', 232, 1, 1, 'U2', 276349, 9056902, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2927, 'Even Better Than The Real Thing', 232, 1, 1, 'U2', 221361, 7279392, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2928, 'One', 232, 1, 1, 'U2', 276192, 9158892, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2929, 'Until The End Of The World', 232, 1, 1, 'U2', 278700, 9132485, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2930, 'Who\'s Gonna Ride Your Wild Horses', 232, 1, 1, 'U2', 316551, 10304369, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2931, 'So Cruel', 232, 1, 1, 'U2', 349492, 11527614, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2932, 'The Fly', 232, 1, 1, 'U2', 268982, 8825399, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2933, 'Mysterious Ways', 232, 1, 1, 'U2', 243826, 8062057, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2934, 'Tryin\' To Throw Your Arms Around The World', 232, 1, 1, 'U2', 232463, 7612124, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2935, 'Ultraviolet (Light My Way)', 232, 1, 1, 'U2', 330788, 10754631, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2936, 'Acrobat', 232, 1, 1, 'U2', 270288, 8824723, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2937, 'Love Is Blindness', 232, 1, 1, 'U2', 263497, 8531766, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2938, 'Beautiful Day', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 248163, 8056723, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2939, 'Stuck In A Moment You Can\'t Get Out Of', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 272378, 8997366, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2940, 'Elevation', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 227552, 7479414, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2941, 'Walk On', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 296280, 9800861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2942, 'Kite', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 266893, 8765761, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2943, 'In A Little While', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 219271, 7189647, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2944, 'Wild Honey', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 226768, 7466069, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2945, 'Peace On Earth', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 288496, 9476171, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2946, 'When I Look At The World', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 257776, 8500491, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2947, 'New York', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 330370, 10862323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2948, 'Grace', 233, 1, 1, 'Adam Clayton, Bono, Larry Mullen, The Edge', 330657, 10877148, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2949, 'The Three Sunrises', 234, 1, 1, 'U2', 234788, 7717990, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2950, 'Spanish Eyes', 234, 1, 1, 'U2', 196702, 6392710, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2951, 'Sweetest Thing', 234, 1, 1, 'U2', 185103, 6154896, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2952, 'Love Comes Tumbling', 234, 1, 1, 'U2', 282671, 9328802, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2953, 'Bass Trap', 234, 1, 1, 'U2', 213289, 6834107, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2954, 'Dancing Barefoot', 234, 1, 1, 'Ivan Kral/Patti Smith', 287895, 9488294, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2955, 'Everlasting Love', 234, 1, 1, 'Buzz Cason/Mac Gayden', 202631, 6708932, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2956, 'Unchained Melody', 234, 1, 1, 'Alex North/Hy Zaret', 294164, 9597568, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2957, 'Walk To The Water', 234, 1, 1, 'U2', 289253, 9523336, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2958, 'Luminous Times (Hold On To Love)', 234, 1, 1, 'Brian Eno/U2', 277760, 9015513, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2959, 'Hallelujah Here She Comes', 234, 1, 1, 'U2', 242364, 8027028, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2960, 'Silver And Gold', 234, 1, 1, 'Bono', 279875, 9199746, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2961, 'Endless Deep', 234, 1, 1, 'U2', 179879, 5899070, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2962, 'A Room At The Heartbreak Hotel', 234, 1, 1, 'U2', 274546, 9015416, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2963, 'Trash, Trampoline And The Party Girl', 234, 1, 1, 'U2', 153965, 5083523, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2964, 'Vertigo', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 194612, 6329502, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2965, 'Miracle Drug', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 239124, 7760916, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2966, 'Sometimes You Can\'t Make It On Your Own', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 308976, 10112863, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2967, 'Love And Peace Or Else', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 290690, 9476723, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2968, 'City Of Blinding Lights', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 347951, 11432026, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2969, 'All Because Of You', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 219141, 7198014, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2970, 'A Man And A Woman', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 270132, 8938285, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2971, 'Crumbs From Your Table', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 303568, 9892349, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2972, 'One Step Closer', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 231680, 7512912, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2973, 'Original Of The Species', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 281443, 9230041, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2974, 'Yahweh', 235, 1, 1, 'Adam Clayton, Bono, Larry Mullen & The Edge', 262034, 8636998, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2975, 'Discotheque', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 319582, 10442206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2976, 'Do You Feel Loved', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 307539, 10122694, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2977, 'Mofo', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 349178, 11583042, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2978, 'If God Will Send His Angels', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 322533, 10563329, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2979, 'Staring At The Sun', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 276924, 9082838, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2980, 'Last Night On Earth', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 285753, 9401017, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2981, 'Gone', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 266866, 8746301, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2982, 'Miami', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 293041, 9741603, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2983, 'The Playboy Mansion', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 280555, 9274144, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2984, 'If You Wear That Velvet Dress', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 315167, 10227333, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2985, 'Please', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 302602, 9909484, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2986, 'Wake Up Dead Man', 236, 1, 1, 'Bono, The Edge, Adam Clayton, and Larry Mullen', 292832, 9515903, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2987, 'Helter Skelter', 237, 1, 1, 'Lennon, John/McCartney, Paul', 187350, 6097636, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2988, 'Van Diemen\'s Land', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 186044, 5990280, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2989, 'Desire', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 179226, 5874535, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2990, 'Hawkmoon 269', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 382458, 12494987, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2991, 'All Along The Watchtower', 237, 1, 1, 'Dylan, Bob', 264568, 8623572, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2992, 'I Still Haven\'t Found What I\'m Looking for', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 353567, 11542247, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2993, 'Freedom For My People', 237, 1, 1, 'Mabins, Macie/Magee, Sterling/Robinson, Bobby', 38164, 1249764, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2994, 'Silver And Gold', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 349831, 11450194, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2995, 'Pride (In The Name Of Love)', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 267807, 8806361, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2996, 'Angel Of Harlem', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 229276, 7498022, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2997, 'Love Rescue Me', 237, 1, 1, 'Bono/Clayton, Adam/Dylan, Bob/Mullen Jr., Larry/The Edge', 384522, 12508716, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2998, 'When Love Comes To Town', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 255869, 8340954, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (2999, 'Heartland', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 303360, 9867748, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3000, 'God Part II', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 195604, 6497570, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3001, 'The Star Spangled Banner', 237, 1, 1, 'Hendrix, Jimi', 43232, 1385810, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3002, 'Bullet The Blue Sky', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 337005, 10993607, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3003, 'All I Want Is You', 237, 1, 1, 'Bono/Clayton, Adam/Mullen Jr., Larry/The Edge', 390243, 12729820, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3004, 'Pride (In The Name Of Love)', 238, 1, 1, 'U2', 230243, 7549085, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3005, 'New Year\'s Day', 238, 1, 1, 'U2', 258925, 8491818, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3006, 'With Or Without You', 238, 1, 1, 'U2', 299023, 9765188, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3007, 'I Still Haven\'t Found What I\'m Looking For', 238, 1, 1, 'U2', 280764, 9306737, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3008, 'Sunday Bloody Sunday', 238, 1, 1, 'U2', 282174, 9269668, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3009, 'Bad', 238, 1, 1, 'U2', 351817, 11628058, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3010, 'Where The Streets Have No Name', 238, 1, 1, 'U2', 276218, 9042305, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3011, 'I Will Follow', 238, 1, 1, 'U2', 218253, 7184825, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3012, 'The Unforgettable Fire', 238, 1, 1, 'U2', 295183, 9684664, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3013, 'Sweetest Thing', 238, 1, 1, 'U2 & Daragh O\'Toole', 183066, 6071385, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3014, 'Desire', 238, 1, 1, 'U2', 179853, 5893206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3015, 'When Love Comes To Town', 238, 1, 1, 'U2', 258194, 8479525, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3016, 'Angel Of Harlem', 238, 1, 1, 'U2', 230217, 7527339, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3017, 'All I Want Is You', 238, 1, 1, 'U2 & Van Dyke Parks', 591986, 19202252, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3018, 'Sunday Bloody Sunday', 239, 1, 1, 'U2', 278204, 9140849, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3019, 'Seconds', 239, 1, 1, 'U2', 191582, 6352121, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3020, 'New Year\'s Day', 239, 1, 1, 'U2', 336274, 11054732, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3021, 'Like A Song...', 239, 1, 1, 'U2', 287294, 9365379, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3022, 'Drowning Man', 239, 1, 1, 'U2', 254458, 8457066, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3023, 'The Refugee', 239, 1, 1, 'U2', 221283, 7374043, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3024, 'Two Hearts Beat As One', 239, 1, 1, 'U2', 243487, 7998323, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3025, 'Red Light', 239, 1, 1, 'U2', 225854, 7453704, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3026, 'Surrender', 239, 1, 1, 'U2', 333505, 11221406, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3027, '"40"', 239, 1, 1, 'U2', 157962, 5251767, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3028, 'Zooropa', 240, 1, 1, 'U2; Bono', 392359, 12807979, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3029, 'Babyface', 240, 1, 1, 'U2; Bono', 241998, 7942573, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3030, 'Numb', 240, 1, 1, 'U2; Edge, The', 260284, 8577861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3031, 'Lemon', 240, 1, 1, 'U2; Bono', 418324, 13988878, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3032, 'Stay (Faraway, So Close!)', 240, 1, 1, 'U2; Bono', 298475, 9785480, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3033, 'Daddy\'s Gonna Pay For Your Crashed Car', 240, 1, 1, 'U2; Bono', 320287, 10609581, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3034, 'Some Days Are Better Than Others', 240, 1, 1, 'U2; Bono', 257436, 8417690, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3035, 'The First Time', 240, 1, 1, 'U2; Bono', 225697, 7247651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3036, 'Dirty Day', 240, 1, 1, 'U2; Bono & Edge, The', 324440, 10652877, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3037, 'The Wanderer', 240, 1, 1, 'U2; Bono', 283951, 9258717, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3038, 'Breakfast In Bed', 241, 1, 8, 196179, 6513325, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3039, 'Where Did I Go Wrong', 241, 1, 8, 226742, 7485054, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3040, 'I Would Do For You', 241, 1, 8, 334524, 11193602, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3041, 'Homely Girl', 241, 1, 8, 203833, 6790788, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3042, 'Here I Am (Come And Take Me)', 241, 1, 8, 242102, 8106249, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3043, 'Kingston Town', 241, 1, 8, 226951, 7638236, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3044, 'Wear You To The Ball', 241, 1, 8, 213342, 7159527, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3045, '(I Can\'t Help) Falling In Love With You', 241, 1, 8, 207568, 6905623, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3046, 'Higher Ground', 241, 1, 8, 260179, 8665244, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3047, 'Bring Me Your Cup', 241, 1, 8, 341498, 11346114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3048, 'C\'est La Vie', 241, 1, 8, 270053, 9031661, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3049, 'Reggae Music', 241, 1, 8, 245106, 8203931, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3050, 'Superstition', 241, 1, 8, 319582, 10728099, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3051, 'Until My Dying Day', 241, 1, 8, 235807, 7886195, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3052, 'Where Have All The Good Times Gone?', 242, 1, 1, 'Ray Davies', 186723, 6063937, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3053, 'Hang \'Em High', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 210259, 6872314, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3054, 'Cathedral', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 82860, 2650998, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3055, 'Secrets', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 206968, 6803255, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3056, 'Intruder', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 100153, 3282142, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3057, '(Oh) Pretty Woman', 242, 1, 1, 'Bill Dees/Roy Orbison', 174680, 5665828, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3058, 'Dancing In The Street', 242, 1, 1, 'Ivy Jo Hunter/Marvin Gaye/William Stevenson', 225985, 7461499, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3059, 'Little Guitars (Intro)', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 42240, 1439530, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3060, 'Little Guitars', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 228806, 7453043, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3061, 'Big Bad Bill (Is Sweet William Now)', 242, 1, 1, 'Jack Yellen/Milton Ager', 165146, 5489609, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3062, 'The Full Bug', 242, 1, 1, 'Alex Van Halen/David Lee Roth/Edward Van Halen/Michael Anthony', 201116, 6551013, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3063, 'Happy Trails', 242, 1, 1, 'Dale Evans', 65488, 2111141, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3064, 'Eruption', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth, Michael Anthony', 102164, 3272891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3065, 'Ain\'t Talkin\' \'bout Love', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth, Michael Anthony', 228336, 7569506, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3066, 'Runnin\' With The Devil', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth, Michael Anthony', 215902, 7061901, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3067, 'Dance the Night Away', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth, Michael Anthony', 185965, 6087433, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3068, 'And the Cradle Will Rock...', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth, Michael Anthony', 213968, 7011402, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3069, 'Unchained', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth, Michael Anthony', 208953, 6777078, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3070, 'Jump', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth', 241711, 7911090, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3071, 'Panama', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, David Lee Roth', 211853, 6921784, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3072, 'Why Can\'t This Be Love', 243, 1, 1, 'Van Halen', 227761, 7457655, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3073, 'Dreams', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony,/Edward Van Halen, Alex Van Halen, Michael Anthony, Sammy Hagar', 291813, 9504119, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3074, 'When It\'s Love', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony,/Edward Van Halen, Alex Van Halen, Michael Anthony, Sammy Hagar', 338991, 11049966, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3075, 'Poundcake', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony,/Edward Van Halen, Alex Van Halen, Michael Anthony, Sammy Hagar', 321854, 10366978, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3076, 'Right Now', 243, 1, 1, 'Van Halen', 321828, 10503352, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3077, 'Can\'t Stop Loving You', 243, 1, 1, 'Van Halen', 248502, 8107896, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3078, 'Humans Being', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony,/Edward Van Halen, Alex Van Halen, Michael Anthony, Sammy Hagar', 308950, 10014683, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3079, 'Can\'t Get This Stuff No More', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony,/Edward Van Halen, Alex Van Halen, Michael Anthony, David Lee Roth', 315376, 10355753, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3080, 'Me Wise Magic', 243, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony,/Edward Van Halen, Alex Van Halen, Michael Anthony, David Lee Roth', 366053, 12013467, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3081, 'Runnin\' With The Devil', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 216032, 7056863, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3082, 'Eruption', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 102556, 3286026, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3083, 'You Really Got Me', 244, 1, 1, 'Ray Davies', 158589, 5194092, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3084, 'Ain\'t Talkin\' \'Bout Love', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 230060, 7617284, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3085, 'I\'m The One', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 226507, 7373922, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3086, 'Jamie\'s Cryin\'', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 210546, 6946086, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3087, 'Atomic Punk', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 182073, 5908861, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3088, 'Feel Your Love Tonight', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 222850, 7293608, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3089, 'Little Dreamer', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 203258, 6648122, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3090, 'Ice Cream Man', 244, 1, 1, 'John Brim', 200306, 6573145, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3091, 'On Fire', 244, 1, 1, 'Edward Van Halen, Alex Van Halen, Michael Anthony and David Lee Roth', 180636, 5879235, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3092, 'Neworld', 245, 1, 1, 'Van Halen', 105639, 3495897, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3093, 'Without You', 245, 1, 1, 'Van Halen', 390295, 12619558, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3094, 'One I Want', 245, 1, 1, 'Van Halen', 330788, 10743970, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3095, 'From Afar', 245, 1, 1, 'Van Halen', 324414, 10524554, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3096, 'Dirty Water Dog', 245, 1, 1, 'Van Halen', 327392, 10709202, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3097, 'Once', 245, 1, 1, 'Van Halen', 462837, 15378082, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3098, 'Fire in the Hole', 245, 1, 1, 'Van Halen', 331728, 10846768, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3099, 'Josephina', 245, 1, 1, 'Van Halen', 342491, 11161521, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3100, 'Year to the Day', 245, 1, 1, 'Van Halen', 514612, 16621333, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3101, 'Primary', 245, 1, 1, 'Van Halen', 86987, 2812555, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3102, 'Ballot or the Bullet', 245, 1, 1, 'Van Halen', 342282, 11212955, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3103, 'How Many Say I', 245, 1, 1, 'Van Halen', 363937, 11716855, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3104, 'Sucker Train Blues', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 267859, 8738780, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3105, 'Do It For The Kids', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 235911, 7693331, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3106, 'Big Machine', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 265613, 8673442, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3107, 'Illegal I Song', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 257750, 8483347, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3108, 'Spectacle', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 221701, 7252876, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3109, 'Fall To Pieces', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 270889, 8823096, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3110, 'Headspace', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 223033, 7237986, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3111, 'Superhuman', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 255921, 8365328, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3112, 'Set Me Free', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 247954, 8053388, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3113, 'You Got No Right', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 335412, 10991094, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3114, 'Slither', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 248398, 8118785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3115, 'Dirty Little Thing', 246, 1, 1, 'Dave Kushner, Duff, Keith Nelson, Matt Sorum, Scott Weiland & Slash', 237844, 7732982, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3116, 'Loving The Alien', 246, 1, 1, 'Dave Kushner, Duff, Matt Sorum, Scott Weiland & Slash', 348786, 11412762, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3117, 'Pela Luz Dos Olhos Teus', 247, 1, 7, 119196, 3905715, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3118, 'A Bencao E Outros', 247, 1, 7, 421093, 14234427, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3119, 'Tudo Na Mais Santa Paz', 247, 1, 7, 222406, 7426757, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3120, 'O Velho E Aflor', 247, 1, 7, 275121, 9126828, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3121, 'Cotidiano N 2', 247, 1, 7, 55902, 1805797, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3122, 'Adeus', 247, 1, 7, 221884, 7259351, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3123, 'Samba Pra Endrigo', 247, 1, 7, 259265, 8823551, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3124, 'So Por Amor', 247, 1, 7, 236591, 7745764, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3125, 'Meu Pranto Rolou', 247, 1, 7, 181760, 6003345, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3126, 'Mulher Carioca', 247, 1, 7, 191686, 6395048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3127, 'Um Homem Chamado Alfredo', 247, 1, 7, 151640, 4976227, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3128, 'Samba Do Jato', 247, 1, 7, 220813, 7357840, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3129, 'Oi, La', 247, 1, 7, 167053, 5562700, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3130, 'Vinicius, Poeta Do Encontro', 247, 1, 7, 336431, 10858776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3131, 'Soneto Da Separacao', 247, 1, 7, 193880, 6277511, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3132, 'Still Of The Night', 141, 1, 3, 'Sykes', 398210, 13043817, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3133, 'Here I Go Again', 141, 1, 3, 'Marsden', 233874, 7652473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3134, 'Is This Love', 141, 1, 3, 'Sykes', 283924, 9262360, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3135, 'Love Ain\'t No Stranger', 141, 1, 3, 'Galley', 259395, 8490428, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3136, 'Looking For Love', 141, 1, 3, 'Sykes', 391941, 12769847, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3137, 'Now You\'re Gone', 141, 1, 3, 'Vandenberg', 251141, 8162193, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3138, 'Slide It In', 141, 1, 3, 'Coverdale', 202475, 6615152, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3139, 'Slow An\' Easy', 141, 1, 3, 'Moody', 367255, 11961332, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3140, 'Judgement Day', 141, 1, 3, 'Vandenberg', 317074, 10326997, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3141, 'You\'re Gonna Break My Hart Again', 141, 1, 3, 'Sykes', 250853, 8176847, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3142, 'The Deeper The Love', 141, 1, 3, 'Vandenberg', 262791, 8606504, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3143, 'Crying In The Rain', 141, 1, 3, 'Coverdale', 337005, 10931921, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3144, 'Fool For Your Loving', 141, 1, 3, 'Marsden/Moody', 250801, 8129820, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3145, 'Sweet Lady Luck', 141, 1, 3, 'Vandenberg', 273737, 8919163, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3146, 'Faixa Amarela', 248, 1, 7, 'Beto Gogo/Jessé Pai/Luiz Carlos/Zeca Pagodinho', 240692, 8082036, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3147, 'Posso Até Me Apaixonar', 248, 1, 7, 'Dudu Nobre', 200698, 6735526, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3148, 'Não Sou Mais Disso', 248, 1, 7, 'Jorge Aragão/Zeca Pagodinho', 225985, 7613817, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3149, 'Vivo Isolado Do Mundo', 248, 1, 7, 'Alcides Dias Lopes', 180035, 6073995, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3150, 'Coração Em Desalinho', 248, 1, 7, 'Mauro Diniz/Ratino Sigem', 185208, 6225948, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3151, 'Seu Balancê', 248, 1, 7, 'Paulinho Rezende/Toninho Geraes', 219454, 7311219, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3152, 'Vai Adiar', 248, 1, 7, 'Alcino Corrêa/Monarco', 270393, 9134882, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3153, 'Rugas', 248, 1, 7, 'Augusto Garcez/Nelson Cavaquinho', 140930, 4703182, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3154, 'Feirinha da Pavuna/Luz do Repente/Bagaço da Laranja', 248, 1, 7, 'Arlindo Cruz/Franco/Marquinhos PQD/Negro, Jovelina Pérolo/Zeca Pagodinho', 107206, 3593684, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3155, 'Sem Essa de Malandro Agulha', 248, 1, 7, 'Aldir Blanc/Jayme Vignoli', 158484, 5332668, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3156, 'Chico Não Vai na Corimba', 248, 1, 7, 'Dudu Nobre/Zeca Pagodinho', 269374, 9122188, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3157, 'Papel Principal', 248, 1, 7, 'Almir Guineto/Dedé Paraiso/Luverci Ernesto', 217495, 7325302, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3158, 'Saudade Louca', 248, 1, 7, 'Acyr Marques/Arlindo Cruz/Franco', 243591, 8136475, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3159, 'Camarão que Dorme e Onda Leva', 248, 1, 7, 'Acyi Marques/Arlindo Bruz/Braço, Beto Sem/Zeca Pagodinho', 299102, 10012231, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3160, 'Sapopemba e Maxambomba', 248, 1, 7, 'Nei Lopes/Wilson Moreira', 245394, 8268712, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3161, 'Minha Fé', 248, 1, 7, 'Murilão', 206994, 6981474, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3162, 'Lua de Ogum', 248, 1, 7, 'Ratinho/Zeca Pagodinho', 168463, 5719129, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3163, 'Samba pras moças', 248, 1, 7, 'Grazielle/Roque Ferreira', 152816, 5121366, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3164, 'Verdade', 248, 1, 7, 'Carlinhos Santana/Nelson Rufino', 332826, 11120708, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3165, 'The Brig', 229, 3, 21, 2617325, 488919543, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3166, '.07%', 228, 3, 21, 2585794, 541715199, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3167, 'Five Years Gone', 228, 3, 21, 2587712, 530551890, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3168, 'The Hard Part', 228, 3, 21, 2601017, 475996611, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3169, 'The Man Behind the Curtain', 229, 3, 21, 2615990, 493951081, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3170, 'Greatest Hits', 229, 3, 21, 2617117, 522102916, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3171, 'Landslide', 228, 3, 21, 2600725, 518677861, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3172, 'The Office: An American Workplace (Pilot)', 249, 3, 19, 1380833, 290482361, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3173, 'Diversity Day', 249, 3, 19, 1306416, 257879716, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3174, 'Health Care', 249, 3, 19, 1321791, 260493577, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3175, 'The Alliance', 249, 3, 19, 1317125, 266203162, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3176, 'Basketball', 249, 3, 19, 1323541, 267464180, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3177, 'Hot Girl', 249, 3, 19, 1325458, 267836576, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3178, 'The Dundies', 250, 3, 19, 1253541, 246845576, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3179, 'Sexual Harassment', 250, 3, 19, 1294541, 273069146, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3180, 'Office Olympics', 250, 3, 19, 1290458, 256247623, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3181, 'The Fire', 250, 3, 19, 1288166, 266856017, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3182, 'Halloween', 250, 3, 19, 1315333, 249205209, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3183, 'The Fight', 250, 3, 19, 1320028, 277149457, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3184, 'The Client', 250, 3, 19, 1299341, 253836788, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3185, 'Performance Review', 250, 3, 19, 1292458, 256143822, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3186, 'Email Surveillance', 250, 3, 19, 1328870, 265101113, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3187, 'Christmas Party', 250, 3, 19, 1282115, 260891300, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3188, 'Booze Cruise', 250, 3, 19, 1267958, 252518021, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3189, 'The Injury', 250, 3, 19, 1275275, 253912762, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3190, 'The Secret', 250, 3, 19, 1264875, 253143200, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3191, 'The Carpet', 250, 3, 19, 1264375, 256477011, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3192, 'Boys and Girls', 250, 3, 19, 1278333, 255245729, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3193, 'Valentine\'s Day', 250, 3, 19, 1270375, 253552710, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3194, 'Dwight\'s Speech', 250, 3, 19, 1278041, 255001728, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3195, 'Take Your Daughter to Work Day', 250, 3, 19, 1268333, 253451012, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3196, 'Michael\'s Birthday', 250, 3, 19, 1237791, 247238398, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3197, 'Drug Testing', 250, 3, 19, 1278625, 244626927, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3198, 'Conflict Resolution', 250, 3, 19, 1274583, 253808658, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3199, 'Casino Night - Season Finale', 250, 3, 19, 1712791, 327642458, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3200, 'Gay Witch Hunt', 251, 3, 19, 1326534, 276942637, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3201, 'The Convention', 251, 3, 19, 1297213, 255117055, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3202, 'The Coup', 251, 3, 19, 1276526, 267205501, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3203, 'Grief Counseling', 251, 3, 19, 1282615, 256912833, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3204, 'The Initiation', 251, 3, 19, 1280113, 251728257, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3205, 'Diwali', 251, 3, 19, 1279904, 252726644, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3206, 'Branch Closing', 251, 3, 19, 1822781, 358761786, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3207, 'The Merger', 251, 3, 19, 1801926, 345960631, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3208, 'The Convict', 251, 3, 22, 1273064, 248863427, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3209, 'A Benihana Christmas, Pts. 1 & 2', 251, 3, 22, 2519436, 515301752, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3210, 'Back from Vacation', 251, 3, 22, 1271688, 245378749, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3211, 'Traveling Salesmen', 251, 3, 22, 1289039, 250822697, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3212, 'Producer\'s Cut: The Return', 251, 3, 22, 1700241, 337219980, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3213, 'Ben Franklin', 251, 3, 22, 1271938, 264168080, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3214, 'Phyllis\'s Wedding', 251, 3, 22, 1271521, 258561054, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3215, 'Business School', 251, 3, 22, 1302093, 254402605, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3216, 'Cocktails', 251, 3, 22, 1272522, 259011909, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3217, 'The Negotiation', 251, 3, 22, 1767851, 371663719, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3218, 'Safety Training', 251, 3, 22, 1271229, 253054534, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3219, 'Product Recall', 251, 3, 22, 1268268, 251208610, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3220, 'Women\'s Appreciation', 251, 3, 22, 1732649, 338778844, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3221, 'Beach Games', 251, 3, 22, 1676134, 333671149, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3222, 'The Job', 251, 3, 22, 2541875, 501060138, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3223, 'How to Stop an Exploding Man', 228, 3, 21, 2687103, 487881159, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3224, 'Through a Looking Glass', 229, 3, 21, 5088838, 1059546140, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3225, 'Your Time Is Gonna Come', 252, 2, 1, 'Page, Jones', 310774, 5126563, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3226, 'Battlestar Galactica, Pt. 1', 253, 3, 20, 2952702, 541359437, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3227, 'Battlestar Galactica, Pt. 2', 253, 3, 20, 2956081, 521387924, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3228, 'Battlestar Galactica, Pt. 3', 253, 3, 20, 2927802, 554509033, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3229, 'Lost Planet of the Gods, Pt. 1', 253, 3, 20, 2922547, 537812711, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3230, 'Lost Planet of the Gods, Pt. 2', 253, 3, 20, 2914664, 534343985, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3231, 'The Lost Warrior', 253, 3, 20, 2920045, 558872190, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3232, 'The Long Patrol', 253, 3, 20, 2925008, 513122217, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3233, 'The Gun On Ice Planet Zero, Pt. 1', 253, 3, 20, 2907615, 540980196, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3234, 'The Gun On Ice Planet Zero, Pt. 2', 253, 3, 20, 2924341, 546542281, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3235, 'The Magnificent Warriors', 253, 3, 20, 2924716, 570152232, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3236, 'The Young Lords', 253, 3, 20, 2863571, 587051735, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3237, 'The Living Legend, Pt. 1', 253, 3, 20, 2924507, 503641007, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3238, 'The Living Legend, Pt. 2', 253, 3, 20, 2923298, 515632754, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3239, 'Fire In Space', 253, 3, 20, 2926593, 536784757, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3240, 'War of the Gods, Pt. 1', 253, 3, 20, 2922630, 505761343, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3241, 'War of the Gods, Pt. 2', 253, 3, 20, 2923381, 487899692, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3242, 'The Man With Nine Lives', 253, 3, 20, 2956998, 577829804, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3243, 'Murder On the Rising Star', 253, 3, 20, 2935894, 551759986, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3244, 'Greetings from Earth, Pt. 1', 253, 3, 20, 2960293, 536824558, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3245, 'Greetings from Earth, Pt. 2', 253, 3, 20, 2903778, 527842860, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3246, 'Baltar\'s Escape', 253, 3, 20, 2922088, 525564224, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3247, 'Experiment In Terra', 253, 3, 20, 2923548, 547982556, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3248, 'Take the Celestra', 253, 3, 20, 2927677, 512381289, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3249, 'The Hand of God', 253, 3, 20, 2924007, 536583079, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3250, 'Pilot', 254, 3, 19, 2484567, 492670102, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3251, 'Through the Looking Glass, Pt. 2', 229, 3, 21, 2617117, 550943353, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3252, 'Through the Looking Glass, Pt. 1', 229, 3, 21, 2610860, 493211809, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3253, 'Instant Karma', 255, 2, 9, 193188, 3150090, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3254, '#9 Dream', 255, 2, 9, 278312, 4506425, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3255, 'Mother', 255, 2, 9, 287740, 4656660, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3256, 'Give Peace a Chance', 255, 2, 9, 274644, 4448025, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3257, 'Cold Turkey', 255, 2, 9, 281424, 4556003, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3258, 'Whatever Gets You Thru the Night', 255, 2, 9, 215084, 3499018, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3259, 'I\'m Losing You', 255, 2, 9, 240719, 3907467, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3260, 'Gimme Some Truth', 255, 2, 9, 232778, 3780807, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3261, 'Oh, My Love', 255, 2, 9, 159473, 2612788, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3262, 'Imagine', 255, 2, 9, 192329, 3136271, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3263, 'Nobody Told Me', 255, 2, 9, 210348, 3423395, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3264, 'Jealous Guy', 255, 2, 9, 239094, 3881620, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3265, 'Working Class Hero', 255, 2, 9, 265449, 4301430, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3266, 'Power to the People', 255, 2, 9, 213018, 3466029, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3267, 'Imagine', 255, 2, 9, 219078, 3562542, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3268, 'Beautiful Boy', 255, 2, 9, 227995, 3704642, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3269, 'Isolation', 255, 2, 9, 156059, 2558399, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3270, 'Watching the Wheels', 255, 2, 9, 198645, 3237063, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3271, 'Grow Old With Me', 255, 2, 9, 149093, 2447453, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3272, 'Gimme Some Truth', 255, 2, 9, 187546, 3060083, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3273, '[Just Like] Starting Over', 255, 2, 9, 215549, 3506308, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3274, 'God', 255, 2, 9, 260410, 4221135, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3275, 'Real Love', 255, 2, 9, 236911, 3846658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3276, 'Sympton of the Universe', 256, 2, 1, 340890, 5489313, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3277, 'Snowblind', 256, 2, 1, 295960, 4773171, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3278, 'Black Sabbath', 256, 2, 1, 364180, 5860455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3279, 'Fairies Wear Boots', 256, 2, 1, 392764, 6315916, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3280, 'War Pigs', 256, 2, 1, 515435, 8270194, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3281, 'The Wizard', 256, 2, 1, 282678, 4561796, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3282, 'N.I.B.', 256, 2, 1, 335248, 5399456, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3283, 'Sweet Leaf', 256, 2, 1, 354706, 5709700, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3284, 'Never Say Die', 256, 2, 1, 258343, 4173799, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3285, 'Sabbath, Bloody Sabbath', 256, 2, 1, 333622, 5373633, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3286, 'Iron Man/Children of the Grave', 256, 2, 1, 552308, 8858616, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3287, 'Paranoid', 256, 2, 1, 189171, 3071042, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3288, 'Rock You Like a Hurricane', 257, 2, 1, 255766, 4300973, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3289, 'No One Like You', 257, 2, 1, 240325, 4050259, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3290, 'The Zoo', 257, 2, 1, 332740, 5550779, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3291, 'Loving You Sunday Morning', 257, 2, 1, 339125, 5654493, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3292, 'Still Loving You', 257, 2, 1, 390674, 6491444, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3293, 'Big City Nights', 257, 2, 1, 251865, 4237651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3294, 'Believe in Love', 257, 2, 1, 325774, 5437651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3295, 'Rhythm of Love', 257, 2, 1, 231246, 3902834, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3296, 'I Can\'t Explain', 257, 2, 1, 205332, 3482099, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3297, 'Tease Me Please Me', 257, 2, 1, 287229, 4811894, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3298, 'Wind of Change', 257, 2, 1, 315325, 5268002, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3299, 'Send Me an Angel', 257, 2, 1, 273041, 4581492, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3300, 'Jump Around', 258, 1, 17, 'E. Schrody/L. Muggerud', 217835, 8715653, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3301, 'Salutations', 258, 1, 17, 'E. Schrody/L. Dimant', 69120, 2767047, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3302, 'Put Your Head Out', 258, 1, 17, 'E. Schrody/L. Freese/L. Muggerud', 182230, 7291473, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3303, 'Top O\' The Morning To Ya', 258, 1, 17, 'E. Schrody/L. Dimant', 216633, 8667599, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3304, 'Commercial 1', 258, 1, 17, 'L. Muggerud', 7941, 319888, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3305, 'House And The Rising Sun', 258, 1, 17, 'E. Schrody/J. Vasquez/L. Dimant', 219402, 8778369, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3306, 'Shamrocks And Shenanigans', 258, 1, 17, 'E. Schrody/L. Dimant', 218331, 8735518, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3307, 'House Of Pain Anthem', 258, 1, 17, 'E. Schrody/L. Dimant', 155611, 6226713, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3308, 'Danny Boy, Danny Boy', 258, 1, 17, 'E. Schrody/L. Muggerud', 114520, 4583091, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3309, 'Guess Who\'s Back', 258, 1, 17, 'E. Schrody/L. Muggerud', 238393, 9537994, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3310, 'Commercial 2', 258, 1, 17, 'L. Muggerud', 21211, 850698, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3311, 'Put On Your Shit Kickers', 258, 1, 17, 'E. Schrody/L. Muggerud', 190432, 7619569, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3312, 'Come And Get Some Of This', 258, 1, 17, 'E. Schrody/L. Muggerud/R. Medrano', 170475, 6821279, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3313, 'Life Goes On', 258, 1, 17, 'E. Schrody/R. Medrano', 163030, 6523458, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3314, 'One For The Road', 258, 1, 17, 'E. Schrody/L. Dimant/L. Muggerud', 170213, 6810820, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3315, 'Feel It', 258, 1, 17, 'E. Schrody/R. Medrano', 239908, 9598588, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3316, 'All My Love', 258, 1, 17, 'E. Schrody/L. Dimant', 200620, 8027065, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3317, 'Jump Around (Pete Rock Remix)', 258, 1, 17, 'E. Schrody/L. Muggerud', 236120, 9447101, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3318, 'Shamrocks And Shenanigans (Boom Shalock Lock Boom/Butch Vig Mix)', 258, 1, 17, 'E. Schrody/L. Dimant', 237035, 9483705, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3319, 'Instinto Colectivo', 259, 1, 15, 300564, 12024875, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3320, 'Chapa o Coco', 259, 1, 15, 143830, 5755478, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3321, 'Prostituta', 259, 1, 15, 359000, 14362307, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3322, 'Eu So Queria Sumir', 259, 1, 15, 269740, 10791921, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3323, 'Tres Reis', 259, 1, 15, 304143, 12168015, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3324, 'Um Lugar ao Sol', 259, 1, 15, 212323, 8495217, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3325, 'Batalha Naval', 259, 1, 15, 285727, 11431382, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3326, 'Todo o Carnaval tem seu Fim', 259, 1, 15, 237426, 9499371, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3327, 'O Misterio do Samba', 259, 1, 15, 226142, 9047970, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3328, 'Armadura', 259, 1, 15, 232881, 9317533, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3329, 'Na Ladeira', 259, 1, 15, 221570, 8865099, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3330, 'Carimbo', 259, 1, 15, 328751, 13152314, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3331, 'Catimbo', 259, 1, 15, 254484, 10181692, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3332, 'Funk de Bamba', 259, 1, 15, 237322, 9495184, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3333, 'Chega no Suingue', 259, 1, 15, 221805, 8874509, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3334, 'Mun-Ra', 259, 1, 15, 274651, 10988338, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3335, 'Freestyle Love', 259, 1, 15, 318484, 12741680, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3336, 'War Pigs', 260, 4, 23, 234013, 8052374, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3337, 'Past, Present, and Future', 261, 3, 21, 2492867, 490796184, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3338, 'The Beginning of the End', 261, 3, 21, 2611903, 526865050, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3339, 'LOST Season 4 Trailer', 261, 3, 21, 112712, 20831818, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3340, 'LOST In 8:15', 261, 3, 21, 497163, 98460675, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3341, 'Confirmed Dead', 261, 3, 21, 2611986, 512168460, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3342, 'The Economist', 261, 3, 21, 2609025, 516934914, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3343, 'Eggtown', 261, 3, 19, 2608817, 501061240, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3344, 'The Constant', 261, 3, 21, 2611569, 520209363, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3345, 'The Other Woman', 261, 3, 21, 2605021, 513246663, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3346, 'Ji Yeon', 261, 3, 19, 2588797, 506458858, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3347, 'Meet Kevin Johnson', 261, 3, 19, 2612028, 504132981, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3348, 'The Shape of Things to Come', 261, 3, 21, 2591299, 502284266, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3349, 'Amanda', 262, 5, 2, 'Luca Gusella', 246503, 4011615, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3350, 'Despertar', 262, 5, 2, 'Andrea Dulbecco', 307385, 4821485, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3351, 'Din Din Wo (Little Child)', 263, 5, 16, 'Habib Koité', 285837, 4615841, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3352, 'Distance', 264, 5, 15, 'Karsh Kale/Vishal Vaid', 327122, 5327463, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3353, 'I Guess You\'re Right', 265, 5, 1, 'Darius "Take One" Minwalla/Jon Auer/Ken Stringfellow/Matt Harris', 212044, 3453849, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3354, 'I Ka Barra (Your Work)', 263, 5, 16, 'Habib Koité', 300605, 4855457, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3355, 'Love Comes', 265, 5, 1, 'Darius "Take One" Minwalla/Jon Auer/Ken Stringfellow/Matt Harris', 199923, 3240609, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3356, 'Muita Bobeira', 266, 5, 7, 'Luciana Souza', 172710, 2775071, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3357, 'OAM\'s Blues', 267, 5, 2, 'Aaron Goldberg', 266936, 4292028, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3358, 'One Step Beyond', 264, 5, 15, 'Karsh Kale', 366085, 6034098, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3359, 'Symphony No. 3 in E-flat major, Op. 55, "Eroica" - Scherzo: Allegro Vivace', 268, 5, 24, 'Ludwig van Beethoven', 356426, 5817216, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3360, 'Something Nice Back Home', 261, 3, 21, 2612779, 484711353, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3361, 'Cabin Fever', 261, 3, 21, 2612028, 477733942, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3362, 'There\'s No Place Like Home, Pt. 1', 261, 3, 21, 2609526, 522919189, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3363, 'There\'s No Place Like Home, Pt. 2', 261, 3, 21, 2497956, 523748920, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3364, 'There\'s No Place Like Home, Pt. 3', 261, 3, 21, 2582957, 486161766, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3365, 'Say Hello 2 Heaven', 269, 2, 23, 384497, 6477217, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3366, 'Reach Down', 269, 2, 23, 672773, 11157785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3367, 'Hunger Strike', 269, 2, 23, 246292, 4233212, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3368, 'Pushin Forward Back', 269, 2, 23, 225278, 3892066, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3369, 'Call Me a Dog', 269, 2, 23, 304458, 5177612, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3370, 'Times of Trouble', 269, 2, 23, 342539, 5795951, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3371, 'Wooden Jesus', 269, 2, 23, 250565, 4302603, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3372, 'Your Savior', 269, 2, 23, 244226, 4199626, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3373, 'Four Walled World', 269, 2, 23, 414474, 6964048, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3374, 'All Night Thing', 269, 2, 23, 231803, 3997982, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3375, 'No Such Thing', 270, 2, 23, 'Chris Cornell', 224837, 3691272, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3376, 'Poison Eye', 270, 2, 23, 'Chris Cornell', 237120, 3890037, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3377, 'Arms Around Your Love', 270, 2, 23, 'Chris Cornell', 214016, 3516224, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3378, 'Safe and Sound', 270, 2, 23, 'Chris Cornell', 256764, 4207769, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3379, 'She\'ll Never Be Your Man', 270, 2, 23, 'Chris Cornell', 204078, 3355715, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3380, 'Ghosts', 270, 2, 23, 'Chris Cornell', 231547, 3799745, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3381, 'Killing Birds', 270, 2, 23, 'Chris Cornell', 218498, 3588776, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3382, 'Billie Jean', 270, 2, 23, 'Michael Jackson', 281401, 4606408, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3383, 'Scar On the Sky', 270, 2, 23, 'Chris Cornell', 220193, 3616618, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3384, 'Your Soul Today', 270, 2, 23, 'Chris Cornell', 205959, 3385722, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3385, 'Finally Forever', 270, 2, 23, 'Chris Cornell', 217035, 3565098, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3386, 'Silence the Voices', 270, 2, 23, 'Chris Cornell', 267376, 4379597, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3387, 'Disappearing Act', 270, 2, 23, 'Chris Cornell', 273320, 4476203, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3388, 'You Know My Name', 270, 2, 23, 'Chris Cornell', 240255, 3940651, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3389, 'Revelations', 271, 2, 23, 252376, 4111051, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3390, 'One and the Same', 271, 2, 23, 217732, 3559040, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3391, 'Sound of a Gun', 271, 2, 23, 260154, 4234990, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3392, 'Until We Fall', 271, 2, 23, 230758, 3766605, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3393, 'Original Fire', 271, 2, 23, 218916, 3577821, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3394, 'Broken City', 271, 2, 23, 228366, 3728955, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3395, 'Somedays', 271, 2, 23, 213831, 3497176, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3396, 'Shape of Things to Come', 271, 2, 23, 274597, 4465399, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3397, 'Jewel of the Summertime', 271, 2, 23, 233242, 3806103, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3398, 'Wide Awake', 271, 2, 23, 266308, 4333050, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3399, 'Nothing Left to Say But Goodbye', 271, 2, 23, 213041, 3484335, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3400, 'Moth', 271, 2, 23, 298049, 4838884, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3401, 'Show Me How to Live (Live at the Quart Festival)', 271, 2, 23, 301974, 4901540, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3402, 'Band Members Discuss Tracks from "Revelations"', 271, 3, 23, 294294, 61118891, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3403, 'Intoitus: Adorate Deum', 272, 2, 24, 'Anonymous', 245317, 4123531, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3404, 'Miserere mei, Deus', 273, 2, 24, 'Gregorio Allegri', 501503, 8285941, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3405, 'Canon and Gigue in D Major: I. Canon', 274, 2, 24, 'Johann Pachelbel', 271788, 4438393, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3406, 'Concerto No. 1 in E Major, RV 269 "Spring": I. Allegro', 275, 2, 24, 'Antonio Vivaldi', 199086, 3347810, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3407, 'Concerto for 2 Violins in D Minor, BWV 1043: I. Vivace', 276, 2, 24, 'Johann Sebastian Bach', 193722, 3192890, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3408, 'Aria Mit 30 Veränderungen, BWV 988 "Goldberg Variations": Aria', 277, 2, 24, 'Johann Sebastian Bach', 120463, 2081895, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3409, 'Suite for Solo Cello No. 1 in G Major, BWV 1007: I. Prélude', 278, 2, 24, 'Johann Sebastian Bach', 143288, 2315495, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3410, 'The Messiah: Behold, I Tell You a Mystery... The Trumpet Shall Sound', 279, 2, 24, 'George Frideric Handel', 582029, 9553140, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3411, 'Solomon HWV 67: The Arrival of the Queen of Sheba', 280, 2, 24, 'George Frideric Handel', 197135, 3247914, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3412, '"Eine Kleine Nachtmusik" Serenade In G, K. 525: I. Allegro', 281, 2, 24, 'Wolfgang Amadeus Mozart', 348971, 5760129, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3413, 'Concerto for Clarinet in A Major, K. 622: II. Adagio', 282, 2, 24, 'Wolfgang Amadeus Mozart', 394482, 6474980, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3414, 'Symphony No. 104 in D Major "London": IV. Finale: Spiritoso', 283, 4, 24, 'Franz Joseph Haydn', 306687, 10085867, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3415, 'Symphony No.5 in C Minor: I. Allegro con brio', 284, 2, 24, 'Ludwig van Beethoven', 392462, 6419730, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3416, 'Ave Maria', 285, 2, 24, 'Franz Schubert', 338243, 5605648, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3417, 'Nabucco: Chorus, "Va, Pensiero, Sull\'ali Dorate"', 286, 2, 24, 'Giuseppe Verdi', 274504, 4498583, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3418, 'Die Walküre: The Ride of the Valkyries', 287, 2, 24, 'Richard Wagner', 189008, 3114209, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3419, 'Requiem, Op.48: 4. Pie Jesu', 288, 2, 24, 'Gabriel Fauré', 258924, 4314850, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3420, 'The Nutcracker, Op. 71a, Act II: Scene 14: Pas de deux: Dance of the Prince & the Sugar-Plum Fairy', 289, 2, 24, 'Peter Ilyich Tchaikovsky', 304226, 5184289, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3421, 'Nimrod (Adagio) from Variations On an Original Theme, Op. 36 "Enigma"', 290, 2, 24, 'Edward Elgar', 250031, 4124707, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3422, 'Madama Butterfly: Un Bel Dì Vedremo', 291, 2, 24, 'Giacomo Puccini', 277639, 4588197, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3423, 'Jupiter, the Bringer of Jollity', 292, 2, 24, 'Gustav Holst', 522099, 8547876, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3424, 'Turandot, Act III, Nessun dorma!', 293, 2, 24, 'Giacomo Puccini', 176911, 2920890, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3425, 'Adagio for Strings from the String Quartet, Op. 11', 294, 2, 24, 'Samuel Barber', 596519, 9585597, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3426, 'Carmina Burana: O Fortuna', 295, 2, 24, 'Carl Orff', 156710, 2630293, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3427, 'Fanfare for the Common Man', 296, 2, 24, 'Aaron Copland', 198064, 3211245, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3428, 'Branch Closing', 251, 3, 22, 1814855, 360331351, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3429, 'The Return', 251, 3, 22, 1705080, 343877320, 1.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3430, 'Toccata and Fugue in D Minor, BWV 565: I. Toccata', 297, 2, 24, 'Johann Sebastian Bach', 153901, 2649938, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3431, 'Symphony No.1 in D Major, Op.25 "Classical", Allegro Con Brio', 298, 2, 24, 'Sergei Prokofiev', 254001, 4195542, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3432, 'Scheherazade, Op. 35: I. The Sea and Sindbad\'s Ship', 299, 2, 24, 'Nikolai Rimsky-Korsakov', 545203, 8916313, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3433, 'Concerto No.2 in F Major, BWV1047, I. Allegro', 300, 2, 24, 'Johann Sebastian Bach', 307244, 5064553, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3434, 'Concerto for Piano No. 2 in F Minor, Op. 21: II. Larghetto', 301, 2, 24, 'Frédéric Chopin', 560342, 9160082, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3435, 'Cavalleria Rusticana \ Act \ Intermezzo Sinfonico', 302, 2, 24, 'Pietro Mascagni', 243436, 4001276, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3436, 'Karelia Suite, Op.11: 2. Ballade (Tempo Di Menuetto)', 303, 2, 24, 'Jean Sibelius', 406000, 5908455, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3437, 'Piano Sonata No. 14 in C Sharp Minor, Op. 27, No. 2, "Moonlight": I. Adagio sostenuto', 304, 2, 24, 'Ludwig van Beethoven', 391000, 6318740, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3438, 'Fantasia On Greensleeves', 280, 2, 24, 'Ralph Vaughan Williams', 268066, 4513190, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3439, 'Das Lied Von Der Erde, Von Der Jugend', 305, 2, 24, 'Gustav Mahler', 223583, 3700206, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3440, 'Concerto for Cello and Orchestra in E minor, Op. 85: I. Adagio - Moderato', 306, 2, 24, 'Edward Elgar', 483133, 7865479, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3441, 'Two Fanfares for Orchestra: II. Short Ride in a Fast Machine', 307, 2, 24, 'John Adams', 254930, 4310896, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3442, 'Wellington\'s Victory or the Battle Symphony, Op.91: 2. Symphony of Triumph', 308, 2, 24, 'Ludwig van Beethoven', 412000, 6965201, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3443, 'Missa Papae Marcelli: Kyrie', 309, 2, 24, 'Giovanni Pierluigi da Palestrina', 240666, 4244149, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3444, 'Romeo et Juliette: No. 11 - Danse des Chevaliers', 310, 2, 24, 275015, 4519239, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3445, 'On the Beautiful Blue Danube', 311, 2, 24, 'Johann Strauss II', 526696, 8610225, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3446, 'Symphonie Fantastique, Op. 14: V. Songe d\'une nuit du sabbat', 312, 2, 24, 'Hector Berlioz', 561967, 9173344, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3447, 'Carmen: Overture', 313, 2, 24, 'Georges Bizet', 132932, 2189002, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3448, 'Lamentations of Jeremiah, First Set \ Incipit Lamentatio', 314, 2, 24, 'Thomas Tallis', 69194, 1208080, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3449, 'Music for the Royal Fireworks, HWV351 (1749): La Réjouissance', 315, 2, 24, 'George Frideric Handel', 120000, 2193734, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3450, 'Peer Gynt Suite No.1, Op.46: 1. Morning Mood', 316, 2, 24, 'Edvard Grieg', 253422, 4298769, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3451, 'Die Zauberflöte, K.620: "Der Hölle Rache Kocht in Meinem Herze"', 317, 2, 25, 'Wolfgang Amadeus Mozart', 174813, 2861468, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3452, 'SCRIABIN: Prelude in B Major, Op. 11, No. 11', 318, 4, 24, 101293, 3819535, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3453, 'Pavan, Lachrimae Antiquae', 319, 2, 24, 'John Dowland', 253281, 4211495, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3454, 'Symphony No. 41 in C Major, K. 551, "Jupiter": IV. Molto allegro', 320, 2, 24, 'Wolfgang Amadeus Mozart', 362933, 6173269, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3455, 'Rehab', 321, 2, 14, 213240, 3416878, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3456, 'You Know I\'m No Good', 321, 2, 14, 256946, 4133694, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3457, 'Me & Mr. Jones', 321, 2, 14, 151706, 2449438, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3458, 'Just Friends', 321, 2, 14, 191933, 3098906, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3459, 'Back to Black', 321, 2, 14, 'Mark Ronson', 240320, 3852953, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3460, 'Love Is a Losing Game', 321, 2, 14, 154386, 2509409, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3461, 'Tears Dry On Their Own', 321, 2, 14, 'Nickolas Ashford & Valerie Simpson', 185293, 2996598, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3462, 'Wake Up Alone', 321, 2, 14, 'Paul O\'duffy', 221413, 3576773, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3463, 'Some Unholy War', 321, 2, 14, 141520, 2304465, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3464, 'He Can Only Hold Her', 321, 2, 14, 'Richard Poindexter & Robert Poindexter', 166680, 2666531, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3465, 'You Know I\'m No Good (feat. Ghostface Killah)', 321, 2, 14, 202320, 3260658, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3466, 'Rehab (Hot Chip Remix)', 321, 2, 14, 418293, 6670600, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3467, 'Intro / Stronger Than Me', 322, 2, 9, 234200, 3832165, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3468, 'You Sent Me Flying / Cherry', 322, 2, 9, 409906, 6657517, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3469, 'F**k Me Pumps', 322, 2, 9, 'Salaam Remi', 200253, 3324343, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3470, 'I Heard Love Is Blind', 322, 2, 9, 129666, 2190831, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3471, '(There Is) No Greater Love (Teo Licks)', 322, 2, 9, 'Isham Jones & Marty Symes', 167933, 2773507, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3472, 'In My Bed', 322, 2, 9, 'Salaam Remi', 315960, 5211774, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3473, 'Take the Box', 322, 2, 9, 'Luke Smith', 199160, 3281526, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3474, 'October Song', 322, 2, 9, 'Matt Rowe & Stefan Skarbek', 204846, 3358125, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3475, 'What Is It About Men', 322, 2, 9, 'Delroy "Chris" Cooper, Donovan Jackson, Earl Chinna Smith, Felix Howard, Gordon Williams, Luke Smith, Paul Watson & Wilburn Squiddley Cole', 209573, 3426106, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3476, 'Help Yourself', 322, 2, 9, 'Freddy James, Jimmy hogarth & Larry Stock', 300884, 5029266, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3477, 'Amy Amy Amy (Outro)', 322, 2, 9, 'Astor Campbell, Delroy "Chris" Cooper, Donovan Jackson, Dorothy Fields, Earl Chinna Smith, Felix Howard, Gordon Williams, James Moody, Jimmy McHugh, Matt Rowe, Salaam Remi & Stefan Skarbek', 663426, 10564704, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3478, 'Slowness', 323, 2, 23, 215386, 3644793, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3479, 'Prometheus Overture, Op. 43', 324, 4, 24, 'Ludwig van Beethoven', 339567, 10887931, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3480, 'Sonata for Solo Violin: IV: Presto', 325, 4, 24, 'Béla Bartók', 299350, 9785346, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3481, 'A Midsummer Night\'s Dream, Op.61 Incidental Music: No.7 Notturno', 326, 2, 24, 387826, 6497867, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3482, 'Suite No. 3 in D, BWV 1068: III. Gavotte I & II', 327, 2, 24, 'Johann Sebastian Bach', 225933, 3847164, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3483, 'Concert pour 4 Parties de V**les, H. 545: I. Prelude', 328, 2, 24, 'Marc-Antoine Charpentier', 110266, 1973559, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3484, 'Adios nonino', 329, 2, 24, 'Astor Piazzolla', 289388, 4781384, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3485, 'Symphony No. 3 Op. 36 for Orchestra and Soprano "Symfonia Piesni Zalosnych" \ Lento E Largo - Tranquillissimo', 330, 2, 24, 'Henryk Górecki', 567494, 9273123, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3486, 'Act IV, Symphony', 331, 2, 24, 'Henry Purcell', 364296, 5987695, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3487, '3 Gymnopédies: No.1 - Lent Et Grave, No.3 - Lent Et Douloureux', 332, 2, 24, 'Erik Satie', 385506, 6458501, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3488, 'Music for the Funeral of Queen Mary: VI. "Thou Knowest, Lord, the Secrets of Our Hearts"', 333, 2, 24, 'Henry Purcell', 142081, 2365930, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3489, 'Symphony No. 2: III. Allegro vivace', 334, 2, 24, 'Kurt Weill', 376510, 6129146, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3490, 'Partita in E Major, BWV 1006A: I. Prelude', 335, 2, 24, 'Johann Sebastian Bach', 285673, 4744929, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3491, 'Le Sacre Du Printemps: I.iv. Spring Rounds', 336, 2, 24, 'Igor Stravinsky', 234746, 4072205, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3492, 'Sing Joyfully', 314, 2, 24, 'William Byrd', 133768, 2256484, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3493, 'Metopes, Op. 29: Calypso', 337, 2, 24, 'Karol Szymanowski', 333669, 5548755, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3494, 'Symphony No. 2, Op. 16 -  "The Four Temperaments": II. Allegro Comodo e Flemmatico', 338, 2, 24, 'Carl Nielsen', 286998, 4834785, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3495, '24 Caprices, Op. 1, No. 24, for Solo Violin, in A Minor', 339, 2, 24, 'Niccolò Paganini', 265541, 4371533, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3496, 'Étude 1, In C Major - Preludio (Presto) - Liszt', 340, 4, 24, 51780, 2229617, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3497, 'Erlkonig, D.328', 341, 2, 24, 261849, 4307907, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3498, 'Concerto for Violin, Strings and Continuo in G Major, Op. 3, No. 9: I. Allegro', 342, 4, 24, 'Pietro Antonio Locatelli', 493573, 16454937, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Milliseconds, Bytes, UnitPrice) VALUES (3499, 'Pini Di Roma (Pinien Von Rom) \ I Pini Della Via Appia', 343, 2, 24, 286741, 4718950, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3500, 'String Quartet No. 12 in C Minor, D. 703 "Quartettsatz": II. Andante - Allegro assai', 344, 2, 24, 'Franz Schubert', 139200, 2283131, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3501, 'L\'orfeo, Act 3, Sinfonia (Orchestra)', 345, 2, 24, 'Claudio Monteverdi', 66639, 1189062, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3502, 'Quintet for Horn, Violin, 2 Violas, and Cello in E Flat Major, K. 407/386c: III. Allegro', 346, 2, 24, 'Wolfgang Amadeus Mozart', 221331, 3665114, 0.99);
INSERT INTO Track (TrackId, Name, AlbumId, MediaTypeId, GenreId, Composer, Milliseconds, Bytes, UnitPrice) VALUES (3503, 'Koyaanisqatsi', 347, 2, 10, 'Philip Glass', 206005, 3305164, 0.99);
