type Visitor = {
  name: string;
  age: number;
  ticketId: string | null;
};

type VisitorWithGtc = Visitor & { gtc?: { version: string; } };
