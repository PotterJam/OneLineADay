export { getLines, createLine }
export type { Line };

import { getData, postData } from "../Api";

type Line = {
  id: number,
  body: string,
  created: string
};

type LineResp = {
  respId: number,
  respBody: string,
  respCreated: string
};

const lineFromResponse = (x: LineResp) => ({ 
  id: x.respId,
  body: x.respBody,
  created: x.respCreated
});

const getLines = async (): Promise<Line[]> => {
  try {
    const response = await getData('/api/lines');
    const lineResps = await response.json();
    return lineResps.map(lineFromResponse);
  } catch (error) {
    return [];
  }
};

const createLine = async (lineBody: String): Promise<Line> => {
  const response = await postData('/api/lines', { message: lineBody });
  const newLine = await response.json();
  return lineFromResponse(newLine);
};