export { createLine }

import { postData } from "../Api";

const createLine = async (lineBody: String) => {
    return await postData('/api/lines', { message: lineBody })
};