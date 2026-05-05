import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "typebox";

const AskUserParams = Type.Object({
  question: Type.String({ description: "Question to ask the user" }),
  options: Type.Optional(Type.Array(Type.String(), { description: "Optional choices for the user to select from" })),
  placeholder: Type.Optional(Type.String({ description: "Placeholder text for free-form input" })),
});

export default function askUser(pi: ExtensionAPI) {
  pi.registerTool({
    name: "ask_user",
    label: "Ask User",
    description: "Ask the user a clarifying question. Use this when user input is required before proceeding.",
    promptSnippet: "Ask the user a clarifying question and wait for their answer.",
    promptGuidelines: [
      "Use ask_user when you cannot safely or confidently proceed without clarification from the user.",
      "Do not use ask_user for questions you can answer by reading files, inspecting code, or making a reasonable assumption.",
    ],
    parameters: AskUserParams,

    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      if (!ctx.hasUI) {
        return {
          isError: true,
          content: [{ type: "text", text: "UI is not available, so ask_user cannot prompt the user." }],
          details: { question: params.question, answer: null },
        };
      }

      let answer: string | null | undefined;
      if (params.options && params.options.length > 0) {
        answer = await ctx.ui.select(params.question, params.options);
      } else {
        answer = await ctx.ui.input(params.question, params.placeholder ?? "Type your answer...");
      }

      if (!answer) {
        return {
          content: [{ type: "text", text: "User cancelled or provided no answer." }],
          details: { question: params.question, answer: null },
        };
      }

      return {
        content: [{ type: "text", text: `User answered: ${answer}` }],
        details: { question: params.question, answer },
      };
    },
  });
}
