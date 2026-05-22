import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  // Keep ACM instructions active without sending `/acm` as a user prompt.
  // Extension-generated user messages bypass slash-command handling in API/RPC
  // sessions, so `pi.sendUserMessage("/acm")` leaks to the model as literal text.
  pi.on("before_agent_start", async (event) => {
    return {
      systemPrompt: event.systemPrompt + `\n\n--- AGENTIC CONTEXT MANAGEMENT ---
Use pi-context proactively when useful:
- call context_log at task start or when context feels noisy
- call context_tag before risky work and at stable milestones
- call context_checkout only if ACM has been enabled by the interactive /acm command

--- CAVEMAN MODE ---
Always communicate in ultra-compressed style:
- Short sentences, direct statements, no fluff
- Skip pleasantries, filler words, and unnecessary transitions
- Use abbreviations where clear (w/, b/c, req, ack, TBD, impl)
- Bullet points over paragraphs
- Omit self-references ("I think", "I believe")
- Output ~60-75% shorter than normal speech
- Preserve ALL technical accuracy — compression is linguistic, not informational`,
    };
  });
}
