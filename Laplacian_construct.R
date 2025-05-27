compute_laplacian <- function() {
# Full list of US states including DC
states <- c("AL", "AK", "AZ", "AR",  "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", 
            "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
            "VT", "VA", "WA", "WV", "WI", "WY")

# Adjacency list for all 51 states
adj_list <- list(
  AL = c("FL", "GA", "MS", "TN"),
  AK = c("WA","CA", "OR"), #nearest nbr {WA, CA, OR}
  AZ = c("CA", "CO", "NM", "NV", "UT", "HI"), #add {HI}
  AR = c("LA", "MS", "MO", "OK", "TN", "TX"),
  CA = c("AZ", "NV", "OR", "AK", "HI"), #add {AK, HI} 
  CO = c("AZ", "KS", "NE", "NM", "OK", "UT", "WY"),
  CT = c("MA", "NY", "RI"),
  DE = c("MD", "NJ", "PA"),
  DC = c("MD", "VA"),  # Borders Maryland and Virginia
  FL = c("AL", "GA"),
  GA = c("AL", "FL", "NC", "SC", "TN"),
  HI = c("CA", "AZ"),  # connect HI to {CA, AZ}
  ID = c("MT", "NV", "OR", "UT", "WA", "WY"),
  IL = c("IA", "IN", "KY", "MO", "WI"),
  IN = c("IL", "KY", "MI", "OH"),
  IA = c("IL", "MN", "MO", "NE", "SD", "WI"),
  KS = c("CO", "MO", "NE", "OK"),
  KY = c("IL", "IN", "MO", "OH", "TN", "VA", "WV"),
  LA = c("AR", "MS", "TX"),
  ME = c("NH"),
  MD = c("DC", "DE", "PA", "VA", "WV"),
  MA = c("CT", "NH", "NY", "RI", "VT"),
  MI = c("IN", "OH", "WI"),
  MN = c("IA", "ND", "SD", "WI"),
  MS = c("AL", "AR", "LA", "TN"),
  MO = c("AR", "IA", "IL", "KS", "KY", "NE", "OK", "TN"),
  MT = c("ID", "ND", "SD", "WY"),
  NE = c("CO", "IA", "KS", "MO", "SD", "WY"),
  NV = c("AZ", "CA", "ID", "OR", "UT"),
  NH = c("MA", "ME", "VT"),
  NJ = c("DE", "NY", "PA"),
  NM = c("AZ", "CO", "OK", "TX", "UT"), 
  NY = c("CT", "MA", "NJ", "PA", "VT"),
  NC = c("GA", "SC", "TN", "VA"),
  ND = c("MN", "MT", "SD"),
  OH = c("IN", "KY", "MI", "PA", "WV"),
  OK = c("AR", "CO", "KS", "MO", "NM", "TX"),
  OR = c("CA", "ID", "NV", "WA", "AK"), #add {AK}
  PA = c("DE", "MD", "NJ", "NY", "OH", "WV"),
  RI = c("CT", "MA"),
  SC = c("GA", "NC"),
  SD = c("IA", "MN", "MT", "NE", "ND", "WY"),
  TN = c("AL", "AR", "GA", "KY", "MO", "MS", "NC", "VA"),
  TX = c("AR", "LA", "NM", "OK"),
  UT = c("AZ", "CO", "ID", "NV", "NM", "WY"),
  VT = c("MA", "NH", "NY"),
  VA = c("DC", "KY", "MD", "NC", "TN", "WV"),
  WA = c("ID", "OR","AK"), # add {AK}
  WV = c("KY", "MD", "OH", "PA", "VA"),
  WI = c("IA", "IL", "MI", "MN"),
  WY = c("CO", "ID", "MT", "NE", "SD", "UT")
)

# Initialize adjacency matrix
n <- length(states)
adjacency <- matrix(0, n, n, dimnames = list(states, states))

# Fill adjacency matrix based on adjacency list
for (state in names(adj_list)) {
  adjacency[state, adj_list[[state]]] <- 1
}

# Compute the degree matrix (D) where D[i, i] is the degree of state i
degree_matrix <- diag(rowSums(adjacency))

# Compute the Laplacian matrix (L) where L = D - A
L <- degree_matrix - adjacency
return(L)
}